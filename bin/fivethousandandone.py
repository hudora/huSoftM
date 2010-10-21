#!/usr/bin/env python
# encoding: utf-8
"""
fivethousandandone.py - a proxy server for SoftM's obscure printing server thingy

Created by Christian Klein on 2010-10-20.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

import asyncore
import logging
import optparse
import os
import socket
import xml.etree.ElementTree as ET
from cStringIO import StringIO



def parse_address(addr):
    """
    Parse a IPv4 address and port
    
    >>> parse_address('localhost')
    ('localhost', 0)
    >>> parse_address('www.norad.mil:1337')
    ('www.norad.mil', 1337)
    """
    tmp = addr.split(':')
    if len(tmp) == 1:
        return (tmp[0], 0)
    try:
        port = int(tmp[1])
    except ValueError:
        port = 0
    return (tmp[0], port)


class CrazySoftMXMLParser(object):
    """Parser für die verrückten SoftM XML-Daten"""
    
    # No real XPath support in ElementTree
    xpaths = {
                'belegtyp': ('XCP00E05', 'C0DTYP'),
                'auftragsnr': ('APP00E01', 'PPAUFN'),
            }
    
    def __init__(self, directory):
        self.directory = directory
        if not os.path.exists(directory):
            os.makedirs(directory)
    
    def get_value(self, tree, name):
        """Get a specific value from the tree"""
        
        tag, attribute = self.xpaths[name]
        element = tree.find(tag)
        if not element is None:
            return element.get(attribute)
    
    def parse(self, fileobj):
        """Parse SoftM XML data"""
        
        tree = ET.parse(fileobj)
        
        belegtyp = self.get_value(tree, 'belegtyp')
        auftragsnr = self.get_value(tree, 'auftragsnr')
        
        # LH[#455]: In Datei schreiben
        filename = os.path.join(self.directory, '%s-%s.xml' % (belegtyp, auftragsnr))
        # with open(filename, 'w') as output
        output = open(filename, 'w')
        output.write(ET.tostring(tree))
        output.close()


class FiveServer(asyncore.dispatcher):
    """
    Receives connections and establishes handlers for each client.
    """
    
    def __init__(self, options):
        self.logger = logging.getLogger('FivethousandAndOne')
        
        self.options = options
        if options.verbose:
            loglevel = logging.DEBUG
        elif options.quiet:
            loglevel = logging.ERROR
        else:
            loglevel = logging.INFO
        self.logger.setLevel(loglevel)
        
        asyncore.dispatcher.__init__(self)
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.set_reuse_addr()
        self.bind((options.address, options.port))
        self.address = self.socket.getsockname()
        self.logger.debug('binding to %s', self.address)
        self.listen(5)
        self.xmlparser = CrazySoftMXMLParser()

    def parse(self, data):
        """Parse data received by FiveReceiver"""
        
        self.xmlparser.parse(data)

    def handle_accept(self):
        """Handle a connection request"""
        
        client_info = self.accept()
        self.logger.debug('handle_accept() -> %s', client_info[1])
        FiveReceiver(client_info[0], self, self.options.remote)


class FiveReceiver(asyncore.dispatcher):
    """Receive data from a client and forward it to the actual server"""
    
    def __init__(self, sock, server, remote):
        self.chunk_size = 256
        self.logger = logging.getLogger('FiveReceiver%s' % str(sock.getsockname()))
        asyncore.dispatcher.__init__(self, sock=sock)
        self.server = server
        self.data = StringIO()
        self.data_len = -1
        self.lenbuf = ''
        
        # open client sock to printserver...
        remote_address = parse_address(remote)
        self.sender = FiveSender(self, remote_address)

    def handle_read(self):
        """Read an incoming message from the client and put it into our outgoing queue."""
        
        data = self.recv(self.chunk_size)
        if not data:
            return
        
        self.sender.send(data)
        
        if self.data_len == -1:
            while data[0].isdigit():
                self.lenbuf += data[0]
                data = data[1:]
            if len(self.lenbuf) == 8:
                self.data_len = int(self.lenbuf)

        self.data.write(data)
        
        # Wenn alle Daten gelesen sind, kann die XML-Parserei losgehen
        if self.data.tell() == self.data_len:
            self.server.parse(self.data)


class FiveSender(asyncore.dispatcher):
    """Sends the received data to the actual server"""
    
    def __init__(self, receiver, address):
        asyncore.dispatcher.__init__(self)
        self.receiver = receiver
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        self.buffer = ''
        self.connect(address)
    
    def handle_read(self):
        """Read data from server and push it back to client"""
        data = self.recv(256)
        self.receiver.send(data)


def main():
    """Main Entry Point"""
    
    parser = optparse.OptionParser()
    parser.add_option('-b', '--bind', dest='address', default='0.0.0.0', help='Address to bind to [default: %default]')
    parser.add_option('-p', '--port', default=5001, type='int', help='TCP port [default: %default]')
    parser.add_option('-r', '--remote', default='172.28.4.98:5001', help='Address of remote server [default: %default]')
    parser.add_option('-q', '--quiet', action="store_true", default=False, help="don't print status messages to stdout")
    
    options, args = parser.parse_args()

    logging.basicConfig(level=logging.DEBUG, format='%(name)s: %(message)s')
    server = FiveServer(options)
    asyncore.loop()

if __name__ == '__main__':
    main()
