#!/usr/bin/env python
# encoding: utf-8
"""
fivethousandandone.py - a proxy server for SoftM's obscure printing server thingy

Created by Christian Klein on 2010-10-20.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

import sys
import asyncore
import logging
import logging.handlers
import optparse
import os
import socket
import tempfile
import uuid
import xml.etree.ElementTree as ET
from cStringIO import StringIO


DIRECTORY = os.path.join(tempfile.gettempdir(), 'SMPrintserver')


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
                'kundennr': ('APP00E01', 'PPKDNR'),
            }
    
    def __init__(self, directory):
        self.directory = directory
        if not os.path.exists(directory):
            os.makedirs(directory)
    
    def get_value(self, tree, name, default=None):
        """Get a specific value from the tree"""
        
        tag, attribute = self.xpaths[name]
        element = tree.find(tag)
        if not element is None:
            return element.get(attribute)
        return default
    
    def parse(self, data):
        """Parse SoftM XML data"""
        
        tree = ET.fromstring(data.decode('iso8859-1').encode('utf-8'))
        
        belegtyp = self.get_value(tree, 'belegtyp')
        auftragsnr = self.get_value(tree, 'auftragsnr')
        kundennr = self.get_value(tree, 'kundennr', '')
        
        # LH[#455]: In Datei schreiben
        
        if belegtyp and auftragsnr:
            filename =  '%s-%s.xml' % (belegtyp, auftragsnr)
        else:
            filename = 'unknown-%s.xml' % uuid.uuid1()
        
        output = open(os.path.join(self.directory, filename), 'w')
        output.write(ET.tostring(tree))
        output.close()
        
        if kundennr.strip() == '66669':
            return False
        return True


class PrintserverClient(asyncore.dispatcher):
    """Sends the received data to the actual server"""
    
    def __init__(self, clientsock, remote, data):
        asyncore.dispatcher.__init__(self)
        self.clientsock = clientsock
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        address = parse_address(remote)
        self.connect(address)
        self.buffer = "%08d" % len(data) + data
    
    def handle_connect(self):
        pass
    
    def writable(self):
        return (len(self.buffer) > 0)

    def handle_write(self):
        sent = self.send(self.buffer)
        self.buffer = self.buffer[sent:]
    
    def handle_close(self):
        self.close()
    
    def handle_read(self):
        """Read data from server and push it back to client"""
        data = self.recv(4096)
        self.clientsock.write(data)


def respond(response):
    """Write response and exit"""
    sys.stdout.write(unicode(response).encode('iso8859-1'))
    sys.exit()


def serve(options):
    """The server main loop"""
    
    logger = logging.getLogger('SoftM Printserver')
    
    # Lese zuerst die Länge der folgenden Nachricht
    length = sys.stdin.read(8)
    if len(length) != 8:
        respond(u"<SMXML00000064>*ERROR SockSrvUngültiger Prozeduraufruf oder ungültiges Argument")
    try:
        length = int(length)
    except ValueError:
        respond(u"<SMXML00000064>*ERROR SockSrvUngültiger Prozeduraufruf oder ungültiges Argument")
    
    # Lese die Nutzlast der Nachricht
    data = sys.stdin.read(length)
    # kann es passieren, dass weniger als `length` Bytes gelesen werden?
    
    # Der XML-Parser entscheidet, ob der Request weitergeleitet wird.
    parser = CrazySoftMXMLParser(DIRECTORY)
    
    try:
        should_forward = parser.parse(data)
    except Exception, exc:
        logger.error("Error while parsing XML: %s" % exc)
        respond(u"<SMXML00000064>*ERROR SockSrvUngültiger Prozeduraufruf oder ungültiges Argument")
    
    if should_forward:
        logger.debug("Open socket to SoftM Printserver...")        
        client = PrintserverClient(sys.stdout, options.remote, data)
        asyncore.loop()
    else:
        logger.debug("Open socket to SoftM Printserver...")
        respond(u"<SMXML00000009>*OK   *OK")


def main():
    """Main Entry Point"""
    
    parser = optparse.OptionParser()
    parser.add_option('-d', '--directory', default=DIRECTORY, help='Log directory [default: %default]')
    parser.add_option('-r', '--remote', default='192.168.112.199:5001', help='Address of remote server [default: %default]')
    parser.add_option('-q', '--quiet', action="store_true", default=False, help="don't print status messages to stdout")
    parser.add_option('-v', '--verbose', action="store_true", default=False, help="Be very verbose")
    
    options, args = parser.parse_args()

    if options.verbose:
        loglevel = logging.DEBUG
    elif options.quiet:
        loglevel = logging.ERROR
    else:
        loglevel = logging.INFO
    
    # So richtig versteh ich dieses logging-Gedöhns ja nicht...
    
    handler = logging.handlers.SysLogHandler('/dev/log')
    handler.setFormatter(logging.Formatter('%(name)s: %(message)s'))
    handler.setLevel(loglevel)
    
    logger = logging.getLogger('SoftM Printserver')
    logger.addHandler(handler)
    logger.setLevel(loglevel)
        
    serve(options)


if __name__ == '__main__':
    main()
