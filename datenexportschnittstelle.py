#!/usr/local/bin/python
# encoding: utf-8

""" Fix-up of SoftM EDI-Export files for Mosaic.

Created 31.08.2006 by Lars Ronge.

Needs editing of paths below.
"""

# this module reimplements some stuff from huTools to avoid runtime dependencies.

import os
import os.path
import logging
import shutil
import time

def makedirhier(dirnmame):
    """Created 'dirname' if needes and all intermediate directories."""
    
    if not os.path.exists(dirnmame):
        os.makedirs(dirnmame)
    

def rewrite_records(inputfile, outputfile):
    """Reads Data confirming to SoftM Datenausgabeschnitstelle from inputfile,
       transforms it and writes it to outputfile."""
    
    outbuf = []
    # Alle Zeilen parsen 
    for line in inputfile:
        # Jeder Zeile ein Leerzeichen voranstellen und newline am Ende entfernen
        line = ' ' + line.rstrip()
        
        if line[20:22] != 'F1':
            # unbekannte Anpassung - Abkommensnummer?
            line = line[0:504] + "1" + line[505:]
            
            # Liefertermin auf Lieferscheindatum setzen (falls nicht gesetzt)
            if line[20:22] == 'F1':
                liefertermin = line[70:78]
                if liefertermin == '00000000':
                    lieferscheindatum = line[113:121]
                    line = line[0:70] + lieferscheindatum + line[78:]
                    logging.info('Liefertermin auf Lieferscheindatum %s gesetzt' % lieferscheindatum)
            
            # Ratio-Nummer aendern
            if line[241:248] in ['09504 6', '09504/6']:
                line = line[0:241] + '0950406' + line[248:]
                logging.info("Ratio-Nummer geaendert")
        outbuf.append(line)
    
    # Write all outut at once
    outputfile.write('\n'.join(outbuf))
    

def handle_file(inpath, outputdir, workdir):
    """Transform a file by changing some records.
    
    This function handles the moving arround of the file."""
    
    filename = os.path.split(inpath)[1]
    guid = "%d-%d" % (os.getpid(), int(time.time()*1000))
    updated_filename = '%s_UPDATED.txt' % os.path.splitext(filename)[0]
    tmp_filename = '%s_UPDATED%s.txt' % (os.path.splitext(filename)[0], guid)
    makedirhier(os.path.join(workdir, 'tmp'))
    tmppath = os.path.join(os.path.join(workdir, 'tmp', tmp_filename))
    logging.debug('Verarbeitung %r -> %r' % (inpath, tmppath))
    inputfile = file(inpath, 'r')
    outputfile = file(tmppath, 'w')
    
    logging.debug("Die F1-Zeilen werden angepasst")
    
    rewrite_records(inputfile, outputfile)
    
    inputfile.close()
    outputfile.close()
    
    # archive original file
    makedirhier(os.path.join(workdir, 'archive/INVOIC'))
    shutil.move(inpath, os.path.join(workdir, 'archive/INVOIC', filename))
    # archive updated file
    makedirhier(os.path.join(workdir, 'backup/INVOIC'))
    shutil.copy(tmppath, os.path.join(workdir, 'backup/INVOIC', updated_filename))
    # move file to destination
    makedirhier(outputdir)
    shutil.move(tmppath, os.path.join(outputdir, updated_filename))
    
    logging.info("Die Datei " + filename + " wurde verarbeitet")


def main():
    """Main function to be called by cron."""
    workdir = 'C:/Inhouse/huSoMosa'
    outputdir = 'C:/Inhouse/INVOIC'
    inputdir = 'Q:/OUT'
    
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s %(levelname)s %(message)s',
                        filename=os.path.join(workdir, 'INVOIC_Log.txt'),
                        filemode='a+')
    
    logging.debug("Verarbeite %r nach %r. Logs in %r" % (inputdir, outputdir, workdir))
    # Alle Dateien im Verzeichnis durchlaufen
    for filename in os.listdir(inputdir):
        if filename.lower().endswith('.txt'):
            handle_file(os.path.join(inputdir, filename), outputdir, workdir)

main()

