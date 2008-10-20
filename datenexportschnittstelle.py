#!/usr/local/bin/python

""" Fix-up of SoftM EDI-Export files for Mosaic.

Created 31.08.2006 by Lars Ronge
"""

import os
import datetime
import logging
import shutil

sPathHuSoMosa = 'C:/Inhouse/huSoMosa'
output_dir = 'C:/Inhouse/INVOIC'
input_dir = 'Q:/OUT'

logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s %(levelname)s %(message)s',
                    filename=sPathHuSoMosa + '/logs/INVOIC_Log.txt',
                    filemode='a+')

logging.debug('Starting Python-Script')


if os.path.exists(output_dir + '/lock.lck'):
    logging.info('Die INVOIC aus SoftM koennen nicht exportiert werden, da im Pfad ' 
                 + output_dir + ' eine Lock-Datei lock.lck vorhanden ist')
elif os.path.exists(input_dir + '/lock.lck'):
    logging.info('Die INVOIC koennen nicht zu Mosaic exportiert werden, da im Pfad '
                 + input_dir + ' eine Lock-Datei lock.lck vorhanden ist')
else:
    # Lock-File in MOSAIC-Pfad schreiben
    lockfile_mosaic = file(output_dir + '/lock.lck', 'a+')
    lockfile_mosaic.write(str(datetime.datetime.now()) + ': Auslesen der INVOIC durch ' + __file__)
    lockfile_mosaic.close()
    
    # Lock-File in SoftM-Pfad schreiben
    lockfile_softm = file(input_dir + '/lock.lck', 'a+')
    lockfile_softm.write(str(datetime.datetime.now()) + ': Auslesen der INVOIC durch ' + __file__)
    lockfile_softm.close()
    
    # Dateien im INHOUSE-Verzeichnis auslesen
    s_aFilesInInhouseDir = os.listdir(input_dir)
    
    # Alle Dateien im Verzeichnis durchlaufen
    for sFileName in s_aFilesInInhouseDir:
        logging.debug('Ueberpruefung der folgenden Datei: ' + sFileName)
        
        # Ist es eine TXT-Datei?
        if sFileName.lower.endswith('.txt'):
            sFileNameBase = sFileName[:(len(sFileName) - 4)]
            
            # Ist es keine aktualisierte TXT-Datei?
            if not sFileName.lower.endswith('_updated.txt'):
                
                logging.debug("Die Datei " + input_dir + "/" + sFileName + " wird zum Lesen geoeffnet")
                iInputFile = file(input_dir + '/' + sFileName, 'r')
                logging.debug("Es wird versucht, die Datei " + sFileNameBase + "_UPDATED.txt anzulegen")
                iOutputFile = file(sPathHuSoMosa + '/tmp/' + sFileNameBase + '_UPDATED.txt', 'w')
                
                logging.debug("Die F1-Zeilen werden angepasst")
                
                # Alle Zeilen parsen 
                for sActualLine in iInputFile:
                    # Jeder Zeile ein Leerzeichen voranstellen
                    sActualLine = ' ' + sActualLine
                    
                    # Die F1-Zeilen anpassen
                    #########################
                    
                    # Abkommensnummer setzen
                    if sActualLine[20:22] == 'F1':
                        sActualLine = sActualLine[0:504] + "1" + sActualLine[505:]
                    
                    # Liefertermin setzen (falls nicht gesetzt  
                    if sActualLine[20:22] == 'F1':
                        sLiefertermin = sActualLine[70:78]
                        logging.debug('Liefertermin: ' + sLiefertermin)
                        if sLiefertermin == '00000000':
                            sLieferscheindatum = sActualLine[113:121]
                            logging.debug('Lieferscheindatum: ' + sLieferscheindatum)
                            sActualLine = sActualLine[0:70] + sLieferscheindatum + sActualLine[78:]
                        
                    # Ratio-Nummer aendern
                    if sActualLine[20:22] == 'F1':
                        if sActualLine[241:248] == '09504 6' or sActualLine[241:248] == '09504/6':
                            sActualLine = sActualLine[0:241] + '0950406' + sActualLine[248:]
                    
                    # Die Zeile in die UPDATE-Datei schreiben
                    iOutputFile.write(sActualLine)
                
                logging.debug("Schliessen der Dateien")
                iInputFile.close()
                iOutputFile.close()
                
                shutil.copy(os.path.join(sPathHuSoMosa, 'tmp', sFileNameBase + '_UPDATED.txt'), 
                            os.path.join(sPathHuSoMosa, 'backup/INVOIC', sFileNameBase + '_UPDATED.txt'))
                shutil.move(os.path.join(sPathHuSoMosa, 'tmp', sFileNameBase + '_UPDATED.txt'),
                            os.path.join(output_dir, + sFileNameBase + '_UPDATED.txt'))
                shutil.move(os.path.join(input_dir, sFileName),
                            os.path.join(sPathHuSoMosa, 'archive/INVOIC', sFileName))
                logging.info("Die Datei " + sFileName + " wurde verarbeitet")
    
    try: 
        os.remove(output_dir + '/lock.lck')
    except:
        logging.error('Lock-Datei konnte nicht entfernt werden: ' + output_dir + '/lock.lck')
      
    try:
        os.remove(input_dir + '/lock.lck')
    except:
        logging.error('Lock-Datei konnte nicht entfernt werden: ' + input_dir + '/lock.lck')
