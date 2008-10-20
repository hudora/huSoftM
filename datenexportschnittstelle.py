# Dieses Skript parst einen Text
# Author:  Lars Ronge
# Version: 0.1
# Date:    31.08.2006

import sys
import os
import datetime
import time
import logging
import shutil
import string
# import mx.ODBC, mx.ODBC.Windows

sPathHuSoMosa       = 'C:/Inhouse/huSoMosa'
sPathINVOICMosaic   = 'C:/Inhouse/INVOIC'
# sPathINVOICMosaic   = 'C:/Inhouse/INVOIC_TEST'
sPathINVOICSoftM    = 'Q:/OUT'

logging.basicConfig(level=logging.DEBUG,
                    format='%(asctime)s %(levelname)s %(message)s',
                    filename=sPathHuSoMosa + '/logs/INVOIC_Log.txt',
                    filemode='a+')

logging.debug('Starting Python-Script')


if os.path.exists(sPathINVOICMosaic + '/lock.lck'):
    logging.info('Die INVOIC aus SoftM koennen nicht exportiert werden, da im Pfad ' + sPathINVOICMosaic + ' eine Lock-Datei lock.lck vorhanden ist')
elif os.path.exists(sPathINVOICSoftM + '/lock.lck'):
    logging.info('Die INVOIC koennen nicht zu Mosaic exportiert werden, da im Pfad ' + sPathINVOICSoftM + ' eine Lock-Datei lock.lck vorhanden ist')
else:
    # Lock-File in MOSAIC-Pfad schreiben
    lockfileMOSAIC = file(sPathINVOICMosaic + '/lock.lck', 'a+')
    lockfileMOSAIC.write(str(datetime.datetime.now()) + ': Auslesen der INVOIC durch ' + __file__)
    lockfileMOSAIC.close()
    
    # Lock-File in SoftM-Pfad schreiben
    lockfileSoftM = file(sPathINVOICSoftM + '/lock.lck', 'a+')
    lockfileSoftM.write(str(datetime.datetime.now()) + ': Auslesen der INVOIC durch ' + __file__)
    lockfileSoftM.close()
    
    # Dateien im INHOUSE-Verzeichnis auslesen
    s_aFilesInInhouseDir = os.listdir(sPathINVOICSoftM)
    
    # Alle Dateien im Verzeichnis durchlaufen
    for sFileName in s_aFilesInInhouseDir:
        logging.debug ('Ueberpruefung der folgenden Datei: ' + sFileName)
        
        # Ist es eine TXT-Datei?
        if string.lower(sFileName[(len(sFileName) - 3):]) == 'txt':
            sFileNameBase = sFileName[:(len(sFileName) - 4)]
            
            # Ist es keine aktualisierte TXT-Datei?
            if string.lower(sFileName[(len(sFileName) - 12):]) <> '_updated.txt':
            
                logging.debug ("Die Datei " + sPathINVOICSoftM + "/" + sFileName + " wird zum Lesen geoeffnet")
                iInputFile = file(sPathINVOICSoftM + '/' + sFileName, 'r')
                logging.debug ("Es wird versucht, die Datei " + sFileNameBase + "_UPDATED.txt anzulegen")
                iOutputFile = file(sPathHuSoMosa + '/tmp/' + sFileNameBase + '_UPDATED.txt', 'w')
                
                logging.debug ("Die F1-Zeilen werden angepasst")
                
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
                        logging.debug ('Liefertermin: ' + sLiefertermin)
                        if sLiefertermin == '00000000':
                            sLieferscheindatum = sActualLine[113:121]
                            logging.debug ('Lieferscheindatum: ' + sLieferscheindatum)
                            sActualLine = sActualLine[0:70] + sLieferscheindatum + sActualLine[78:]
                        
                    # Ratio-Nummer aendern
                    if sActualLine[20:22] == 'F1':
                        if sActualLine[241:248] == '09504 6' or sActualLine[241:248] == '09504/6':
                            sActualLine = sActualLine[0:241] + '0950406' + sActualLine[248:]
                    
                    # Die Zeile in die UPDATE-Datei schreiben
                    iOutputFile.write(sActualLine)
                
                logging.debug ("Schliessen der Dateien")
                iInputFile.close()
                iOutputFile.close()
                
                shutil.copy(sPathHuSoMosa + '/tmp/' + sFileNameBase + '_UPDATED.txt', sPathHuSoMosa + '/backup/INVOIC/' + sFileNameBase + '_UPDATED.txt')
                shutil.move(sPathHuSoMosa + '/tmp/' + sFileNameBase + '_UPDATED.txt', sPathINVOICMosaic + '/' + sFileNameBase + '_UPDATED.txt')
                shutil.move(sPathINVOICSoftM + '/' + sFileName, sPathHuSoMosa + '/archive/INVOIC/' + sFileName)
                logging.info("Die Datei " + sFileName + " wurde verarbeitet")
    
    try: 
        os.remove(sPathINVOICMosaic + '/lock.lck')
    except:
        logging.error('Lock-Datei konnte nicht entfernt werden: ' + sPathINVOICMosaic + '/lock.lck')
      
    try:
        os.remove(sPathINVOICSoftM + '/lock.lck')
    except:
        logging.error('Lock-Datei konnte nicht entfernt werden: ' + sPathINVOICSoftM + '/lock.lck')
