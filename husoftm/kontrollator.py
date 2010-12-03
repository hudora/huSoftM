#!/usr/bin/env python
# encoding: utf-8
"""
kontrollator.py - Spüre voll ausgelieferte Aufträge ohne Rechnungen auf.

Created by Daniel Lerose and Christian Klein  on 2010-11-10.
Copyright (c) 2010 HUDORA. All rights reserved.
"""

import datetime
import husoftm.rechnungen
import husoftm.auftraege
import os


def check_rechnung(auftragsnr):
    rechnungen = husoftm.rechnungen.auftragsnr_to_rechnungsnr(auftragsnr)
    return bool(rechnungen)


def main():
    
    os.environ['HUSOFTM_TAG'] = 'husoftm.kontrollator.py'
    
    tmp = datetime.date.today() - datetime.timedelta(days=7)
    auftraege = husoftm.auftraege.auftraege(mindate=tmp, maxdate=datetime.date.today())
    for auftrag in auftraege:
        if auftrag['voll_ausgeliefert'] != 1:
            continue
        
        if check_rechnung(auftrag['auftragsnr']) == False:
            print "Keine Rechnung fuer Auftrag %s" % auftrag['auftragsnr']


if __name__ == "__main__":
    main()