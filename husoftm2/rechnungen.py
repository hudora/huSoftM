#!/usr/bin/env python
# encoding: utf-8
"""
rechnungen.py

Created by Christian Klein on 2010-12-13.
Copyright (c) 2010 HUDORA. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import date2softm, add_prefix, remove_prefix


def get_rechnung_by_date(startdate, enddate=None, limit=None):
    """Gib eine Liste von Rechnungsnummern zurück, die zwischen startdate und enddate erzeugt wurden."""

    conditions = ["FKSTAT <> 'X'",
                  "FKRGNR <> 0",
                  "FKDTER > %s" % date2softm(startdate)]
    if enddate:
        conditions.append("FKDTER <= %s" % date2softm(enddate))

    rows = query(tables=['AFK00'], condition=' AND '.join(conditions), fields=['FKRGNR', 'FKDTER'],
                 ordering=['FKDTER'], limit=limit, ua='husoftm2.rechnungen')
    rechnungsnr = []
    for row in rows:
        rechnungsnr.append("RG%s" % row['rechnungsnr'])
    return rechnungsnr


def get_rechnung(rechnungsnr):
    """Findet eine Rechnung anhand ihrer Rechnungsnr"""

    conditions = ["FKSTAT <> 'X'",
                  "FKRGNR = %d" % remove_prefix(rechnungsnr, 'RG')]
    rows = query(tables=['AFK00'], condition=' AND '.join(conditions), limit=1, ua='husoftm2.rechnungen')
    if not rows:
        return
    rechnung = rows[0]

    # Rechnungspositionen
    conditions = ["FURGNR <> 'X'"]
    rows = query(tables=['AFU00'], condition='FURGNR = %d' % remove_prefix(rechnungsnr, 'RG'),
                 ua='husoftm2.rechnungen')

    rechnung['positionen'] = rows

    # Evtl. in Funktion auslagern wie in husoftm2.kunden:
    rechnung['rechnungsnr'] = add_prefix(rechnung['rechnungsnr'], 'RG')
    rechnung['auftragsnr'] = add_prefix(rechnung['auftragsnr'], 'SO')
    rechnung['kundennr_rechnungsempfaenger'] = add_prefix(rechnung['kundennr_rechnungsempfaenger'], 'SC')
    rechnung['kundennr_warenempfaenger'] = add_prefix(rechnung['kundennr_warenempfaenger'], 'SC')
    if rechnung['verbandsnr']:
        rechnung['verbandsnr'] = add_prefix(rechnung['verbandsnr'], 'SC')

    return rechnung
