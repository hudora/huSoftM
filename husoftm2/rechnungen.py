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
                  "FKRGNR = %s" % remove_prefix(rechnungsnr, 'RG')]
    rows = query(tables=['AFK00'], condition=' AND '.join(conditions), limit=1, ua='husoftm2.rechnungen')
    if rows:
        # evtl. in Funktion auslagern wie in husoftm2.kunden
        rows[0]['rechnungsnr'] = add_prefix(rows[0]['rechnungsnr'], 'RG')
        rows[0]['auftragsnr'] = add_prefix(rows[0]['auftragsnr'], 'SO')
        rows[0]['kundennr_rechnungsempfaenger'] = add_prefix(rows[0]['kundennr_rechnungsempfaenger'], 'SC')
        rows[0]['kundennr_warenempfaenger'] = add_prefix(rows[0]['kundennr_warenempfaenger'], 'SC')
        if rows[0]['verbandsnr']:
            rows[0]['verbandsnr'] = add_prefix(rows[0]['verbandsnr'], 'SC')
        return rows[0]
