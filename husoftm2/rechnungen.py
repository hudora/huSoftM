#!/usr/bin/env python
# encoding: utf-8
"""
rechnungen.py

Created by Christian Klein on 2010-12-13.
Copyright (c) 2010 HUDORA. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import date2softm


def get_rechnung_by_date(startdate, enddate=None, limit=None):
    """Gib eine Liste von Rechnungsnummern zurück, die """
    
    conditions = ["FKSTAT <> 'X'", "FKRGNR <> 0", "FKDTER > %s" % date2softm(startdate)]    
    if enddate:
        conditions.append("FKDTER <= %s" % date2softm(enddate))

    rows = query(tables=['AFK00'], condition=' AND '.join(conditions), fields=['FKRGNR', 'FKDTER'],
                 ordering=['FKDTER'], limit=limit, ua='husoftm2.rechnungen')
    rechnungsnr = []
    for row in rows:
        rechnungsnr.append("RG%s" % row['rechnungsnr'])
    return rechnungsnr
