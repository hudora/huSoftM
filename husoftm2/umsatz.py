#!/usr/bin/env python
# encoding: utf-8
"""
umsatz.py

Created by Christian Klein on 2011-03-25.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

import datetime
import decimal
import huTools.world


from husoftm2.tools import pad, remove_prefix
from husoftm2.backend import query
import husoftm2.kunden


def get_umsatz(kundennr, jahr):
    """Ermittle den Umsatz für den Kunden im gegebenen Jahr"""

    if isinstance(jahr, datetime.date):
        jahr = jahr.year
    elif isinstance(jahr, basestring):
        jahr = int(jahr)

    # Das ist deutlich schneller als ein IN-Operator in der eigentlichen Abfrage
    kunde = husoftm2.kunden.get_kunde(kundennr)
    if kunde['land'] == 'DE':
        ktonr = 85100
    elif huTools.world.in_european_union(kunde['land']):
        ktonr = 82100
    else:
        ktonr = 81100

    conditions = ['BUJJBU=%d' % (jahr - 2000),  # Keinen Kommentar zu SoftM!
                  'BUGKTO=%s' % pad('BUGKTO', ktonr),
                  'AKAUFN=BUAUFN',
                  'AKKDNR=%s' % pad('AKKDNR', remove_prefix(kundennr, 'SC')),
                 ]

    rows = query(tables=['BBU00', 'AAK00'], fields=['BUSOHA', 'SUM(BUNEBT)'], grouping=['BUSOHA'],
                 querymappings={'BUSOHA': 'art', 'SUM(BUNEBT)': 'umsatz'},
                 condition=' AND '.join(conditions), ua='husoftm2.umsatz.get_umsatz', cachingtime=86400)

    umsatz = decimal.Decimal()
    for row in rows:
        if row['art'] == 'H':
            umsatz -= decimal.Decimal(row['umsatz'])
        elif row['art'] == 'S':
            umsatz += decimal.Decimal(row['umsatz'])
    return umsatz
