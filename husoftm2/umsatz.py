#!/usr/bin/env python
# encoding: utf-8
"""
umsatz.py
TODO: mit kunden.py zusammenlegen

Created by Christian Klein on 2011-03-25.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

raise NotImplementedError("Dieses Modul sollte nciht emhr verwendet werden")


import datetime
import decimal
import huTools.world


from husoftm2.tools import pad, remove_prefix, softm2date, date2softm, sql_quote
from husoftm2.backend import query, as400_2_int
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


def artikel_mengenumsatz_range(artnr, startdate, enddate):
    """Liefert die FakturirtenUmsatz-MEngen für den Interval [startdate; enddata[

    >>> artikel_mengenumsatz_range('10101', datetime.date(2009, 6, 2), datetime.date(2009, 6, 5))
    {datetime.date(2009, 6, 2): 500, datetime.date(2009, 6, 4): 1000, datetime.date(2009, 6, 3): 0}
    """

    conditions = ['FUDTRI>=%s' % date2softm(startdate),
                  'FUDTRI<%s' % date2softm(enddate),
                  'FUARTN=%s' % sql_quote(artnr),
                 ]

    rows = query(tables=['AFU00'], fields=['SUM(FUMNG)', 'FUDTRI'], grouping=['FUDTRI'],
             querymappings={'FUDTRI': 'tag', 'SUM(FUMNG)': 'menge'},
             condition=' AND '.join(conditions), ua='husoftm2.umsatz', cachingtime=86400)
    ret = {}
    for row in rows:
        ret[softm2date(row['tag'])] = as400_2_int(row['menge'])
    return ret
