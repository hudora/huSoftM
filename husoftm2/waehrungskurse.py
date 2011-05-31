#!/usr/bin/env python
# encoding: utf-8
"""
waehrungskurse.py - Funktionen zu Währungskursen in SoftM

Created by Christian Klein on 2011-05-31.
Copyright (c) 2011 HUDORA GmbH. All rights reserved.
"""
from husoftm2.backend import query
from husoftm2.tools import date2softm, sql_quote


def get_waehrungskurs(date, currency='USD'):
    """Frage zum Stichtag gültigen Währungskurs aus SoftM ab"""

    conditions = ['TKDTST<=%s' % date2softm(date),
                  'TKWSL=%s' % sql_quote(currency)]
    rows = query(['XTK00'], condition=' AND '.join(conditions), ordering='TKDTST DESC', limit=1,
                  ua='husoftm2.waehrungskurse')
    if rows:
        return rows[0]
