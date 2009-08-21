#!/usr/bin/env python
# encoding: utf-8
"""
preise_ek.py Einkaufspreise aus SoftM.

Keep in mind that the data provided by this module is considered a trade secret.

Created by Maximillian Dornseif on 2009-08-02.
Copyright (c) 2009 HUDORA. All rights reserved.
"""


import doctest
import sys
from decimal import Decimal
from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote
from pprint import pprint # used for doctests


def preisentwicklung(artnrs):
    """Ermittelt alle Preise für eine Liste von Artikelnummern.
    
    ACHTUNG: erwartet eine Liste von Artikelnummern. In der Regel will man die Preise
    für alle Versionen auf einmal ermitteln.
    
    >>> pprint(preisentwicklung(['77068', '77068/00'])[:4])
    [(datetime.date(2005, 9, 1),
      datetime.date(2006, 7, 3),
      '77068',
      (Decimal('0.06'), 'USD')),
     (datetime.date(2006, 7, 4),
      datetime.date(2007, 7, 24),
      '77068',
      (Decimal('0.07'), 'USD')),
     (datetime.date(2007, 7, 4),
      datetime.date(2008, 3, 31),
      '77068/00',
      (Decimal('0.07'), 'USD')),
     (datetime.date(2007, 7, 24),
      datetime.date(2008, 3, 31),
      '77068',
      (Decimal('0.12'), 'USD'))]
    """
    
    # common error: providing a string, not a list
    assert not isinstance(artnrs, basestring)
    
    artnrsstr = ','.join([sql_quote(artnr) for artnr in artnrs])
    rows = get_connection().query(['XPN00', 'XPR00'], ordering='PRDTVO',
        condition=("PNSANR=PRSANR and PRANW='E' and PRSTAT=' ' and PNSTAT=' '"
                   " and PRARTN IN (%s)" % artnrsstr))
    return [(row['gueltig_ab_date'],
             row['gueltig_bis_date'],
             row['artnr'],
             ((Decimal(str(row['preis'])).quantize(Decimal(10) ** -2)),
             row['waehrung'])) for row in rows]
    

if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    sys.exit(failure_count)
