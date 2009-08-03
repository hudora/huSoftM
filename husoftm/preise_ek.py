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
    
    ACHTUNG: erwartet eine Liste von Artikelnummern. In der regel will man die Preise
    für alle Versionen auf einmal ermitteln.
    
    >>> pprint(preisentwicklung(['14600', '14600/00', '14600/01']))
    [(datetime.date(2004, 12, 16),
      datetime.date(2005, 4, 12),
      '14600',
      Decimal("16.10")),
     (datetime.date(2005, 4, 13),
      datetime.date(2006, 3, 22),
      '14600',
      Decimal("16.60")),
     (datetime.date(2006, 3, 23),
      datetime.date(9999, 12, 31),
      '14600',
      Decimal("17.43")),
     (datetime.date(2006, 9, 19),
      datetime.date(9999, 12, 31),
      '14600/01',
      Decimal("17.43"))]
    """
    
    artnrsstr = ','.join([sql_quote(artnr) for artnr in artnrs])
    rows = get_connection().query(['XPN00', 'XPR00'], ordering='PRDTVO',
        condition=("PNSANR=PRSANR and PRANW='E' and PRSTAT=' ' and PNSTAT=' '"
                   " and PRARTN IN (%s)" % artnrsstr))
    return [(row['gueltig_ab_date'],
             row['gueltig_bis_date'],
             row['artnr'],
             (Decimal(str(row['preis'])).quantize(Decimal(10) ** -2))) for row in rows]
    

if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    sys.exit(failure_count)
