#!/usr/bin/env python
# encoding: utf-8
"""
husoftm/lieferanten.py - zugriff auf Lieferantenstammdaten.

Created by Maximillian Dornseif on 2009-08-22.
Copyright (c) 2009 HUDORA. All rights reserved.
"""

import doctest
import sys
from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote


def name(lieferantennr):
    """Liefert den Namen zu einer Lieferantennummer zurück.
    
    >>> name('99999')
    'Blocklieferant'
    """
    assert isinstance(lieferantennr, basestring)
    
    rows = get_connection().query(['XXA00'],
        condition=("XASTAT=' ' and XALINR LIKE %s" % sql_quote('%%%s' % lieferantennr)))
    if rows:
        return rows[0]['name1']
    return None
    

def alle_lieferanten():
    """Gibt alle Lieferantennummer zurück.
    
    >>> alle_lieferanten() #doctest: +ELLIPSIS
    ['40000', '50000', '50001', ... '99999']
    """
    
    rows = get_connection().query(['XXA00'], fields=['XALINR'], condition=("XASTAT=' ' and XALINR<>''"))
    return sorted([x[0] for x in rows])
    

if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    sys.exit(failure_count)
