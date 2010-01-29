#!/usr/bin/env python
# encoding: utf-8
""" versandart.py

Created by Christoph Borgolte on 15-01-2010 for HUDORA.
Copyright (c) 2010 HUDORA. All rights reserved.
"""

from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote


def versandart(vart, anwendung='A'):
    """Gibt die Kurz- und Bezeichnung einer Versandart wieder.

    >>> versandart(11)
    ('DPD/Danzas EXW', 'DPD/Danzas unfrei/EXW')
    >>> versandart(14)
    ('Cretschmar', 'Cretschmar Cargo')
    
    """
    vart = "%03d" % int(vart)
    condition = "TYFNR <> '' AND TYANW = %s AND TYVSAR = %s" % (sql_quote(anwendung), sql_quote(vart))
    rows = get_connection().query('XTY00', condition=condition)
    if len(rows) == 0:
        return "", ""

    if len(rows) > 1:
        print "Achtung: mehr als eine passende Versandart:", rows
    return rows[0]['kurzbezeichnung'], rows[0]['bezeichnung']


if __name__ == "__main__":
    import doctest
    import sys
    failure_count, test_count = doctest.testmod()

    #for i in range(23):
        #print i, versandart(i)

    sys.exit(failure_count)
