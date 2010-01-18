#!/usr/bin/env python
# encoding: utf-8
""" versandart.py --- Describe Tool here äöü---
"""

# versandart.py
# Created by Christoph Borgolte on 15-01-2010 for HUDORA.
# Copyright (c) 2009 HUDORA. All rights reserved.

from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote

def versandart(vart, anwendung='A'):
    """Gibt die Kurz- und Bezeichnung einer Versandart wieder.

    >>> husoftm.versandart.versandart(11)
    ('DPD/Danzas EXW', 'DPD/Danzas unfrei/EXW')
    >>> husoftm.versandart.versandart(14)
    ('Cretschmar', 'Cretschmar Cargo')
    
    """

    # FIXME Hier ist noch einiges unklar. Es gibt beispielsweise zwei Versandarten '14':
    #   -('014', 'A', 'Cretschmar', 'Cretschmar Cargo', ''),
    #   -('014', 'A', u'K\xfchne & Nagel', u'K\xfchne & Nagel', ''),
    # dh. der eindeutige Key ist nicht ganz klar. Es steht auch keins der beiden auf gelöscht (satzstatus 'X')

    vart = "%03d" % int(vart)
    condition="TYANW = %s AND TYVSAR = %s" % (sql_quote(anwendung), sql_quote(vart))
    rows = get_connection().query('XTY00', condition=condition)
    if len(rows) == 0:
        return "", ""

    if len(rows) > 1:
        print "Achtung: mehr als eine passende Versandart:", rows
    return rows[0]['kurzbezeichnung'], rows[0]['bezeichnung']

