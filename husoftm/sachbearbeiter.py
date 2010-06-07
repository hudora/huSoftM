#!/usr/bin/env python
# encoding: utf-8
"""
sachbearbeiter.py - Teil von huSoftM.

Created by Christian Klein on 2010-03-25.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

from husoftm.connection2 import get_connection
import cs.caching as caching
from husoftm.tools import sql_quote

def name(sachbearbeiternr):
    """Returns the name"""
    
    rows = get_connection().query('XSB00', fields=['SBNAME'], condition="SBSBNR = %s" % sql_quote(sachbearbeiternr))
    if len(rows) == 0:
        raise ValueError("Kein Sachbearbeiter mit Nummer %s" % sachbearbeiternr)
    return rows[0][0]


def _selftest():
    """Test basic functionality"""
    pass

if __name__ == '__main__':
    _selftest()
