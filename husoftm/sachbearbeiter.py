#!/usr/bin/env python
# encoding: utf-8
"""
sachbearbeiter.py - Teil von huSoftM.

Created by Christian Klein on 2010-03-25.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote
import warnings


warnings.warn("husoftm.sachbearbeiter is deprecated, use husoftm2.sachbearbeiter instead",
              DeprecationWarning, stacklevel=2)


def name(sachbearbeiternr):
    """Returns the name"""

    rows = get_connection().query('XSB00', fields=['SBNAME'], condition="SBSBNR = %s" % sql_quote(sachbearbeiternr))
    if len(rows) == 0:
        raise ValueError("Kein Sachbearbeiter mit Nummer %s" % sachbearbeiternr)
    return rows[0][0]
