#!/usr/bin/env python
# encoding: utf-8
"""
lieferanten.py

Created by Christoph Borgolte on 2011-05-31.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import pad, remove_prefix, add_prefix


def get_address(lieferantennr):
    """Liefert die Adresse zu einer Lieferantennummer."""
    # Prefix für Lieferanten (SP) entfernen. Für Prefix siehe
    # https://sites.google.com/a/hudora.de/intern/it-administration/nummern/nummernkreise
    lieferantennr = remove_prefix(lieferantennr, 'SP')
    rows = query('XXA00', condition=("XASTAT=' ' and XALINR = %s" % pad('XALINR', lieferantennr)))
    if rows:
        row = rows[0]
        row['lieferantennr'] = add_prefix(row['lieferantennr'], 'SP')
        return row
    return None
