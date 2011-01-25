#!/usr/bin/env python
# encoding: utf-8
"""
kommissionierbelege.py

Created by Christian Klein on 2011-01-03.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

from husoftm2.lieferscheine import get_ls_kb_data
from husoftm2.tools import remove_prefix, sql_quote


def _kommibelege(additional_conditions=None, limit=None, header_only=False):
    conditions = ["LKSANB = 0", "LKSTAT<>'X'"]
    return get_ls_kb_data(conditions, additional_conditions, limit, header_only, is_lieferschein=False)


def get_kommibeleg(komminr, header_only=False):
    """Gibt einen Kommissionierbeleg zurück"""

    prefix = 'KA'
    if komminr.startswith('KB'):
        prefix = 'KB'
    komminr = remove_prefix(komminr, prefix)

    belege = _kommibelege(["LKKBNR = %s" % sql_quote(komminr)], limit=1, header_only=header_only)
    if belege:
        return belege[0]
    return {}
