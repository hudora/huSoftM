#!/usr/bin/env python
# encoding: utf-8
"""
kommissionierbelege.py

Created by Christian Klein on 2011-01-03.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

from husoftm2.lieferscheine import get_ls_kb_data
from husoftm2.tools import remove_prefix, sql_quote


def get_kommibeleg(komminr, header_only=False):
    """Gibt einen Kommissionierbeleg zurück"""

    prefix = 'KA'
    if komminr.startswith('KB'):
        prefix = 'KB'
    komminr = remove_prefix(komminr, prefix)

    conditions = ["LKLFSN <> 0", "LKKBNR = %s" % sql_quote(komminr), "LKSTAT<>'X'"]
    try:
        belege = get_ls_kb_data(conditions, additional_conditions=conditions, header_only=header_only,
                                is_lieferschein=False)
    except RuntimeError:
        return {}

    if belege:
        return belege[0]
    return {}
