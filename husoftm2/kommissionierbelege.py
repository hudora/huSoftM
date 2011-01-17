#!/usr/bin/env python
# encoding: utf-8
"""
kommissionierbelege.py

Created by Christian Klein on 2011-01-03.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import sql_quote, land2iso
from husoftm2.texte import texte_trennen, texte_auslesen
import husoftm2.sachbearbeiter

from husoftm2.lieferscheine import get_ls_kb_data

def _kommibelege(additional_conditions=None, limit=None, header_only=False):
    conditions = ["LKSANB = 0", "LKSTAT<>'X'"]
    return get_ls_kb_data(conditions, additional_conditions, limit, header_only)


def get_kommibeleg(komminr, header_only=False):
    """Gibt einen Kommissionierbeleg zurück"""

    komminr = str(komminr)
    if komminr.startswith('KA'):
        komminr = komminr[2:]
    belege = _kommibelege(["LKKBNR = %s" % sql_quote(komminr)], limit=1, header_only=header_only)
    if belege:
        return belege[0]
    return {}
