#!/usr/bin/env python
# encoding: utf-8
"""
kommissionierauftraege.py - Kommiaufträge und Rückmeldungen
(Früher "Kommibelege" genannt.)

Technisch hängt das in SoftM an den Lieferscheinen.

Created by Christian Klein on 2011-01-03.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

from husoftm2.lieferscheine import get_ls_kb_data
from husoftm2.tools import remove_prefix, sql_quote
from husoftm2.backend import query
import warnings


def get_kommiauftrag(komminr, header_only=False):
    """Gibt einen Kommiauftrag zurück"""

    prefix = 'KA'
    if komminr.startswith('KB'):
        warnings.warn("get_kommiauftrag('%s') ist nicht mit 'KA' Nummer aufgerufen worden." % komminr,
                      DeprecationWarning)
        prefix = 'KB'
    komminr = remove_prefix(komminr, prefix)

    # In der Tabelle ALK00 stehen Kommissionierbelege und Lieferscheine.
    # Die Kommissionierbelege haben '0' als Lieferscheinnr.
    # Zusätzlich werden die (logisch) gelöschten Lieferscheine rausgefiltert.
    conditions = ["LKLFSN = 0", "LKKBNR = %s" % sql_quote(komminr), "LKSTAT<>'X'"]
    try:
        belege = get_ls_kb_data(conditions, header_only=header_only,
                                is_lieferschein=False)
    except RuntimeError:
        return {}

    if belege:
        beleg = belege[0]
        # Falls es bereits einen Lieferschein gibt, die Lieferscheinnr in das dict schreiben.
        # Ansonsten die Eintrag 'lieferscheinnr' entfernen (wäre sonst SL0)
        rows = query(['ALK00'], condition="LKLFSN <> 0 AND LKKBNR = %s" % sql_quote(komminr))
        if rows:
            beleg['lieferscheinnr'] = rows[0]['lieferscheinnr']
        else:
            beleg.pop('lieferscheinnr', None)
        return beleg
    return {}


def get_kommibeleg(komminr, header_only=False):
    warnings.warn("get_kommibeleg() is obsolete, use get_kommiauftrag() instead", DeprecationWarning)
    return get_kommiauftrag(komminr)


def get_rueckmeldedaten(komminr):
    """Liefert Informationen aus der SoftM Rückmeldeschnittstelle zurück - nur für debugging Zwecke."""
    komminr = remove_prefix(komminr, 'KA')
    rows = query(['ISR00'], condition="IRKBNR = %s" % sql_quote(komminr))
    ret = {}
    for row in rows:
        ret[str(row['kommibelegposition'])] = row
    return ret
