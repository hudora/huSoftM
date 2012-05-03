#!/usr/bin/env python
# encoding: utf-8
"""
warenvereinnahmungen.py
Implementiert Behandlung von Warenvereinnahmungen.
Warenvereinnahmungen sind Lagerzugänge aus Bestellungen und werden über die Stapelschnittstelle ISZ00
verwaltet.

Created by Christian Klein on 2011-04-04.
Copyright (c) 2011 HUDORA GmbH. All rights reserved.
"""

raise NotImplementedError("Dieses Modul sollte nciht emhr verwendet werden")


from husoftm2.backend import query, x_en
from husoftm2.tools import add_prefix, sql_quote


def get_new(limit=100):
    """Liefert (max. 100) neue Warenvereinnahmungen zurück"""

    ret = []
    # Wichtig ist es, jedes Caching zu unterbinden, denn möglicherweise arbeiten wir mit get_new()
    # in einer engen Schleife, da würde Caching alles durcheinanderwerfen.
    for kopf in query(['ISZ00'], fields=['IZBSTN', 'IZWVNR'],
                      condition="IZSTAT<>'X'", limit=limit, ua='husoftm2.warenvereinnahmungen',
                      cachingtime=0):
        kopf['bestellnr'] = add_prefix(kopf['bestellnr'], 'PO')
        ret.append(kopf)
    return ret


def mark_processed(warenvereinnahmungsnr):
    """Markiert eine Warenvereinnahmung, so dass er von get_new() nicht mehr zurück gegeben wird."""
    condition = "IZWVNR=%s" % sql_quote(warenvereinnahmungsnr)
    return x_en('ISZ00', condition=condition, ua='husoftm2.warenvereinnahmungen')
