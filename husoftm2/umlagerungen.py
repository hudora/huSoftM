#!/usr/bin/env python
# encoding: utf-8
"""
umlagerungen.py

Created by Christian Klein on 2011-04-01.
Copyright (c) 2011 HUDORA GmbH. All rights reserved.
"""
from husoftm2.backend import query, x_en
from husoftm2.tools import add_prefix, remove_prefix, sql_quote


def get_new(limit=100):
    """
    Liefert unverarbeitete Umlagerungsaufträge zurück.

    Die Umlagerungsaufträge entsprechen Kommissionieraufträgen.
    """

    ret = []
    # Wichtig ist es, jedes Caching zu unterbinden, denn möglicherweise arbeiten wir mit get_new()
    # in einer engen Schleife, da würde Caching alles durcheinanderwerfen.
    for kopf in query(['ISK00'], fields=['IKKBNR', 'IKKPOS'],
                      condition="IKSTAT<>'X'", limit=limit, ua='husoftm2.umlagerungen',
                      cachingtime=0):
        kopf['komminr'] = add_prefix(kopf['komminr'], 'KA')
        ret.append(kopf)
    return ret


def mark_processed(kommiauftragnr, posnr):
    """Markiert einen Umlagerungsauftrag, so dass er von get_new() nicht mehr zurück gegeben wird."""
    conditions = ["IKKBNR=%s" % sql_quote(remove_prefix(kommiauftragnr, 'KA')),
                  "IKKPOS=%d" % int(posnr)]
    return x_en('ISK00', condition=' AND '.join(conditions), ua='husoftm2.umlagerungen')
