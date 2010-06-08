#!/usr/bin/env python
# encoding: utf-8
"""
rechnungen.py - zugriff auf rechnungen in SoftM

Created by Maximillian Dornseif on 2009-06-04.
Copyright (c) 2009 HUDORA. All rights reserved.
"""


from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote, sql_escape


__revision__ = "$Revision: 5770 $"


def kundenauftragsnr_to_rechnungsnr(kundenauftragsnr):
    """Liefert eine Liste mit Rechnungsnummern zurück, die zu einer Kundenauftragsnummer gehören.
    
    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix SR zurückgegeben."""
    
    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKNRKD = %s" % (sql_quote(kundenauftragsnr)))
    return [("SR%s" % r[0]) for r in rows]
    

def auftragsnr_to_rechnungsnr(auftragsnr):
    """Liefert eine Liste mit Rechnungsnummern zurück, die zu einer Auftragsnummer gehören.

    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix SR zurückgegeben."""

    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKAUFN = %s" % (sql_quote(auftragsnr)))
    return [("SR%s" % r[0]) for r in rows]


def rechnungen_for_kunde(kundennr):
    """Liefert eine Liste mit Rechnungsnummern zurück"""
    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKKDNR = %s" % sql_quote(kundennr))
    return rows


def get_rechnung(rechnungsnr):
    """Liefert ein Tupel aus Rechnungskopf und den Positionen"""
    kopf = get_connection().query(['AFK00'],
                   condition="FKRGNR = %s" % sql_escape(rechnungsnr))
    if len(kopf) != 1:
        raise RuntimeError('inkonsistente Kopfdaten in AFK00')
    kopf = kopf[0]
    positionen = get_connection().query(['AFU00'],
                   condition="FURGNR=%s" % sql_escape(rechnungsnr))
    return kopf, positionen

class Rechnung(object):
    """Highlevel Objekt für Rechnungen"""
    pass
