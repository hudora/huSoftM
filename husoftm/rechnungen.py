#!/usr/bin/env python
# encoding: utf-8
"""
rechnungen.py - zugriff auf rechnungen in SoftM

Created by Maximillian Dornseif on 2009-06-04.
Copyright (c) 2009 HUDORA. All rights reserved.
"""

import sys
import os


def main():
    pass


if __name__ == '__main__':
    main()


#!/usr/bin/env python
# encoding: utf-8
"""
bestaende.py - high level functions to access SoftM on the AS/400.

Created by Maximillian Dornseif on 2006-10-19.

Hier werden Warenbestände, verfügbare Mengen und dergleichen ermittelt.

Für die Frage, ob wir einen Artikel verkaufen können ist freie_menge() die richtige Funktion.
Für die Frage, ob ein bestimmter Artikel in einem bestimmten Lager ist, ist bestand() geignet.

    alternativen(artnr)                           alternativartikel / versionen
    bestellmengen(artnr)                          von uns bei Lieferanten bestellte Mengen
    auftragsmengen(artnr, lager=0)                bei uns von Kunden bestellte Mengen
    umlagermenge(artnr, lager, vonlager=None)     Menge, die zur Zeit von einem Lager ans andere unterwegs ist
    umlagermengen(anlager, vonlager=None)           Alle Artikelmengen, die zur Zeit von einem Lager ans andere unterwegs ist
    buchbestand(artnr, lager=0)                   Artikel am Lager
    buchbestaende(lager=0)                        Alle Artikel an einem Lager 
    freie_menge(artnr)                            Menge, die Verkauft werden kann
    frei_ab(menge, artnr, dateformat="%Y-%m-%d")  ab wann ist eine bestimmte Menge frühstens verfügbar?
    bestand(artnr, lager, vonlager=None)          Wieviel ist zur Zeit an einem Lager oder trifft kurzum ein?
    besteande(lager)                              wie bestand() aber für alle Artikel
    bestandsentwicklung(artnr, dateformat="%Y-%m-%d")            Prognose der Bestandsänderungen
    versionsvorschlag(menge, artnr, date, dateformat="%Y-%m-%d") Vorschlag zur Versionsstückelung
"""

__revision__ = "$Revision: 5770 $"

import couchdb.client
import datetime
import memcache
import time
import warnings
import husoftm.caching as caching

from husoftm.connection2 import get_connection, as400_2_int
from husoftm.tools import sql_escape, sql_quote
from huTools.robusttypecasts import int_or_0
import huTools.async

def kundenauftragsnr_to_rechnungsnr(kundenauftragsnr):
    """Liefert eine Liste mit Rechnungsnummern zurück, die zu einer Kundenauftragsnummer gehören.
    
    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix SR zurückgegeben."""
    
    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKNRKD = %s" % (sql_quote(kundenauftragsnr)))
    return [("SR%s" % r[0]) for r in rows]
    
#print kundenauftragsnr_to_rechnungsnr("WL0775262")

def auftragsnr_to_rechnungsnr(auftragsnr):
    """Liefert eine Liste mit Rechnungsnummern zurück, die zu einer Auftragsnummer gehören.

    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix SR zurückgegeben."""

    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKAUFN = %s" % (sql_quote(auftragsnr)))
    return [("SR%s" % r[0]) for r in rows]

