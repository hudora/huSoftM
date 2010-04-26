#!/usr/bin/env python
# encoding: utf-8
"""
artikel.py - Zugriff auf Artikeldaten. Teil von husoftm.

Created by Maximillian Dornseif on 2007-04-28.
Copyright (c) 2007, 2009, 2010 HUDORA GmbH. All rights reserved.
"""

from decimal import Decimal
from husoftm.connection2 import get_connection, as400_2_int
from husoftm.tools import sql_quote
import cs.caching as caching
import husoftm
import re
import unittest
import warnings


def _auf_zwei_stellen(floatnum):
    """Converts a float to a Decimal() object with two digits precision.
    
    >>> _auf_zwei_stellen(1.0/3.0)
    0.33
    """
    return Decimal(str(floatnum)).quantize(Decimal(10) ** -2)
    

def buchdurchschnittspreis(artnr):
    """Gibt den (aktuellen) Buchdurchschnittspreis für einen Artikel zurück.
    
    >>> buchdurchschnittspreis('04711')
    13.65
    """
    rows = get_connection().query('XLF00', fields=['LFPRBD'],
                                  condition="LFLGNR=0 AND LFARTN=%s AND LFSTAT<>'X'" % sql_quote(artnr))
    if rows:
        return _auf_zwei_stellen(rows[0][0])
    else:
        return Decimal()


def preis(artnr):
    """Gibt den (aktuellen) Listenpreis für einen Artikel zurück.
    
    >>> preis('04711')
    13.65
    """
    
    rows = get_connection().query('XAR00', fields=['ARPREV'],
                                  condition="ARARTN=%s AND ARSTAT<>'X'" % sql_quote(artnr))
    if rows:
        return _auf_zwei_stellen(rows[0][0])
    else:
        return 0


class Artikel(object):
    """Repräsentiert einen Artikel in SoftM."""
    
    def fill_from_softm(self, row):
        self.artnr = row.get('artnr', '') # u'14600'
        self.laenge = row.get('laenge', '') # 0.14999999999999999
        self.tiefe = row.get('tiefe', '') # 0.78000000000000003
        self.breite = row.get('breite', '') # 0.29999999999999999
        self.volumen = row.get('volumen', '') # 0
        self.gewicht = row.get('gewicht', '') # 3.2330000000000001
        self.gewicht_netto = row.get('gewicht_netto', '') # 3.2330000000000001
        self.abc1 = row.get('abc1', '') # u'A'
        self.abc2 = row.get('abc2', '') # u'A'
        self.abc3 = row.get('abc3', '') # u'A'
        self.ean = row.get('ean', '') # 4005998146002L
        self.status = row.get('status', '')
        self.zolltarifnummer = row.get('zolltarifnummer', '') # u'95010090'
        self.ursprungsland = row.get('ursprungsland', '') # 720
        self.handelsform = row.get('handelsform', '') # u''
        self.verkaufsteil = row.get('verkaufsteil', '') # u'2'
        self.mindestabnahmemenge = row.get('mindestabnahmemenge', '') # 0
        self.artikelhauptgruppe = row.get('artikelhauptgruppe', '') # u'400'
        self.matchcode = row.get('matchcode', '') # u'BIG WHEEL, SKATEROLL'
        self.ersatzartikel = row.get('ersatzartikel', '') # u''
        self.andruck_in_rechnung = row.get('andruck_in_rechnung', '') # 0
        self.ARBEZ1 = row.get('ARBEZ1', '') # u'HUDORA Big Wheel, sil'
        self.ARBEZ2 = row.get('ARBEZ2', '') # u'ber ,Aluscooter, STIW'
        self.ARBEZ3 = row.get('ARBEZ3', '') # u'A "GUT"'
        self.zuteilungssperre = row.get('zuteilungssperre', '') # u''
        self.webshop = row.get('webshop', '') # u''
        self.setartikel = row.get('setartikel', '') # 0
        self.listenpreis = row.get('listenpreis', '') # 32.950000000000003
        self.info = row.get('info', '') # u''
        self.darreichungsform = row.get('darreichungsform', '') # u''
        self.artikelgruppe = row.get('artikelgruppe', '') # u'401'
        self.mengeneinheit = row.get('mengeneinheit', '') # 1
        self.auslaufartikel = row.get('auslaufartikel', '') # 0
        self.ist_verpackung = row.get('ist_verpackung', '') # 0
        self.ersatzteil = row.get('ersatzteil', '') # u'0'
        self.aenderung_date = row.get('aenderung_date', '') # datetime.date(2007, 4, 28)
        self.artikeltexte_vorhanden = row.get('artikeltexte_vorhanden', '') # 0
        # self. = row.get('Sachb.letzte \xc3\x84nderung', '') # 303
        # self. = row.get('BenutzerPr.letzte \xc3\x84nderung', '') # u'MD'
        # u'', 'BenutzerPr.Erfassung', '') # u''
        # self. = row.get('erfassung_date', '') # datetime.date(2004, 12, 2)
        # self. = row.get('Sachb. Erfassung', '') # 169
        # self. = row.get('zollgruppe', '') # u''
        if row['erfassung_date']:
            self.erfassung = row['erfassung_date']
        if row['aenderung_date']:
            self.aenderung = row['aenderung_date']
        else:
            self.aenderung = self.erfassung
        self.name = "%s%s%s" % (row.get('ARBEZ1', ''), row.get('ARBEZ2', ''), row.get('ARBEZ3', ''))
        return self


# TODO: remove
# wird ausschliesslich in trunk/web/MoftS/produktpass/bin/softmsync genutzt

def get_artikel(artnr=None, ean=None):
    """Returns articles for artnr and/or ean, depending on what parameter is not None.

    Use both parameters if ean is not unique, like in the following example.
    This function can be used to check if an EAN and article number fit to each other.

    Non-unique example:
    >>> a = get_artikel(artnr='76095')
    >>> b = get_artikel(artnr=None, ean=a.ean)
    RuntimeError: Ergebnis nicht eindeutig für: Artnr None && EAN 4005998760956L"""

    cond1 = "ARARTN=%s" % sql_quote(artnr)
    cond2 = "AREAN=%s" % sql_quote(ean)
    if artnr and ean:
        condition = cond1+" AND "+cond2
    elif artnr:
        condition = cond1
    elif ean:
        condition = cond2
    else:
        raise RuntimeError("Artikel ohne EAN und Artnr nicht möglich.")

    rows = get_connection().query(['XAR00'], condition=condition)
    rowcount = len(rows)
    if rowcount > 1:
        raise RuntimeError("Ergebnis nicht eindeutig für: Artnr %r && EAN %r" % (artnr, ean))
    elif rowcount == 0:
        return None
    return Artikel().fill_from_softm(rows[0])


def get_artikel_by_ean(ean):
    """Returns all articles w/ given EAN."""
    condition = "AREAN=%s" % sql_quote(ean)
    rows = get_connection().query(['XAR00'], condition=condition)
    return [Artikel().fill_from_softm(row) for row in rows]


def guess_artnr(ean):
    """Try to guess an article number, for the list of artnrs given for one EAN.

    Normally this are set articles or articles w/ versions or variants.
    Returns:
    - If it is a set article, the main article number
    - If it is an article w/ variants or versions, the stripped artnr."""

    articles = get_artikel_by_ean(ean)
    if len(articles) == 0:
        return None
    if len(articles) == 1:
        return articles[0].artnr
    
    pattern = re.compile('(\d+)')

    # by checking for article sets
    candidates = [c for c in articles if c.setartikel]
    candidates = list(map(pattern.search, [c.artnr for c in candidates]))
    candidates = set([c.group(0) for c in candidates])
    if len(candidates) == 1:
        return candidates.pop()

    #by removing version information
    cand_artnrs = list(map(pattern.search, [c.artnr for c in articles]))
    cand_artnrs = set([c.group(0) for c in cand_artnrs])
    if len(cand_artnrs) == 1:
        return cand_artnrs.pop()
    # no ideas left
    return None


def komponentenaufloesung(mengenliste):
    """Löst Artikel in ihre Komponenten auf.
    
    >>> komponentenaufloesung([(5, '00049'), (4, '00537')])
    [(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441'), (4, u'42050/A'), (12, u'42051/A'), (4, u'42052/A')]
    >>> komponentenaufloesung([(2, '00001')])
    [(2, '00001')]
    
    Achtung: Diese Funktion implementiert mehrere Tage caching
    """
    
    ret = []
    for menge, artnr in mengenliste:
        # check if we have a cached result
        memc = caching.get_cache()
        rows = memc.get('husoftm.komponentencache.%r' % (artnr))
        if rows == None: # empty lists are cached too, so check against None here
            rows = get_connection().query(['ASK00'], fields=['SKLFNR', 'SKKART', 'SKMENG'],
                                          condition="SKARTN='%s'" % artnr)
            memc.set('husoftm.komponentencache.%r' % (artnr), rows, 60*60*72) # 4 d
        if not rows:
            # kein Setartikel
            ret.append((menge, artnr))
        else:
            for row in rows:
                ret.append((menge * row['menge_im_set'], row['komponenten_artnr']))
    return ret
    

# TODO: do we need KomponentenResolver() and komponentenaufloesung?

class KomponentenResolver(object):
    
    def __init__(self):
        self.cache = {}
    
    def fill_cache(self):
        # TODO
        rows = get_connection().query('ASK00', fields=['SKARTN', 'SKLFNR', 'SKKART', 'SKMENG'])
        for row in rows:
            if row['artnr'] not in self.cache:
                self.cache[row['artnr']] = []
            self.cache[row['artnr']].append(row)
    
    def resolve(self, mengenliste):
        warnings.warn("KomponentenResolver.resolve() is deprecated use komponentenaufloesung()",
                      DeprecationWarning, stacklevel=2) 
        ret = []
        if not self.cache:
            self.fill_cache()
        for menge, artnr in mengenliste:
            if artnr not in self.cache:
                # kein Setartikel
                ret.append((menge, artnr))
            else:
                for row in self.cache.get(artnr, []):
                    ret.append((menge * row['menge_im_set'], row['komponenten_artnr']))
        return ret
    

def get_umschlag(artnr):
    """Gibt aufgeschlüsselt nach Datum zurück, wie viel Einheiten fakturiert wurden.
    
    >>> get_umschlag('14600')
    [(datetime.date(2005, 1, 25), 104),
     (datetime.date(2005, 1, 27), 8),
     ...
     (datetime.date(2008, 5, 29), 2),
     (datetime.date(2009, 2, 10), 2)]
     
    Achtung: Diese Funktion implementiert mehrere Tage caching.
    """
    
    # check if we have a cached result
    memc = caching.get_cache()
    cacheddata = memc.get('husoftm.umschlag.%r' % (artnr))
    if cacheddata:
        del memc
        return cacheddata
    
    condition = (
    "FKRGNR=FURGNR"             # JOIN
    " AND FKAUFA<>'U'"          # Keine Umlagerung
    " AND FKSTAT<>'X'"          # nicht gelöscht
    " AND FKDTFA > 0"           # Druckdatum nicht leer
    " AND FUKZRV=2"             # ?
    " AND FURGNI<>0"            # ?
    " AND FKFORM=' '"           # ?
    " AND FURGNR<>0"            # es gibt eine Rechnungsnummer
    " AND FKMJBU>'10412'"       # keine legacy Daten
    " AND FUARTN=%s")           # nur bestimmten Artikel beachten
    
    rows = get_connection().query(['AFU00', 'AFK00'], fields=['FKDTFA', 'SUM(FUMNG)'],
                   condition=condition % (sql_quote(artnr)),
                   ordering='FKDTFA', grouping='FKDTFA',
                   querymappings={'SUM(FUMNG)': 'menge', 'FKDTFA': 'rechnung_date'})
    ret = [(x['rechnung_date'], as400_2_int(x['menge'])) for x in rows if x['menge'] > 0]
    
    memc.set('husoftm.umschlag.%r' % (artnr), ret, 60*60*24*6) # 6 d
    del memc
    return ret


def abgabepreisbasis(artnr):
    """Gibt eine Liste mit den durchschnittlichen Rechnungspreisen pro Monat zurück.
    
    Liefert eine Liste von 4-Tuples
    (datum, AVG(preis), menge, gesammtpreis)
    
    [ ...
    (datetime.date(2009, 2, 10), 32.95, 2, 65.90), 
    (datetime.date(2009, 10, 19), 17.44, 2, 34.88)]
    """
    
    condition = (
    "FKRGNR=FURGNR"             # JOIN
    " AND FKAUFA<>'U'"          # Keine Umlagerung
    " AND FKSTAT<>'X'"          # nicht gelöscht
    " AND FKDTFA > 0"           # Druckdatum nicht leer
    " AND FUKZRV=2"             # ?
    " AND FURGNI<>0"            # ?
    " AND FKFORM=' '"           # ?
    " AND FURGNR<>0"            # es gibt eine Rechnungsnummer
    " AND FUPNET>0"             # keine Gutschriften
    " AND FKMJBU>'10412'"       # keine legacy Daten
    " AND FUARTN=%s")           # nur bestimmten Artikel beachten
    
    rows = get_connection().query(['AFU00', 'AFK00'], fields=['FKDTFA', 'SUM(FUMNG)', 'SUM(FUPNET)', 'COUNT(FKRGNR)'],
                   condition=condition % (sql_quote(artnr)),
                   ordering='FKDTFA', grouping='FKDTFA',
                   querymappings={'SUM(FUMNG)': 'menge', 'SUM(FUPNET)': 'nettopreis', 'COUNT(FKRGNR)': 'rechnungen', 'FKDTFA': 'rechnung_date'})
    ret = []
    for row in rows:
        menge = as400_2_int(row['menge'])
        if menge:
            ret.append((row['rechnung_date'], float(row['nettopreis'])/float(row['menge']), menge, float(row['nettopreis'])))
    ret.sort()
    return ret


import husoftm.mock_as400


class KomponentenaufloesungTests(unittest.TestCase):

    def test_komponentenaufloesung(self):
        self.assertEqual(komponentenaufloesung([(5, '00049')]),
                         [(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441')])
        self.assertEqual(komponentenaufloesung([[4, u'00049']]),
                         [(4, u'A42438'), (4, u'A42439'), (4, u'A42440'), (8, u'A42441')])
        self.assertEqual(komponentenaufloesung(((0, '00049'), )), 
                         [(0, u'A42438'), (0, u'A42439'), (0, u'A42440'), (0, u'A42441')])
    
    def test_uses_cache(self):
        komponentenaufloesung([(3, '00049')])
        oldlen = len(husoftm.mock_as400.querylog)
        komponentenaufloesung([(3, '00049')])
        newlen = len(husoftm.mock_as400.querylog)
        self.assertEqual(oldlen, newlen)


def _test():
    """Diverse einfache Tests."""
    abgabepreisbasis('14600')
    get_umschlag('14600')
    _auf_zwei_stellen(1.0/3.0)
    komponentenaufloesung([(5, '00049')]),
    ([(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441')]
        == komponentenaufloesung([(5, '00049')]))
    buchdurchschnittspreis('14600')
    preis('14600')

if __name__ == "__main__":
    _test()
    unittest.main()
