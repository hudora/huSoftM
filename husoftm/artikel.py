#!/usr/bin/env python
# encoding: utf-8
"""
artikel.py - Zugriff auf Artikeldaten. Teil von husoftm.

Created by Maximillian Dornseif on 2007-04-28.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

__revision__ = "$Revision$"

import unittest
import husoftm
from husoftm.connection import get_connection
from husoftm.tools import sql_quote


def buchdurchschnittspreis(artnr):
    """Gibt den Buchdurchschnittspreis für einen Artikel zurück"""
    rows = get_connection().query('XLF00', fields=['LFPRBD'],
                                  condition="LFLGNR=0 AND LFARTN=%s AND LFSTAT<>'X'" % sql_quote(artnr))
    if rows:
        return rows[0][0]
    else:
        return 0
    

class Artikel(object):
    
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
    

def get_artikel(artnr):
    rows = get_connection().query(['XAR00'], condition="ARARTN='%s'" % artnr)
    if len(rows) > 1:
        raise RuntimeError("Mehr als einen Artikel gefunden: %r" % artnr)
    if len(rows) == 0:
        return None
    return Artikel().fill_from_softm(rows[0])
    

_komponentencache = {}


def komponentenaufloesung(mengenliste):
    """Lößt Artikel in ihre Komponenten auf.
    
    >>> komponentenaufloesung([(5, '00049'), (4, '00537')])
    [(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441'), (4, u'42050/A'), (12, u'42051/A'), (4, u'42052/A')]
    >>> komponentenaufloesung([(2, '00001')])
    [(2, '00001')]
    """
    
    ret = []
    for menge, artnr in mengenliste:
        if (menge, artnr) not in _komponentencache:
            _komponentencache[(menge, artnr)] \
                = get_connection().query(['ASK00'], fields=['SKLFNR', 'SKKART', 'SKMENG'],
                                                 condition="SKARTN='%s'" % artnr)
        rows = _komponentencache[(menge, artnr)]
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
    

import husoftm.mock_as400


class komponentenaufloesungTests(unittest.TestCase):

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
    import doctest
    husoftm.MoftSconnection = husoftm.TestMoftSconnection
    doctest.testmod()
    unittest.main()

if __name__ == "__main__":
    print komponentenaufloesung([(5, '00049')])
    #_test()
