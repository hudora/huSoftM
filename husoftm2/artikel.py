#!/usr/bin/env python
# encoding: utf-8
"""
artikel.py - Zugriff auf Artikeldaten. Teil von husoftm.

Created by Maximillian Dornseif on 2007-04-28.
Copyright (c) 2007, 2009, 2010 HUDORA GmbH. All rights reserved.
"""

import datetime
import unittest
from husoftm2.backend import query
from husoftm2.tools import date2softm, sql_quote, pad, remove_prefix


def get_artikelnummern():
    """Gibt eine Liste mit allen Artikelnummern zurück."""

    rows = query(['XAR00'], fields=['ARARTN'])
    return [x[0] for x in rows]


def komponentenaufloesung(mengenliste):
    """Löst Artikel in ihre Komponenten auf.

    >>> komponentenaufloesung([(5, '00049'), (4, '00537')])
    [(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441'), (4, u'42050/A'), (12, u'42051/A'), (4, u'42052/A')]
    >>> komponentenaufloesung([(2, '00001')])
    [(2, '00001')]
    """

    ret = []
    for menge, artnr in mengenliste:
        rows = query(['ASK00'], fields=['SKLFNR', 'SKKART', 'SKMENG'],
                     condition="SKARTN=%s" % sql_quote(artnr), cachingtime=24 * 60 * 60)
        if not rows:
            # kein Setartikel
            ret.append((int(menge), artnr))
        else:
            for row in rows:
                ret.append((int(menge * row['menge_im_set']), row['komponenten_artnr']))
    return ret


def set_artikel(artnr):
    """Gib alle Set-Artikel zurück, die die ArtNr. enthalten

    Der Rückgabewert ist ein dict mit ArtNr. als Schlüssel und den Mengen als Werten.
    """

    rows = query(['ASK00'], fields=['SKARTN', 'SKMENG'], condition='SKKART=%s' % sql_quote(artnr),
                 cachingtime=24 * 60 * 60)
    if not rows:
        return [{'artnr': artnr, 'menge_im_set': 1}]
    return rows


def get_kundenartikelnr(kundennr, artnrs, date=None):
    """Liefert eine Zuordnung von SoftM-Artikelnr zu Kundenartikelnr für einen Kunden zurück.

    Wird ein Datum angegeben, werden nur die Datensätze ausgewählt, die ab diesem Datum gültig sind.
    Wird kein Datum angegeben, wird das aktuelle Datum verwendet.

    Der Rückgabewert ist ein dict mit den SoftM-Artikelnr als Schlüssel und einem dict
    mit Schlüsseln 'bezeichnung' und 'kundenartnr' als Werte. Bsp:
    >>> get_kundenartikelnr('SC17200', ['11111'])
    {u'11111': {'bezeichnung': u'Artikelbezeichnung',
                'kundenartnr': u'HD-12345-XYZ'}}
    """

    if date is None:
        date = datetime.date.today()

    kundennr = remove_prefix(kundennr, 'SC')
    conditions = ['KAKDNR=%s' % pad('KAKDNR', kundennr),
                  'KAARTN IN (%s)' % ','.join(sql_quote(artnr) for artnr in artnrs),
                  'KADTPR <= %s' % date2softm(date)]
    rows = query(['AKA00'], fields=['KAARTN', 'KAKART', 'KABEZE'], condition=' AND '.join(conditions),
                 ua='husoftm2.artikel.get_kundenartikelnr')
    mapping = {}
    for row in rows:
        artnr = row.pop('artnr')
        mapping[artnr] = row
    return mapping


class KomponentenaufloesungTests(unittest.TestCase):

    def test_komponentenaufloesung(self):
        self.assertEqual(komponentenaufloesung([(5, '00049')]),
                         [(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441')])
        self.assertEqual(komponentenaufloesung([[4, u'00049']]),
                         [(4, u'A42438'), (4, u'A42439'), (4, u'A42440'), (8, u'A42441')])
        self.assertEqual(komponentenaufloesung(((0, '00049'), )),
                         [(0, u'A42438'), (0, u'A42439'), (0, u'A42440'), (0, u'A42441')])


def _test():
    """Diverse einfache Tests."""
    print get_artikelnummern()
    komponentenaufloesung([(5, '00049')]),
    ([(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441')]
        == komponentenaufloesung([(5, '00049')]))


if __name__ == "__main__":
    _test()
    unittest.main()
