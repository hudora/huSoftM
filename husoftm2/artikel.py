#!/usr/bin/env python
# encoding: utf-8
"""
artikel.py - Zugriff auf Artikeldaten. Teil von husoftm.

Created by Maximillian Dornseif on 2007-04-28.
Copyright (c) 2007, 2009, 2010 HUDORA GmbH. All rights reserved.
"""

import unittest
from husoftm2.backend import query


def komponentenaufloesung(mengenliste):
    """Löst Artikel in ihre Komponenten auf.

    >>> komponentenaufloesung([(5, '00049'), (4, '00537')])
    [(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441'), (4, u'42050/A'), (12, u'42051/A'), (4, u'42052/A')]
    >>> komponentenaufloesung([(2, '00001')])
    [(2, '00001')]
    """

    ret = []
    for menge, artnr in mengenliste:
        rows = query(['ASK00'], fields=['SKLFNR', 'SKKART', 'SKMENG'], condition="SKARTN='%s'" % artnr)
        if not rows:
            # kein Setartikel
            ret.append((menge, artnr))
        else:
            for row in rows:
                ret.append((menge * row['menge_im_set'], row['komponenten_artnr']))
    return ret


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
    komponentenaufloesung([(5, '00049')]),
    ([(5, u'A42438'), (5, u'A42439'), (5, u'A42440'), (10, u'A42441')]
        == komponentenaufloesung([(5, '00049')]))


if __name__ == "__main__":
    _test()
    unittest.main()
