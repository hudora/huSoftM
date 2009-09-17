#!/usr/bin/env python
# encoding: utf-8

"""
Vermischte Hilfen zur SoftM Nutzung.

Created by Maximillian Dornseif on 2007-03-29.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import datetime
import doctest
import logging
import sys
import time
import unittest

__revision__ = "$Revision$"


logging.basicConfig(level=logging.DEBUG)
LOG = logging.getLogger('husoftm.tools')


# SoftM verwendet scheinbar Autokennzeichen
# http://en.wikipedia.org/wiki/List_of_international_license_plate_codes
# Wir verwenden fuer alles neue ISO 3166-1 Alpha-2 Country Codes siehe http://en.wikipedia.org/wiki/ISO_3166-1
# BTW: Die FIFA verwendet noch andere codes!

SOFTMLKZ2ISOLAND = {'': 'DE',
                    'D': 'DE',
                    'CC': 'CC',
                    '???': '??', # WTF
                    'A': 'AT', # Oesterreich
                    'L': 'LU', # Luxemburg
                    'F': 'FR', # Frankreich
                    'B': 'BE', # Belgien
                    'I': 'IT', # Italien
                    'E': 'ES', # Spanien
                    'SLO': 'SI', # Slowenien
                    'EST': 'EE', # Estland
                    'RUS': 'RU', # Rusland
                    'S': 'SE', # Schweden
                    'IRL': 'IE', # Irland
                    'AUS': 'AU', # Australien
                    'CDN': 'CA', # Canada
                    'FIN': 'FI', # Finland
                    'GEO': 'GE', # Georgien
                    'LTL': 'LT', # Litauen
                    'N': 'NO', # Norwegen
                    'AZE': 'AZ', # Azerbaidschan
                    'RCH': 'CL', # Chile
                    'CH': 'CH', # Schweiz
                    'DK': 'DK', # Daenemark
                    'NL': 'NL', # Niederlande
                    'GR': 'GR', # Griechenland
                    'BG': 'BG', # Bulgarien
                    'FL': 'LI', # Fuerstentum Liechtenstein
                    'CZ': 'CZ', # Tschechische Republik
                    'HR': 'HR', # Kroatien
                    'CH': 'CH', # Schweiz
                    'H': 'HU', # Ungarn
                    'IR': 'IR', # Iran
                    'TR': 'TR', # Tuerkei
                    'SK': 'SK', # Slowakei
                    'LV': 'LV', # Lettland
                    'IS': 'IS', # Island
                    'PL': 'PL', # Polen
                    'PA': 'PA', # Panama
                    'P': 'PT', # Portugal
                    'UA': 'UA', # Ukraine
                    'RA': 'AR', # Argentinien
                    'ET': 'EG', # Aegypten
                    'RO': 'RO', # Rumaenien
                    'GB': 'GB', # Vereinigtes Koenigreich # UK ist erlaubt aber nicht empfohlen
                    'NZ': 'NZ', # Neuseeland
                    'RI': 'ID', # Republik Indonesia
                    'HK': 'HK', # Hongkong
                    'RS': 'RS', # Republik Serbien
                    'CY': 'CY', # Zypern (Cyprus)
                    'ZA': 'ZA', # Südafrika
}
# invert dictionary
ISOLAND2SOFTMLKZ = dict([[v, k] for k, v in SOFTMLKZ2ISOLAND.items()])


def land2iso(softmlaenderkennzeichen):
    """Wandelt einen SoftM Ländercode (Autokennzeichen) in einen ISO 3166-1 Alpha-2 Country Code um.
    
    >>> land2iso('D')
    'DE'
    >>> land2iso('DE')
    'DE'
    >>> land2iso('LTL')
    'LT'
    """
    ret = SOFTMLKZ2ISOLAND.get(softmlaenderkennzeichen, None)
    if not ret:
        if softmlaenderkennzeichen in SOFTMLKZ2ISOLAND.values():
            # Vermutlich haben wir ein gueltiges ISO-Laenderkennzeichen als Parameter bekommen
            return softmlaenderkennzeichen
        raise ValueError("Unbekannter Laendercode aus SoftM: %r" % (softmlaenderkennzeichen, ))
    return ret
    

def iso2land(isoland):
    """Wandelt einen ISO 3166-1 Alpha-2 Country Code in einen SoftM Ländercode (Autokennzeichen) um.
    
    >>> iso2land('DE')
    'D'
    >>> iso2land('LT')
    'LTL'
    """
    
    ret = ISOLAND2SOFTMLKZ.get(isoland, None)
    if not ret:
        raise ValueError("Unbekannter ISO 3166-1 Alpha-2 Country Code: %r" % (isoland, ))
    return ret
    

def date2softm(date):
    """Wandelt ein datetime Objekt in das von SoftM verwendete Format.
    
    >>> date2softm(datetime.date(2000, 01, 01))
    '1000101'
    >>> date2softm(datetime.date(1901, 06, 05))
    '010605'
    """
    
    if not date:
        return ''
    if date.year > 1999:
        return date.strftime('1%y%m%d')
    return date.strftime('%y%m%d')
    

def softm2date(date):
    """Wandelt das von SoftM verwendete Datumsformat in ein datetime Objekt.
    
    >>> softm2date('1060821')
    datetime.date(2006, 8, 21)
    >>> softm2date('740821')
    datetime.date(1974, 8, 21)
    >>> softm2date('740821.0')
    datetime.date(1974, 8, 21)
    """
    
    try:
        date = str(date).strip()
        if date.endswith('.0'):
            date = date[:-2]
        if date.endswith('999999'): # Datum = 999999: Unbestimmt
            return datetime.date(9999, 12, 31)
        if date:
            if len(date) == 7:
                return datetime.date(*time.strptime(str(int(date)), '1%y%m%d')[:3])
            if len(date) == 6:
                return datetime.date(*time.strptime(str(int(date)), '%y%m%d')[:3])
    except ValueError, msg:
        raise ValueError("can't convert %s to date: %s" % (date, msg))
    return None
    

def sql_escape(data):
    """SQL-Escapes a string to be fed into DB2.
    >>> sql_escape("foo")
    'foo'
    >>> sql_escape("foo's bar says ''foobar''")
    "foo''s bar says ''''foobar''''"
    """
    data_cpy = data
    if isinstance(data_cpy, unicode):
        data_cpy = data_cpy.encode('utf-8', 'replace')
    return str(data_cpy).replace("'", "''")


def sql_quote(data):
    """Quotes and SQL-Escapes a string to be fed into DB2.
    >>> sql_quote("foo")
    "'foo'"
    >>> sql_quote("foo's bar says ''foobar''")
    "'foo''s bar says ''''foobar'''''"
    """
    return "'%s'" % sql_escape(data)
    

class _GenericTests(unittest.TestCase):
    """Vermischte Tests."""
    
    def test_land2iso(self):
        """Testet die Umwandlung von Länderkennzeichen."""
        self.assertEqual(land2iso('SLO'), 'SI')
        self.assertEqual(land2iso('D'), 'DE')
        self.assertEqual(land2iso('DE'), 'DE')
        self.assertEqual(land2iso('LTL'), 'LT')
        self.assertRaises(ValueError, land2iso, 'XDE')
        self.assertEqual(land2iso(''), 'DE')
    
    def test_data_conversions(self):
        """Tested die Datumsumwandlung."""
        self.assertEqual(softm2date('1070605'), datetime.date(2007, 06, 05))
        self.assertEqual(date2softm(datetime.datetime(2007, 06, 05)), '1070605')
        self.assertEqual(date2softm(softm2date('1070605')), '1070605')
        self.assertEqual(softm2date('       '), None)
        self.assertEqual(softm2date(''), None)
    

if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    unittest.main()
    sys.exit(failure_count)
