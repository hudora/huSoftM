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
from husoftm.fields import PADDINGFIELDS

__revision__ = "$Revision$"


logging.basicConfig(level=logging.DEBUG)
LOG = logging.getLogger('husoftm.tools')


# SoftM verwendet scheinbar Autokennzeichen
# http://en.wikipedia.org/wiki/List_of_international_license_plate_codes
# Wir verwenden fuer alles neue ISO 3166-1 Alpha-2 Country Codes siehe http://en.wikipedia.org/wiki/ISO_3166-1
# BTW: Die FIFA verwendet noch andere codes!

SOFTMLKZ2ISOLAND = {'': 'DE',
                    '???': '??', # WTF
                    'A': 'AT', # Oesterreich
                    'AUS': 'AU', # Australien
                    'AZE': 'AZ', # Azerbaidschan
                    'B': 'BE', # Belgien
                    'BG': 'BG', # Bulgarien
                    'CC': 'CC',
                    'CDN': 'CA', # Canada
                    'CH': 'CH', # Schweiz
                    'CY': 'CY', # Zypern (Cyprus)
                    'CZ': 'CZ', # Tschechische Republik
                    'D': 'DE',
                    'DK': 'DK', # Daenemark
                    'E': 'ES', # Spanien
                    'EST': 'EE', # Estland
                    'ET': 'EG', # Aegypten
                    'F': 'FR', # Frankreich
                    'FIN': 'FI', # Finland
                    'FL': 'LI', # Fuerstentum Liechtenstein
                    'GB': 'GB', # Vereinigtes Koenigreich # UK ist erlaubt aber nicht empfohlen
                    'GEO': 'GE', # Georgien
                    'GR': 'GR', # Griechenland
                    'H': 'HU', # Ungarn
                    'HK': 'HK', # Hongkong
                    'HR': 'HR', # Kroatien
                    'I': 'IT', # Italien
                    'IR': 'IR', # Iran
                    'IRL': 'IE', # Irland
                    'IS': 'IS', # Island
                    'L': 'LU', # Luxemburg
                    'LTL': 'LT', # Litauen
                    'LV': 'LV', # Lettland
                    'M': 'MT', # Malta
                    'N': 'NO', # Norwegen
                    'NL': 'NL', # Niederlande
                    'NZ': 'NZ', # Neuseeland
                    'P': 'PT', # Portugal
                    'PA': 'PA', # Panama
                    'PL': 'PL', # Polen
                    'RA': 'AR', # Argentinien
                    'RCH': 'CL', # Chile
                    'RI': 'ID', # Republik Indonesia
                    'RO': 'RO', # Rumaenien
                    'RS': 'RS', # Republik Serbien
                    'RUS': 'RU', # Rusland
                    'S': 'SE', # Schweden
                    'SK': 'SK', # Slowakei
                    'SLO': 'SI', # Slowenien
                    'TR': 'TR', # Tuerkei
'UA': 'UA', # Ukraine
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
    """Wandelt ein datetime Objekt oder String in das von SoftM verwendete Format.
    
    >>> date2softm(datetime.date(2000, 01, 01))
    '1000101'
    >>> date2softm(datetime.date(1901, 06, 05))
    '010605'
    >>> date2softm('2001-12-13')
    '1011213'
    """
    
    if not date:
        return ''
    if isinstance(date, basestring):
        date = datetime.datetime.strptime(date, '%Y-%m-%d')
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


def str2softmdate(value, fmt='%Y-%m-%d'):
    """
    Convert a string to a SoftM date value.
    
    The default date format is ISO 8601.

    >>> str2softmdate("1990-07-08")
    '900708'
    >>> str2softmdate("2006-02-25")
    '1060225'
    """
    date = datetime.datetime.strptime(value, fmt)
    return date2softm(value)


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


def pad(field, value):
    """Pad field to fixed length"""
    if field in PADDINGFIELDS:
        return PADDINGFIELDS[field] % value
    return value


def create_range(fieldname, start=None, end=None, func=None):
    """
    Create range statement for field named fieldname

    func is a function that needs to be applied to start and end.
    If func is None, an anonymous identity function will be created.
    """

    if func is None:
        func = lambda x: x

    if start and end:
        condition = "BETWEEN %s AND %s" % (func(start), func(end))
    elif start:
        condition = "> %s" % func(start)
    elif end:
        condition = "< %s" % func(end)
    else:
        return ""
    return " ".join((fieldname, condition))


def set_attributes(src, dest):
    """
    Set attributes of an object taken from a dictionary(-like) object.
    
    Only keys which are lowercase are used.
    
    >>> class testobject(object):
    ...     pass
    ...
    >>> obj = testobject()
    >>> attribs = {'value': 0xaffe, 'name': 'ck', 'Hobby': 'horse riding'}
    >>> set_attributes(attribs, obj)
    >>> vars(obj)
    {'name': 'ck', 'value': 45054}
    """
    
    for key, value in src.items():
        if key.islower(): # uppercase: SoftM fild names, lowercase: plain-text field names
            setattr(dest, key, value)


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
        self.assertEqual(softm2date('1070605'), datetime.date(2007, 6, 5))
        self.assertEqual(date2softm(datetime.datetime(2007, 6, 5)), '1070605')
        self.assertEqual(date2softm(softm2date('1070605')), '1070605')
        self.assertEqual(softm2date('       '), None)
        self.assertEqual(softm2date(''), None)
    

if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    unittest.main()
    sys.exit(failure_count)
