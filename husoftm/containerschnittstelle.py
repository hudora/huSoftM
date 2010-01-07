#!/usr/bin/env python
# encoding: utf-8
""" filename --- Describe Tool here äöü---
"""

# filename
# Created by Christoph Borgolte on 07-01-2010 for HUDORA.
# Copyright (c) 2009 HUDORA. All rights reserved.

__revision__ = '$Revision: 6040 $'


from husoftm.connection2 import get_connection, as400_2_int
from husoftm.tools import sql_escape, sql_quote

# debugging stuff
import datetime
from decimal import Decimal
__testdata__ = [{'basename': datetime.datetime(2009, 6, 5, 10, 32, 35),
                 'bestellnr': 43386,
                 'bestellpositionen': [{'art': 'ZS',
                                 'artnr': '76081',
                                 'bestell_preis': Decimal('0.36'),
                                 'bestellnr': 43386,
                                 'bestellpos': 1,
                                 'bezogener_satz_EWZ00': 0,
                                 'buchungsbetrag': Decimal('699.84'),
                                 'buchungstext': '76081',
                                 'erfassungs_date': datetime.date(2009, 5, 18),
                                 'info_intern': '',
                                 'konto': '31000',
                                 'kurs_zugang': 757.00227099999995,
                                 'kursfaktor_zugang': 3,
                                 'lager': 100,
                                 'lagerbewegung_rechnung': 956478,
                                 'lagerbewegung_zugang': 956478,
                                 'menge_berechnet': 1944.0,
                                 'opnr': 0,
                                 'rechnungs_date': datetime.date(2009, 5, 10),
                                 'rechnungsnr': 900004470,
                                 'rechnungsposnr': 1,
                                 'satznummer_warenzugang': 35585,
                                 'satzstatus': 'X',
                                 'status': 0,
                                 'tatsaechlicher_preis': Decimal('0.36'),
                                 'warenvereinnahmungsnr': 4155,
                                 'zugang_date': datetime.date(2009, 6, 5)}],
                 'dateifuehrungsschluessel': '',
                 'firma': '01',
                 'satznr': 2,
                 'status': 'S',
                 'warenvereinnahmungsnr': 4155,
                 'zugang_date': datetime.date(2009, 6, 5),
                 'zugang_time': 103235},
                {'basename': datetime.datetime(2009, 8, 26, 9, 34, 25),
                 'bestellnr': 43541,
                 'bestellpositionen': [{'art': 'ZS',
                                 'artnr': '76081',
                                 'bestell_preis': Decimal('0.36'),
                                 'bestellnr': 43541,
                                 'bestellpos': 1,
                                 'bezogener_satz_EWZ00': 0,
                                 'buchungsbetrag': Decimal('881.28'),
                                 'buchungstext': '76081',
                                 'erfassungs_date': datetime.date(2009, 8, 12),
                                 'info_intern': '',
                                 'konto': '31000',
                                 'kurs_zugang': 717.00007200000005,
                                 'kursfaktor_zugang': 3,
                                 'lager': 100,
                                 'lagerbewegung_rechnung': 1007988,
                                 'lagerbewegung_zugang': 1007988,
                                 'menge_berechnet': 2448.0,
                                 'opnr': 0,
                                 'rechnungs_date': datetime.date(2009, 7, 27),
                                 'rechnungsnr': 900004647,
                                 'rechnungsposnr': 1,
                                 'satznummer_warenzugang': 36152,
                                 'satzstatus': 'X',
                                 'status': 0,
                                 'tatsaechlicher_preis': Decimal('0.36'),
                                 'warenvereinnahmungsnr': 4364,
                                 'zugang_date': datetime.date(2009, 8, 26)}],
                 'dateifuehrungsschluessel': '',
                 'firma': '01',
                 'satznr': 3,
                 'status': 'S',
                 'warenvereinnahmungsnr': 4364,
                 'zugang_date': datetime.date(2009, 8, 26),
                 'zugang_time': 93425}]


def list_containerschnittstelle():
    """Whats inside of the container interface."""
    # FIXME: Es gibt unterschiedliche Stati:
    #   'X' - logisch gelöscht -> nicht nutzen;
    #   'S' - Storno -> speziell nutzen???;
    #   ''  - normal -> nutzen
    rows = get_connection().query('ISZ00')#, fields=['*'])
    return rows


def list_bestellpositionen(bestellnr, warenvereinnahmungsnr):
    """Retrieve order postions of a certain order."""
    #rows = get_connection().query('EWZ00', condition="WZBSTN=%s and WZWVNR=%s and WZSTAT<>'X'" %
    # TODO: for debugging use deleted positions as well
    rows = get_connection().query('EWZ00', condition="WZBSTN=%s and WZWVNR=%s" %
                                  (sql_quote(bestellnr), sql_quote(warenvereinnahmungsnr)))
    return rows


def container_list():
    """Retrieves a list of every container and adds order positions to it."""
    containers = list_containerschnittstelle()
    for container in containers:
        container['bestellpositionen'] = list_bestellpositionen(container['bestellnr'],
                                                                container['warenvereinnahmungsnr'])
    return containers


def container_msg(order_dict):
    """Creates xml representations of inward good movements.

    Returns a list of xml representations for every orderline of the given order.
    """
    guidbase = "%(bestellnr)s-%(warenvereinnahmungsnr)s" % order_dict
    batchnr = guidbase
    msgs = []
    for pos in order_dict['bestellpositionen']:
        msg = {"guid": "%s-%s" % (guidbase, pos['bestellpos']),
               "menge": as400_2_int(pos['menge_berechnet']), # FIXME: ist das korrekt?
               "artnr": pos['artnr'],
               "batchnr": batchnr}
        msgs.append(msg)
    return msgs


if __name__ == '__main__':
    import pprint
    #pprint.pprint(container_list())
    for container in __testdata__:
        print(container_msg(container))
