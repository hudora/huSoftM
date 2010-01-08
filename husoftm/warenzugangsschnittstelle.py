#!/usr/bin/env python
# encoding: utf-8

""" warenzugangsschnittstelle.py --- Zugriff auf die Schnittstelle für Warenzugänge (ISZ00) ---

Die ISZ00 ist eine Schnittstelle, die eingerichtet wurde, um Warenzugänge an das Lager 100 mit unserer Lagerverwaltungssoftware myPL abzuwickeln.
Siehe hierzu auch http://github.com/hudora/huSoftM/blob/master/doc/Lagerschnittstelle.pdf.


Created by Christoph Borgolte on 07-01-2010 for HUDORA.
Copyright (c) 2009 HUDORA. All rights reserved.


Funktionen:

  list_warenzugang - Listet alle noch nicht gelöschten oder stornierten Zugänge auf, die sich in der Schnittstelle befinden.
  list_warenzugang_storno - Listet alle noch nicht gelöschten aber stornierten Zugänge auf, die sich in der Schnittstelle befinden.
Diese Daten werden jeweils mit den zugehörigen Bestellpositionen erweitert.

"""


from husoftm.connection2 import get_connection, as400_2_int
from husoftm.tools import sql_escape, sql_quote

from husoftm.bestellungen import get_zugaenge_warenvereinnahmungsnr_simple


def list_warenzugang():
    """Listet alle noch nicht gelöschten oder stornierten Zugänge auf.

    Die Informationen aus der ISZ00 werden noch durch die zugehörigen Bestellpositionen erweitert.
    Rückgabewert ist eine Liste von Dictionaries, die jeweils eine komplette Bestellung repräsentieren.
    """
    rows = get_connection().query('ISZ00', condition="IZSTAT<>'X' AND IZSTAT<>'S'")
    return _update_zugangsinfo(rows)


def list_warenzugang_storno():
    """Listet alle stornierten Zugänge auf.

    Die Informationen aus der ISZ00 werden noch durch die zugehörigen Bestellpositionen erweitert.
    Rückgabewert ist eine Liste von Dictionaries, die jeweils eine komplette Bestellung repräsentieren.
    """
    rows = get_connection().query('ISZ00', condition="IZSTAT<>'X' AND IZSTAT='S'")
    return _update_zugangsinfo(rows)


def _update_zugangsinfo(zugaenge):
    """Fügt die zugehörigen Bestellpositionen zu jeder Bestellung der übergebenen Liste hinzu. """
    for zugang in zugaenge:
        zugang['bestellpositionen'] = get_zugaenge_warenvereinnahmungsnr_simple(zugang['bestellnr'],
                                                                                zugang['warenvereinnahmungsnr'])
    return zugaenge


if __name__ == '__main__':
    import pprint
    print "zugang"
    pprint.pprint(list_warenzugang())
    print "storno"
    pprint.pprint(list_warenzugang_storno())
