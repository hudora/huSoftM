#!/usr/bin/env python
# encoding: utf-8
"""
auftraege.py

Created by Christian Klein on 2010-03-15.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm.connection2 import get_connection
from husoftm.tools import sql_escape, sql_quote, date2softm, pad

AUFTRAGSARTEN = {
    "": "Normalauftrag",
    "$": "Auftrag aus Stapelschnittstelle",
    "A": "Abruf-Auftrag",
    "FO": "FOB-Auftrag direkt (ohne Lager)",
    "GM": "Gutschrift / Menge und Wert",
    "GW": u"Gutschrift / nur wertmäßig",
    "IM": "Interne Gutschrift /Wert+Menge",
    "IW": u"Interne Gutschrift / Wertmäßig",
    "KO": "Konfektionierungsauftrag",
    "ME": "Messeauftrag",
    "MU": "Musterlieferung",
    "NB": u"Nachbelastung / nur wertmäßig",
    "PK": "Personalkauf",
    "R": "Rahmenauftrag",
    "S": "Serviceauftrag",
    "SR": "Sofort-Rechnung (wertm.)",
    "S3": u"Streckengeschäft Direkt",
    "S4": u"Streckengeschäft über Lager",
    "U": "Umlagerungsauftrag",
    "WA": "Werbeauftrag",
    "Z2": "Zuteilungsvorlauf 2 Wochen",
}


def auftragsart(art):
    return AUFTRAGSARTEN.get(art, 'Unbekannt')


def auftraege(mindate=None, maxdate=None, additional_conditions=None, limit=None):
    """
    Alle Aufträge ermitteln
    
    additional_conditions kann SQL-Bedingungen enthalten, die
    die Auftragssuche einschränken.
    
    TODO: Wenn Datum in additional_conditions, dann Datumsfelder überprüfen und ggf. konvertieren
    """
    
    conditions = ["AKSTAT<>'X'"]
    
    if mindate and maxdate:
        conditions.append("AKDTER BETWEEN %s AND %s" % (date2softm(mindate), date2softm(maxdate)))
    elif mindate:
        conditions.append("AKDTER > %s" % date2softm(mindate))
    elif maxdate:
        conditions.append("AKDTER < %s" % date2softm(maxdate))
    
    # You should REALLY know what you are doing!
    if additional_conditions:
        conditions.extend(additional_conditions)
    
    condition = " AND ".join(conditions)
    rows = get_connection().query('AAK00', ordering=['AKAUFN DESC', 'AKDTLT'], condition=condition, limit=limit)
    return rows


# def get_auftragskopf(auftragsnr):
#     """Auftragskopf für Auftrag ermitteln"""
#     
#     rows = get_connection().query('AAK00',
#         condition="AKSTAT<>'X' AND AKAUFN=%s" % sql_quote(auftragsnr))
#     if len(rows) != 1:
#         raise RuntimeError('inkonsistente Kopfdaten in AAK00 für Auftragsnr %s' % auftragsnr)
#     return rows[0]


def get_auftrag(auftragsnr):
    """Auftrag mit Auftragsnummer auftragsnr ermitteln"""
        
    rows = get_connection().query('AAK00',
        condition="AKSTAT<>'X' AND AKAUFN=%s" % sql_escape(auftragsnr))
    if len(rows) != 1:
        raise RuntimeError('inkonsistente Kopfdaten in AAK00 für Auftragsnr %s' % auftragsnr)
    kopf = rows[0]
    
    positionen = get_connection().query(['AAP00'], ordering=['APAUFN DESC', 'APDTLT'],
        condition="APSTAT<>'X' AND APAUFN=%s" % sql_escape(auftragsnr))
    return kopf, positionen


def auftraege_for_kunde(kundennr, limit=None):
    """Alle Aufträge für eine Kundennummer ermitteln"""
    
    condition = "AKKDNR = %s" % sql_quote(pad('AKKDNR', kundennr))
    rows = get_connection().query('AAK00', ordering=['AKAUFN DESC', 'AKDTLT'],
                                  condition=condition, limit=limit)
    return rows


def auftraege_for_artnr(artnr, additional_conditions=None, limit=None):
    """Alle Aufträge zu einer Artikelnummer"""
    conditions = ['AAK00.AKAUFN = ALN00.LNAUFN', 'ALN00.LNARTN=%s' % sql_quote(artnr)]
    if additional_conditions:
        conditions.extend(additional_conditions)
    condition = " AND ".join(conditions)
    rows = get_connection().query(['AAK00', 'ALN00'], condition=condition,
                                  ordering=['AKAUFN DESC', 'AKDTLT'], limit=limit)
    return rows


def lieferadresse(auftragsnr):
    """Lieferadresse für Auftrag ermitteln"""
    
    conn = get_connection()
    # Gibt es eine abweichende Lieferadresse?
    condition = "ADAART = 1 AND ADRGNR=%s " % sql_quote(auftragsnr)
    rows = conn.query('XAD00', condition=condition)
    
    # if len(rows) > 1:
    #     raise RuntimeError("Es gibt mehr als eine abweichende Lieferadresse für Auftrag %s" % auftragsnr)
    # elif len(rows) == 1:
    #     return rows[0]
    if len(rows) > 0:
        return rows[0]
    
    condition = "AKAUFN=%s AND AKKDNR = KDKDNR" % sql_quote(auftragsnr)
    rows = conn.query(['AAK00', 'XKD00'], condition=condition,
                      fields=['KDNAME', 'KDNAM2', 'KDNAM3', 'KDNAM4', 'KDSTR', 'KDPLZ', 'KDORT', 'KDLKZ'])
    if len(rows) != 1:
        raise RuntimeError('inkonsistente Kopfdaten in AAK00 für Auftragsnr %s' % auftragsnr)
    
    return rows[0]


def find_text(text):
    """Durchsucht alle Auftragstexte nach 'text'.
    
    Rückgabewert ist eine Liste mit gefundenen Texten und dazugehörigen Auftragsnummern.

    Das ist ganz nützlich, wenn nach einem GUID gesucht werden soll:
    
    >>> find_text('EUDA4BGY')
    [{'auftragsnr': 1130969, 'text': 'Referenz: 0EUDA4BGYJCLT4VSNI2CP5XCPDQ'},
     {'auftragsnr': 1130970, 'text': 'Referenz: 1EUDA4BGYJCLT4VSNI2CP5XCPDQ'}]
    """
    rows = get_connection().query('AAT00', fields=['ATTX60', 'ATAUFN'], condition="ATTX60 LIKE %s" % sql_quote("%%%s%%" % text))
    return rows


def get_guid(auftragsnr):
    """
    Gibt den GUID zu einer Auftragsnr zurück, sofern vorhanden.
    """
    condition = "ATTX60 LIKE %s AND ATAUFN = %s AND ATAUPO = 0 AND ATTART = 8" % (sql_quote("#:guid:%%"),
                                                                                  sql_quote(auftragsnr))
    rows = get_connection().query('AAT00', fields=['ATTX60'], condition=condition)
    if rows:
        return rows[0][0].replace('#:guid:', '')
    return ''


def get_auftragnr(guid):
    """
    Gibt die Auftragsnr zu einem GUID zurück, sofern vorhanden.
    """
    
    condition = "ATTX60 = %s AND ATAUPO = 0 AND ATTART = 8" % sql_quote("#:guid:" + guid)
    rows = get_connection().query('AAT00', fields=['ATAUFN'], condition=condition)
    if rows:
        return rows[0][0]
    return None
