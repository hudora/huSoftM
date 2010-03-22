#!/usr/bin/env python
# encoding: utf-8
"""
auftraege.py

Created by Christian Klein on 2010-03-15.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm.connection2 import get_connection
from husoftm.tools import sql_escape, sql_quote, date2softm


AUFTRAGSARTEN = {
    "": "Normalauftrag",
    "$": "Auftrag aus Stapelschnittstell",
    "A": "Abruf-Auftrag",
    "FO": "FOB-Auftrag direkt (o. Lager)",
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
    return AUFTRAGSARTEN[art]


def auftraege(mindate=None, maxdate=None, additional_conditions=None):
    """
    Alle Aufträge
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
    rows = get_connection().query('AAK00', ordering=['AKAUFN DESC', 'AKDTLT'], condition=condition)
    return rows


def get_auftrag(auftragsnr):
    
    rows = get_connection().query('AAK00',
        condition="AKSTAT<>'X' AND AKAUFN=%s" % sql_escape(auftragsnr))
    if len(rows) != 1:
        raise RuntimeError('inkonsistente Kopfdaten in AKSTAT')
    kopf = rows[0]
    
    positionen = get_connection().query(['AAP00'], ordering=['APAUFN DESC', 'APDTLT'],
        condition="APSTAT<>'X' AND APAUFN=%s" % sql_escape(auftragsnr))
    return kopf, positionen


def auftraege_for_kunde(kundennr):
    conditions = ["AKKDNR = %s" % sql_quote(artnr)]
    
    condition = " AND ".join(conditions)
    rows = get_connection().query('AAK00', ordering=['AKAUFN DESC', 'AKDTLT'], condition=condition)
    return rows
