#!/usr/bin/env python
# encoding: utf-8
"""
auftraege.py

Created by Christian Klein on 2010-03-15.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.tools import sql_escape, sql_quote, date2softm, land2iso, pad
from husoftm2.texte import texte_trennen, texte_auslesen
from husoftm2.backend import query
import husoftm2.sachbearbeiter


AUFTRAGSARTEN = {
    "": "Normalauftrag",
    "$": "Auftrag aus Stapelschnittstelle",
    "A": "Abruf-Auftrag",
    "FO": "FOB-Auftrag direkt (ohne Lager)",
    "GM": u"Gutschrift / Menge und Wert",
    "GW": u"Gutschrift / nur wertmäßig",
    "IM": u"Interne Gutschrift /Wert+Menge",
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


def _auftraege(additional_conditions=None, mindate=None, maxdate=None, limit=None, header_only=False):
    """
    Alle Aufträge ermitteln
    `additional_conditions` kann eine Liste von SQL-Bedingungen enthalten, die die Auftragssuche einschränken.
    `mindate` & `maxdate` können den Anliefertermin einschränken.
    `limit` kann die Zahl der zurückgelieferten Aufträge einschraenken. Dabei werden groessere
    Auftragsnummern zuerst zurueck gegeben.

    Rückgabewert sind dicts nach dem Lieferungprotokoll.
    Wenn header_only == True, werden nur Auftragsköpfe zurück gegeben, was deutlich schneller ist.
    """

    conditions = ["AKSTAT<>'X'",
                  "ADAART=1",
                  ]
    if mindate and maxdate:
        conditions.append("AKDTER BETWEEN %s AND %s" % (date2softm(mindate), date2softm(maxdate)))
    elif mindate:
        conditions.append("AKDTER > %s" % date2softm(mindate))
    elif maxdate:
        conditions.append("AKDTER < %s" % date2softm(maxdate))
    if additional_conditions:
        conditions.extend(additional_conditions)

    condition = " AND ".join(conditions)
    koepfe = {}
    kopftexte = {}

    # Köpfe und Adressen einlesen
    for kopf in query(['AAK00'], ordering=['AKAUFN DESC'], condition=condition,
                      joins=[('XKD00', 'AKKDNR', 'KDKDNR'),
                             ('XAD00', 'AKAUFN', 'ADRGNR'),
                             ('AAT00', 'AKAUFN', 'ATAUFN')],  # die Texte um nach GUIDs suchen zu können
                             limit=limit, ua='husoftm2.auftraege'):
        d = dict(kundennr="SC%s" % kopf['kundennr_warenempf'],
                 auftragsnr=kopf['auftragsnr'],
                 auftragsnr_kunde=kopf['auftragsnr_kunde'],
                 erfassung=kopf['AAK_erfassung_date'],
                 aenderung=kopf['AAK_aenderung_date'],
                 sachbearbeiter=husoftm2.sachbearbeiter.resolve(kopf['sachbearbeiter']),
                 anliefertermin=kopf['liefer_date'],
                 teillieferung_erlaubt=(kopf['teillieferung_erlaubt'] == 1),
                 erledigt=(kopf['voll_ausgeliefert'] == 1),
                 positionen=[],
                 # * *auftragsnr_kunde* - id of the order submitted by the customer
                 # * *info_kunde* - Freitext der für den Empfänger relevanz hat
                 )
        if kopf.get('XAD_name1'):
            d['lieferadresse'] = dict(name1=kopf['XAD_name1'],
                                      name2=kopf['XAD_name2'],
                                      name3=kopf['XAD_name3'],
                                      strasse=kopf['XAD_strasse'],
                                      land=husoftm2.tools.land2iso(kopf['XAD_laenderkennzeichen']),
                                      plz=kopf['XAD_plz'],
                                      ort=kopf['XAD_ort'],
                                      )
        koepfe[kopf['auftragsnr']] = d

    if header_only:
        return koepfe.values()

    allauftrnr = koepfe.keys()
    # Texte einlesen
    postexte, kopftexte = texte_auslesen(allauftrnr)
    while allauftrnr:
        # In 50er Schritten Auftragspositionen lesen und den 50 Aufträgen zuordnen
        batch = allauftrnr[:50]
        allauftrnr = allauftrnr[50:]

        # Positionen einlesen
        for row in query(['AAP00'], condition="APSTAT<>'X' AND APAUFN IN (%s)" % ','.join((str(x) for x in batch)),
                         ua='husoftm2.auftraege'):
            d = dict(menge=int(row['bestellmenge']),
                     artnr=row['artnr'],
                     liefer_date=row['liefer_date'],
                     menge_offen=int(row['menge_offen']),
                     fakturierte_menge=int(row['fakturierte_menge']),
                     erledigt=(row['voll_ausgeliefert'] == 1),
                     # 'position': 2,
                     # 'teilzuteilungsverbot': u'0',
                     )
            texte = postexte.get(row['auftragsnr'], {}).get(row['position'], [])
            texte, attrs = texte_trennen(texte)
            d['infotext_kunde'] = texte
            if 'guid' in attrs:
                d['guid'] = attrs['guid']
            koepfe[row['auftragsnr']]['positionen'].append(d)

        # Kopftexte zuordnen
        for auftragsnr, texte in kopftexte.items():
            texte, attrs = texte_trennen(texte)
            koepfe[auftragsnr]['infotext_kunde'] = texte
            if 'guid' in attrs:
                koepfe[auftragsnr]['guid'] = attrs['guid']

    return koepfe.values()


def get_auftrag_by_auftragsnr(auftragsnr, header_only=False):
    """Auftrag mit Auftragsnummer auftragsnr zurueckgeben"""

    auftragsnr = str(int(auftragsnr.strip('SO')))  # clean up, avoid attacks
    auftraege = _auftraege(["AKAUFN=%s" % sql_escape(auftragsnr)], header_only=header_only)
    if len(auftraege) > 1:
        raise RuntimeError("Mehr als ein Auftrag mit auftragsnr %s vorhanden" % auftragsnr)
    if not auftraege:
        return None
    return auftraege[0]


def get_auftrag_by_guid(guid, header_only=False):
    """Auftrag mit GUID guid zurueckgeben. ACHTUNG guids sind cniht zwingend eindeutig!"""
    # TO BE FIXED
    condition = "ATTX60 = %s AND ATAUPO = 0 AND ATTART = 8 AND ATAUFN=AKAUFN" % sql_quote("#:guid:" + guid)
    auftraege = _auftraege([condition], header_only=header_only)
    if len(auftraege) > 1:
        raise RuntimeError("Mehr als ein Auftrag mit guid %s vorhanden" % guid)
    if not auftraege:
        return None
    return auftraege[0]


def get_auftrag(nr, header_only=False):
    "Findet einen Auftrag anhand des bezeichners. Akzeptiert GUID oder AuftragsNr"
    if len(nr) > 9:
        tasks = [get_auftrag_by_guid, get_auftrag_by_auftragsnr]
    else:
        tasks = [get_auftrag_by_auftragsnr, get_auftrag_by_guid]
    for task in tasks:
        auftrag = task(nr, header_only=header_only)
        if auftrag:
            return auftrag
    return None


def auftraege_kunde(kundennr, limit=None, header_only=False):
    """Alle Aufträge für eine Kundennummer ermitteln.
    Gibt eine Liste von dict()s zurück."""
    kundennr = str(int(kundennr.strip('SC')))  # clean up, avoid attacks
    auftraege = _auftraege(["AKKDNR=%s" % pad('AKKDNR', kundennr)], limit=limit, header_only=header_only)
    return auftraege


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    header = False
    pprint(get_auftrag_by_auftragsnr('SO1163764', header_only=header))
    pprint(get_auftrag_by_guid('Online_20101202', header_only=header))
    pprint(get_auftrag('SO1163764', header_only=header))
    pprint(get_auftrag('Online_20101202', header_only=header))
    pprint(auftraege_kunde('SC66669', limit=20, header_only=header))


if __name__ == '__main__':
    from timeit import Timer
    #import cProfile, pstats
    #prof = cProfile.Profile()
    #prof = prof.runctx("_selftest()", globals(), locals())
    #stats = pstats.Stats(prof)
    #stats.sort_stats("time")  # Or cumulative
    #stats.print_stats(80)  # 80 = how many to print
    t = Timer("_selftest()", "from __main__ import _selftest")
    print t.timeit(1)
