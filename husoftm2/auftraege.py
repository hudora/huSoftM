#!/usr/bin/env python
# encoding: utf-8
"""
auftraege.py

Created by Christian Klein on 2010-03-15.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.tools import sql_escape, sql_quote, date2softm, land2iso, pad
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
                  'ATAUFN=AKAUFN',
                  'ATAUPO=0',
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
    # Köfe samt Kopftexten einlesen
    # TODO: was ist, wenn es keine Kopftexte gibt?
    for kopf in query(['AAK00', 'AAT00'], ordering=['AKAUFN DESC'], condition=condition,
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
        if kopf['text']:
            kopftexte.setdefault(kopf['auftragsnr'], []).append(kopf['text'])
        koepfe[str(kopf['auftragsnr'])] = d

    # Texte zusortieren
    for kopf in koepfe.values():
        # Zusatzdaten aus Kopftexten extrahieren
        for text in kopftexte.get(kopf['auftragsnr'], []):
            if text.startswith('#:'):
                key = str(text.split(':')[1])
                value = ':'.join(text.split(':')[2:])
                if key not in ['guid']:
                    raise RuntimeError("Unbekannte Auftragszusatzdaten: %s:%s" % key, value())
                kopf[key] = value
            else:
                kopf.setdefault('infotext_kunde', []).append(text)
        if kopf.get('infotext_kunde'):
            kopf['infotext_kunde'] = '\n'.join(kopf['infotext_kunde'])
        # Auftragsnummer normalisieren
        kopf['auftragsnr'] = 'SO%s' % kopf['auftragsnr']

    if header_only:
        return koepfe.values()

    allauftrnr = koepfe.keys()
    while allauftrnr:
        # In 50er Schritten Auftragspositionen lesen und den 50 Aufträgen zuordnen
        batch = allauftrnr[:50]
        allauftrnr = allauftrnr[50:]
        for row in query(['AAP00'], condition="APSTAT<>'X' AND APAUFN IN (%s)" % ','.join(batch)):
            d = dict(menge=int(row['bestellmenge']),
                     artnr=row['artnr'],
                     liefer_date=row['liefer_date'],
                     menge_offen=int(row['menge_offen']),
                     fakturierte_menge=int(row['fakturierte_menge']),
                     erledigt=(row['voll_ausgeliefert'] == 1),
                     # 'position': 2,
                     # 'teilzuteilungsverbot': u'0',
                     )
            koepfe[str(row['auftragsnr'])]['positionen'].append(d)

        # Gibt es eine abweichende Lieferadresse?
        for row in query(['XAD00'], condition="ADAART=1 AND ADRGNR IN (%s)" % ','.join(batch)):
            row['land'] = land2iso(row['laenderkennzeichen'])
            del row['laenderkennzeichen']
            koepfe[str(row['nr'])]['lieferadresse'] = row
            del koepfe[str(row['nr'])]['lieferadresse']['nr']

    # fehlen noch Adressen? - Aus den Stammdaten extrahieren
    auftr2kunde = dict([(x['auftragsnr'], x['kundennr']) for x in koepfe.values() if 'name1' not in x])
    kundennrbatch = [x.strip('SC') for x in set(auftr2kunde.values())]
    if kundennrbatch:
        kundenadressen = {}
        for row in query(['XKD00'], condition="KDKDNR IN (%s)" % ','.join([pad('KDKDNR', x) for x in kundennrbatch])):
            row['kundennr'] = "SC%s" % row['kundennr']
            kundenadressen[row['kundennr']] = row
        for kopf in koepfe.values():
            if 'name1' not in kopf:
                kopf.update(kundenadressen[kopf['kundennr']])
    # Länderkennzeichen in ISO-Code wandeln
    for kopf in koepfe.values():
        kopf['land'] = land2iso(kopf['laenderkennzeichen'])
        del kopf['laenderkennzeichen']
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
    #from pprint import pprint
    header = False
    (get_auftrag_by_auftragsnr('SO1163764', header_only=header))
    (get_auftrag_by_guid('Online_20101202', header_only=header))
    (get_auftrag('SO1163764', header_only=header))
    (get_auftrag('Online_20101202', header_only=header))
    (auftraege_kunde('SO66669', limit=20, header_only=header))


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
