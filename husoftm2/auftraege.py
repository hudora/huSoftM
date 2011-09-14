#!/usr/bin/env python
# encoding: utf-8
"""
auftraege.py

Created by Christian Klein on 2010-03-15.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.tools import sql_escape, sql_quote, date2softm, pad, add_prefix, remove_prefix
from husoftm2.texte import texte_trennen, txt_auslesen
from husoftm2.backend import query
import datetime
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
    "SF": "Sofortfaktura",
    "SR": "Sofort-Rechnung (wertm.)",
    "S3": u"Streckengeschäft Direkt",
    "S4": u"Streckengeschäft über Lager",
    "U": "Umlagerungsauftrag",
    "WA": "Werbeauftrag",
    "Z2": "Zuteilungsvorlauf 2 Wochen",
}


def _auftraege(additional_conditions=None, addtables=None, mindate=None, maxdate=None, limit=None,
               header_only=False, canceled=False):
    """
    Alle Aufträge ermitteln
    `additional_conditions` kann eine Liste von SQL-Bedingungen enthalten, die die Auftragssuche
    einschränken.
    `mindate` & `maxdate` können den Anliefertermin einschränken.
    `limit` kann die Zahl der zurückgelieferten Aufträge einschraenken. Dabei werden groessere
    Auftragsnummern zuerst zurueck gegeben.
    `header_only` ruft nur Auftragsköpfe ab und ist bedeutend schneller
    `canceled` wenn True, werden auch stornierte Aufträge zurück gegeben.

    Rückgabewert sind dicts nach dem Lieferungprotokoll.
    Wenn header_only == True, werden nur Auftragsköpfe zurück gegeben, was deutlich schneller ist.
    """

    # Solange der Client das nciht gesondert verlangt, werden stornierte Aufträge ignoriert.
    if not canceled:
        conditions = ["AKSTAT<>'X'"]
    else:
        conditions = []
    # Anliefertermin ist ein Range
    if mindate and maxdate:
        conditions.append("AKDTER BETWEEN %s AND %s" % (date2softm(mindate), date2softm(maxdate)))
    # Anliefertermin ist nach unten begrenzt
    elif mindate:
        conditions.append("AKDTER > %s" % date2softm(mindate))
    # Anliefertermin ist nach oben begrenzt
    elif maxdate:
        conditions.append("AKDTER < %s" % date2softm(maxdate))
    # vom Aufrufer direkt angegebenen, weitere SQL Bedingungen zufügen. Diese werden mit `AND` verkettet.
    if additional_conditions:
        conditions.extend(additional_conditions)

    condition = " AND ".join(conditions)
    koepfe = {}
    kopftexte = {}

    auftragsnr_to_lieferadresse_kdnr = {}

    if addtables is None:
        addtables = []

    # Köpfe und Adressen einlesen
    for kopf in query(['AAK00'] + addtables, ordering=['AKAUFN DESC'], condition=condition,
                      joins=[('XKD00', 'AKKDNR', 'KDKDNR')],
                             limit=limit, ua='husoftm2.auftraege'):
        d = dict(kundennr="SC%s" % kopf['kundennr_warenempf'],
                 kundennr_rechnung="SC%s" % kopf['kundennr_rechnungsempf'],
                 name1=kopf['name1'],
                 name2=kopf['name2'],
                 name3=kopf['name3'],
                 strasse=kopf['strasse'],
                 land=husoftm2.tools.land2iso(kopf['laenderkennzeichen']),
                 plz=kopf['plz'],
                 ort=kopf['ort'],
                 auftragsnr="SO%s" % kopf['auftragsnr'],
                 auftragsnr_kunde=kopf['auftragsnr_kunde'],
                 erfassung=kopf['AAK_erfassung_date'],
                 aenderung=kopf['AAK_aenderung_date'],
                 sachbearbeiter=husoftm2.sachbearbeiter.resolve(kopf['sachbearbeiter']),
                 anliefertermin=kopf['liefer_date'],
                 teillieferung_erlaubt=(kopf['teillieferung_erlaubt'] == 1),
                 # TODO: md: ich denke, "erledigt" ist ein Auftrag auch, wenn er storneirt wurde,
                 # oder wenn alle Positionen auf voll_ausgeliefert stehen.
                 erledigt=(kopf['voll_ausgeliefert'] == 1),
                 positionen=[],
                 art=kopf['art'],
                 storniert=(kopf['aak_status'] == 'X'),
                 # * *info_kunde* - Freitext der für den Empfänger relevanz hat
                 )
        koepfe[kopf['auftragsnr']] = d

        # Auftrag geht an die 'normale' Lieferadresse: Kein .\d\d\d-Suffix an die `lieferadresse.kundennr`
        if kopf['versandadressnr'] == 0:
            auftragsnr_to_lieferadresse_kdnr[kopf['auftragsnr']] = add_prefix(kopf['kundennr_warenempf'],
                                                                              'SC')
        # Auftrag geht an eine abweichende Lieferadresse: .00?-Suffix an die `lieferadresse.kundennr` hängen.
        else:
            lieferadresse_kdnr = add_prefix("%s.%03d" % (kopf['kundennr_warenempf'], kopf['versandadressnr']),
                                            "SC")
            auftragsnr_to_lieferadresse_kdnr[kopf['auftragsnr']] = lieferadresse_kdnr

    if header_only:
        return koepfe.values()

    allauftrnr = koepfe.keys()
    # Texte auslesen
    # Die dritte und vierte Position des Werts von txt_auslesen sind posdaten und kopfdaten.
    # Es handelt sich dabei wohl um Texte, die nicht angedruckt werden sollen.
    # Bis auf weiteres werden diese hier ignoriert.
    postexte, kopftexte, _, _ = txt_auslesen(allauftrnr)
    while allauftrnr:
        # In 50er Schritten Auftragspositionen lesen und den 50 Aufträgen zuordnen
        batch = allauftrnr[:50]
        allauftrnr = allauftrnr[50:]

        # Abweichende Lieferadressen
        for row in query(['XAD00'], ua='husoftm2.lieferscheine',
                         condition="ADAART=1 AND ADRGNR IN (%s)" % ','.join([str(x) for x in batch])):
            koepfe[row['nr']]['lieferadresse'] = dict(name1=row['name1'],
                                                      name2=row['name2'],
                                                      name3=row['name3'],
                                                      strasse=row['strasse'],
                                                      land=husoftm2.tools.land2iso(row['laenderkennzeichen']),
                                                      plz=row['plz'],
                                                      kundennr=auftragsnr_to_lieferadresse_kdnr[row['nr']],
                                                      ort=row['ort'])
        # Positionen einlesen
        for row in query(['AAP00'], condition="APSTAT<>'X' AND APAUFN IN (%s)" % ','.join([str(x)
                                                                                           for x in batch]),
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

            # Preis einfügen
            if row.get('verkaufspreis'):
                d['preis'] = row['verkaufspreis']

            texte = postexte.get(row['auftragsnr'], {}).get(row['position'], [])
            texte, attrs = texte_trennen(texte)
            d['infotext_kunde'] = texte
            if 'guid' in attrs:
                d['guid'] = attrs['guid']
            koepfe[row['auftragsnr']]['positionen'].append(d)

        # Kopftexte zuordnen
        for auftragsnr, texte in kopftexte.items():
            texte, attrs = texte_trennen(texte)
            koepfe[remove_prefix(auftragsnr, 'SO')]['infotext_kunde'] = texte
            if 'guid' in attrs:
                koepfe[remove_prefix(auftragsnr, 'SO')]['guid'] = attrs['guid']

    return koepfe.values()


def get_auftrag_by_auftragsnr(auftragsnr, header_only=False):
    """Auftrag mit Auftragsnummer auftragsnr zurueckgeben"""

    auftragsnr = remove_prefix(auftragsnr, 'SO')
    auftraege = _auftraege(["AKAUFN=%s" % sql_escape(auftragsnr)], header_only=header_only)
    if len(auftraege) > 1:
        raise RuntimeError("Mehr als ein Auftrag mit auftragsnr %s vorhanden" % auftragsnr)
    if not auftraege:
        return None
    return auftraege[0]


def get_auftrag_by_auftragsnr_tmp(auftragsnr_tmp, header_only=False):
    """Auftrag anhand der auftragsnr_tmp zurueckgeben. Wird von ic2/auftrag verwendet."""
    conditions = ["ATTX60=%s" % sql_quote("#:auftragsnr_tmp:" + auftragsnr_tmp),
                  "ATAUPO=0",
                  "ATTART=8",
                  "ATAUFN=AKAUFN"]
    auftraege = _auftraege([" AND ".join(conditions)], addtables=['AAT00'],
                           header_only=header_only, canceled=True)
    if len(auftraege) > 1:
        raise RuntimeError("Mehr als ein Auftrag mit auftragsnr_tmp %s vorhanden" % auftragsnr_tmp)
    if not auftraege:
        return None
    return auftraege[0]


def get_auftrag_by_guid(guid, header_only=False):
    """Auftrag mit GUID guid zurueckgeben. ACHTUNG guids sind nicht zwingend eindeutig!"""
    # TO BE FIXED - md: was ist hier zu fixen?
    condition = "ATTX60 = %s AND ATAUPO = 0 AND ATTART = 8 AND ATAUFN=AKAUFN" % sql_quote("#:guid:" + guid)
    auftraege = _auftraege([condition], addtables=['AAT00'], header_only=header_only)
    if len(auftraege) > 1:
        raise RuntimeError("Mehr als ein Auftrag mit guid %s vorhanden" % guid)
    if not auftraege:
        return None
    return auftraege[0]


def get_auftrag(nr, header_only=False):
    "Findet einen Auftrag anhand des Bezeichners. Akzeptiert GUID oder AuftragsNr"
    if len(nr) > 9:
        tasks = [get_auftrag_by_guid, get_auftrag_by_auftragsnr]
    else:
        tasks = [get_auftrag_by_auftragsnr, get_auftrag_by_guid]
    for task in tasks:
        auftrag = task(nr, header_only=header_only)
        if auftrag:
            return auftrag
    return None


def get_guid(auftragsnr):
    """
    Gibt den GUID zu einer Auftragsnr zurück, sofern vorhanden.
    """
    auftragsnr = remove_prefix(auftragsnr, 'SO')
    condition = "ATTX60 LIKE %s AND ATAUFN = %s AND ATAUPO = 0 AND ATTART = 8" % (sql_quote("#:guid:%%"),
                                                                                  sql_quote(auftragsnr))
    rows = query('AAT00', fields=['ATTX60'], condition=condition)
    if rows:
        return rows[0][0].replace('#:guid:', '')
    return ''


def auftraege_kunde(kundennr, limit=None, header_only=False):
    """Alle Aufträge für eine Kundennummer ermitteln.
    Gibt eine Liste von dict()s zurück."""
    kundennr = remove_prefix(kundennr, 'SC')
    auftraege = _auftraege(["AKKDNR=%s" % pad('AKKDNR', kundennr)], limit=limit, header_only=header_only)
    return auftraege


def get_auftragsarten_by_auftragsnrs(auftragsnrs):
    """Ermittelt die Auftragsart (Freitext) der Aufträge der gegebenen Auftragsnummern.

    auftragsnrs: liste von Auftragsnummern
    return: Dictionary Auftragnr -> Auftragsart
    """
    condition = "AKAUFN in (%s)" % ','.join(sql_quote(remove_prefix(nr, 'SO')) for nr in auftragsnrs)
    rows = query(['AAK00'], fields=['AKAUFN', 'AKAUFA'], condition=condition)
    return dict(("SO%s" % row['auftragsnr'], AUFTRAGSARTEN[row['art']]) for row in rows)


def verspaetete_auftraege(datum=None):
    """Gibt eine Liste von Auftragsnummern zurück, die vor <datum> ausgeliefert hätten werden müssen,
    aber noch nicht ausgeliefert sind.
    """

    if not datum:
        datum = datetime.date.today()

    conditions = ["AKKZVA=0",     # Auftrag nicht voll ausgeliefert
                  "APKZVA=0",     # Position nicht voll ausgeliefert
                  "AKAUFN>0",     # Auftragsnummer wurde vergeben
                  "AKSTAT<>'X'",  # Auftrag nicht glöscht
                  "APSTAT<>'X'",  # Position nicht gelöscht
                  'APDTLT < %s' % date2softm(datum),
                  ]
    rows = query(['AAK00'],
                     condition=' AND '.join(conditions),
                     grouping=['AKAUFN'], fields=['AKAUFN'], ordering=['AKAUFN DESC'],
                     joins=[('AAP00', 'AKAUFN', 'APAUFN')],
                     ua='husoftm2.auftraege.verspaetete_auftraege')
    return ["SO%s" % row[0] for row in rows]
    return _auftraege(conditions, addtables=['AAT00'], header_only=False)


def get_rahmenauftraege(kundennr, artnr):
    """Gib die Auftragsnummern aller offenen Rahmenaufträge für den Kunden und den Artikel als Liste zurück.

    >>> get_rahmenauftraege('SC66663', '14600')
    ['SO1205711']
    """
    kundennr = remove_prefix(kundennr, 'SC')
    conditions = ["AKAUFN=APAUFN", "AKKZVA=0", "APKZVA=0", "AKSTAT<>'X'", "APSTAT<>'X'", "AKAUFN>0",
                  "AKAUFA='R'", "AKKDNR=%s" % pad('AKKDNR', kundennr), "APARTN=%s" % sql_quote(artnr)]
    rows = query(['AAK00', 'AAP00'], condition=' AND '.join(conditions), fields=['AKAUFN'],
                 ua='husoftm2.auftraege.get_rahmenauftraege')
    return [add_prefix(row[0], 'SO') for row in rows]


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
