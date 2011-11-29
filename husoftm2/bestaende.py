#!/usr/bin/env python
# encoding: utf-8

"""
bestaende.py - high level functions to access SoftM on the AS/400.

Created by Maximillian Dornseif on 2006-10-19.
Copyright (c) 2006-2010 HUDORA. All rights reserved.

Hier werden Warenbestände, verfügbare Mengen und dergleichen ermittelt.

Für die Frage, ob wir einen Artikel verkaufen können ist freie_menge() die richtige Funktion.
Für die Frage, ob ein bestimmter Artikel in einem bestimmten Lager ist, ist bestand() geignet.

    bestellmengen(artnr)                          von uns bei Lieferanten bestellte Mengen
    auftragsmengen(artnr, lager=0)                bei uns von Kunden beauftragte Mengen
    umlagermenge(artnr, lager)                    Menge, die zur Zeit von einem Lager ans andere
                                                  unterwegs ist
    buchbestand(artnr, lager=0)                   Artikel am Lager
    buchbestaende(lager=0)                        Alle Artikel an einem Lager
    bestandsentwicklung(artnr)                    Prognose der Bestandsänderungen
    freie_menge(artnr)                            Menge, die Verkauft werden kann
    ist_frei_am(menge, artnr, date)               Ist eine bestimmte menge an date zu haben?
    frei_ab(menge, artnr, dateformat="%Y-%m-%d")  ab wann ist eine bestimmte Menge frühstens verfügbar?
    bestand(artnr, lager)                         Wieviel ist zur Zeit an einem Lager oder trifft
                                                  kurzum ein?
    artikelverfuegbarkeit(artnr, lage, max_date, resolve_sets)
                                                  Bestimme alle Mengenänderungen für einen Artikel für
                                                  die Artikelverfügbarkeit


Es gibt verschiedene Mengen von denen wir reden.
 * Bezogen aufs Lager
   * verfügbare menge - ist die Menge die wir am Lager haben und die zur Zeit noch keinem Kundenauftrag
     zugeordnet ist, d.h. die noch nicht zugeteilt ist.
   * freie menge - ist die Menge die wir (heute) noch verkaufen können (entspricht der verfügbaren Menge
     abzüglich der noch nicht zugeteilten Kundenaufträge). Ware, die im Zulauf ist, wird dabei nicht
     berücksichtigt.

 * bezogen auf Aufträge
   * bestellmenge - wieviel will der Kunde haben will
   * gelieferte - menge wieviel schon geliefert wurde
   * offene menge - wieviel noch zu liefern ist

"""


from husoftm2.artikel import set_artikel
from husoftm2.tools import sql_quote, add_prefix, str2softmdate, date2softm
from husoftm2.backend import query, query_async, as400_2_int
import cs.masterdata.eaplight
import datetime
import husoftm2.artikel
import itertools
import time
import unittest


def buchbestaende(artnrs=None, lager=0):
    """Gibt die Buchbestand einiger oder aller Artikels für ein Lager zurück oder (lager=0) für alle Lager

    >>> buchbestand(['14600/03'])
    {u'14600/03': 338}
    """
    conditions = ["LFLGNR=%d" % int(lager),
                  "LFMGLP<>0",
                  "LFSTAT<>'X'"]
    if artnrs:
        conditions += ["LFARTN IN (%s)" % ','.join([sql_quote(artnr) for artnr in artnrs])]
    rows = query(['XLF00'], fields=['LFARTN', 'LFMGLP'], condition=' AND '.join(conditions))
    return dict([(artnr, int(menge)) for (artnr, menge) in rows])


def buchbestaende_async(artnrs=None, lager=0, returnhandler=lambda x: x):
    """Gibt die Buchbestand einiger oder aller Artikels für ein Lager zurück oder (lager=0) für alle Lager

    >>> buchbestand(['14600/03'])
    {u'14600/03': 338}
    """

    def formatresult(rows):
        return returnhandler(dict([(artnr, int(menge)) for (artnr, menge) in rows]))

    conditions = ["LFLGNR=%d" % int(lager),
                  "LFMGLP<>0",
                  "LFSTAT<>'X'"]
    if artnrs:
        conditions += ["LFARTN IN (%s)" % ','.join([sql_quote(artnr) for artnr in artnrs])]
    return query_async(['XLF00'], fields=['LFARTN', 'LFMGLP'], condition=' AND '.join(conditions),
                       returnhandler=formatresult)


def buchbestand(artnr, lager=0):
    """Gibt den Buchbestand eines Artikels für ein Lager zurück oder (lager=0) für alle Lager

    >>> buchbestand('14600/03')
    338
    """
    ret = buchbestaende([artnr], lager)
    if ret:
        return int(buchbestaende([artnr], lager).values()[0])
    return 0


def buchbestand_async(artnr, lager=0):
    """Gibt den Buchbestand eines Artikels für ein Lager zurück oder (lager=0) für alle Lager

    >>> buchbestand('14600/03')
    338
    """

    def formatresult(data):
        if data:
            return data.values()[0]
        return 0

    return buchbestaende_async([artnr], lager, returnhandler=formatresult)


def bestellmengen(artnr, lager=0):
    """Liefert eine Liste mit allen bestellten aber noch nicht gelieferten Wareneingängen.

    >>> bestellmengen('14865')
    {datetime.date(2009, 2, 20): 1200,
     datetime.date(2009, 5, 5): 300}
    """
    return bestellmengen_async(artnr, lager).get_result()


def bestellmengen_async(artnr, lager=0):
    """Siehe bestellmengen()"""
    def formatresult(rows):
        return dict([(x['liefer_date'], as400_2_int(x['SUM(BPMNGB-BPMNGL)']))
                    for x in rows if as400_2_int(x['SUM(BPMNGB-BPMNGL)']) > 0])

    conditions = ["BPSTAT<>'X'",
                  "BPKZAK=0",
                  "BPARTN=%s" % sql_quote(artnr)]
    if lager:
        conditions += ["BPLGNR=%d" % int(lager)]

    # detailierte Informationen gibts in EWZ00
    return query_async('EBP00', fields=['BPDTLT', 'SUM(BPMNGB-BPMNGL)'], ordering='BPDTLT',
                 grouping='BPDTLT', condition=' AND '.join(conditions),
                 returnhandler=formatresult)


def bestellmengen2(artnr, lager=0, max_date=None):
    """Liefert eine Liste mit allen Bestellten aber noch nicht gelieferten Wareneingängen.

    husoftm2.bestaende.bestellmengen2 addiert Mengen und gruppiert nach Lieferdatum,
    so dass einzelne Bewegungen nicht nachzuvollziehen sind.
    Für eine Verfügbarkeitsanzeige wie in der SoftM Suite müssen die Sätze einzeln ermittelt werden.
    """

    conditions = ["BPSTAT<>'X'",
                  "BPKZAK=0",
                  "BPARTN=%s" % sql_quote(artnr)]

    # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 ergibt eine leere Rückgabe
    if lager:
        conditions += ["BPLGNR=%d" % int(lager)]

    # Nur Datensätze berücksichtigen, die nicht jünger sind als das geg. Datum
    if max_date:
        conditions.append("BPDTLT <= %s" % str2softmdate(max_date))

    rows = query('EBP00', ordering='BPDTLT', condition=' AND '.join(conditions))
    return rows


def bestellmengen_ausgeliefert(mindate=None, maxdate=None, artnrs=None, lager=0):
    """Gibt alle gelieferten Wareneingängen aus Bestellungen zurück.

    Eingrenzen kann man die Suche über den Zeitraum [mindate, maxdate], das Lager und eine Liste von
    Artikelnummern.

    Rückgabewert ist eine Liste von dicts, die jeweils den Liefertermin, die Artikelnummer, das Lager und die
    Menge enthalten.
    """
    conditions = ["BPSTAT<>'X'", "BPKZAK=1"]
    if lager:
        conditions += ["BPLGNR=%d" % int(lager)]
    if artnrs:
        conditions += ["BPARTN IN (%s)" % ','.join([sql_quote(artnr) for artnr in artnrs])]
    if mindate:
        conditions += ["BPDTLZ >= %s" % str2softmdate(mindate)]
    if maxdate:
        conditions += ["BPDTLZ <= %s" % str2softmdate(maxdate)]
    rows = query(['EBP00'], fields=['BPLGNR', 'BPDTLZ', 'BPARTN', 'BPMNGL'],
                 condition=' AND '.join(conditions))
    return rows


def auftragsmengen(artnr, lager=0):
    """Liefert eine Liste offener Aufträge (Warenausgänge) für einen Artikel OHNE UMLAGERUNGEN.

    >>> auftragsmengen(14865)
    {datetime.date(2009, 3, 2): 340,
     datetime.date(2009, 4, 1): 300,
     datetime.date(2009, 5, 4): 260,
     datetime.date(2009, 6, 2): 300}
    """
    return auftragsmengen_async(artnr, lager).get_result()


def auftragsmengen_async(artnr, lager=0, returnhandler=lambda x: x):
    "Siehe auftragsmengen()."

    def formatresult(rows):
        ret = dict([(x['liefer_date'], as400_2_int(x['menge_offen'])) for x in rows if x['menge_offen'] > 0])
        return returnhandler(ret)

    conditions = [
        "APARTN=%s" % (sql_quote(artnr)),  # Artikelnummer
        "AKAUFN=APAUFN",
        "AKAUFA<>'U'",                     # kein Umlagerungsauftrag
        "APSTAT<>'X'",                     # Position nicht logisch gelöscht
        "APKZVA=0",                        # Position nicht als 'voll ausgeliefert' markiert
        "(APMNG-APMNGF) > 0",              # (noch) zu liefernde menge ist positiv
        "AKSTAT<>'X'",                     # Auftrag nicht logisch gelöscht
        "AKKZVA=0"]                        # Auftrag nicht als 'voll ausgeliefert' markiert

    if lager:
        # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 gibt nix
        conditions = conditions + ["APLGNR=%d" % lager]
    return query_async(['AAP00', 'AAK00'], fields=['APDTLT', 'SUM(APMNG-APMNGF)'],
                   condition=' AND '.join(conditions),
                   ordering='APDTLT', grouping='APDTLT',
                   querymappings={'SUM(APMNG-APMNGF)': 'menge_offen', 'APDTLT': 'liefer_date'},
                   returnhandler=formatresult)


def auftragsmengen_geliefert_async(artnr, lager=0, returnhandler=lambda x: x):
    """Ausgelieferte menden nach tagen gruppiert ausgeben.
    >>> auftragsmengen_geliefert_async('12345').get_result()
    {datetime.date(2010, 12, 8): 16,
     datetime.date(2010, 9, 8): 8,
     datetime.date(2007, 8, 21): 10,
     ...}
    """

    def formatresult(rows):
        ret = dict([(x['liefer_date'], as400_2_int(x['menge_fakturiert'])) for x in rows if x['menge_fakturiert'] > 0])
        return returnhandler(ret)

    conditions = [
        "APARTN=%s" % (sql_quote(artnr)),  # Artikelnummer
        "AKAUFN=APAUFN",
        "AKAUFA<>'U'",                     # kein Umlagerungsauftrag
        "APSTAT<>'X'",                     # Position nicht logisch gelöscht
        "APMNGF > 0",                      # fakturierte Menge ist > 0
        "AKSTAT<>'X'",                     # Auftrag nicht logisch gelöscht
        ]

    if lager:
        # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 gibt nix
        conditions = conditions + ["APLGNR=%d" % lager]
    return query_async(['AAP00', 'AAK00'], fields=['APDTLT', 'SUM(APMNGF)'],
                   condition=' AND '.join(conditions),
                   ordering='APDTLT DESC', grouping='APDTLT',
                   querymappings={'SUM(APMNGF)': 'menge_fakturiert', 'APDTLT': 'liefer_date'},
                   returnhandler=formatresult)


def auftragsmengen_alle_artikel():
    """Liefert eine Liste offener Aufträge aller Artikel furu alle Läger.

    >>> auftragsmengen_alle_artikel(34)
    {'14550': {datetime.date(2008, 11, 30): 3450,
               datetime.date(2008, 12, 1): 8,
               datetime.date(2008, 12, 15): 5056},
     '14565': {datetime.date(2009, 2, 9): 750,
               datetime.date(2009, 3, 23): 1008,
               datetime.date(2009, 4, 27): 625},
     '14566': {datetime.date(2009, 2, 2): 4000,
               datetime.date(2009, 6, 1): 400},
     '14635': {datetime.date(2008, 11, 19): 20,
               datetime.date(2008, 11, 24): 763,
               datetime.date(2008, 11, 27): 200}}
    """

    conditions = [
    "AKAUFN=APAUFN",
    "AKAUFA<>'U'",               # keine Umlagerungen
    "APSTAT<>'X'",               # Position nicht logisch gelöscht
    "APKZVA=0",                  # Position nicht als 'voll ausgeliefert' markiert
    "(APMNG-APMNGF) > 0",        # (noch) zu liefernde menge ist positiv
    "AKSTAT<>'X'",               # Auftrag nicht logisch gelöscht
    "AKKZVA=0"]                  # Auftrag nicht als 'voll ausgeliefert' markiert

    rows = query(['AAP00', 'AAK00'],
            fields=['APARTN', 'APDTLT', 'SUM(APMNG-APMNGF)', 'COUNT(*)'],
            condition=' AND '.join(conditions),
            ordering='APDTLT', grouping=['APARTN', 'APDTLT'],
            querymappings={'SUM(APMNG-APMNGF)': 'menge_offen', 'APARTN': 'artnr',
                           'COUNT(*)': 'orderlines', 'APDTLT': 'liefer_date'})
    ret = {}
    for row in rows:
        if row['menge_offen']:
            ret.setdefault(str(row['artnr']), {})[row['liefer_date']] = (as400_2_int(row['menge_offen']),
                                                                         row['orderlines'])
    return ret


def auftragsmengen2(artnr, lager=0, max_date=None, resolve_sets=False):
    """Liefert eine Liste offener Aufträge (Warenausgänge) für einen Artikel.

    husoftm2.bestaende.auftragsmengen addiert Mengen und gruppiert nach Lieferdatum,
    so dass einzelne Bewegungen nicht nachzuvollziehen sind.
    Für eine Verfügbarkeitsanzeige wie in der SoftM Suite müssen die Sätze einzeln ermittelt werden.

    >>> auftragsmengen2('12345')
    [{'AAK_aenderung_date': None,
      'AAK_aenderung_time': 0,
      'AAK_erfassung': datetime.datetime(2011, 7, 25, 0, 0),
      'AAK_erfassung_date': datetime.date(2011, 7, 25),
      'AAK_erfassung_time': 0,
      'AAP_status': u'',
      'AAK_status': u'',
      'art': u'R',
      'artnr': u'12345',
      'auftragsart': u'R',
      'auftragsnr': 1205724,
      'auftragsnr_kunde': u'Scooter dezentral',
      'bestellmenge': Decimal('800.0'),
      'fakturierte_menge': Decimal('0.0'),
      'kundennr_rechnungsempf': u'64612',
      'kundennr_warenempf': u'64612',
      'lager': 200,
      'liefer_date': datetime.date(2011, 10, 4),
      'menge_offen': Decimal('800.0'),
      'menge_offen2': Decimal('800.0'),
      'menge_zugeteilt': Decimal('800.0'),
      'position': 3,
      'sachbearbeiter': 14,
      'teillieferung_erlaubt': 1,
      'teilzuteilungsverbot': u'0',
      'verkaufspreis': Decimal('3.63'),
      'versandadressnr': 1,
      'voll_ausgeliefert': 0,
      'warenempfaenger': u'64612',
      'zugangslager': 0},
    ...]
    """

    # TODO: das sind zu viele felder, die wir hier ziehen

    conditions = [
        "AKAUFN=APAUFN",                   # Natural Join von Auftragsköpfen und -positionen
        "APSTAT<>'X'",                     # Position nicht logisch gelöscht
        "APKZVA=0",                        # Position nicht als 'voll ausgeliefert' markiert
        "(APMNG-APMNGF) > 0",              # (noch) zu liefernde menge ist positiv
        "AKSTAT<>'X'",                     # Auftrag nicht logisch gelöscht
        "AKKZVA=0"]                        # Auftrag nicht als 'voll ausgeliefert' markiert

    # Wenn resolve_sets gesetzt ist, werden übergeordneten Set-Artikel, zu denen die ArtNr. gehört,
    # bei den Auftragspositionen mit berücksichtigt.
    sets = {}
    if resolve_sets:
        sets = dict((row['artnr'], row['menge_im_set']) for row in set_artikel(artnr))
        artnrs = [artnr] + sets.keys()
        conditions.append("APARTN IN (%s)" % ",".join(sql_quote(artnr) for artnr in artnrs))
    else:
        conditions.append("APARTN=%s" % sql_quote(artnr))

    # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 ergibt eine leere Rückgabe
    if lager:
        conditions.append("APLGNR=%d" % lager)

    # Nur Datensätze berücksichtigen, die nicht jünger sind als das geg. Datum
    if max_date:
        conditions.append("AKDTLT <= %s" % str2softmdate(max_date))

    rows = query(['AAP00', 'AAK00'],
                   condition=' AND '.join(conditions), ordering='APDTLT', cachingtime=60 * 60)

    # Korrigiere die Mengen für Komponenten von Set-Artikeln:
    # Die Menge wird mit der Anzahl der Komponenten in einem Set-Artikel multipliziert.
    for row in rows:
        if row['artnr'] in sets:
            factor = sets[row['artnr']]
            row['menge_offen'] *= factor
            row['bestellmenge'] *= factor
            row['menge_zugeteilt'] *= factor
    return rows


def umlagermenge(artnr, anlager=100):
    """Ermittelt wieviel Umlagerungen für einen Artikel der nach anlager unterwegs sind.

    Rueckgabe:
     - Wenn eine Artikelnummer angegeben wird, dann eine Menge als int
    """

    # Das Auslieferungslager steht in AKLGN1, Das Ziellager steht in AKLGN2
    # In APLGNR steht AUCH das Auslieferungslager
    tables = ['AAP00', 'AAK00']
    conditions = [
        "APARTN=%s" % sql_quote(artnr),
        "AKLGN2=%d" % int(anlager),   # Zugangslager
        "AKAUFN=APAUFN",
        "AKAUFA='U'",                 # Umlagerungsauftrag
        "APSTAT<>'X'",                # Position nicht logisch gelöscht
        "APKZVA=0",                   # Position nicht als 'voll ausgeliefert' markiert
        #"(APMNG-APMNGF-APMNGG) > 0"  # (noch) zu liefernde menge ist positiv
        "AKSTAT<>'X'",                # Auftrag nicht logisch gelöscht
        "AKKZVA=0"]                   # Auftrag nicht als 'voll ausgeliefert' markiert

    rows = query(tables=tables, fields=['SUM(APMNG)'], querymappings={}, condition=' AND '.join(conditions))
    if rows and rows[0] and rows[0][0]:
        return as400_2_int(rows[0][0])
    return 0


def bestand(artnr, lager=0):
    """Ermittelt den Lagerbestand (Buchbestand + kurzum in diesem Lager eintreffene Güter) eines Artikels.

    >>> bestand('76095')
    53
    """
    return buchbestand(artnr, lager) + umlagermenge(artnr, lager)


def bewegungen(artnr, dateformat="%Y-%m-%d", lager=0):
    """Sammeln aller Bewegungen zu einem Artikel - ein Bisschen wie das Artikelkonto.

    Gibt eine Liste von (datum, bewegung) Tuplen zurück, wobei Daten nicht eindeutig sein muessen.

    >>> bewegungen('76095')
    [('2010-w50', 0)]

    dateformat bestimmt dabei die Keys des Dictionaries und steuert die Granularität. "%Y-w%W" sorgt
    für eine wochenweise Auflösung, "%Y-%m-%d" für tageweise.
    """

    # Startwert ist der Buchbestand
    bewegungen = [(datetime.date.today().strftime(dateformat), int(buchbestand(artnr, lager)))]
    # Bestellmengen positiv
    bewegungen.extend([(x[0].strftime(dateformat), int(x[1])) for x in bestellmengen(artnr, lager).items()])
    # Auftragsmengen negativ
    bewegungen.extend([(x[0].strftime(dateformat), -1 * x[1]) for x in auftragsmengen(artnr, lager).items()])
    bewegungen.sort()
    return bewegungen


def artikelverfuegbarkeit(artnr, lager=0, max_date=None, resolve_sets=True):
    """Bestimme alle Mengenänderungen für einen Artikel für die Artikelverfügbarkeit

    Der Rückgabewert ist eine Liste von dicts, die sortiert ist nach Datum und nach Belegnummer.
    Besser waere der NAme "Artikelkonto" oder "Artikelbewegungen".
    """

    satzarten = {'LB': u'Lagerbestand',
                 'ZT': u'Auftrag',
                 'RB': u'Auftrag, zugeteilt',
                 'UZ': u'Zugang',
                 'OB': u'Offene Bestellung'
    }

    # wir Starten mir einem leeren Datendatz
    records = [{'satzart': 'LB', 'datum': datetime.date.min, 'belegnr': '', 'position': 0}]

    ## Auftragspositionen, die den Artikel enthalten.
    # Achtung! Es können auch Set-Artikel im Auftrag enthalten sein,
    # diese müssen ebenfalls berücksichtigt und im richtigen Verhältnis umgerechnet werden.
    rows = auftragsmengen2(artnr, lager, max_date, resolve_sets)

    # Beim Durchlaufen der Auftragspositionen wird die zugeteilte Menge aufsummiert.
    zugeteilte_menge = 0

    for row in rows:
        zugeteilte_menge += row['menge_zugeteilt']

        # Für Umlagerungsaufträge werden zwei Datensätze hinzugefügt:
        # Dies ist der Datensatz für den Zugang. Der Datensatz für den Abgang
        # wird "ganz normal" wie die anderen Datensätze behandelt.
        if row['auftragsart'] == 'U':
            record = dict(belegnr=add_prefix(row['auftragsnr'], 'SO'),
                          position=row['position'],
                          lager=row['zugangslager'],
                          satzart='UZ',
                          menge_offen=row['menge_offen2'],
                          bestellmenge=row['bestellmenge'],
                          datum=row['liefer_date'])
            records.append(record)

        if row['menge_zugeteilt'] > 0:
            satzart = 'ZT'
        else:
            satzart = 'RB'

        record = dict(belegnr=add_prefix(row['auftragsnr'], 'SO'),
                      position=row['position'],
                      lager=row['lager'],
                      satzart=satzart,
                      kundennr=add_prefix(row['warenempfaenger'], 'SC'),
                      menge_offen=row['menge_offen2'],
                      bestellmenge=row['bestellmenge'],
                      datum=row['liefer_date'])
        records.append(record)

    ## Bestellpositionen
    rows = bestellmengen2(artnr, lager)
    for row in rows:
        record = dict(belegnr=add_prefix(row['bestellnr'], 'PO'),
                      position=row['bestellpos'],
                      lager=row['lager'],
                      satzart='OB',
                      kundennr=add_prefix(row['lieferant'], 'SC'),
                      menge_offen=row['bestellmenge'],
                      bestellmenge=row['bestellmenge'],
                      datum=row['liefer_date'])
        records.append(record)

    # Bestand und verfügbare Menge berechnen:
    # Der Startwert des Lagerbestands ist der Buchbestand.
    bestand = int(buchbestand(artnr, lager))
    verfuegbare_menge = bestand - zugeteilte_menge

    # Die Datensätze müssen nach Datum sortiert werden
    # Die Sätze werden nach Datum, Belegnummer und Position sortiert.
    # SoftM berücksichtigt außerdem noch die Satzart, die Reihenfolge ist aber nicht ganz klar
    # und außerdem auch unerheblich.
    records = sorted(records, key=lambda x: (x['datum'], x['belegnr'], x['position']))

    for index, record in enumerate(records):
        record['lfdnr'] = index

        # Berechne die verfügbare Menge und den Lagerbestand aus den Mengen des Datensatzes:

        if record['satzart'] == 'ZT':                    # zugeteilte Menge aus Auftrag
            bestand -= record['menge_offen']
        elif record['satzart'] == 'RB':                  # Bedarf aus Auftrag (noch nicht zugeteilt)
            bestand -= record['menge_offen']
            verfuegbare_menge -= record['menge_offen']
        elif record['satzart'] == 'UZ':                  # Zugang aus Umlagerungsauftrag
            verfuegbare_menge += record['bestellmenge']
        elif record['satzart'] == 'OB':                  # Zugang aus Bestellung
            bestand += record['menge_offen']
            verfuegbare_menge += record['menge_offen']
        elif record['satzart'] == 'LB':                  # (initialer) Lagerbestand
            record['datum'] = ''
        else:
            raise RuntimeError('Unknown record type %r' % record['satzart'])

        record['bestand'] = bestand
        record['verfuegbar'] = verfuegbare_menge
        record['satzart'] = satzarten[record['satzart']]

    return records


def bewegungen_to_bestaende(bewegungen):
    """Bewegungsmengen aufsummieren.

    >>> bewegungen_to_bestaende({'2009-02-20': 1200,
                                 '2009-03-02': 100,
                                 '2009-04-01': -150,
                                 '2009-04-01': -1000})
    {'2009-02-20': 1200,
     '2009-03-02': 1300,
     '2009-04-01': 150}
    """
    menge = 0
    bestentwicklung = {}
    for datum, bewegungsmenge in sorted(bewegungen):
        menge += bewegungsmenge
        bestentwicklung[datum] = menge
    return bestentwicklung


def bestandsentwicklung(artnr, dateformat="%Y-%m-%d", lager=0):
    """Liefert ein Dictionary, dass alle zukünftigen, bzw. noch nicht ausgeführten Bewegungen
    für einen Artikel beinhaltet. Ist kein Bestand für den Artikel vorhanden, wird {} zurückgegeben.

    Setartikel werden hierbei berücksichtigt.

    dateformat bestimmt dabei die Keys des Dictionaries und steuert die Granularität. "%Y-w%W" sorgt
    für eine wochenweise Auflösung, "%Y-%m-%d" für tageweise.

    >>> bestandsentwicklung('14865')
    {'2009-02-20': 1200,
     '2009-03-02': 860,
     '2009-04-01': 560,
     '2009-05-04': 300}
    """

    # Auflösung von Set-Artikeln in ihre Unterartikel. Die Bestandsentwicklung des Sets entspricht der
    # Bestandsentwicklung der Unterartikel dividiert durch die jeweilige Anzahl der Unterartikel pro Set.
    komponenten = husoftm2.artikel.komponentenaufloesung([(1, artnr)])
    bestentw_all = []  # list of dicts
    for komponente_menge, komponente_artnr in komponenten:
        bewegungen_komponente = bewegungen(komponente_artnr, dateformat, lager)
        bestaende_komponente = bewegungen_to_bestaende(bewegungen_komponente)
        # Auf "Anteil" am Endprodukt umrechnen
        bestaende_komponente = dict([(datum, mng / komponente_menge)
                                     for (datum, mng) in bestaende_komponente.items()])
        bestentw_all.append(bestaende_komponente)

    # Die kleinste Menge eines Sub-Artikels ist die an diesem Datum verfügbare Menge
    alldays = [komponentenentwicklung.keys() for komponentenentwicklung in bestentw_all]
    alldays = set(itertools.chain(*alldays))
    entwicklung = {}
    for datum in alldays:
        entwicklung[datum] = min(entwicklung.get(datum, 99999999) for entwicklung in bestentw_all)

    # Bei Setartikeln werden die Auftragsmengen (evtl. auch die Bestellmengen) mal für den Set,
    # und mal für die Subartikel behandelt.
    # Darum hier noch die Bewegungen des Setartikels überlagern
    if len(komponenten) > 1:  # Handelt es sich um einen Setartikel
        # TODO: auch set-komponenten erfassen
        bewegungen_set = bewegungen(artnr, dateformat, lager)

        # Bewegungsmengen aus der gerade ermittelten Bestandsendwicklung der Unterartikel erzeugen
        entwicklung = sorted(entwicklung.items())
        startdate, startmenge = entwicklung[0]
        bewegungen_komponenten = [(startdate, startmenge)]
        menge = startmenge
        for date, mengenaenderung in entwicklung[1:]:
            bewegungen_set.append((date, mengenaenderung - menge))
            menge = mengenaenderung

        # Bewegungen mit denen des Setartikels kombinieren und erneut eine Bestandsentwicklung
        # daraus berechnen
        entwicklung = bewegungen_to_bestaende(bewegungen_komponenten + bewegungen_set)

    return entwicklung


def versionsvorschlag(menge, orgartnr, date, dateformat="%Y-%m-%d"):
    """Gib einen Vorschlag für Zusammenstellung von Artikeln zurück.

    >>> versionsvorschlag(2000, '22006', '2009-01-04')
    (True, [(1184, '22006'), (816, '22006/03')])
    >>> versionsvorschlag(2000, '76095', '2009-01-04')
    (False, [(0, '76095')])
    """
    ret = []
    benoetigt = menge
    for artnr in cs.masterdata.eaplight.get_alternatives(orgartnr):
        dummy, untermenge = ist_frei_am(benoetigt, artnr, date, dateformat)
        if untermenge > 0:
            ret.append((min(benoetigt, untermenge), artnr))
            benoetigt -= untermenge
        if benoetigt <= 0:
            return True, ret
    return False, ret


def freie_menge(artnr, dateformat="%Y-w%W"):
    """Liefert die sofort verkaufbare (freie / available) Menge zurück.

    Dabei wird nicht beachtet, dass eventuell eine Wiederbeschaffung erfolgen kann. Wenn die Bestände
    in ferner Zukunft gegen Null gehen, geht auch die (jetzt) verfügbare Menge gegen null.

    dateformat bestimmt dabei die Keys des Dictionaries und steuert die Granularität. "%Y-w%W" sorgt
    für eine wochenweise Auflösung, "%Y-%m-%d" für tageweise.

    >>> freie_menge('14600')
    2345

    """

    today = datetime.date.today().strftime(dateformat)
    bestandse = bestandsentwicklung(artnr, dateformat)
    # remove historic data, since this tends to be negative
    bestandse = [x[1] for x in bestandse.items() if x[0] >= today]
    if bestandse:
        return max([min(bestandse), 0])
    else:
        return 0


def ist_frei_am(menge, artnr, date, dateformat="%Y-%m-%d"):
    """Ermittelt, ob die Menge für einen Artikel zu dem Datum date frei ist.

    Rückgabewert ist ein Tupel. Dessen erster Eintrag gibt an, ob die Menge vorhanden ist,
    der zweite Eintrag entspricht der gesamt freien Menge zu diesem Datum.
    """
    bentwicklung = bestandsentwicklung(artnr, dateformat).items()
    if bentwicklung:
        bentwicklung.sort()
        # date should not be bigger than last date in bentwicklung
        mindate = min((date, bentwicklung[-1][0]))
        for index in range(len(bentwicklung)):
            if bentwicklung[index][0] > mindate:
                break
        bentwicklung = bentwicklung[max(0, index - 1):]
        verfuegbar = min(quantity for dummy, quantity in bentwicklung)
        return (verfuegbar >= menge), verfuegbar
    return False, 0


def frei_ab(menge, artnr, dateformat="%Y-%m-%d", lager=0):
    """Finds the earliest date when menge is frei (available) or None if it isn't available at all.

    >>> frei_ab(50, '76095')
    None
    >>> frei_ab(500, '01104')
    datetime.date(2008, 11, 22)
    """

    # Algorythmus: vom Ende der Bestandskurve nach hinten gehen, bis wir an einen punkt Kommen, wo die
    # Kurve niedriger ist, als die geforderte Menge - ab da ist die Menge frei.

    today = datetime.date.today().strftime("%Y-%m-%d")
    bentwicklung = bestandsentwicklung(artnr, dateformat, lager)
    # remove historic data, since this tends to be negative
    bentwicklung = dict([x for x in bentwicklung.items() if x[0] >= today])

    # shortcut: the bestand never drops below menge
    if bentwicklung and min(bentwicklung.values()) >= int(menge):
        return datetime.date.today()

    bentwicklung = bentwicklung.items()
    bentwicklung.sort(reverse=True)
    previous_date = None
    for datum, menge_frei in bentwicklung:
        if menge_frei < int(menge):
            if previous_date:
                return datetime.date.fromtimestamp(time.mktime(time.strptime(previous_date, dateformat)))
            else:
                return False
        previous_date = datum
    return None


def get_lagerbestandsaenderung(artnr, datum_start, datum_ende):
    """Gibt die Veränderung des Lagerbestandes eines Artikels in einem bestimmten Zeitraum zurück.

    Beispiel:
    >>> get_lagerbestandsaenderung('11007', datetime.date(2011, 7, 31), datetime.date(2011, i, 1))
    <<< Decimal('-380.000')

    artnr
        Artikelnummer, dessen Änderung abgefragt werden soll
    datum_start
        Datum (exclusive!) ab dem die Änderung abgefragt werden soll.

        Exklusive, damit gilt:
        >>> assert datum1 < datum2 < datum3
        >>> assert get_lagerbestandsaenderung(artnr, datum1, datum2) \
                 + get_lagerbestandsaenderung(artnr, datum2, datum3) == \
                            get_lagerbestandsaenderung(artnr, datum1, datum3)
        Dieses Verhalten sollte die Addition mit gecachten Ergebnissen vereinfachen.
    datum_end
        Datum (inclusive) bis zu dem die Änderung abgefragt werden soll.
    returns
        Einen positiven Wert, wenn der Bestand gewachsen ist, einen negativen, wenn er gesunken ist.
    """
    # Die Tabelle XLB00 gibt, laut Doku, einen Lückenlosen Nachweis der Bestandsentwicklung eines Lagers wieder
    # https://docs.google.com/a/hudora.de/viewer?a=v&pid=sites&srcid=aHVkb3JhLmRlfGludGVybnxneDo0Yjk4Mjc3ZjI2MmVjZGVm
    #
    # Tabelle XLB00 (Lagerbewegungen)
    #   LBARTN: Artikelnummer
    #   LBDTBL: Datum
    #   LBMNGB: Menge in Bestandsführungseinheit
    #   LBBTYP: Bewegungstyp
    #     '001' Zugang
    #     '005' Zugang aus Inventur
    #     '006' Zugang wegen Abgangsstorno
    #     '011' Nachbewertung
    #     '012' Wertkorrektur
    #     '013' Wertrücknahme
    #     '021' Abgang
    #     '022' Abgang aus Inventur
    #     '023' Abgang wegen Zugangsstorno
    #     '050' Inventurbestätigung
    #     '060' Nur Sperren Lagerbewegung
    #     '061' Nur Entsperren Lagerbewegung
    #     '070' InfoBwg ohne Mengen- und Wertänderung

    # Es werden summiert alle Änderungen im Zeitraum abgefragt...
    conditions = [
        "LBARTN = %s" % sql_quote(artnr),
        "LBDTBL > %s" % date2softm(datum_start),
        "LBDTBL <= %s" % date2softm(datum_ende),
    ]

    rows = query(['XLB00'], fields=['LBDTBL', 'LBMNGB', 'LBBTYP', 'LBLGNR'],
                 condition=' AND '.join(conditions), ordering='LBDTBL')

    zugangstypen = [1, 5, 6]     # Zugang, Zugang aus Inventur, Zugang wegen Abgangsstorno
    abgangstypen = [21, 22, 23]  # Abgang, Abgang aus Inventur, Abgang wegen Zugangsstorno
    absoluttypen = [50]  # Inveturbestätigung

    bestaende = {}
    for row in rows:
        bestaende.setdefault(row['lager'], 0)

        if row['typ'] in zugangstypen:
            bestaende[row['lager']] += row['menge']
        elif row['typ'] in abgangstypen:
            bestaende[row['lager']] -= row['menge']
        elif row['typ'] in absoluttypen:
            bestaende[row['lager']] = row['menge']

    return sum(bestaende.values())


def _test_get_lagerbestandsaenderung():
    # Lange Zeiträume, um Performance zu Testen
    print get_lagerbestandsaenderung('11992/02', datetime.date(1900, 1, 1), datetime.date(2012, 1, 1))
    print get_lagerbestandsaenderung('76105', datetime.date(1900, 1, 1), datetime.date(2012, 1, 1))
    print get_lagerbestandsaenderung('14695', datetime.date(1900, 1, 1), datetime.date(2012, 1, 1))

    # Und einen Konkreten Historischen Stand
    print get_lagerbestandsaenderung('76105', datetime.date(1900, 1, 1), datetime.date(2008, 5, 23))


def _test():
    """Some very simple tests."""
    from pprint import pprint
    print "buchbestand('14600/03') = ",
    print buchbestand('14600/03')
    print "bestellmengen('14600/03') = ",
    pprint(bestellmengen('14600/03'))
    print "auftragsmengen('14600/03') = ",
    pprint(auftragsmengen('14600/03'))
    print "bestand('14600/03') = ",
    pprint(bestand('14600/03'))
    print "bestand('14600/03', 100) = ",
    pprint(bestand('14600/03', 100))
    for artnr in '76095 14600/03 14865 71554/A 01104 10106 14890 WK22002 00000'.split():
        print "bewegungen(%r) = " % artnr,
        pprint(bewegungen(artnr))
        print "freie_menge(%r) = " % artnr,
        pprint(freie_menge(artnr))
        print "bestandsentwicklung(%r) = " % artnr,
        pprint(bestandsentwicklung(artnr))
        print "frei_ab(50, %r) = " % artnr,
        pprint((frei_ab(50, artnr)))
        print "umlagermenge(%r, 100) = " % artnr,
        pprint((umlagermenge(artnr, 100)))
        print "bestand(%r, 100) = " % artnr,
        pprint((bestand(artnr, 100)))
    return frei_ab(1000, '14600/03')


if __name__ == '__main__':
#    _test_get_lagerbestandsaenderung()
    _test()
    unittest.main()
