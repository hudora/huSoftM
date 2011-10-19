#!/usr/bin/env python
# encoding: utf-8
"""
preise.py - Zugriff auf Artikelpreise.

Created by Maximillian Dornseif on 2007-04-28.
Copyright (c) 2007, 2009, 2010 HUDORA GmbH. All rights reserved.
"""
import datetime

from husoftm2.backend import query
from husoftm2.tools import sql_quote, date2softm, remove_prefix, pad


def abgabepreise_kunde(artnrs, kundennr, auftragsdatum=None):
    """
    Verkaufspreis für einen oder mehrere Artikel in Abhängigkeit von kundennr und Auftragsdatum ermitteln.

    Der Rückgabewert ist ein dict mit den ArtNr. als  Schlüssel.
    Die Werte sind Preisinformationen als Tupel (Preis, Herkunft)
    oder None, falls zu der ArtNr. kein Preis ermittelt werden konnte.

    Die Logik funktioniert genau wie bei abgabepreis_kunde:
    Es werden zuerst kundenspezifische Preise, dann kundengruppen-spezifische Preise und als
    letztes Listenpreise ermittelt.
    """
    if not auftragsdatum:
        auftragsdatum = datetime.date.today()

    artnrs = set(artnrs)
    kundennr = remove_prefix(kundennr, 'SC')
    date_str = sql_quote(date2softm(auftragsdatum))

    abgabepreise = {}

    # 1. Preise für Kunden hinterlegt?
    conditions = ["PNSANR=PRSANR",
                  "PRANW='A'",
                  "PRSTAT=' '",
                  "PNSTAT=' '",
                  "PRDTBI>=%s" % date_str,
                  "PRDTVO<=%s" % date_str,
                  ]
    condition_kunde = conditions + ["PRKDNR=%s" % sql_quote("%8s" % kundennr),
                                    "PRARTN IN (%s)" % ",".join([sql_quote(artnr) for artnr in artnrs])]
    rows = query(tables=['XPN00', 'XPR00'],
                 fields=['PRARTN', 'PNPRB'],
                 condition=' AND '.join(condition_kunde),
                 ordering='PRDTVO')
    for row in rows:
        if row['artnr'] in artnrs:
            artnrs.remove(row['artnr'])
        abgabepreise[row['artnr']] = (int(row['preis'] * 100), u'Kundenpreis')

    if not artnrs:
        return abgabepreise

    # 2. Preise aus Preislistennr. des Kunden ermitteln
    condition_gruppe = conditions + [
                            # "PRPRLK = %s" % sql_quote(kunde['kunden_gruppe']),
                            "PRARTN IN (%s)" % ",".join([sql_quote(artnr) for artnr in artnrs]),
                            "KDKDNR=%s" % pad('KDKDNR', kundennr),
                            "PRPRLK=KDKGRP"
                       ]
    rows = query(tables=['XPN00', 'XPR00', 'XKD00'],
                 fields=['PRARTN', 'PNPRB', 'PRPRLK'],
                 condition=' AND '.join(condition_gruppe),
                 ordering='PRDTVO')

    for row in rows:
        if row['artnr'] in artnrs:
            artnrs.remove(row['artnr'])
        abgabepreise[row['artnr']] = (int(row['preis'] * 100), u'Preisliste %s' % row['preisliste_kunde'])

    if not artnrs:
        return abgabepreise

    # 3. Listenpreis aus Artikelstammdaten
    for artnr, preis in listenpreise(artnrs).iteritems():
        if artnr in artnrs:
            artnrs.remove(artnr)
        abgabepreise[artnr] = (preis, u'Listenpreis')

    for artnr in artnrs:
        abgabepreise[artnr] = None

    return abgabepreise


def abgabepreis_kunde(artnr, kundennr, auftragsdatum=None):
    """
    Verkaufspreis für einen Artikel in Abhängigkeit von kundennr und Auftragsdatum ermitteln.

    Höchste Priorität hat der für einen Kunden hinterlegt Preis.
    Zweithöchste Priorität hat der für die Preisliste (Kundengruppe) hinterlegte Preis
    Niedrigste Priorität hat der Listenpreis aus den Artikelstammdaten.

    Rückgabe ist tuple mit Preis und Herkunft des Preises.

    >>> abgabepreis_kunde('04711', 99954)
    (1500, 'Preisliste 95')

    >>> abgabepreis_kunde('04711', 98000)
    (1400, 'Listenpreis')

    >>> abgabepreis_kunde('04711', 94763)
    (1300, 'Kundenpreis')

    """

    if not auftragsdatum:
        auftragsdatum = datetime.date.today()

    # Kundennr als Zeichenkette
    kundennr = remove_prefix(kundennr, 'SC')
    date_str = sql_quote(date2softm(auftragsdatum))

    # 1. Preis für Kunde hinterlegt?
    conditions = ["PNSANR=PRSANR",
                  "PRANW='A'",
                  "PRSTAT=' '",
                  "PNSTAT=' '",
                  "PRARTN=%s" % sql_quote(artnr),
                  "PRDTBI>=%s" % date_str,
                  "PRDTVO<=%s" % date_str,
                  ]
    condition_kunde = conditions + ["PRKDNR=%s" % pad('PRKDNR', kundennr)]
    rows = query(['XPN00', 'XPR00'], fields=['PNPRB'], condition=' AND '.join(condition_kunde),
                 ordering='PRDTVO', limit=1)
    if rows:
        return (int(rows[0][0] * 100), 'Kundenpreis')

    # 2. Preis aus Preislistennr. des Kunden ermitteln
    condition_gruppe = conditions + [
                            # "PRPRLK = %s" % sql_quote(kunde['kunden_gruppe']),
                            "KDKDNR=%s" % pad('KDKDNR', kundennr),
                            "PRPRLK=KDKGRP"
                       ]
    rows = query(['XPN00', 'XPR00', 'XKD00'], fields=['PNPRB', 'PRPRLK'],
                 ordering='PRDTVO', condition=' AND '.join(condition_gruppe), limit=1)
    if rows:
        return (int(rows[0]['preis'] * 100), 'Preisliste %s' % rows[0]['preisliste_kunde'])

    # 3. Listenpreis aus Artikelstammdaten
    return (listenpreis(artnr), 'Listenpreis')


def buchdurchschnittspreise(artnrs=None):
    """Gibt die (aktuellen) Buchdurchschnittspreise in Cent für ein Dict von Artikeln zurück.
    Wenn keine Artikelnummern angegeben werden, gibt es alle Buchdurchschinttspreise zurück.

    >>> buchdurchschnittspreise(['14600', '14600/00', '14600/01', '14600/02', '14600/03'])
    {u'14600': 3165,
     u'14600/00': 0,
     u'14600/01': 0,
     u'14600/02': 3132,
     u'14600/03': 3944}
    """

    conditions = ["LFLGNR=0",
                  "LFSTAT<>'X'"]
    if artnrs:
        conditions += ["LFARTN IN (%s)" % ','.join([sql_quote(x) for x in artnrs])]
    rows = query('XLF00', fields=['LFARTN', 'LFPRBD'], condition=' AND '.join(conditions))
    return dict([(artnr, int(float(preis * 100))) for (artnr, preis) in rows])


def listenpreise(artnrs=None):
    """Gibt den (aktuellen) Listenpreis in Cent für eine Liste von Artikeln zurück.
    Wenn keine Artikelnummern angegeben werden, gibt es alle Listenpreise zurück.

    >>> preise(['04711'])
    {'04711': 1365}
    """

    conditions = ["ARSTAT<>'X'"]
    if artnrs:
        conditions += ['ARARTN IN (%s)' % ','.join([sql_quote(x) for x in artnrs])]
    rows = query('XAR00', fields=['ARARTN', 'ARPREV'], condition=' AND '.join(conditions))
    return dict([(x['artnr'], int(100 * float(x['listenpreis']))) for x in rows])


def listenpreis(artnr):
    """Listenpreis für einene einzelenen Artikel."""
    preise = listenpreise([artnr]).values()
    if preise:
        return preise[0]


def durchschnittlicher_abgabepreis(artnr, kundennr=None, startdatum=None):
    """Gibt eine Liste mit den durchschnittlichen Rechnungspreisen pro Monat zurück.

    Liefert eine Liste von 4-Tuples (datum, AVG(preis), menge, umsatz)

    Wenn eine Kundennummer mitgeliefert wird, werden nur Rechungen für diesen Kunden betrachtet.
    Wenn ein startdatum angegebenw wird, werden nur vorgänge nach diesem Datum betrachtet.

    [ ...
    (datetime.date(2009, 2, 1), 3295, 2, 6590),
    (datetime.date(2009, 10, 1), 1744, 2, 3488)]

    Die Funktion ist ausgesprochen langsam - bis zu 8 Sekunden.
    """

    conditions = [
        "FUARTN=%s" % (sql_quote(artnr)),  # nur bestimmten Artikel beachten
        "FKRGNR=FURGNR",   # JOIN
        "FKAUFA<>'U'",     # Keine Umlagerung
        "FKSTAT<>'X'",     # nicht gelöscht
        "FKDTFA>0",        # Druckdatum nicht leer
        "FUKZRV=2",        # ?
        "FURGNI<>0",       # ?
        "FKFORM=' '",      # ?
        "FURGNR<>0",       # es gibt eine Rechnungsnummer
        "FUPNET>0",        # keine Gutschriften
        ]

    if kundennr:
        kundennr = remove_prefix(kundennr, 'SC')
        conditions = ["(FKKDNR=%s OR FKKDRG=%s)" % (sql_quote('%8s' % kundennr),
                                                    sql_quote('%8s' % kundennr))] + conditions
    if not startdatum:
        conditions = ["FKDTFA>'10501'"] + conditions  # keine legacy Daten
    else:
        conditions = ["FKDTFA>%s" % sql_quote(date2softm(startdatum))[:5]] + conditions

    rows = query(['AFU00', 'AFK00'], fields=["FKDTFA", 'SUM(FUMNG)', 'SUM(FUPNET)', 'COUNT(FKRGNR)'],
                 condition=' AND '.join(conditions),
                 grouping='FKDTFA', cachingtime=60 * 60 * 24 * 3,
                 querymappings={'SUM(FUMNG)': 'menge', 'SUM(FUPNET)': 'nettopreis',
                                'COUNT(FKRGNR)': 'rechnungen', 'FKDTFA': 'rechnung_date'})
    mengen = {}
    umsatz = {}
    for row in rows:
        menge = int(float(row['menge']))
        nettopreis = float(row['nettopreis']) * 100
        if menge:
            datum = datetime.date(row['rechnung_date'].year, row['rechnung_date'].month, 1)
            if datum not in mengen:
                mengen[datum] = umsatz[datum] = 0
            mengen[datum] += int(menge)
            umsatz[datum] += int(nettopreis)
    ret = []
    for datum in sorted(mengen.keys()):
        ret.append((datum, int(umsatz[datum] / mengen[datum]), mengen[datum], umsatz[datum]))
    return ret


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    pprint(listenpreise(['14600', '14600/00', '14600/01', '14600/02', '14600/03']))
    pprint(buchdurchschnittspreise(['14600', '14600/00', '14600/01', '14600/02', '14600/03']))
    #pprint(durchschnittlicher_abgabepreis('14600'))
    pprint(durchschnittlicher_abgabepreis('74501/01', 'SC77900'))
    pprint(abgabepreis_kunde('74501/01', 'SC77900'))
    print len(buchdurchschnittspreise())

if __name__ == '__main__':
    from timeit import Timer
    t = Timer("_selftest()", "from __main__ import _selftest")
    print t.timeit(1)
