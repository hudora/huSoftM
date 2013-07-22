#!/usr/bin/env python
# encoding: utf-8
"""
preise.py - Zugriff auf Artikelpreise.

Created by Maximillian Dornseif on 2007-04-28.
Copyright (c) 2007, 2009, 2010 HUDORA GmbH. All rights reserved.
"""
import datetime
import warnings

import husoftm2.texte
from husoftm2.backend import query
from husoftm2.tools import sql_quote, date2softm, add_prefix, remove_prefix, pad


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
    warnings.warn("use `cs.salesforce.preise` instead!", DeprecationWarning, stacklevel=2)
    if not auftragsdatum:
        auftragsdatum = datetime.date.today()

    artnrs = set(artnrs)
    kundennr = remove_prefix(kundennr, 'SC')
    date_str = sql_quote(date2softm(auftragsdatum))

    abgabepreise = {}

    # 1. Preise für Kunden hinterlegt?
    conditions = ["PNSANR=PRSANR",
                  "PRANW='A'",
                  "PRPRLK<>''"
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
                            "KZKDNR=%s" % pad('KZKDNR', kundennr),
                            "PRPRLK=KZPREL"
                       ]
    rows = query(tables=['XPN00', 'XPR00', 'AKZ00'],
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


def listenpreis(artnr):
    """Listenpreis für einene einzelenen Artikel."""
    preise = listenpreise([artnr]).values()
    if preise:
        return preise[0]


def listenpreise(artnrs=None):
    """Gibt den (aktuellen) Listenpreis in Cent für eine Liste von Artikeln zurück.
    Wenn keine Artikelnummern angegeben werden, gibt es alle Listenpreise zurück.

    >>> preise(['04711'])
    {'04711': 1365}
    """

    warnings.warn("use `cs.salesforce.preise` instead!", DeprecationWarning, stacklevel=2)
    conditions = ["ARSTAT<>'X'"]
    if artnrs:
        conditions += ['ARARTN IN (%s)' % ','.join([sql_quote(x) for x in artnrs])]
    rows = query('XAR00', fields=['ARARTN', 'ARPREV'], condition=' AND '.join(conditions))
    return dict([(x['artnr'], int(100 * float(x['listenpreis']))) for x in rows])


def listenpreis(artnr):
    """Listenpreis für einene einzelnen Artikel."""
    preise = listenpreise([artnr]).values()
    if preise:
        return preise[0]


def preislisten(gueltig_bis=None):
    """Gib die 'Namen' aller Preislisten für Kundengruppen und Kunden zurück"""

    conditions = ["PNSANR=PRSANR",
                  "PRANW='A'",
                  "PRSTAT=' '",
                  "PNSTAT=' '",
                  ]

    if gueltig_bis:
        conditions.append("PRDTBI>=%s" % date2softm(gueltig_bis))

    preislisten = husoftm2.texte.get_map('PRL')

    rows = query(tables=['XPR00', 'XPN00'],
                 fields=['PRKDNR', 'PRPRLK', 'PRARTN', 'PNPRB', 'PRDTVO', 'PRDTBI'],
                 condition=' AND '.join(conditions))
    for row in rows:
        if row['kunde']:
            row['kunde'] = add_prefix(row['kunde'], 'SC')
        if row['preisliste_kunde']:
            row['preislistenname'] = preislisten.get(row['preisliste_kunde'],
                                                     "?=%s" % row['preisliste_kunde'])

    return rows


def kundenpreise(kundennr, gueltig_von=None, gueltig_bis=None):
    """Alle kundenspezifischen Preis für einen Kunden"""

    kundennr = remove_prefix(kundennr, 'SC')

    conditions = ["PNSANR=PRSANR",
                  "PRANW='A'",
                  "PRSTAT=' '",
                  "PNSTAT=' '",
                  "PRKDNR=%s" % pad('PRKDNR', kundennr)
                  ]

    if gueltig_von:
        conditions.append("PRDTVO>=%s" % sql_quote(date2softm(gueltig_von)))
    if gueltig_bis:
        conditions.append("PRDTBI>=%s" % sql_quote(date2softm(gueltig_bis)))

    rows = query(tables=['XPN00', 'XPR00'],
                 fields=['PRARTN', 'PNPRB', 'PRDTVO', 'PRDTBI'],
                 condition=' AND '.join(conditions),
                 ordering='PRDTVO'
                 )

    preise = {}
    for row in rows:
        print row
        preise[row['artnr']] = dict(preis=int(row['preis'] * 100),
                                    gueltig_von=row.get('gueltig_ab_date'),
                                    gueltig_bis=row.get('gueltig_bis_date'))
    return preise


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
