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
    auftragsmengen(artnr, lager=0)                bei uns von Kunden bestellte Mengen
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


Es gibt verschiedene Mengen von denen wir reden.
 * Bezogen aufs Lager
   * verfügbare menge - ist die Menge die wir am Lager haben und die zur Zeit noch keinem Kundenauftrag
     zugeordnet ist, d.h. die noch nicht zugeteilt ist.
   * freie menge - ist die Menge die wir noch verkaufen können (entspricht der verfügbaren Menge abzüglich
     der noch nicht zugeteilten Kundenaufträge)
 * bezogen auf Aufträge
   * bestellmenge - wieviel will der Kunde haben will
   * gelieferte - menge wieviel schon geliefert wurde
   * offene menge - wieviel noch zu liefern ist

"""


from husoftm.tools import sql_quote
from husoftm2.backend import query, as400_2_int
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


def buchbestand(artnr, lager=0):
    """Gibt den Buchbestand eines Artikels für ein Lager zurück oder (lager=0) für alle Lager

    >>> buchbestand('14600/03')
    338
    """
    ret = buchbestaende([artnr], lager)
    if ret:
        return buchbestaende([artnr], lager).values()[0]
    return 0


def bestellmengen(artnr, lager=0):
    """Liefert eine Liste mit allen Bestellten aber noch nicht gelieferten Wareneingängen.

    >>> bestellmengen('14865')
    {datetime.date(2009, 2, 20): 1200,
     datetime.date(2009, 5, 5): 300}
    """
    conditions = ["BPSTAT<>'X'",
                  "BPKZAK=0",
                  "BPARTN=%s" % sql_quote(artnr)]
    if lager:
        conditions += ["BPLGNR=%s" % sql_quote(lager)]

    # detailierte Informationen gibts in EWZ00
    rows = query('EBP00', fields=['BPDTLT', 'SUM(BPMNGB-BPMNGL)'], ordering='BPDTLT',
                 grouping='BPDTLT', condition=' AND '.join(conditions))
    return dict([(x['liefer_date'], as400_2_int(x['SUM(BPMNGB-BPMNGL)']))
                 for x in rows if as400_2_int(x['SUM(BPMNGB-BPMNGL)']) > 0])


def auftragsmengen(artnr, lager=0):
    """Liefert eine Liste offener Aufträge (Warenausgänge) für einen Artikel OHNE UMLAGERUNGEN.

    >>> auftragsmengen(14865)
    {datetime.date(2009, 3, 2): 340,
     datetime.date(2009, 4, 1): 300,
     datetime.date(2009, 5, 4): 260,
     datetime.date(2009, 6, 2): 300}
    """
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
    rows = query(['AAP00', 'AAK00'], fields=['APDTLT', 'SUM(APMNG-APMNGF)'],
                   condition=' AND '.join(conditions),
                   ordering='APDTLT', grouping='APDTLT',
                   querymappings={'SUM(APMNG-APMNGF)': 'menge_offen', 'APDTLT': 'liefer_date'})
    return dict([(x['liefer_date'], as400_2_int(x['menge_offen'])) for x in rows if x['menge_offen'] > 0])


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

    Gibt eine Liste von (datum, bewegung) Tuplen zurück, wobei daten nciht eindeutig sein muessen.

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


def _bewegungen_to_bestaende(bewegungen):
    """Bewegungsmengen aufsummieren.

    >>> _bewegungen_to_bestaende({'2009-02-20': 1200,
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
        bestaende_komponente = _bewegungen_to_bestaende(bewegungen_komponente)
        # Auf "Anteil" am Endprodukt umrechnen
        bestaende_komponente = dict(((datum, mng / komponente_menge) for (datum, mng) in bestaende_komponente.items()))
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
        entwicklung = _bewegungen_to_bestaende(bewegungen_komponenten + bewegungen_set)

    return entwicklung


def freie_menge(artnr, dateformat="%Y-w%W"):
    """Liefert die sofort verkaufbare (freie / available) Menge zurück.

    Dabei wird nicht beachtet, dass eventuell eine Wiederbeschaffung erfolgen kann. Wenn die Bestände
    in ferner Zukunft gegen Null gehen, geht auch die (jetzt) verfügbare Menge gegen null.

    dateformat bestimmt dabei die Keys des Dictionaries und steuert die Granularität. "%Y-w%W" sorgt
    für eine wochenweise Auflösung, "%Y-%m-%d" für tageweise.

    >>> freie_menge('14600')
    2345

    """

    today = datetime.date.today().strftime("%Y-w%W")
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
    _test()
    unittest.main()
