#!/usr/bin/env python
# encoding: utf-8
"""
bestaende.py - high level functions to access SoftM on the AS/400.

Created by Maximillian Dornseif on 2006-10-19.

Hier werden Warenbestände, verfügbare Mengen und dergleichen ermittelt.

Für die Frage, ob wir einen Artikel verkaufen können ist freie_menge() die richtige Funktion.
Für die Frage, ob ein bestimmter Artikel in einem bestimmten Lager ist, ist bestand() geignet.

    alternativen(artnr)                           alternativartikel / versionen
    bestellmengen(artnr)                          von uns bei Lieferanten bestellte Mengen
    auftragsmengen(artnr, lager=0)                bei uns von Kunden bestellte Mengen
    umlagermenge(artnr, lager)                    Menge, die zur Zeit von einem Lager ans andere
                                                  unterwegs ist
    umlagermengen(anlager)                        Alle Artikelmengen, die zur Zeit von einem Lager
                                                  ans andere unterwegs ist
    buchbestand(artnr, lager=0)                   Artikel am Lager
    buchbestaende(lager=0)                        Alle Artikel an einem Lager 
    freie_menge(artnr)                            Menge, die Verkauft werden kann
    frei_ab(menge, artnr, dateformat="%Y-%m-%d")  ab wann ist eine bestimmte Menge frühstens verfügbar?
    bestand(artnr, lager)                         Wieviel ist zur Zeit an einem Lager oder trifft
                                                  kurzum ein?
    besteande(lager)                              wie bestand() aber für alle Artikel
    bestandsentwicklung(artnr, dateformat="%Y-%m-%d")            Prognose der Bestandsänderungen
    versionsvorschlag(menge, artnr, date, dateformat="%Y-%m-%d") Vorschlag zur Versionsstückelung
    

Siehe auch https://cybernetics.hudora.biz/intern/trac/wiki/HudoraGlossar zu den Bezeichnungen.

"""

__revision__ = "$Revision$"

from husoftm.connection2 import get_connection, as400_2_int
from husoftm.tools import sql_escape, sql_quote
import husoftm.artikel
import couchdb.client
import cs.caching as caching
import cs.masterdata.article
import datetime
import huTools.async
import time
import unittest
import warnings
import itertools


COUCHSERVER = "http://couchdb.local.hudora.biz:5984"


def _umlagermenge_helper(artnr, lager=100):
    """ Ermittelt wieviel Umlagerungen für einen oder alle Artikel unterwegs sind.

    Parmeter:
     - artnr - 0 oder None -> alle Artikel, die sich in der Umlagerung befinden auflisten,
               sonst die gesuchte Artikelnummer
     - lager - Das Lager, an das die Umlagerungen unterwegs sind (default 100)

    Rueckgabe:
     - Wenn eine Artikelnummer angegeben wird, dann eine Menge als int
     - Wenn keine Artikelnummer angegeben wird, dann alle Artikeln die sich
       zu dem gegebenen Zugangslager (lager) unterwegs sind.
    """
    
    # This is just a private helper funcion, which should only be called by umlagermenge and
    # umlagermengen. The reason therefore is, that this function has different return types, depending
    # on the artnr you provide. umlagermenge and umlagermengen are wrappers around this function.
    
    # Das Auslieferungslager steht in AKLGN1, Das Ziellager steht in AKLGN2
    # In APLGNR steht AUCH das Abgangslager

    tables = ['AAP00', 'AAK00']
    condition = (
    "AKAUFN=APAUFN"
    " AND AKAUFA='U'"                 # Umlagerungsauftrag
    " AND APSTAT<>'X'"                # Position nicht logisch gelöscht
    " AND APKZVA=0"                   # Position nicht als 'voll ausgeliefert' markiert
    #" AND (APMNG-APMNGF-APMNGG) > 0"  # (noch) zu liefernde menge ist positiv
    " AND AKSTAT<>'X'"                # Auftrag nicht logisch gelöscht
    " AND AKKZVA=0")                 # Auftrag nicht als 'voll ausgeliefert' markiert
    fields = ['SUM(APMNG)']
    grouping = []

    if lager:
        # Zugangslager
        condition += " AND AKLGN2=%d" % int(lager)

    if artnr:
        # Artikelnummer
        condition += " AND APARTN=%s" % sql_quote(artnr)
    else:
        fields.insert(0, 'APARTN')
        grouping = ['APARTN']

    rows = get_connection().query(tables=tables, fields=fields, querymappings={}, condition=condition,
                                  grouping=grouping)

    if not artnr:
        # Wenn kein bestimmter Artikel abgefragt wird, dann das Abfrageergebnis in ein dict umwandeln
        ret = {}
        for artnr, menge in rows:
            ret[artnr] = as400_2_int(menge)
        return ret
    else:
        if rows and rows[0] and rows[0][0]:
            return as400_2_int(rows[0][0])
    return 0
    

# TODO: use cs module instead of alternativen()

def alternativen(artnr):
    """Gets a list of article numbers which are alternatives (usually versions).
    
    >>> alternativen('14600')
    ['14600', '14600/01', '14600/02', '14600/03']
    """
    
    warnings.warn("hudoftm.bestaende.alternativen() is deprecated,"
                  + " use cs.masterdata.article.alternatives() instead",
                  DeprecationWarning, stacklevel=2) 
    server = couchdb.client.Server(COUCHSERVER)
    db = server['eap']
    artnrs = db.get(artnr, {}).get('alternatives', [])
    if artnr not in artnrs:
        artnrs.append(artnr)
    return sorted(artnrs)
    

def get_lagerbestand(artnr=None, lager=0):
    """Deprecheated, don't use anymore."""
    warnings.warn("get_lagerbestand() is deprecated use buchbestand()", DeprecationWarning, stacklevel=2) 
    return buchbestand(artnr, lager)
    

def buchbestand(artnr, lager=0):
    """Gibt den Buchbestand eines Artikels für ein Lager zurück oder (lager=0) für alle Lager
    
    >>> buchbestand('14600')
    2345
    
    """
    
    rows = get_connection().query('XLF00', fields=['LFMGLP'],
               condition="LFLGNR=%d AND LFARTN=%s AND LFMGLP<>0 AND LFSTAT<>'X'" % (int(lager), 
                                                                                    sql_quote(artnr)))
    if rows:
        return as400_2_int(rows[0][0])
    return 0
    

def buchbestaende(lager=0):
    """Gibt den Buchbestand aller Artikel für ein Lager zurück oder (lager=0) für alle Lager
    
    >>> buchbestaende()
    {'01012': 1.0,
     '01013': 246.0,
     '01020': 395.0,
     '01022': 2554.0,
     '01023': 7672.0,
     '01104': 109.0,
     '01104/01': 758.0,
     '01105': 203.0,
     '01105/01': 799.0,
     '01106/01': 1012.0}
    
    """
    
    rows = get_connection().query('XLF00', fields=['LFARTN', 'SUM(LFMGLP)'], grouping=['LFARTN'],
               condition="LFLGNR=%d AND LFMGLP<>0 AND LFSTAT<>'X'" % (int(lager)))
    return dict([(str(artnr), as400_2_int(quantity)) for artnr, quantity in rows])
    

def get_verfuegbaremenge(artnr=None, lager=0):
    """Deprecheated, don't use anymore."""
    warnings.warn("get_verfuegbaremenge() is deprecated use verfuegbare_menge()",
                  DeprecationWarning, stacklevel=2) 
    return verfuegbare_menge(artnr, lager)
    

def verfuegbare_menge(artnr, lager=0):
    """Gibt die aktuell verfügbare Menge eines Artikels an einem Lager zurück oder (lager=0) für alle Lager
    
    Achtung! Die verfügbare Menge ist nicht die "freie Menge".
    
    >>> verfuegbare_menge('12345')
    3456
    """
    
    rows = get_connection().query('XLF00', fields=['LFMGLP', 'LFMGK4'],
               condition="LFLGNR=%s AND LFARTN=%s AND LFMGLP<>0 AND LFSTAT<>'X'" % (
                    sql_escape(lager), sql_quote(artnr)))
    if rows:
        (menge, lfmgk4) = rows[0]
        return as400_2_int(menge) - as400_2_int(lfmgk4)
    else:
        return 0
    

def verfuegbare_mengen(lager=0):
    """Gibt die aktuell verfügbaren Mengen aller Artikel eines Lagers zurück. Siehe auch besteande().
    
    >>> verfuegbare_mengen(34)
    {'10106': 6,
     '12551': 2854,
     ...
     '83165': 598}
    """
    
    rows = get_connection().query('XLF00', grouping=['LFARTN'], querymappings={},
                                  fields=['LFARTN', 'SUM(LFMGLP)', 'SUM(LFMGK4)', 'SUM(LFMGLP-LFMGK4)'],
                                  condition="LFLGNR=%s AND LFMGLP<>0 AND LFSTAT<>'X'" % sql_escape(lager))
    return dict([(str(artnr), as400_2_int(menge) - as400_2_int(lfmgk4))
                 for (artnr, menge, lfmgk4, dummy) in rows])
    

def freie_menge(artnr, dateformat="%Y-%m-%d"):
    """Liefert die sofort verkaufbare (freie / available) Menge zurück.
    
    Dabei wird nicht beachtet, dass eventuell eine Wiederbeschaffung erfolgen kann. Wenn die Bestände 
    in ferner Zukunft gegen Null gehen, geht auch die (jetzt) verfügbare Menge gegen null.
    
    >>> freie_menge('14600')
    2345
    
    """
    
    today = datetime.date.today().strftime("%Y-%m-%d")
    bestandse = bestandsentwicklung(artnr, dateformat)
    # remove historic data, since this tends to be negative
    bestandse = [x[1] for x in bestandse.items() if x[0] >= today]
    if bestandse:
        return max([min(bestandse), 0])
    else:
        return 0


def _bewegungen(artnr, dateformat="%Y-%m-%d", lager=0):
    """Sammeln aller Bewegungen zu einem Artikel."""
    
    # start processing all three queries in separate threads
    bestellmengen_future = huTools.async.Future(bestellmengen, artnr, lager)
    auftragsmengen_future = huTools.async.Future(auftragsmengen, artnr, lager)
    buchbestand_future = huTools.async.Future(buchbestand, artnr, lager)
    
    # This could be sped up by using futures.
    # Startwert ist der Buchbestand
    bewegungen = [(datetime.date.today().strftime(dateformat), int(buchbestand_future()))]
    # Bestellmengen positiv
    bewegungen.extend([(x[0].strftime(dateformat), int(x[1])) for x in bestellmengen_future().items()])
    # Auftragsmengen negativ
    bewegungen.extend([(x[0].strftime(dateformat), -1*x[1]) for x in auftragsmengen_future().items()])
    bewegungen.sort()
    return bewegungen


def _sum_bewegungen(bewegungen):
    """Bewegungsmengen aufsummieren."""

    menge = 0
    bestentwicklung = {}
    for datum, bewegungsmenge in bewegungen:
        menge += bewegungsmenge
        bestentwicklung[datum] = menge
    return bestentwicklung
    

def bestandsentwicklung(artnr, dateformat="%Y-%m-%d", lager=0):
    """Liefert ein Dictionary, dass alle zukünftigen, bzw. noch nicht ausgeführten Bewegungen
    
    für einen Artikel beinhaltet. Ist kein Bestand für den Artikel vorhanden, wird None zurückgegeben.
    
    dateformat bestimmt dabei die Keys des Dictionaries und steuert die Granularität. "%Y-%W" sorgt 
    z.B. für eine wochenweise Auflösung.

    >>> bestandsentwicklung('14865')
    {'2009-02-20': 1200,
     '2009-03-02': 860,
     '2009-04-01': 560,
     '2009-05-04': 300}
     
     Achtung: Diese Funktion implementiert bis zu 120 Sekunden caching.
    """
    # check if we have a cached result.
    memc = caching.get_cache()
    memc_key = 'husoftm.bestandsentwicklung.%r.%r.%r' % (artnr, dateformat, lager)
    cache = memc.get(memc_key)
    if cache:
        return cache

    # Auflösung von Set-Artikeln in ihre Unterartikel.
    # Die Bestandsentwicklung des Sets der Bestandsentwicklung der Unterartikel dividiert durch
    # die jeweilige Anzahl der Unterartikel pro Set entsprechen.
    components = husoftm.artikel.komponentenaufloesung([(1, artnr)])
    bestentw_all = []
    for mng_set, artnr_set in components:
        bestentwicklung = dict(((datum, mng/mng_set) for (datum, mng) in
                                _sum_bewegungen(_bewegungen(artnr_set, dateformat, lager)).items()))
        bestentw_all.append(bestentwicklung)

    # Die kleinste Menge eines Sub-Artikels ist die an diesem Datum verfügbare Menge
    # !Es gibt Setartikel, deren Unterartikel sich überschneiden.
    # !Darum die get Methode, mit einem Defaultwert von 99999999
    commonkeys = [dct.keys() for dct in bestentw_all]
    commonkeys = set(itertools.chain(*commonkeys))
    bestentwicklung = {}
    for key in commonkeys:
        bestentwicklung[key] = min(dct.get(key, 99999999) for dct in bestentw_all)

    # Bei Setartikeln werden die Auftragsmengen (evtl. auch die Bestellmengen) mal für den Set,
    # und mal für die Subartikel behandelt.
    # Darum hier noch die Bewegungen des Setartikels überlagern
    if artnr_set != artnr: # Das darf nur auf Setartikel angewendet werden!
        bewegungen_set = _bewegungen(artnr, dateformat, lager)

        # Bewegungsmengen aus der gerade ermittelten Bestandsendwicklung der Unterartikel erzeugen
        bestentwicklung = sorted(bestentwicklung.items())
        date, mng = bestentwicklung[0]
        bewegungen_sub = [(date, mng)]
        for date, x in bestentwicklung[1:]:
            bewegungen_sub.append((date, x - mng))
            mng = x

        # Bewegungen mit denen des Setartikels kombinieren und erneut eine Bestandsentwicklung
        # daraus berechnen
        bewegungen = bewegungen_sub + bewegungen_set
        bewegungen.sort()
        bestentwicklung = _sum_bewegungen(bewegungen)

    if not bestentwicklung:
        # kein Bestand - diese Information 6 Stunden cachen
        memc.set(memc_key, bestentwicklung, 60*60*6)
    else:
        # Bestand - die menge fuer 2 Minuten cachen
        memc.set(memc_key, bestentwicklung, 60*2)
    
    return bestentwicklung


def bestellmengen(artnr, lager=0):
    """Liefert eine liste mit allen Bestellten aber noch nicht gelieferten Wareneingängen.
    
    >>> bestellmengen('14865')
    
    {datetime.date(2009, 2, 20): 1200,
     datetime.date(2009, 5, 5): 300}
    """
    condition = "BPSTAT<>'X' AND BPKZAK=0 AND BPARTN=%s" % sql_quote(artnr)
    if lager:
        condition += "AND BPLGNR=%s" % sql_quote(lager)
    # detailierte Informationen gibts in EWZ00
    rows = get_connection().query('EBP00', fields=['BPDTLT', 'SUM(BPMNGB-BPMNGL)'], ordering='BPDTLT',
                                  grouping='BPDTLT', condition=condition)
    return dict([(x['liefer_date'], as400_2_int(x['SUM(BPMNGB-BPMNGL)']))
                 for x in rows if as400_2_int(x['SUM(BPMNGB-BPMNGL)']) > 0])
    

def auftragsmengen(artnr, lager=None):
    """Liefert eine Liste offener Aufträge für einen Artikel OHNE UMLAGERUNGEN.
    
    >>> auftragsmengen(14865)
    {datetime.date(2009, 3, 2): 340,
     datetime.date(2009, 4, 1): 300,
     datetime.date(2009, 5, 4): 260,
     datetime.date(2009, 6, 2): 300}
    """
    
    condition = (
    "AKAUFN=APAUFN"
    " AND AKAUFA<>'U'"                # kein Umlagerungsauftrag
    " AND APARTN=%s"                  # Artikelnummer
    " AND APSTAT<>'X'"                # Position nicht logisch gelöscht
    " AND APKZVA=0"                   # Position nicht als 'voll ausgeliefert' markiert
    " AND (APMNG-APMNGF) > 0"  # (noch) zu liefernde menge ist positiv
    " AND AKSTAT<>'X'"                # Auftrag nicht logisch gelöscht
    " AND AKKZVA=0")                  # Auftrag nicht als 'voll ausgeliefert' markiert
    
    if lager:
        # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 gibt nix
        condition = condition + (" AND APLGNR=%d" % lager)
    rows = get_connection().query(['AAP00', 'AAK00'], fields=['APDTLT', 'SUM(APMNG-APMNGF)'],
                   condition=condition % (sql_quote(artnr)),
                   ordering='APDTLT', grouping='APDTLT',
                   querymappings={'SUM(APMNG-APMNGF)': 'menge_offen', 'APDTLT': 'liefer_date'})
    return dict([(x['liefer_date'], as400_2_int(x['menge_offen'])) for x in rows if x['menge_offen'] > 0])


def get_offene_auftraege(lager=100):
    """Liefert eine Liste offener Aufträge OHNE UMLAGERUNGEN.
    
    Ported to connection2 from MoftS.lib.mofts.client.as400.py to be used in logistik/versandauslastung
    """
    # TODO: merge w/ auftragsmengen_alle_artikel()
    mappings = {
            'APARTN': 'artnr',
            # das kann bei ueberlieferungen zu negativen werten fuehren 
            # und ist bei auftraegen mit mengenaenderungen gelegentlich 0 - siehe Case 227:
            'APMNG-APMNGF-APMNGG': 'menge',
            'APMNG': 'bestellmenge',
            'AKKDNR': 'kundennummer',
            'AKAUFN': 'auftragsnummer',
            'APDTLT': 'liefer_date',
            'AKAUFA': 'art'}
    fields = mappings.keys()
    condition = "AKAUFN=APAUFN AND APKZVA=0 AND AKKZVA=0 AND AKAUFA<>'U' AND AKSTAT<>'X' AND APSTAT<>'X' AND APLGNR=%s" % lager
    ordering = 'AKAUFN'
    rows = get_connection().query(['AAP00', 'AAK00'],
            fields=fields,
            condition=condition,
            ordering='APDTLT',
            #grouping=['APARTN', 'APDTLT'], # das funktioniert nicht
            querymappings=mappings)
    return rows


def auftragsmengen_alle_artikel(lager=0, umlagerungen=False):
    """Liefert eine Liste offener Aufträge aller Artikel.

    Per default OHNE UMLAGERUNGEN.

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
    
    condition = (
    "AKAUFN=APAUFN"
    " AND APSTAT<>'X'"                # Position nicht logisch gelöscht
    " AND APKZVA=0"                   # Position nicht als 'voll ausgeliefert' markiert
    " AND (APMNG-APMNGF) > 0"         # (noch) zu liefernde menge ist positiv
    " AND AKSTAT<>'X'"                # Auftrag nicht logisch gelöscht
    " AND AKKZVA=0")                  # Auftrag nicht als 'voll ausgeliefert' markiert

    if not umlagerungen: # kein Umlagerungsauftrag:
        condition += " AND AKAUFA<>'U'"
    
    if lager:
        # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 gibt nix
        condition += " AND APLGNR=%d" % lager
    rows = get_connection().query(['AAP00', 'AAK00'],
            fields=['APARTN', 'APDTLT', 'SUM(APMNG-APMNGF)'],
            condition=condition,
            ordering='APDTLT', grouping=['APARTN', 'APDTLT'],
            querymappings={'SUM(APMNG-APMNGF)': 'menge_offen', 'APARTN': 'artnr', 'APDTLT': 'liefer_date'})
    ret = {}
    for row in rows:
        if row['menge_offen']:
            ret.setdefault(str(row['artnr']), {})[row['liefer_date']] = as400_2_int(row['menge_offen'])
    return ret
    

def versionsvorschlag(menge, orgartnr, date, dateformat="%Y-%m-%d"):
    """Gib einen Vorschlag für Zusammenstellung von Artikeln zurück.
    
    >>> versionsvorschlag(2000, '22006', '2009-01-04')
    (True, [(1184, '22006'), (816, '22006/03')])
    >>> versionsvorschlag(2000, '76095', '2009-01-04')
    (False, [(0, '76095')])
    """
    ret = []
    benoetigt = menge
    for artnr in cs.masterdata.article.alternatives(orgartnr):
        dummy, untermenge = frei_am(benoetigt, artnr, date, dateformat)
        if untermenge > 0:
            ret.append((min(benoetigt, untermenge), artnr))
            benoetigt -= untermenge
        if benoetigt <= 0:
            return True, ret
    return False, ret
    

def frei_am(menge, artnr, date, dateformat="%Y-%m-%d"):
    """Ermittelt, ob die Menge für einen Artikel zu dem Datum date frei ist.
    
    Rückgabewert ist ein Tupel. Dessen erster Eintrag gibt an, ob die Menge vorhanden ist,
    der zweite Eintrag entspricht der gesamt freien Menge zu diesem Datum.
    """
    bentwicklung = bestandsentwicklung(artnr, dateformat).items()
    if bentwicklung:
        bentwicklung.sort()
        # date should not be bigger than last date in bentwicklung
        mindate = min((date, bentwicklung[-1][0]))
        bentwicklung = (x for x in bentwicklung if x[0] >= mindate)
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
    

def get_umlagerungen(artnr=None):
    """Ermittelt wieviel Umlagerungen für einen Artikel unterwegs sind"""
    warnings.warn("get_umlagerungen() is deprecated use umlagermenge()", DeprecationWarning, stacklevel=2) 
    return umlagermenge(artnr, anlager=100)


def umlagermengen(anlager=100):
    """Ermittelt wieviel Umlagerungen aus einem oder allen Lagern nach Lager anlager unterwegs sind."""
    return _umlagermenge_helper(None, anlager)


def umlagermenge(artnr, anlager=100):
    """Ermittelt wieviel Umlagerungen für einen Artikel der nach anlager unterwegs sind."""
    assert(artnr)
    return _umlagermenge_helper(artnr, anlager)


def get_lagerbestandmitumlagerungen(artnr):
    """Deprecheated, don't use anymore."""
    warnings.warn("get_lagerbestandmitumlagerungen() is deprecated use bestand()",
                  DeprecationWarning, stacklevel=2) 
    return bestand(artnr, lager=100)
    

def bestand(artnr, lager=0):
    """Ermittelt den Lagerbestand (Buchbestand + kurzum in diesem Lager eintreffene Güter) eines Artikels.
    
    >>> bestand('76095')
    53
    """
    
    return buchbestand(artnr, lager=lager) + umlagermenge(artnr, lager)
    

def besteande(lager):
    """Ermittelt den Lagerbestand (Buchbestand + in diesem Lager eintreffene Güter) aller Artikel am Lager.
    
    >>> bestaende(100)
    {'01013': 61,
     '01020': 96,
     '01022': 2197,
     '01023': 1000,
     '01104': 110,
     '01104/01': 502,
     ...
     'WK83162': 380,
     'WK84020': 68}
     """
    
    rows = get_connection().query('XLF00', fields=['LFARTN', 'LFMGLP'],
               condition="LFLGNR=%d AND LFMGLP<>0 AND LFSTAT<>'X'" % (int(lager)))
    bbesteande = dict([(str(row[0]), as400_2_int(row[1])) for row in rows])
    
    # Offene Umlagerungen an dieses Lager zurechnen.
    uml = umlagermengen(anlager=lager)
    for artnr, umlagerungsmenge in uml.items():
        bbesteande[str(artnr)] = bbesteande.get(artnr, 0) + as400_2_int(umlagerungsmenge)
    return bbesteande
    

# class TestUmlagerungen(unittest.TestCase):
#     """Testet die Berechnung der Umlagerungen."""
#     
#     def test_misc(self):
#         """Performs tests on several functions and compares their results.
#         
#         It performs checks for every article in stock, so it is very time consuming and can only work during
#         low activity periods because of constant changes in stock during daytime.
#         """
#         
#         lager = 100
#         bstnde = besteande(lager)
#         for artnr, menge in bstnde.items():
#             bstnd = bestand(artnr, lager)
#             self.assertEqual(bstnd, menge)
#             
#             umenge = umlagermenge(artnr, lager)
#             bbestand = buchbestand(artnr, lager)
#             self.assertEqual(bbestand+umenge, menge)

def _test():
    """Some very simple tests."""
    #print "buchbestaende() =",
    (buchbestaende())
    #print "auftragsmengen_alle_artikel(34) = ",
    (auftragsmengen_alle_artikel(34))
    #print "verfuegbare_mengen(34) = ",
    (verfuegbare_mengen(34))
    #print "besteande(100) = ",
    (besteande(100))
    # for artnr in '76095 14600/03 14865 71554/A 01104 10106 14890 WK22002'.split():
    for artnr in ['14600/03', 'WK22002', '00000']:
        #print "versionsvorschlag(2000, %r, '2009-01-04') = " % artnr,
        (versionsvorschlag(2000, artnr, '2009-01-04'))
        #print "buchbestand(%r) = " % artnr,
        (buchbestand(artnr))
        #print "verfuegbare_menge(%r) = " % artnr,
        (verfuegbare_menge(artnr))
        #print "bestandsentwicklung(%r) = " % artnr,
        ((bestandsentwicklung(artnr)))
        #print "frei_ab(50, %r) = " % artnr,
        ((frei_ab(50, artnr)))
        #print "umlagermenge(%r, 100) = " % artnr,
        ((umlagermenge(artnr, 100)))
        #print "bestand(%r, 100) = " % artnr,
        ((bestand(artnr, 100)))
    return frei_ab(1000, '14600/03')
    

if __name__ == '__main__':
    _test()
    unittest.main()
