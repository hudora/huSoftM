#!/usr/bin/env python
# encoding: utf-8
"""
bestaende.py - high level functions to access SoftM on the AS/400.

Created by Maximillian Dornseif on 2006-10-19.
"""

__revision__ = "$Revision$"

import datetime, time
from types import *
from husoftm.connection import get_connection
from husoftm.tools import sql_escape, sql_quote

def _int_or_0(data):
    try:
        if type(data) in (ListType, TupleType):
            if data and data[0]:
                return int(data[0])
        if data:
            return int(data)
        return 0
    except TypeError:
        return 0
    

def get_lagerbestand(artnr=None, lager=0):
    """Gibt den Buchbestand eines Artikels für ein Lager zurück oder (lager=0) für alle Lager"""
    
    if not artnr:
        rows = get_connection().query('XLF00', fields=['LFARTN', 'LFMGLP'],
                   condition="LFLGNR = %s AND LFMGLP <> 0 AND LFSTAT = ' '" % sql_escape(lager))
        ret = {}
        for artnr, menge in rows:
            ret[artnr] = int(menge)
        return ret
    else:
        rows = get_connection().query('XLF00', fields=['LFMGLP'],
                   condition="LFLGNR=%d AND LFARTN='%s' AND LFMGLP<>0 AND LFSTAT=' '" % (int(lager), artnr))
        return _int_or_0(rows)
    return rows
    

def get_verfuegbaremenge(artnr=None, lager=0):
    """Gibt die aktuell verfügbare Menge eines Artikels an einem Lager zurück oder (lager=0) für alle Lager"""
    
    if not artnr:
        rows = get_connection().query('XLF00', fields=['LFARTN', 'LFMGLP', 'LFMGK4'],
                   condition="LFLGNR=%s AND LFMGLP<>0 AND LFSTAT=' '" % (sql_escape(lager),))
        ret = {}
        for artnr, menge, lfmgk4 in rows:
            ret[artnr] = int(menge) - int(lfmgk4)
        return ret
    else:
        rows = get_connection().query('XLF00', fields=['LFMGLP', 'LFMGK4', 'LFMGLP-LFMGK4'],
                   condition="LFLGNR=%s AND LFARTN='%s' AND LFMGLP<>0 AND LFSTAT = ' '" % (
                        sql_escape(lager), sql_escape(artnr)))
        if rows:
            (menge, lfmgk4, foo) = rows[0]
            return _int_or_0(menge) - _int_or_0(lfmgk4)
        else:
            return 0
    

def get_umlagerungen(artnr=None, vonlager=26):
    """Ermittelt wieviel Umlagerungen für einen Artikel unterwegs sind"""
    
    if not artnr:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['APARTN', 'SUM(APMNG)'],
                                              condition="AKAUFN=APAUFN AND APKZVA=0 AND AKKZVA=0 AND " +
                                              "APAUFA='U' AND AKSTAT<>'X' AND APSTAT<>'X' AND " +
                                              "APLGNR=%s" % sql_escape(vonlager),
                                              grouping='APARTN', nomapping=True)
        ret = {}
        for artnr, menge in rows:
            ret[artnr] = int(menge)
        return ret
    else:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['SUM(APMNG)'],
                                              condition="AKAUFN=APAUFN AND APKZVA=0 AND AKKZVA=0 AND " +
                                              "APAUFA='U' AND AKSTAT<>'X' AND APSTAT<>'X' AND " +
                                              ("APLGNR=%s AND APARTN='%s'" % (sql_escape(vonlager), 
                                               sql_escape(artnr))), nomapping=True)
        # Das Auslieferungslager steht in AKLGN1, Das Ziellager steht in AKLGN2
        # Zur Zeit verwenden wir APLGNR, dass kann man dann ignorieren
        return _int_or_0(rows)
    

def get_lagerbestandmitumlagerungen(artnr=None, lager=100, vonlager=26):
    """Ermittelt den Lagerbestand inclusive der kurzum in idesem Lager eintreffenen Güter."""
    
    bestand = get_lagerbestand(artnr=artnr, lager=lager)
    # TODO: eigentlich ist 'vonlager' uninteressant. Hauptsache die ware kommt - woher ist ja egal.
    # wenn wir vonlager=0 setzen, reicht das?
    umlagerungen = get_umlagerungen(artnr=artnr, vonlager=vonlager)
    if artnr:
        # sinmple: no calculation needed
        return _int_or_0(bestand) + _int_or_0(umlagerungen)
    else:
        for artnr, menge in bestand.items():
            bestand[artnr] = menge + umlagerungen.get(artnr, 0)
        # add stuff we have only in umlagerungen
        lagerart = set(bestand.keys())
        umlagerart = set(umlagerungen.keys())
        for artnr in umlagerart.difference(lagerart):
            if artnr in bestand: # TODO: ifclause should be removed - belt and suspenders aproach
                raise RuntimeError("Bad Thinking!")
            bestand[artnr] = umlagerungen[artnr]
        return bestand
    

def get_bestellungen(artnr):
    # TODO: test
    # detailierte Informationen gibts in EWZ00
    rows = get_connection().query('EBP00', fields=['BPARTN', 'BPDTLT', 'BPMNGB-BPMNGL'],
                   condition="BPSTAT<>'X' AND BPKZAK=0 AND BPARTN=%s" % sql_quote(artnr))
    print rows
    

def get_offene_auftraege(artnr, lager=0):
    """Liefert eine Liste offener Aufträge OHNE UMLAGERUNGEN."""
    # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 gibt nix
    #mappings = {'APARTN': 'artnr',
    #               'APMNG-APMNGF-APMNGG':  'menge', # das kann bei ueberlieferungen zu negativen werten fuehren
    #                                                # und ist bei auftraegen mit mengenaenderungen gelegentlich 0 - siehe Case 227
    #               'APMNG': 'bestellmenge',
    #               'AKKDNR': 'kundennummer',
    #               'AKAUFN': 'auftragsnummer',
    #               'APDTLT': 'liefer_date',
    #               'AKAUFA': 'art', }
    if lager:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['SUM(APMNG-APMNGF-APMNGG)', 'APDTLT'],
                       condition="AKAUFN=APAUFN AND APKZVA=0 AND AKKZVA=0 AND APAUFA<>'U' AND AKSTAT<>'X' AND APSTAT<>'X' AND APARTN=%s AND APLGNR=%d" % (sql_quote(artnr), lager),
                       ordering='APDTLT', grouping='APDTLT')
    else:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['SUM(APMNG-APMNGF-APMNGG)', 'APDTLT'],
                       condition="AKAUFN=APAUFN AND APKZVA=0 AND AKKZVA=0 AND APAUFA<>'U' AND AKSTAT<>'X' AND APSTAT<>'X' AND APARTN=%s" % (sql_quote(artnr)),
                       ordering='APDTLT', grouping='APDTLT')
    
    print rows


### TODO: legacy code follows. Needs to be ported to the get_connection().query() interface.


def bestandsentwicklung(artnr, dateformat="%Y-%m-%d"):
    """Liefert ein dictionary, dass alle zukünftigen Bewegungen für einen Artikel beinhaltet.
    
    dateformat bestimmt dabei die keys des dictionaries und steuert die Granularität. "%Y-%W" sorgt 
    z.B. für eine wochenweise Auflösung.
    """
    
    softm = as400.MoftSconnection()
    bewegungen = []
    # Startwert ist der heutige Lagerbestand
    start = get_lagerbestand(artnr=artnr)
    if start:
        start = start[0]['menge']
    else:
        start = 0
    
    for bestellung in get_bestellungen(artnr=artnr):
        bewegungen.append((bestellung['liefer_date'].strftime(dateformat), bestellung['menge']))
    for auftrag in softm.get_offene_auftraege(artnr=artnr):
        bewegungen.append((auftrag['liefer_date'].strftime(dateformat), -1*auftrag['menge']))
    bewegungen.sort()
    # summieren
    menge = start
    bestentwicklung = {}
    for datum, bewegungsmenge in bewegungen:
        menge += bewegungsmenge
        bestentwicklung[datum] = menge
    return bestentwicklung

def bestandsentwicklung_mit_versionen(artnr, dateformat="%Y-%m-%d"):
    
    artnr = artnr.split('/')[0]
    artnrs = set([artnr])
    
    p = Product.objects.get(artnr=artnr)
    artnrs.add([x.artnr for x in p.versions.all()])
    
    bewegungen = []
    for artnr in artnrs:
        bewegungen.extend(bestandsentwicklung(artnr, dateformat).items())
    bewegungen.sort()
    menge = 0
    bestentwicklung = {}
    for datum, bewegungsmenge in bewegungen:
        menge += bewegungsmenge
        bestentwicklung[datum] = menge
    return bestentwicklung

def verfuegbare_menge(artnr, dateformat="%Y-%m-%d"):
    """Liefert die sofort verfügbare Menge zurück.
    
    Dabei wird nicht beachtet, dass eventuell eine Wiederbeschaffung erfolgen kann. Wenn die Bestände 
    in ferner Zukunft gegen Null gehen, geht auch die (jetzt) verfügbare Menge gegen null."""
    
    return max([min(bestandsentwicklung_mit_versionen(artnr, dateformat).values()), 0])
    #return max([min(bestandsentwicklung(artnr, dateformat).values()), 0])


def verfuegbar_am(artnr, date, dateformat="%Y-%m-%d"):
    """  """
    bentwicklung = bestandsentwicklung(artnr, dateformat).items()
    if not bentwicklung:
        return None
    bentwicklung.sort()
    # date should not be bigger than last date in bentwicklung
    date = min([date, bentwicklung[-1][0]]) 
    bentwicklung = [x for x in bentwicklung if x[0] >= date]
    return min([quantity for date, quantity in bentwicklung])
        

def versionsvorschlag(menge, artnr, date, dateformat="%Y-%m-%d"):
    """Gib einen Vorschlag für Zusammenstellung von Artikeln zurück"""
    
    artnr = artnr.split('/')[0]
    artnrs = set([artnr])
    
    p = Product.objects.get(artnr=artnr)
    artnrs.add([x.artnr for x in p.versions.all()])

    ret = []
    benoetigt = menge
    print "benoetigt:", benoetigt
    for artnr in sorted(artnrs):
        bentwicklung = bestandsentwicklung(artnr, dateformat).items()
        if not bentwicklung:
            print "no bentwicklung for", artnr
            continue
        bentwicklung.sort()
        # date should not be bigger than last date in bentwicklung
        date = min([date, bentwicklung[-1][0]]) 
        
        print date, artnr, bentwicklung
        
        bentwicklung = [x for x in bentwicklung if x[0] >= date]
        verfuegbar = min([quantity for date, quantity in bentwicklung])
        
        print "verfuegbar:", artnr, verfuegbar
        
        if verfuegbar > 0:
            ret.append((min(benoetigt, verfuegbar), artnr))
            benoetigt -= verfuegbar
        if benoetigt <= 0:
            return True, ret
    return False, ret


def verfuegbar_ab(artnr, dateformat="%Y-%m-%d"):
    #bentwicklung = bestandsentwicklung_mit_versionen(artnr, dateformat)
    bentwicklung = bestandsentwicklung(artnr, dateformat)
    if min(bentwicklung.values()) >= MINIMALMENGE:
        return datetime.date.today()
    bentwicklung = bentwicklung.items()
    bentwicklung.sort()
    bentwicklung.reverse()
    previous_date = None
    for datum, menge in bentwicklung:
        if menge < MINIMALMENGE:
            if previous_date:
                return datetime.date.fromtimestamp(time.mktime(time.strptime(previous_date, dateformat)))
            else:
                return datetime.date.fromtimestamp(time.mktime(time.strptime(datum, dateformat))) + WIEDERBESCHAFFUNGSZEIT
        previous_date = datum
    # we should  never reach here
    return datetime.date.today() + datetime.timedelta(days=360)


if __name__ == '__main__':
    start = datetime.datetime.now()
    #import pprint
    #pprint.pprint(bestandsentwicklung('14600/03'))
    # print datetime.datetime.now() - start
    # print "verfuegbar_am", verfuegbar_am('14600/03', '2008-12-06')
    # print datetime.datetime.now() - start
    # print "verfuegbar_ab", verfuegbar_ab('14600/03')
    # print datetime.datetime.now() - start
    # print "verfuegbare_menge", verfuegbare_menge('14600/03')
    
    #print verfuegbar_am("76095/01", '2009-01-04')
    #print versionsvorschlag(200000, '14600', '2009-01-04')
    print get_bestellungen('14600/03')
    print get_offene_auftraege('14600/03')
