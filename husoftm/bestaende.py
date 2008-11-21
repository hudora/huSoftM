#!/usr/bin/env python
# encoding: utf-8
"""
bestaende.py - high level functions to access SoftM on the AS/400.

Created by Maximillian Dornseif on 2006-10-19.

Hier werden Warenbestände, verfügbare Mengen und dergleichen ermittelt.

"""

__revision__ = "$Revision$"

import datetime
import time
import warnings  
from types import ListType, TupleType
from husoftm.connection import get_connection
from husoftm.tools import sql_escape, sql_quote


def _int_or_0(data):
    """Helper for unwinding SoftM nested list replies."""
    try:
        if type(data) in (ListType, TupleType):
            if data and data[0]:
                if type(data[0]) in (ListType, TupleType):
                    return int(data[0][0])
                return int(data[0])
        if data:
            return int(data)
        return 0
    except TypeError:
        return 0
    

# see https://cybernetics.hudora.biz/intern/trac/wiki/HudoraGlossar -> Mengen for further enlightenment


def get_lagerbestand(artnr=None, lager=0):
    warnings.warn("get_lagerbestand() is deprecated use buchbestand()", DeprecationWarning, stacklevel=2) 
    return get_lagerbestand(artnr, lager)
    

def buchbestand(artnr, lager=0):
    """Gibt den Buchbestand eines Artikels für ein Lager zurück oder (lager=0) für alle Lager
    
    >>> buchbestand('14600')
    2345
    
    """
    
    rows = get_connection().query('XLF00', fields=['LFMGLP'],
               condition="LFLGNR=%d AND LFARTN='%s' AND LFMGLP<>0 AND LFSTAT=' '" % (int(lager), artnr))
    return _int_or_0(rows)
    

def get_verfuegbaremenge(artnr=None, lager=0):
    warnings.warn("get_verfuegbaremenge() is deprecated use verfuegbar()", DeprecationWarning, stacklevel=2) 
    return verfuegbare_menge(artnr, lager)


def verfuegbare_menge(artnr, lager=0):
    """Gibt die aktuell verfügbare Menge eines Artikels an einem Lager zurück oder (lager=0) für alle Lager
    
    Achtung! Die verfügbare Menge ist nicht die "freie Menge".
    
    >>> verfuegbare_menge('12345')
    3456
    """
    
    rows = get_connection().query('XLF00', fields=['LFMGLP', 'LFMGK4', 'LFMGLP-LFMGK4'],
               condition="LFLGNR=%s AND LFARTN='%s' AND LFMGLP<>0 AND LFSTAT = ' '" % (
                    sql_escape(lager), sql_escape(artnr)))
    if rows:
        (menge, lfmgk4, dummy) = rows[0]
        return _int_or_0(menge) - _int_or_0(lfmgk4)
    else:
        return 0
    

def freie_menge(artnr, dateformat="%Y-%m-%d"):
    """Liefert die sofort verkaufbare (freie / available) Menge zurück.
    
    Dabei wird nicht beachtet, dass eventuell eine Wiederbeschaffung erfolgen kann. Wenn die Bestände 
    in ferner Zukunft gegen Null gehen, geht auch die (jetzt) verfügbare Menge gegen null.
    
    >>> freie_menge('14600')
    2345
    
    """
    
    return max([min(bestandsentwicklung(artnr, dateformat).values()), 0])
    

def bestandsentwicklung(artnr, dateformat="%Y-%m-%d"):
    """Liefert ein Dictionary, dass alle zukünftigen Bewegungen für einen Artikel beinhaltet.
    
    dateformat bestimmt dabei die keys des dictionaries und steuert die Granularität. "%Y-%W" sorgt 
    z.B. für eine wochenweise Auflösung.
    
    >>> bestandsentwicklung('14865')
    {'2009-02-20': 1200,
     '2009-03-02': 860,
     '2009-04-01': 560,
     '2009-05-04': 300}
    """
    
    bewegungen = [(x[0].strftime(dateformat), x[1]) for x in bestellmengen(artnr).items()]
    bewegungen.extend([(x[0].strftime(dateformat), -1*x[1]) for x in auftragsmengen(artnr).items()])
    bewegungen.sort()
    # summieren
    # Startwert ist der heutige Lagerbestand
    menge = buchbestand(artnr)
    bestentwicklung = {}
    for datum, bewegungsmenge in bewegungen:
        menge += bewegungsmenge
        bestentwicklung[datum] = menge
    return bestentwicklung
    

def bestellmengen(artnr):
    """Liefert eine liste mit allen Bestellten aber noch nicht gelieferten Wareneingängen.
    
    >>> bestellmengen('14865')
    
    {datetime.date(2009, 2, 20): 1200,
     datetime.date(2009, 5, 5): 300}
    """
    
    # detailierte Informationen gibts in EWZ00
    rows = get_connection().query('EBP00', fields=['BPDTLT', 'SUM(BPMNGB-BPMNGL)'], ordering='BPDTLT',
                                 grouping='BPDTLT',
                                 condition="BPSTAT<>'X' AND BPKZAK=0 AND BPARTN=%s" % sql_quote(artnr))
    return dict([(x['liefer_date'], x['SUM(BPMNGB-BPMNGL)']) for x in rows if x['SUM(BPMNGB-BPMNGL)'] > 0])
    

def auftragsmengen(artnr, lager=0):
    """Liefert eine Liste offener Aufträge OHNE UMLAGERUNGEN.
    
    >>> auftragsmengen(14865)
    {datetime.date(2009, 3, 2): 340,
     datetime.date(2009, 4, 1): 300,
     datetime.date(2009, 5, 4): 260,
     datetime.date(2009, 6, 2): 300}
    """
    
    condition = (
    "AKAUFN=APAUFN"
    " AND APAUFA<>'U'"                # kein Umlagerungsauftrag
    " AND APARTN=%s"                  # Artikelnummer
    " AND APSTAT<>'X'"                # Position nicht logisch gelöscht
    " AND APKZVA=0"                   # Position nicht als 'voll ausgeliefert' markiert
    " AND (APMNG-APMNGF-APMNGG) > 0"  # (noch) zu liefernde menge ist positiv
    " AND AKSTAT<>'X'"                # Auftrag nicht logisch gelöscht
    " AND AKKZVA=0")                  # Auftrag nicht als 'voll ausgeliefert' markiert
    
    if lager:
        # Achtung, hier gibt es KEIN Lager 0 in der Tabelle. D.h. APLGNR=0 gibt nix
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['APDTLT', 'SUM(APMNG-APMNGF-APMNGG)'],
                       condition=(condition + " AND APLGNR=%d") % (sql_quote(artnr), lager),
                       ordering='APDTLT', grouping='APDTLT',
                       querymappings={'SUM(APMNG-APMNGF-APMNGG)': 'menge_offen'})
    else:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['APDTLT', 'SUM(APMNG-APMNGF-APMNGG)'],
                       condition=condition % (sql_quote(artnr)),
                       ordering='APDTLT', grouping='APDTLT',
                       querymappings={'SUM(APMNG-APMNGF-APMNGG)': 'menge_offen', 'APDTLT': 'liefer_date'})
    return dict([(x['liefer_date'], x['menge_offen']) for x in rows if x['menge_offen'] > 0])
    

def versionsvorschlag(menge, artnr, date, dateformat="%Y-%m-%d"):
    """Gib einen Vorschlag für Zusammenstellung von Artikeln zurück.
    
    >>> pprint.pprint(versionsvorschlag(2000, '76095', '2009-01-04'))
    
    """
    
    #p = Product.objects.get(artnr=artnr)
    #artnrs.add([x.artnr for x in p.versions.all()])
    # FIXME: versionsnummernermittlung - we are missing a sensible api here
    artnrs = ['22006', '22006/02', '22006/03']
    
    ret = []
    benoetigt = menge
    for artnr in sorted(artnrs):
        bentwicklung = bestandsentwicklung(artnr, dateformat).items()
        if not bentwicklung:
            continue
        bentwicklung.sort()
        # date should not be bigger than last date in bentwicklung
        date = min([date, bentwicklung[-1][0]]) 
        
        bentwicklung = [x for x in bentwicklung if x[0] >= date]
        verfuegbar = min([quantity for date, quantity in bentwicklung])
        
        if verfuegbar > 0:
            ret.append((min(benoetigt, verfuegbar), artnr))
            benoetigt -= verfuegbar
        if benoetigt <= 0:
            return True, ret
    return False, ret
    

def frei_ab(menge, artnr, dateformat="%Y-%m-%d"):
    """Finds the earliest date when menge is frei (available) or None if it isn't available at all."""
    
    # Algorythmus: vom Ende der Bestandskurve nach hinten gehen, bis wir an einen punkt kommen, wo die
    # Kurve niedriger ist, als die geforderte Menge - ab da ist die Menge frei.
    
    bentwicklung = bestandsentwicklung(artnr, dateformat)
    # shortcut: the bestand never drops below menge
    if min(bentwicklung.values()) > menge:
        return datetime.date.today()
    
    bentwicklung = bentwicklung.items()
    bentwicklung.sort()
    bentwicklung.reverse()
    previous_date = None
    for datum, menge_frei in bentwicklung:
        if menge_frei < menge:
            if previous_date:
                return datetime.date.fromtimestamp(time.mktime(time.strptime(previous_date, dateformat)))
            else:
                return False
        previous_date = datum
    return False
    

def get_umlagerungen(artnr, vonlager=26):
    warnings.warn("get_umlagerungen() is deprecated use umlagermenge()", DeprecationWarning, stacklevel=2) 
    return umlagermenge(artnr, anlager=100, vonlager=vonlager)
    

def umlagermenge(artnr, anlager=100, vonlager=None):
    """Ermittelt wieviel Umlagerungen für einen Artikel unterwegs sind"""
    
    # Das Auslieferungslager steht in AKLGN1, Das Ziellager steht in AKLGN2
    # In APLGNR steht AUCH das Abgangslager
    
    condition = (
    "AKAUFN=APAUFN"
    " AND APAUFA='U'"                 # kein Umlagerungsauftrag
    " AND APARTN=%s"                  # Artikelnummer
    " AND APSTAT<>'X'"                # Position nicht logisch gelöscht
    " AND APKZVA=0"                   # Position nicht als 'voll ausgeliefert' markiert
    " AND (APMNG-APMNGF-APMNGG) > 0"  # (noch) zu liefernde menge ist positiv
    " AND AKSTAT<>'X'"                # Auftrag nciht logisch gelöscht
    " AND AKKZVA=0"                   # Auftrag nicht als 'voll ausgeliefert' markiert
    " AND AKLGN2=%d")                 # Zugangslager
    
    if vonlager:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['SUM(APMNG-APMNGF-APMNGG)'], nomapping=True,
            condition=(condition + " AND AKLGN1=%s") % 
                      (sql_quote(artnr), int(anlager), int(vonlager)))
    else:
        rows = get_connection().query(['AAP00', 'AAK00'], fields=['SUM(APMNG-APMNGF-APMNGG)'], nomapping=True,
            condition=condition % (sql_quote(artnr), int(anlager)))
    return _int_or_0(rows)
    

def get_lagerbestandmitumlagerungen(artnr, vonlager=26):
    warnings.warn("get_lagerbestandmitumlagerungen() is deprecated use bestand()",
                  DeprecationWarning, stacklevel=2) 
    return bestand(artnr, anlager=100, vonlager=vonlager)
    

def bestand(artnr, lager=100, vonlager=None):
    """Ermittelt den Lagerbestand (Buchbestand + kurzum in diesem Lager eintreffene Güter."""
    
    return buchbestand(artnr, lager=lager) + umlagermenge(artnr, lager, vonlager)
    

def test():
    """Some very simple tests."""
    import pprint
    for artnr in '76095 14600/03 14865 71554/A 01104 10106 14890 WK22002'.split():
        print "xx", artnr, "xxxxxxxxxxxxxxxxxxxxxxxx"
        pprint.pprint(versionsvorschlag(2000, artnr, '2009-01-04'))
        pprint.pprint(buchbestand(artnr))
        pprint.pprint(verfuegbare_menge(artnr))
        pprint.pprint((bestandsentwicklung(artnr)))
        pprint.pprint((frei_ab(50, artnr)))
        pprint.pprint((umlagermenge(artnr)))
        pprint.pprint((bestand(artnr)))
    

if __name__ == '__main__':
    test()
