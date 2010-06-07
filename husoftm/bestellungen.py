#!/usr/bin/env python
# encoding: utf-8
"""
bestellungen.py

Created by Maximillian Dornseif on 2009-07-02.
Copyright (c) 2009 HUDORA. All rights reserved.
"""

import types
import unittest
from decimal import Decimal
from husoftm.connection2 import get_connection
from husoftm.tools import sql_escape, sql_quote, date2softm


class Bestellung(object):
    """Repräsentiert eine bestellung in SoftM."""
    pass
    

def kursfaktorkorrektur(rows, kursname='kurs', kursfaktorname='kursfaktor', umdrehen=True):
    """Wandelt einen Kurs anhand des Kursfaktors inhand das von uns gewohnte Format.
    
    d.h. wenn man 1.33 US$ für einen Euro bekommt, spichert SoftM, dass man für den
    Euro 0.75 U$ bekommt. Dazu wird auch noch mit dem Kursfaktor skaliert um
    Rundungsfehler zu vermeiden. Diese Routine verwandelt alles zurück in das gewohnte
    Format und gibt "Decimal" Objekte mit 4 Nachkommatellen zurück.
    
    Wenn umdrehen == False dann werden nur Kursfaktor und Nachkommastellen angepasst
    """
    if not isinstance(rows, list):
        rows = [rows]
    ret = []
    for row in rows:
        multiplikator = Decimal(10**(row[kursfaktorname]))
        if row[kursname]:
            if umdrehen:
                row[kursname] = (1/(Decimal(str(row[kursname]))/multiplikator)).quantize(Decimal(10) ** -4)
            else:
                row[kursname] = (Decimal(str(row[kursname]))/multiplikator).quantize(Decimal(10) ** -4)
        ret.append(row)
    return ret
    

def get_bestellung(bestellnr):
    """Liefert alle Positionen zu einer bestellnr.
    
    >>> bestellungen(43123)
    [{kopfdaten},
     [{'artnr': u'64114',
      'bestellmenge': 300,
      'bestellnr': 43042,
      'created_by': 62,
      'dateifuehrungsschluessel': u'',
      'geliefert_kz': 0,
      'gelieferte_menge': 0,
      'lager': 16,
      'liefer_date': datetime.date(2009, 12, 24),
      'lieferant': u'90088',
      'menge_offen': 300,
      'position': 11,
      'status': u'',
      'streckengeschaeft': 0,
      'termin1_date': datetime.date(2009, 12, 24),
      'termin2_date': datetime.date(2009, 12, 24),
      'updated_by': 62,
      'wunsch2_date': datetime.date(2009, 12, 24),
      'wunsch_date': None,
      'zugang_date': None},
      ...]
    ]
    
    """
    
    kopf = get_connection().query('EBL00', ordering=['BLBSTN DESC'],
        condition="BLSTAT<>'X' AND BLBSTN=%s" % sql_escape(bestellnr))
    if len(kopf) != 1:
        raise RuntimeError('inkonsistente Kopfdaten in EBL00')
    kopf = kursfaktorkorrektur(kopf)[0]
    
    # leer? WTF?
    # print get_connection().query('ESL00', condition="SLBSTN=%s" % sql_escape(ponr))
    
    # BZT00 - zusatztexte
    # positionen = get_connection().query(['EBP00', 'EWZ00'], ordering=['BPBSTN DESC', 'BPDTLT'],
    #     condition="WZBSTN=BPBSTN AND WZBSTP=BPBSTP AND BPSTAT<>'X' AND BPBSTN=%s" % sql_escape(ponr))
    positionen = get_connection().query(['EBP00'], ordering=['BPBSTN DESC', 'BPDTLT'],
        condition="BPSTAT<>'X' AND BPBSTN=%s" % sql_escape(bestellnr))
    # detailierte Informationen über den Zugang gibts in EWZ00
    
    # AND BPKZAK=0 to get only the open ones
    # Buchungsdaten: SELECT * FROM SMKDIFP/BBU00 WHERE BUBELN = 900003977 
    # Lagerveraenderung: SELECT * FROM SMKDIFP/XLB00 WHERE LBBSTN = '43072'
    # ?: SELECT * FROM EWZ00 WHERE WZBSTN = 43072
    return kopf, positionen
    

def _get_zugaenge_helper(rows):
    """Sammelt daten zu einer Bestellung aus verschiedenen Tabellen."""
    rows = kursfaktorkorrektur(rows, 'kurs_zugang', 'kursfaktor_zugang')
    ret = []
    for row in rows:
        lagerbuchungen = []
        if row['lagerbewegung_rechnung']:
            buchung = get_connection().query('XLB00',
                    condition="LBSANR=%s" % sql_escape(row['lagerbewegung_rechnung']))
            if len(buchung) > 1:
                raise RuntimeError('mehr als einen XLB Satz zu einem EWZ Satz: %r' % buchung)
            buchung = kursfaktorkorrektur(buchung)[0]
            lagerbuchungen.append(buchung)
        if row['lagerbewegung_zugang'] and row['lagerbewegung_zugang'] != row['lagerbewegung_rechnung']:
            buchung = get_connection().query('XLB00',
                    condition="LBSANR=%s" % sql_escape(row['lagerbewegung_zugang']))
            if len(buchung) > 1:
                raise RuntimeError('mehr als einen XLB Satz zu einem EWZ Satz: %r' % buchung)
            lagerbuchungen.append(kursfaktorkorrektur(buchung)[0])
        row['_lagerbuchungen'] = lagerbuchungen
        ret.append(row)
    return ret
    

def get_zugaenge_artnr(artnr):
    """Liefert alle Warenzugaenge eines Artikels"""
    
    rows = get_connection().query('EWZ00', ordering=['WZDTWZ'],
        condition="WZARTN='%s'" % sql_escape(artnr))
    return _get_zugaenge_helper(rows)
    

def get_zugaenge_bestellnr(bestellnr):
    """Liefert alle Warenzugaenge einer Bestellnummer"""
    
    rows = get_connection().query('EWZ00', # ordering=['WZDTWZ'],
        condition="WZBSTN=%s" % sql_escape(bestellnr))
    return _get_zugaenge_helper(rows)
    

def get_zugaenge_warenvereinnahmungsnr_simple(bestellnr, warenvereinnahmungsnr):
    """Liefert alle Warenzugaenge zu einer Bestellnummer und zug. Warenvereinnahmungsnummer.
    
    Sammelt *nicht* alle Daten zu einer Bestellung, sondern nur die jeweils gelieferten Positionen.
    """
    rows = get_connection().query('EWZ00', condition="WZBSTN=%s and WZWVNR=%s" %
                                  (sql_quote(bestellnr), sql_quote(warenvereinnahmungsnr)))
    return rows
    

def get_bestellungen_artnr(artnr):
    """Liefert alle Warenzugaenge einer Artikelnummer."""
    
    # BZT00 - zusatztexte
    positionen = get_connection().query(['EBP00', 'EBL00'], ordering=['BPDTLT'],
        condition="BLBSTN=BPBSTN AND BLSTAT<>'X' AND BPSTAT<>'X' AND BPARTN=%s" % sql_quote(artnr))
    ret = []
    for position in positionen:
        position['_zugaenge'] = [x for x in get_zugaenge_bestellnr(position['bestellnr'])
                                     if x['artnr'] == artnr]
        for zugang in position['_zugaenge']:
            # Buchungsdaten
            buchungen = get_connection().query('BBU00', 
                condition='BUBELN=%s' % sql_escape(zugang['rechnungsnr']))
            zugang['_fibubuchungen'] = kursfaktorkorrektur(buchungen, umdrehen=False)
        position['_lager_stapelschnittstelle'] = get_connection().query(
            'ESL00', condition="SLBSTN=%s AND SLBSTP=%s AND SLARTN=%s"
            % (sql_escape(position['bestellnr']), sql_escape(position['bestellpos']), sql_escape(artnr)))
        ret.append(position)
    # ?: SELECT * FROM EWZ00 WHERE WZBSTN = 43072
    return kursfaktorkorrektur(ret)


def bestellungskoepfe(mindate=None, maxdate=None, additional_conditions=None):
    conditions = ["BLSTAT<>'X'"]
    
    if mindate and maxdate:
        conditions.append("BPDTER BETWEEN %s AND %s" % (date2softm(mindate), date2softm(maxdate)))
    elif mindate:
        conditions.append("BPDTER > %s" % date2softm(mindate))
    elif maxdate:
        conditions.append("BPDTER < %s" % date2softm(maxdate))
    
    # You should REALLY know what you are doing!
    if additional_conditions:
        conditions.extend(additional_conditions)
    
    condition = " AND ".join(conditions)
    
    rows = get_connection().query('EBL00', ordering=['BLBSTN DESC', 'BLDTBE'], condition=condition)
    return rows


def bestellungen(mindate=None, maxdate=None, additional_conditions=None):
    """Liefert eine Liste mit allen bestellten aber nicht stornierten Wareneingängen.
    
    >>> bestellungen()
    [{'artnr': u'64114',
      'bestellmenge': 300,
      'bestellnr': 43042,
      'created_by': 62,
      'dateifuehrungsschluessel': u'',
      'geliefert_kz': 0,
      'gelieferte_menge': 0,
      'lager': 16,
      'liefer_date': datetime.date(2009, 12, 24),
      'lieferant': u'90088',
      'menge_offen': 300,
      'position': 11,
      'status': u'',
      'streckengeschaeft': 0,
      'termin1_date': datetime.date(2009, 12, 24),
      'termin2_date': datetime.date(2009, 12, 24),
      'updated_by': 62,
      'wunsch2_date': datetime.date(2009, 12, 24),
      'wunsch_date': None,
      'zugang_date': None},
      ...]
    
    """
    
    conditions = ["BPSTAT<>'X'"]
    
    if mindate and maxdate:
        conditions.append("BPDTER BETWEEN %s AND %s" % (date2softm(mindate), date2softm(maxdate)))
    elif mindate:
        conditions.append("BPDTER > %s" % date2softm(mindate))
    elif maxdate:
        conditions.append("BPDTER < %s" % date2softm(maxdate))
    
    # You should REALLY know what you are doing!
    if additional_conditions:
        conditions.extend(additional_conditions)
    
    condition = " AND ".join(conditions)
    
    rows = get_connection().query('EBP00', ordering=['BPBSTN DESC', 'BPDTLT'], condition=condition)
    # AND BPKZAK=0 to get only the open ones
    return rows
    

def testbestellung():
    """Experimental code for understandign SoftM."""
    
    fieldwidth = 13
    bestellungen_artnr = get_bestellungen_artnr('10167')
    
    print "B=Bestellposition, Z=Warenzugang S=Lagerstapelschnittstelle L=Lagerbuchung"
    bestellfelder = ['erfassung_date', 'artnr', 'kurs', 'lager', 'bestellmenge', 'gelieferte_menge', 
                     'gebuchte_menge',
  'tatsaechlicher_preis', 'bestell_preis_eur', 'abgerufener_positionswert',
  'streckengeschaeft',
  'wert',
  'zugang_date',
  'liefer_date',
    ]
    print "B   ",
    for feldname in bestellfelder:
        if feldname:
            print ("%%%ds" % fieldwidth) % feldname[:fieldwidth],
        else:
            print ("%%%ds" % fieldwidth) % '-',
    print
    
    lagerstapelfelder = ['beleg_date', 'artnr', 'kurs', 'lager', 'menge', 'rechnungs_date', None,
                         'wert', None, 'lager_korrektur_wert',
                         None,
                         'belegnr',
                         'lagerbewegungsnr',
                         ]
    print " S  ",
    for feldname in lagerstapelfelder:
        if feldname:
            print ("%%%ds" % fieldwidth) % feldname[:fieldwidth],
        else:
            print ("%%%ds" % fieldwidth) % '-',
        
    print
    
    zugangsfelder = ['rechnungs_date', 'artnr', 'kurs_zugang', 'lager', 'menge_berechnet', 'zugang_date',
                     None,
                     'tatsaechlicher_preis', 'bestell_preis', 'buchungsbetrag',
                     'rechnungsnr', 
                     'lagerbewegung_zugang', 'buchungstext']
    print "  Z ",
    for feldname in zugangsfelder:
        if feldname:
            print ("%%%ds" % fieldwidth) % feldname[:fieldwidth],
        else:
            print ("%%%ds" % fieldwidth) % '-',
    print
    
    buchungsfleder = ['beleg_date', 'artnr', 'kurs', 'lager', 'menge', 'zugangsmenge', 'bewegungsmenge', 
                      'wert', 'wert_aktuell', 'wert_erfassung',
                      'lagerwert_vor_buchung', 
    'bestand_vor_buchung',
'bestandsaenderung', 'belegnummer']
    print "   L",
    for feldname in buchungsfleder:
        if feldname:
            print ("%%%ds" % fieldwidth) % feldname[:fieldwidth],
        else:
            print ("%%%ds" % fieldwidth) % '-',
    print
    
    fibufelder = [
  'beleg_date',
  'op_info',
  'kurs',
  'personenkonto',
  'gegenkonto_sachbuchhaltung',
  'konto_sachbuchhaltung',
  'buchungsbetrag',
  'buchungsbetrag_gegenkonto',
  # 'Kopfsatznr.',
  # 'Satznr.Pers.OP',
  # 'waehrungbetrag',
  # 'auftragsnr',
  'erfassung_date',
  'belegnr',
  'faellig_date',
  'herkunft',
  'satz_date',
  'satznummer',
  ]
    print " F  ",
    for feldname in fibufelder:
        if feldname:
            print ("%%%ds" % fieldwidth) % feldname[:fieldwidth],
        else:
            print ("%%%ds" % fieldwidth) % '-',
    print

    
    for bestellung in bestellungen_artnr:
        print "B   ",
        for feldname in bestellfelder:
            if feldname is not None:
                data = bestellung[feldname]
            else:
                data = '-'
            print ("%%%ds" % fieldwidth) % data, 
        print
        for stapel in bestellung['_lager_stapelschnittstelle']:
            print " S  ",
            for feldname in lagerstapelfelder:
                if feldname is not None:
                    data = stapel[feldname],
                else:
                    data = '-'
                print ("%%%ds" % fieldwidth) % data, 
            print
            
        for zugang in bestellung['_zugaenge']:
            print "  Z ",
            for feldname in zugangsfelder:
                if feldname is not None:
                    data = zugang[feldname]
                else:
                    data = '-'
                print ("%%%ds" % fieldwidth) % data, 
            print
            for buchung in zugang['_lagerbuchungen']:
                print "   L",
                for feldname in buchungsfleder:
                    print ("%%%ds" % fieldwidth) % buchung[feldname], 
                print
            for buchung in zugang['_fibubuchungen']:
                print " F  ",
                for feldname in fibufelder:
                    print ("%%%ds" % fieldwidth) % buchung[feldname], 
                print
        print


class MiscTests(unittest.TestCase):
    
    def test_kursfaktorkorrektur(self):
        raise
        self.assertEqual(kursfaktorkorrektur([{'kurs': 751, 'kursfaktor': 4}]),
                         [{'kurs': Decimal("13.3156"), 'kursfaktor': 4}])
        self.assertEqual(kursfaktorkorrektur({'kurs': 751, 'kursfaktor': 4}),
                         [{'kurs': Decimal("13.3156"), 'kursfaktor': 4}])
        self.assertEqual(kursfaktorkorrektur({'kurs': 751.0, 'kursfaktor': 3}),
                         [{'kurs': Decimal("1.3316"), 'kursfaktor': 3}])
        self.assertEqual(kursfaktorkorrektur({'kurs': 751, 'kursfaktor': 2}),
                         [{'kurs': Decimal("0.1332"), 'kursfaktor': 2}])
        self.assertEqual(kursfaktorkorrektur({'kurs': 1000, 'kursfaktor': 1}),
                         [{'kurs': Decimal("0.0100"), 'kursfaktor': 1}])
        self.assertEqual(kursfaktorkorrektur({'kurs': 1000, 'kursfaktor': 0}),
                         [{'kurs': Decimal("0.0010"), 'kursfaktor': 0}])
    

if __name__ == '__main__':
    (get_bestellung(43248))
    (get_zugaenge_bestellnr(41971))
    (get_zugaenge_bestellnr(43072))
    #testbestellung()
    unittest.main()
