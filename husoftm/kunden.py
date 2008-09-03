#!/usr/bin/env python
# encoding: utf-8
"""
kunden.py - High Level Access Kundeninformationen. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-04-13.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import datetime
import husoftm.connection
import husoftm.tools


class Kunde(object):
    """Representation of SoftM "Kunden" data objects."""
    
    def __init__(self, kundennr='', name1='', name2='', name3='', name4='',
                 adresse='', plz='', ort='', land=''):
        self.kundennr = kundennr
        self.name1 = name1
        self.name2 = name2
        self.name3 = name3
        self.name4 = name4
        self.adresse = adresse
        self.plz = plz
        self.ort = ort
        self.land = land
    
    def __repr__(self):
        return str(vars(self)) # crude ...
    
    def fill_from_softm(self, row):
        """Initializes the object from data returned by SoftM."""
        self.kundennr = row.get('kundennr', '') # 10003
        self.sortierfeld = row.get('sortierfeld', '') # AUER* NEUB
        self.name1 = row.get('name1', '') # Sport & Mode Auer
        self.name2 = row.get('name2', '') # 
        self.name3 = row.get('name3', '') # 
        #self.name4 = row.get('name4', '') # 
        self.adresse = row.get('strasse', '') # Marktplatz 5
        self.plz = row.get('postleitzahl', '') # 75387
        self.ort = row.get('ort', '') # Neubulach
        self.tel = row.get('tel', '') # 07053/7910
        self.fax = row.get('fax', '') # 07053/6041
        #self.url = row.get('url', '')
        self.mobil = row.get('mobil', '')
        self.mail = row.get('mail', '')
        #self.kundengruppe = row.get('kundengruppe_id', '')
        #self.postfach = row.get('postfach', '')
        #self.postfach_plz = row.get('postfach_plz', '')
        #self.created_at = row.get('erfassung_date', '') # 2004-12-01
        #self.updated_at = row.get('aendertung_date', '') # 2007-04-11
        #self.mitgliednr = row.get('mitgliednr', '')
        #self.ustid = row.get('ustid', '') # '132838685'
        self.adressdatei_id = row.get('adressdatei_id', '') # 123656179
        #self.company = row.get('company', '') # '06'
        #self.verband = row.get('verband', '')
        #self.gebiet = row.get('gebiet', '') # ': u'04'
        #self.distrikt = row.get('distrikt', '') # ': u'16'
        #self.vertreter = row.get('vertreter', '') # ': u'201'
        #self.branche = row.get('branche', '') # ': u'13'
        #self.versandart = row.get('versandart', '') # ': u''
        self.iln = unicode(row.get('ILN', '')).strip()
        self.land = husoftm.tools.land2iso(row['laenderkennzeichen']) # D
        if row['erfassung_date']:
            self.erfassung = row['erfassung_date']
        if row['aenderung_date']:
            self.aenderung = row['aenderung_date']
        else:
            self.aenderung = self.erfassung
        self.sachbearbeiter = row.get('sachbearbeiter', '') # ': u'Birgit Bonrath'
        #self.verpackungsvorschrift = row.get('verpackungsvorschrift', '') # ': u'',
        #self.lieferbedingung = row.get('lieferbedingung', '') # ': u''
        #self.auslieferunglager = row.get('auslieferunglager', 0) # ': 0
        #self.interne_firmennr = row.get('interne_firmennr', '') # ': u''
        self.unsere_lieferantennr = row.get('unsere_lieferantennumemr', '')
        # 'skontoschluessel': 16, 
        # 'mahnsperre': u'', 
        # 'delcredereschl\xc3\xbcssel': 0, 
        # 'bonnitaet': u'', 
        # 'liefersperre': 0, 
        # 'kreditlimit2': 0, 
        # 'lastschrift': u'', 
        # 'offener_aftragswert': 2.7000000000000002, 
        # 'kreditlimit': 2.7000000000000002, 
        # 'inland_ausland': 0, 
        return self
    

def get_kundennrn():
    """Returns a list of all 'Kudennummern'."""
    
    rows = husoftm.connection.get_connection().query('XKD00', fields=['KDKDNR'])
    return [int(x) for x in rows]


def get_changed_after(date):
    """Returns a list of all Kundennummern where the underlaying Data has changed since <date>."""
    
    date = int(date.strftime('1%y%m%d'))
    rows = husoftm.connection.get_connection().query('XKD00', fields=['KDKDNR'],
                                          condition="KDDTER>%d OR KDDTAE>=%d" % (date, date))
    ret = set([int(x) for x in rows])
    # 
    rows = husoftm.connection.get_connection().query('AKZ00', fields=['KZKDNR'],
                                          condition="KZDTAE>=%d" % (date))
    ret = ret|set([int(x) for x in rows])
    return list(ret)


def get_kunde(kdnnr):
    """Get the Kunde object representing Kundennummer <kdnnr>."""
    
    rows = husoftm.connection.get_connection().query(['XKD00', 'XKS00', 'AKZ00'],
           condition="KDKDNR='%8d' AND KSKDNR='%8d' AND KZKDNR LIKE '%s'" % (int(kdnnr), int(kdnnr),
                     '%' + str(kdnnr)))
    if not rows:
        # no AKZ00 entry
        rows = husoftm.connection.get_connection().query(['XKD00', 'XKS00'],
               condition="KDKDNR='%8d' AND KSKDNR='%8d'" % (int(kdnnr), int(kdnnr)))
    if not rows:
        # no XKS00 entry
        rows = husoftm.connection.get_connection().query(['XKD00', 'AKZ00'],
               condition="KDKDNR='%8d' AND KZKDNR='%8d'" % (int(kdnnr), int(kdnnr)))
    if not rows:
        # no AKZ and XKS00 entry
        rows = husoftm.connection.get_connection().query(['XKD00'],
               condition="KDKDNR='%8d'" % (int(kdnnr)))
    if len(rows) > 1:
        raise RuntimeError("Mehr als einen Kunden gefunden: %r" % kdnnr)
    
    return Kunde().fill_from_softm(rows[0])
    

def get_kunde_by_iln(iln):
    """Get Kunden Address based on ILN.
    
    See http://cybernetics.hudora.biz/projects/wiki/AddressProtocol for the structure of returned data."""
    
    rows = husoftm.connection.get_connection().query(['XKS00'], condition="KCE2IL='%s'" % (int(iln), ))
    if rows:
        # stammadresse
        return get_kunde(rows[0]['kundennr'])
    else:
        # abweichende Lieferadresse
        rows = husoftm.connection.get_connection().query(['AVA00'], condition="VAILN='%s'" % (int(iln), ))
        if rows:
            rows = husoftm.connection.get_connection().query(['XXA00'],
                condition="XASANR='%s'" % (int(rows[0]['satznr']), ))
            if rows:
                return Kunde().fill_from_softm(rows[0])
    

def _selftest():
    """Test basic functionality"""
    print get_kunde_by_iln('4306544031019')
    print get_kunde_by_iln('4306544000008')
    return
    print len(get_changed_after(datetime.date(2007, 1, 1)))
    nummern = get_kundennummern()
    print len(nummern)
    for kdnnr in nummern:
        print get_kunde(kdnnr=kdnnr) # ['aendertung_date']

if __name__ == '__main__':
    _selftest()
