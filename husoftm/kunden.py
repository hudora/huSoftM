#!/usr/bin/env python
# encoding: utf-8
"""
kunden.py - High Level Access Kundeninformationen. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-04-13.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

import datetime
import husoftm.connection2
import cs.caching as caching
import husoftm.tools


class Kunde(object):
    """Representation of SoftM "Kunden" data objects."""
    
    def __init__(self, kundennr='', name1='', name2='', name3='', name4='',
                 strasse='', plz='', ort='', land=''):
        self.kundennr = kundennr
        self.name1 = name1
        self.name2 = name2
        self.name3 = name3
        self.name4 = name4
        self.strasse = strasse
        self.plz = plz
        self.ort = ort
        self.land = land
        self.sortierfeld = self.fax = self.tel = self.aenderung = self.iln = self.erfassung = None
        self.sachbearbeiter = self.mail = self.mobil = self.unsere_lieferantennr = self.adressdatei_id = None
    
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
        self.strasse = row.get('strasse', '') # Marktplatz 5
        self.plz = row.get('plz', '') # 75387
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
        if 'iln' in row:
            self.iln = unicode(int(row['iln'])).strip()
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
        self.verband = row.get('verband', '')
        self.mitgliednr = row.get('mitgliednr', '')
        self.ustid = row.get('ustid', '')
        self.kundengruppe = row.get('kundengruppe', '')
        self.vertreter = row.get('vertreter', '')
        self.gebiet = row.get('gebiet', '')
        self.branche = row.get('branche', '')
        self.distrikt = row.get('distrikt', '')
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
        # self.satzstatus = row.get('satzstatus', '')
        self.satzstatus = husoftm.connection2.get_connection().query('XKD00', fields=['KDSTAT'], condition="KDKDNR = %s" % husoftm.tools.sql_quote(row.get('kundennr')))
        return self
    

def get_kundennummern():
    """Returns a list of all 'Kundennummern'."""
    
    rows = husoftm.connection2.get_connection().query('XKD00', fields=['KDKDNR'])
    return [int(x[0]) for x in rows]


def get_changed_after(date):
    """Returns a list of all Kundennummern where the underlaying Data has changed since <date>."""
    
    date = int(date.strftime('1%y%m%d'))
    rows = husoftm.connection2.get_connection().query('XKD00', fields=['KDKDNR'],
                                          condition="KDDTER>%d OR KDDTAE>=%d" % (date, date))
    ret = set([int(x[0]) for x in rows])
    # 
    rows = husoftm.connection2.get_connection().query('AKZ00', fields=['KZKDNR'],
                                          condition="KZDTAE>=%d" % (date))
    ret = ret|set([int(x[0]) for x in rows])
    return list(ret)


#@caching.cache_function(60*60*2)
def get_kunde(kdnnr):
    """Get the Kunde object representing Kundennummer <kdnnr>.
    
    <kdnnr> must be an Integer in the Range 10000..99999.
    If no data exists for that KdnNr ValueError is raised."""
    
    rows = husoftm.connection2.get_connection().query(['XKD00', 'XKS00', 'AKZ00'],
           condition="KDKDNR='%8d' AND KSKDNR='%8d' AND KZKDNR LIKE '%s'" % (int(kdnnr), int(kdnnr),
                     '%' + str(kdnnr)))
    if not rows:
        # no AKZ00 entry
        rows = husoftm.connection2.get_connection().query(['XKD00', 'XKS00'],
               condition="KDKDNR='%8d' AND KSKDNR='%8d'" % (int(kdnnr), int(kdnnr)))
    if not rows:
        # no XKS00 entry
        rows = husoftm.connection2.get_connection().query(['XKD00', 'AKZ00'],
               condition="KDKDNR='%8d' AND KZKDNR='%8d'" % (int(kdnnr), int(kdnnr)))
    if not rows:
        # no AKZ and XKS00 entry
        rows = husoftm.connection2.get_connection().query(['XKD00'],
               condition="KDKDNR='%8d'" % (int(kdnnr)))
    if len(rows) > 1:
        raise RuntimeError("Mehr als einen Kunden gefunden: %r" % kdnnr)
    
    if not rows:
        raise ValueError("Keine Daten für Kundennummer %r gefunden" % kdnnr)
        
    return Kunde().fill_from_softm(rows[0])
    

@caching.cache_function(60*60*2)
def get_kunde_by_iln(iln):
    """Get Kunden Address based on ILN.
    
    See http://cybernetics.hudora.biz/projects/wiki/AddressProtocol for the structure of returned data.
    
    <kdnnr> must be an valit GLN/ILN encoded as an String.
    If no data exists for that GLN/ILN ValueError is raised.
    """
    
    rows = husoftm.connection2.get_connection().query(['XKS00'], condition="KCE2IL='%s'" % (int(iln), ))
    if rows:
        # stammadresse
        return get_kunde(rows[0]['kundennr'])
    else:
        # abweichende Lieferadresse
        rows = husoftm.connection2.get_connection().query(['AVA00'], condition="VAILN='%s'" % (int(iln), ))
        if rows:
            rows2 = husoftm.connection2.get_connection().query(['XXA00'],
                condition="XASANR='%s'" % (int(rows[0]['satznr']), ))
            if rows2:
                kunde = Kunde().fill_from_softm(rows2[0])
                kunde.kundennr = kunde.kundennr + ('/%03d' % int(rows[0]['versandadresssnr']))
                return kunde
    raise ValueError("Keine Daten für GLN/ILN %r gefunden" % iln)


def get_lieferadressen(kdnr):
    """Sucht zusätzliche Lieferadressen für eine Kundennr raus.

    Gibt eine Liste aller möglichen Lieferadressen in Form von Kunden-Objekten zurück.
    >>>get_lieferadressen(13041)
    [{'sortierfeld': '', 'tel': '+49 2041 690000', 'erfassung': datetime.date(2004, 12, 16),
      'strasse': 'An der Knippenburg 4', 'kundennr': '13041/001', 'mobil': '', 'gebiet': '',
      'aenderung': datetime.date(2004, 12, 16), 'mail': '', 'adressdatei_id': '', 'ustid': '',
      'distrikt': '', 'fax': '', 'ort': 'Bottrop', 'plz': '46238', 'vertreter': '', 'sachbearbeiter': '',
      'name4': '', 'name2': 'Schuhe GmbH & Co. KG', 'name3': 'Distributionszentrum West',
      'name1': 'Heinrich Deichmann', 'kundengruppe': '', 'land': 'DE', 'verband': '', 'iln': u'',
      'unsere_lieferantennr': '', 'mitgliednr': '', 'branche': ''},
     {'sortierfeld': '', 'tel': '+ 49 9852 9060', 'erfassung': datetime.date(2004, 12, 16), 'strasse': 'Deichmann-Str. 1',
      'kundennr': '13041/002', 'mobil': '', 'gebiet': '', 'aenderung': datetime.date(2004, 12, 16), 'mail': '',
      'adressdatei_id': '', 'ustid': '', 'distrikt': '', 'fax': '', 'ort': 'Feuchtwangen', 'plz': '91555', 'vertreter': '',
      'sachbearbeiter': '', 'name4': '', 'name2': 'Schuhe GmbH & Co. KG', 'name3': u'Distributionszentrum S\xfcd',
      'name1': 'Heinrich Deichmann', 'kundengruppe': '', 'land': 'DE', 'verband': '', 'iln': u'',
      'unsere_lieferantennr': '', 'mitgliednr': '', 'branche': ''},
     ...]
    """
    # folgende Abfrage wäre toll, funktioniert aber nicht:
    # avrows = husoftm.connection2.get_connection().query(['AVA00'], condition="VAKDNR='%s'" % int(kdnr))
    allrows = husoftm.connection2.get_connection().query(['AVA00'], condition="VASTAT <>'X'")
    avrows = [row for row in allrows if int(row['kundennr']) == int(kdnr)]
    kunden = []
    for row in avrows:
        xarows = husoftm.connection2.get_connection().query(['XXA00'], condition="XASANR='%s'" %
                                                            int(row['satznr']))
        if xarows:
            assert(len(xarows) == 1)
            kunde = Kunde().fill_from_softm(xarows[0])
            kunde.kundennr = kunde.kundennr + ('/%03d' % int(row['versandadresssnr']))
            kunden.append(kunde)
    return kunden


@caching.cache_function(60*60*2)
def get_kundenbetreuer(kundennr):
    """'Liefert einen String, der den Betreuer im Hause für einen bestimmten Kunden identifizert oder ''."""
    rows = husoftm.connection2.get_connection().query(['AKZ00'], fields=['KZINFO'],
           condition="KZKDNR LIKE '%s'" % ('%' + str(kundennr)))
    if rows:
        return rows[0][0]
    return ''
    

def _selftest():
    """Test basic functionality"""
    get_kundenbetreuer('17200')
    get_kunde_by_iln('4306544031019')
    get_kunde_by_iln('4306544000008')
    get_changed_after(datetime.date(2007, 1, 1))
    get_kundennummern()
    #for kdnnr in nummern:
    #    get_kunde(kdnnr=kdnnr) # ['aendertung_date']

if __name__ == '__main__':
    _selftest()
