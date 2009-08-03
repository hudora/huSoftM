#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. teil von pySoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

__revision__ = "$Revision$"

import logging
from husoftm.connection import get_connection
from husoftm.tools import land2iso

logging.basicConfig(level=logging.WARN)
log = logging.getLogger('husoftm.lieferschein')


def kbpos2artnr(komminr, posnr):
    """Gibt die Artikelnummer zu einer bestimmten Position eines Kommissionierbelegs zurück."""
    rows = get_connection().query('ALN00', fields=['LNARTN'], 
               condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
    return rows[0][0]
    

def _read_base_row_from_softm_lieferschein(lsnr):
    """Liest einen satz aus der ALK00."""
    
    rows = get_connection().query('ALK00', condition="LKLFSN = %d" % (int(lsnr), ))
    if len(rows) != 1:
        raise IndexError("Probleme bei der Auswahl des Lieferscheins - kein Datensatz mit LKLFSN"
                         + " = %d | %r" % (int(lsnr), rows))
    return rows
    

class Adresse(object):
    """Repräsentiert eine Kundenadresse"""
    # Sollte dem Adressprotokoll folgen - muss aber noch überprüft werden.
    
    def __repr__(self):
        return repr(vars(self))
    

class Lieferschein(object):
    """Repräsentiert einen SoftM Lieferschein. Folgt dem Lieferung Protokol.
    
    Siehe https://cybernetics.hudora.biz/projects/wiki/LieferungProtocol
    
    >>> Lieferschein(4034544)
    <Lieferschein object>
    """
    
    def __init__(self, lsnr=None):
        self._read_from_softm(lsnr)
    
    def _read_from_softm(self, lsnr):
        """Basierend auf der ALK00 wird ein Datensatz aus allen verwandten Tabellen extraiert."""
        rows = _read_base_row_from_softm_lieferschein(lsnr)
        row = rows[0]
        for key, value in row.items():
            if key.islower(): # uppercase == SoftM field names, lowercaqse = plain-text field names
                setattr(self, key, value)
        
        pos_key = self.satznr
        if self.bezogener_kopf:
            pos_key = self.bezogener_kopf
        # positionen lesen
        self.positionen = []
        rows = get_connection().query('ALN00', condition="LNSANK = %d" % (int(pos_key), ))
        
        for row in rows:
            position = Lieferscheinposition()
            for key, value in row.items():
                if key.islower(): # uppercase == SoftM field names, lowercaqse = plain-text field names
                    setattr(position, key, value)
            position.menge = position.menge
            position.artnr = position.artnr
            self.positionen.append(position)
        
        self._get_lieferadresse()
        self._get_abweichendelieferadresse()
        self._get_texte()
        
        # nach dem https://cybernetics.hudora.biz/projects/wiki/AddressProtocol Felder setzen
        self.name1 = self.lieferadresse.name1
        self.name2 = self.lieferadresse.name2
        self.name3 = self.lieferadresse.name3
        self.strasse = self.lieferadresse.strasse
        self.land = land2iso(self.lieferadresse.laenderkennzeichen)
        self.plz = self.lieferadresse.plz
        self.ort = self.lieferadresse.ort
        
        
        # Read auftragsnr_kunde
        rows = get_connection().query('AAK00', condition="AKAUFN='%d'" % (int(self.auftragsnr), ))
        if len(rows) != 1:
            raise RuntimeError("Problems on finding auftragsnr_kunde")
        self.auftragsnr_kunde = rows[0]["auftragsnr_kunde"]
        
        self.anlieferdatum = rows[0]['liefer_date']
        self.anlieferdatum_min = self.anlieferdatum_max = self.anlieferdatum
        if rows[0]['kundenwunsch_date']:
            self.anlieferdatum_max = rows[0]['kundenwunsch_date']
        
        self.art = rows[0]['art']
        if rows[0]['fixtermin']:
            self.fixtermin = True
        else:
            self.fixtermin = False

    def _get_texte(self):
        """Texte zu einem Lieferschein extraieren"""
        anfangstext = []
        endetext = []
        rows = get_connection().query('AAT00', ordering='ATLFNR',
                          condition="ATAUPO=0 AND ATKZLF=1 AND ATAUFN='%d'" % (int(self.auftragsnr), ))
        for row in rows:
            if int(row['textart']) in [7, 8]:
                anfangstext.append(row['text'])
            elif int(row['textart']) in [9]:
                endetext.append(row['text'])
        self.anfangstext = '\n'.join(anfangstext)
        self.endetext = '\n'.join(endetext)
        self.infotext_kunde = '\n'.join(anfangstext + endetext)
        return rows
    
    def _get_lieferadresse(self):
        """'Normale' Lieferadresse zu einem Lieferschein extraieren."""
        # Lieferadresse lesen
        rows = get_connection().query('XKD00', condition="KDKDNR LIKE '%s'" % 
                               ('%' + unicode(int(self.warenempfaenger)), ))
        if len(rows) != 1:
            raise RuntimeError("Probleme bei der Auswahl der Lieferadresse")
        self.lieferadresse = Adresse()
        row = rows[0]
        # bsp fuer kundennr 83000:
        # row = {'adressdatei_id': 123665288,
        # 'aenderung_date': datetime.date(2008, 10, 27),
        # 'erfassung_date': datetime.date(2004, 12, 1),
        # 'fax': u'+49 39954 360229',
        # 'kundengruppe_id': u'20',
        # 'kundennr': u'83000',
        # 'laenderkennzeichen': u'D',
        # 'mail': u'cs@nettosupermarkt.de',
        # 'mobil': u'',
        # 'name1': u'NETTO Supermarkt GmbH & Co. OHG',
        # 'name2': u'',
        # 'name3': u'',
        # 'name4': u'',
        # 'ort': u'Stavenhagen',
        # 'plz': u'17153',
        # 'postfach': u'',
        # 'postfach_plz': u'',
        # 'sortierfeld': u'NETTO  STA',
        # 'strasse': u'Industriegebiet - Preezer Str. 22',
        # 'tel': u'+49 39954 360202',
        # 'url': u''}
        for key, value in row.items():
            if key.islower(): # uppercase == SoftM fild names, lowercaqse = plain-text field names
                setattr(self.lieferadresse, key, value)
    
    def _get_abweichendelieferadresse(self):
        """Abweichende Lieferadresse wenn vorhanden extraieren."""
        # Wenn eine gesonderte Lieferadresse angegeben ist, self.lieferadresse damit überschreiben
        rows = get_connection().query('XAD00',
                                      condition="ADAART=1 AND ADRGNR='%d' " % int(self.auftragsnr))
        if len(rows) > 1:
            raise RuntimeError("Probleme bei der Auswahl der Lieferadresse")
        elif len(rows) == 1:
            row = rows[0]
            # bsp fuer auftragsnr 655501:
            # row = {'adressaufbereitung': 0,
            #  'laenderkennzeichen': u'D',
            #  'name1': u'NETTO Supermarkt GmbH & Co. OHG',
            #  'name2': u'',
            #  'name3': u'',
            #  'name4': u'',
            #  'ort': u'Wustermark',
            #  'plz': u'14641',
            #  'strasse': u'Magdeburger Str. 2'}
            for key, value in row.items():
                if key.islower(): # uppercase == SoftM fild names, lowercaqse = plain-text field names
                    setattr(self.lieferadresse, key, value)
            
            # Diese felder nur füllen wen eine gesonderte Lieferadresse angegeben ist.
            self.tel = self.lieferadresse.tel
            self.fax = self.lieferadresse.fax
            self.mobil = self.lieferadresse.mobil
            self.email = self.lieferadresse.mail
    
    def __unicode__(self):
        return u"SL%d, %d Positionen, %r" % (self.lieferscheinnr, len(self.positionen), self.liefer_date)
    

class Lieferscheinposition(object):
    """Bildet eine 'Orderline' in einem Lieferschein ab."""
    
    def __repr__(self):
        ret = "%d/%d/%d/%d x %s" % (self.menge, self.menge_offen, self.menge_komissionierbeleg,
                                    self.menge_fakturierung, self.artnr)
        
        if self.lieferscheinstorno:
            ret += ', STORNIERT'
        return ret
    

class Kommibeleg(Lieferschein):
    """Bildet einen Komissionierbeleg ab (der datentechnisch in SoftM ein Lieferschein ist)."""
    
    def _read_base_row_from_softm(self, kbnr):
        """Liest einen Komissionierbeleg aus der AAK00."""
        
        rows = get_connection().query('ALK00', condition="LKKBNR = %d AND LKSANB= 0" % (int(kbnr), ))
        
        if len(rows) != 1:
            raise IndexError(("Probleme bei der Auswahl des Lieferscheins - kein Datensatz mit LKKBNR"
                             + " = %d | %r") % (int(kbnr), rows))
        return rows
    

def _test():
    (vars(Lieferschein(4034544)))
    kbpos2artnr(3023551, 1)
    
if __name__ == '__main__':
    _test()
