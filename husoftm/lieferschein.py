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

logging.basicConfig(level=logging.WARN)
log = logging.getLogger('husoftm.lieferschein')

# TODO: check for compliance with https://cybernetics.hudora.biz/intern/wordpress/2007/10/antville-16236/


class Adresse(object):
    """Repräsentiert eine Kundenadresse"""
    # Sollte dem Adressprotocoll folgen - muss aber ncoh überprüft werden.
    
    def __repr__(self):
        return repr(vars(self))
    

class Lieferschein(object):
    """Representiert einen SoftM Lieferschein."""
    
    def __init__(self, lsnr=None):
        self._read_from_softm(lsnr)
    
    def _read_base_row_from_softm(self, lsnr):
        """Leist einen satz aus der ALK00."""
        
        rows = get_connection().query('ALK00', condition="LKLFSN = %d" % (int(lsnr), ))
        if len(rows) != 1:
            raise IndexError("Probleme bei der Auswahl des Lieferscheins - kein Datensatz mit LKLFSN"
                             + " = %d | %r" % (int(lsnr), rows))
        return rows
    
    def _read_from_softm(self, lsnr):
        """Basierend auf der ALK00 wird ein Datensatz aus allen verwandten Tabellen extraiert."""
        rows = self._read_base_row_from_softm(lsnr)
        row = rows[0]
        for key, value in row.items():
            if key.islower(): # uppercase == SoftM fild names, lowercaqse = plain-text field names
                setattr(self, key, value)
        
        pos_key = self.satznummer
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
            self.positionen.append(position)
        
        # Lieferadresse lesen
        rows = get_connection().query('XKD00', condition="KDKDNR LIKE '%s'" % 
                               ('%' + unicode(int(self.warenempfaenger)), ))
        if len(rows) != 1:
            raise RuntimeError("Probleme bei der Auswahl der Lieferadresse")
        self.lieferadresse = Adresse()
        row = rows[0]
        for key, value in row.items():
            if key.islower(): # uppercase == SoftM fild names, lowercaqse = plain-text field names
                setattr(self.lieferadresse, key, value)
        
        # Wenn eine gesonderte Lieferadresse angegeben ist, self.lieferadresse damit überschreiben
        rows = get_connection().query('XAD00', condition="ADAART=1 AND ADRGNR='%d' " % int(self.auftragsnr))
        if len(rows) > 1:
            raise RuntimeError("Probleme bei der Auswahl der Lieferadresse")
        elif len(rows) == 1:
            row = rows[0]
            for key, value in row.items():
                if key.islower(): # uppercase == SoftM fild names, lowercaqse = plain-text field names
                    setattr(self.lieferadresse, key, value)
        
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
        
        # Read customer's orderno
        rows = get_connection().query('AAK00', condition="AKAUFN='%d'" % (int(self.auftragsnr), ))
        if len(rows) != 1:
            raise RuntimeError("Problems on finding the customer's orderno")
        self.auftragsnummer_kunde = rows[0]["auftragsnummer_kunde"]
        
        self.art = rows[0]['art']
        if rows[0]['fixtermin']:
            self.fixtermin = True
        else:
            self.fixtermin = False
    
    def check_integrity(self):
        """Dient der Erkundung der SoftM Datenstrukturen."""
        
        if self.bezogener_kopf:
            print "Bezogener Kopf", self.bezogener_kopf
        if self.alle_positionen_fakturiert:
            print "alle_positionen_fakturiert", self.alle_positionen_fakturiert
        if self.teilfakturiert:
            print "teilfakturiert", self.teilfakturiert
        if self.dateifuehrungsschluessel:
            print "dateifuehrungsschluessel", self.dateifuehrungsschluessel
        if self.satzstatus:
            print "satzstatus", self.satzstatus
        for position in self.positionen:
            if position.auftragsnr != self.auftragsnr:
                log.error(('Auftragsnummer zwischen Lieferschein (%r/%r) und Lieferscheinposition (%r/%r)'
                           + ' unterschiedlich') % (self, self.auftragsnr, position, position.auftragsnr))
            if position.lieferscheinnr != self.lieferscheinnr:
                log.error(('Lieferscheinnr zwischen Lieferschein (%r/%r) und Lieferscheinposition (%r/%r)'
                           + ' unterschiedlich') % (self, self.lieferscheinnr, position,
                                                    position.lieferscheinnr))
            if position.kommissionierbelegnr != self.kommissionierbelegnr:
                log.error(('Kommissionierbelegnr zwischen Lieferschein (%r/%r) und Lieferscheinposition'
                           + ' (%r/%r) unterschiedlich') % (self, self.kommissionierbelegnr, position,
                                                            position.kommissionierbelegnr))
            if ((position.lieferscheinstorno and not position.storno_date) or
                (position.storno_date and not position.lieferscheinstorno)):
                log.error('Stornoinformationen inkonsistent: %r|%r|%r' % (position, 
                                                                          position.lieferscheinstorno,
                                                                          position.storno_date))
            
            if position.voll_ausgeliefert:
                print position, "voll_ausgeliefert", position.hrl_status
            if position.setartikel:
                print position, "setartikel", position.setartikel
            if unicode(position.zuteilungskennzeichen) != '1':
                print position, "zuteilungskennzeichen", position.zuteilungskennzeichen
            if unicode(position.lieferschein_ausloesung) != '0':
                print position, "lieferschein_ausloesung", position.lieferschein_ausloesung
            # if position.rueckstand_erlaubt:
            #     print position, "rueckstand_erlaubt", position.rueckstand_erlaubt
            # if position.rechnungsstatus:
            #     print position, "rechnungsstatus", position.rechnungsstatus
    
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
    

class Komissionierbeleg(Lieferschein):
    """Bildet einen Kommissionierbeleg ab."""
    
    def _read_base_row_from_softm(self, kbnr):
        """Liest einen Komissionierbeleg aus der AAK00."""
        
        rows = get_connection().query('ALK00', condition="LKKBNR = %d AND LKSANB= 0" % (int(kbnr), ))
        
        if len(rows) != 1:
            raise IndexError(("Probleme bei der Auswahl des Lieferscheins - kein Datensatz mit LKKBNR"
                             + " = %d | %r") % (int(kbnr), rows))
        return rows
