#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. teil von pySoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

__revision__ = "$Revision$"

from husoftm.connection2 import get_connection
from husoftm.tools import sql_escape, sql_quote, land2iso, set_attributes
import cs.caching
import logging

logging.basicConfig(level=logging.WARN)
log = logging.getLogger('husoftm.lieferschein')


def get_lieferscheinnrs_for_lager(lager):
    "Liefert eine Liste mit allen nicht voll ausgelieferten Lieferscheinnummern für ein Lager."""
    rows = get_connection().query("ALK00", fields=["LKLFSN"],
                                  condition="LKLGNR=%r AND LKLFSN>0 AND LKKZVA=0 AND LKSTAT <> 'X'" % lager)
    return sorted(set((int(row[0]) for row in rows)))


def get_lieferscheine_rechnungsstatus(lieferscheinnrs):
    """Liefert den minimalen Rechnungsstatus aller Positionen einer Liste von Lieferscheinen zurück.
    
    lieferscheinnrs is expected to be a list.

    Returns a list containing tuples w/ lieferscheinnr and its rechnungsstatus
    """

    mappings = {'LKLFSN': 'lieferscheinnummer',
                'MIN(LNRGST)': 'rechnungsstatus'}
    
    if lieferscheinnrs:
        condition="LKSANK=LNSANK AND (LKLFSN IN (%s))" % ", ".join(str(lsnr) for lsnr in lieferscheinnrs)
        rows = get_connection().query(["ALK00", "ALN00"], fields=mappings.keys(),
                                      condition=condition,
                                      grouping=["LKLFSN"],
                                      querymappings=mappings)
        return [(int(row['lieferscheinnummer']), int(row['rechnungsstatus'])) for row in rows]
    return []


def lieferscheine_for_auftrag(auftragsnr):
    """Return all Lieferschein objects for a given auftragsnr"""
    
    conditions = ["LKAUFS = %s" % sql_quote(auftragsnr), "LKLFSN <> 0"]
    condition = " AND ".join(conditions)
    rows = get_connection().query(["ALK00"], condition=condition)
    return [Lieferschein(row['lieferscheinnr']) for row in rows]


def kommibelege_for_auftrag(auftragsnr):
    """Return all Kommibeleg objects for a given auftragsnr"""
    
    conditions = ["LKAUFS = %s" % sql_quote(auftragsnr), "LKLFSN = 0"]
    condition = " AND ".join(conditions)
    rows = get_connection().query(["ALK00"], condition=condition)
    return [Kommibeleg(row['kommissionierbelegnr']) for row in rows]


@cs.caching.cache_function(60*60*72) # 3 days
def kbpos2artnr(komminr, posnr):
    """Gibt die Artikelnummer zu einer bestimmten Position eines Kommissionierbelegs zurück."""
    rows = get_connection().query('ALN00', fields=['LNARTN'],
               condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
    return rows[0][0]
    

#@cs.caching.cache_function(60*60*24) # 1 day
def kbpos2artnr_lager(komminr, posnr):
    """Gibt die Artikelnummer und das Abgangs-Lager zu einer bestimmten Position eines Kommissionierbelegs zurück."""
    rows = get_connection().query('ALN00', fields=['LNARTN', 'LNLGNR'],
               condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
    return rows[0]['artnr'], rows[0]['lager']


#@cs.caching.cache_function(60*60*24) # 1 day
def kbpos2artnr_zugangslager(komminr, posnr):
    """Gibt die Artikelnummer und das ZUGANGS-Lager zu einer bestimmten Position eines Kommissionierbelegs zurück."""
    # Read auftragsnr_kunde
    rows = get_connection().query('ALN00', fields=['LNARTN', 'LNAUFN'],
               condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
    artnr, auftragsnr = rows[0]['artnr'], rows[0]['auftragsnr']
    rows = get_connection().query('AAK00', condition="AKAUFN='%d'" % int(auftragsnr))
    return artnr, rows[0]["zugangslager"]


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

    condition = "LKLFSN = %d"
    
    def __init__(self, lsnr=None):
        self._read_from_softm(int(lsnr))

    def _read_from_softm(self, lsnr):
        """Basierend auf der ALK00 wird ein Datensatz aus allen verwandten Tabellen extrahiert."""
        
        # Lieferscheinkopf JOIN Kundenadresse JOIN Auftragskopf um die Anzahl der Queries zu minimieren
        conditions = [self.condition % lsnr, "LKKDNR = KDKDNR", "LKAUFS = AKAUFN"]        
        rows = get_connection().query(['ALK00', 'AAK00', 'XKD00'], condition=" AND ".join(conditions))
        if len(rows) != 1:
            raise RuntimeError("Probleme bei der Auswahl des Lieferscheins - kein Datensatz mit %s" %
                                (self.condition % lsnr))
        set_attributes(rows[0], self)
        
        self.anlieferdatum = self.liefer_date
        self.anlieferdatum_min = self.anlieferdatum_max = self.anlieferdatum

        if self.kundenwunsch_date:
            self.anlieferdatum_max = self.kundenwunsch_date
        
        self.fixtermin = bool(self.fixtermin)
        
        self.pos_key = self.satznr
        if self.bezogener_kopf:
            self.pos_key = self.bezogener_kopf
        
        # Property für delayed execution
        self._positionen = None
        self._infotext_kunde = None
        
        self.lieferadresse = Adresse()
        self._get_abweichendelieferadresse()

        # nach dem https://cybernetics.hudora.biz/projects/wiki/AddressProtocol Felder setzen
        self.lieferadresse.name1 = self.name1
        self.lieferadresse.name2 = self.name2
        self.lieferadresse.name3 = self.name3
        self.lieferadresse.strasse = self.strasse
        self.lieferadresse.land = self.land = land2iso(self.laenderkennzeichen)
        self.lieferadresse.plz = self.plz
        self.lieferadresse.ort = self.ort

    @property
    def positionen(self):
        """Liste der Lieferscheinpositionen"""
        # TODO: JOIN mit Texten
        if self._positionen is None:
            
            self._positionen = []
            rows = get_connection().query('ALN00', condition="LNSANK = %d" % int(self.pos_key))
            for row in rows:
                position = Lieferscheinposition()
                set_attributes(row, position)
                position.anfangstext, position.endetext = self._get_pos_texte(position.auftrags_position)
                self._positionen.append(position)
                
        return self._positionen

    def _get_pos_texte(self, pos):
        """Positionsanfangs- und Endetexte als string zurückgeben."""
        anfangstext = []
        endetext = []
        rows = get_connection().query('AAT00', ordering='ATLFNR',
                                      condition="ATAUPO=%d AND ATKZLF=1 AND ATAUFN='%d'"
                                      % (pos, int(self.auftragsnr)))
        for row in rows:
            text = row['text']
            textnr = row['nr']
            textart = int(row['textart'])
            if textart in [7, 8]:
                anfangstext.append((textnr, text))
            elif textart in [9]:
                endetext.append((textnr, text))
        # sortieren, damit mehrzeilige Texte zusammen hängen
        anfangstext.sort()
        endetext.sort()
        anfangstext = '\n'.join([entry[1] for entry in anfangstext])
        endetext = '\n'.join([entry[1] for entry in endetext])
        return anfangstext, endetext

    @property
    def infotext_kunde(self):
        """Texte zu einem Lieferschein"""
        if self._infotext_kunde is None:        
            self.anfangstext, self.endetext = self._get_pos_texte(pos=0)
            self._infotext_kunde = '\n'.join([self.anfangstext, self.endetext]).strip()
        return self._infotext_kunde
    
    def _get_abweichendelieferadresse(self):
        """Abweichende Lieferadresse wenn vorhanden extrahieren."""
        # Wenn eine gesonderte Lieferadresse angegeben ist, Adresse damit überschreiben
        rows = get_connection().query('XAD00', condition="ADAART=1 AND ADRGNR='%d' " % int(self.auftragsnr))
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
            set_attributes(row, self)
    
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

    condition = "LKKBNR = %d AND LKSANB = 0"


def _test():
    Kommibeleg(3023551)
    (vars(Lieferschein(4034544)))
    kbpos2artnr(3023551, 1)
    
if __name__ == '__main__':
    _test()
