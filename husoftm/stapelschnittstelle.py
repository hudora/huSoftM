#!/usr/bin/env python
# encoding: utf-8
"""
stapelschnittstelle.py - Ansprechen der SoftM Auftrags-Stapelschnittstelle.

ACHTUNG: Die SoftM Stapel-Schnittstelle ist - dürftig dokumentiert. Das
bedeutet, dass dieser Code grosse Chancen bietet, nicht wie erwartet zu funktionieren. Er ist nur für
experimentelle Zwecke geeignet - oder für Leute mit starken Nerven.
Aber wenn sie schwache Nerven hätten, würden sie kein SoftM einsetzen, oder?

Die SoftM Stapelschnittstelle auch von SoftM umfangreich genutzt: so ist die EDI-Schnittstelle eigentlich nur
ein Umsetzer, der die Daten in die Stapelschnittstelle schreibt. Mit der Stapelschnittstelle kann man nahezu
alle Prameter eines Auftrages beschreiben.

Created by Maximillian Dornseif on 2007-05-16.
Copyright (c) 2007, 2008 HUDORA GmbH. All rights reserved.
"""

import datetime
import time
import os
import thread
import random
import textwrap
import unittest
from husoftm.connection import get_connection
from husoftm.tools import date2softm, sql_quote, iso2land
from husoftm.stapleschnittstelle_const import ABK00, ABA00, ABT00, ABV00, ABK00

__revision__ = "$Revision$"


class StapelSerializer(object):
    """Abstrakte Klasse zum serialisieren der verschiedenen Sätze der Stapelschnittstelle."""
    
    # to be implemented by subclass
    rowmapping = {}
    tablename = None
    
    def __init__(self):
        # pull in defaults
        for dummy, rowconf in self.rowmapping.items():
            if rowconf.get('default', None) != None:
                setattr(self, rowconf.get('name'), rowconf.get('default'))
    
    def to_sql(self):
        """Generate SQL command for this Object"""
        felddict = {}
        # pull in required rows
        for rowid, rowconf in self.rowmapping.items():
            if rowconf.get('required', False) == True:
                felddict[rowid] = getattr(self, rowconf['name'])
            elif hasattr(self, rowconf['name']):
                felddict[rowid] = getattr(self, rowconf['name'])
            # convert datetime
            if rowid in felddict and hasattr(felddict[rowid], 'strftime'):
                felddict[rowid] = date2softm(felddict[rowid])
            
            # remove empty fields
            if rowid in felddict and not felddict[rowid]:
                del felddict[rowid]
            
        sql = "INSERT INTO %s (%s) VALUES(%s)" % (self.tablename,
                                                 ', '.join(felddict.keys()), 
                                                 ','.join([sql_quote(x) for x in felddict.values()]))
        return sql
    

class Kopf(StapelSerializer):
    """Repräsentiert einen Auftragsstapelkopfsatz in ABK00."""
    rowmapping = ABK00
    tablename = 'ABK00'
    

class Position(StapelSerializer):
    """Repräsentiert einen Auftragsstapelpositionssatz in ABA00."""
    rowmapping = ABA00
    tablename = 'ABA00'
    

class Text(StapelSerializer):
    """Repräsentiert einen Auftragsstapeltextsatz in ABT00."""
    rowmapping = ABT00
    tablename = 'ABT00'
    

class Adresse(StapelSerializer):
    """Repräsentiert einen Addresszusatz in ABV00."""
    rowmapping = ABV00
    tablename = 'ABV00'
    

def getnextvorgang():
    """Ermittelt die nächste freie Vorgangsnummer."""
    
    rows = get_connection().query('ABK00', fields=['MAX(BKVGNR)'])
    return int(rows[0][0]+1)
    

def vorgangsnummer_bekannt(vorgangsnummer):
    """Prüft, ob sich eine bestimmte Vorgangsnummer bereits im System befindet."""
    rows = get_connection().query('ABK00', fields=['BKVGNR'],
                                  condition="BKVGNR=%s" % sql_quote(vorgangsnummer))
    if rows:
        return True
    return False
    

def kundenauftragsnummer_bekannt(kundenauftragsnummer):
    """Prüft, ob eine Kunden-Auftragsnummer bereits verwendet wurde."""
    
    rows = get_connection().query('ABK00', fields=['BKVGNR'],
                                          condition="BKNRKD=%s" % sql_quote(kundenauftragsnummer))
    if rows:
        return True
    return False
    

def schnittstelle_leer():
    """Ermittelt, ob sich ungeprüfte Vorgänge in der stapelschnittstelle befinden."""
    
    rows = get_connection().query('ABK00', fields=['BKVGNR'],
                                          condition="BKAUFN = 0 AND BKKZBA = 0")
    if rows:
        return False
    return True
    

def _create_kopftext(texte, vorgangsnummer, newtext, auftragsbestaetigung=1, lieferschein=1, rechnung=1):
    """Fügt einen Kopftext hinzu."""
    
    # split text into chunks of 60 chars
    for line in textwrap.wrap(newtext, 60):
        text = Text()
        texte.append(text)
        text.vorgang = vorgangsnummer
        text.vorgangsposition = 0
        text.textart = 8
        text.auf_auftragsbestaetigung = auftragsbestaetigung
        text.auf_lieferschein = lieferschein
        text.auf_rechnung = rechnung
        text.text = line
        text.textnummer = len(texte)
    

def _create_positionssatz(positionen, vorgangsnummer, aobj_position, texte):
    """Fügt einen Positionssatz ("orderline") hinzu."""
    position = Position()
    positionen.append(position)
    position.position = len(positionen)
    position.vorgang = vorgangsnummer
    position.vorgangsposition = len(positionen)
    position.bestellmenge = aobj_position.menge
    position.artikel = aobj_position.artnr
    position.erfassungsdatum = date2softm(datetime.date.today())
    if hasattr(aobj_position, 'kundenartnr') and aobj_position.kundenartnr:
        text = Text()
        texte.append(text)
        text.vorgang = vorgangsnummer
        text.vorgangsposition = position.vorgangsposition
        text.textart = 8
        text.auf_lieferschein = 1
        text.auf_rechnung = 1
        text.text = "Kundenartikelnummer: %s" % aobj_position.kundenartnr
    

def _create_addressatz(adressen, vorgangsnummer, aobj_adresse, is_lieferadresse=True):
    """Fügt einen Zusatz-Addressatz hinzu."""
    adresse = Adresse()
    if is_lieferadresse:
        adresse.adressart = 1
    adresse.vorgang = vorgangsnummer
    adresse.name1 = getattr(aobj_adresse, 'name1', '')
    adresse.name2 = getattr(aobj_adresse, 'name2', '')
    adresse.name3 = getattr(aobj_adresse, 'name3', '')
    adresse.strasse = getattr(adresse, 'strasse', '')
    adresse.plz = getattr(aobj_adresse, 'plz', '')
    adresse.ort = getattr(aobj_adresse, 'ort', '')
    adresse.laenderkennzeichen = iso2land(getattr(aobj_adresse, 'land', 'DE'))
    adressen.append(adresse)
    

def _auftrag2records(vorgangsnummer, auftrag):
    """Convert a auftrag into records objects representing AS/400 SQL statements."""
    kopf = Kopf()
    kopf.vorgang = vorgangsnummer
    kopf.kundennr = '%8s' % auftrag.kundennr.split('/')[0]
    if len(auftrag.kundennr.split('/')) > 1:
        # abweichende Lieferadresse in address-zusatzdatei
        kopf.lieferadresse = int(auftrag.kundennr.split('/')[1])
    if hasattr(auftrag, 'kundenauftragsnr'):
        kopf.kundenauftragsnr = auftrag.kundenauftragsnr
    
    if hasattr(auftrag, 'bestelldatum') and auftrag.bestelldatum:
        kopf.bestelldatum = date2softm(auftrag.bestelldatum)
    else:
        kopf.bestelldatum = date2softm(datetime.date.today())
    
    kopf.auftragsart = ''
    kopf.sachbearbeiter = 1
    if auftrag.anlieferdatum_max:
        kopf.kundenwunschtermin = date2softm(auftrag.anlieferdatum_max)
    else:
        kopf.kundenwunschtermin = ''
    
    if hasattr(auftrag, 'anlieferdatum_min') and auftrag.anlieferdatum_min:
        kopf.liefertermin = date2softm(auftrag.anlieferdatum_min)
    else:
        kopf.liefertermin = date2softm(datetime.date.today())
    
    positionen = []
    texte = []
    
    if hasattr(auftrag, 'infotext_kunde'):
        _create_kopftext(texte, vorgangsnummer, auftrag.infotext_kunde, auftragsbestaetigung=1,
                         lieferschein=1, rechnung=1)
    if hasattr(auftrag, 'bestelltext'):
        _create_kopftext(texte, vorgangsnummer, auftrag.bestelltext, auftragsbestaetigung=1,
                         lieferschein=0, rechnung=0)
    
    adressen = []
    # add Lieferadresse if needed
    if (hasattr(auftrag, 'lieferadresse')
        and hasattr(auftrag.lieferadresse, 'name1')
        and hasattr(auftrag.lieferadresse, 'ort')
        and hasattr(auftrag.lieferadresse, 'land')):
        _create_addressatz(adressen, vorgangsnummer, auftrag.lieferadresse)
    
    for aobj_position in auftrag.positionen:
        _create_positionssatz(positionen, vorgangsnummer, aobj_position, texte)
    kopf.vorgangspositionszahl = len(positionen)
    return kopf, positionen, texte, adressen
    

def auftrag2softm(auftrag, belegtexte=[]):
    """Schreibt etwas, dass dem AuftragsFormat entspricht in SoftM."""
    
    # this should be done by the caller:
    # if kundenauftragsnummer_bekannt(auftragskennzeichen):
    #     print "Auftragskennzeichen %r wurde bereits verwendet." % auftragskennzeichen
    #     return None
    # if not schnittstelle_leer():
    #     print "In der Stapelschnittstelle befinden sich nicht verarbeitete Nachrichten."
    #    return None
    
    
    while True:
        # SoftM (Mister Hering) suggested updating Adtastapel as soon as possible to avoid 
        # dupes - missing IDs would be no problem.
        vorgangsnummer = getnextvorgang()
        get_connection().update_adtastapel(vorgangsnummer)
        
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        
        # see http://blogs.23.nu/c0re/stories/18926/ for the aproach we try here to write data into SoftM.
        # But instead of using the date for our token we use the DFSL field
        uuid = hex((int(time.time() * 10000) 
                    ^ (os.getpid() << 24) 
                    ^ thread.get_ident()) 
                    % 0xFFFFFFFFFF).rstrip('L')[2:]
        
        kopf.dateifuehrungsschluessel = uuid
        
        # Do something like "Retransmission Back-Off" on Ethernet for collision avoidance:
        # sleep for a random amount of time
        time.sleep(random.random() / 10.0)
        
        #check im somobdy else has been writing to the DB.
        if not vorgangsnummer_bekannt(vorgangsnummer):
            # no, so we can proceed
            break
        
        # else: retry while loop with a new vorgangsnummer
        
    
    # start writing data into the database
    get_connection().insert_raw(kopf.to_sql())
    rowcount = get_connection().query('ABK00', fields=['COUNT(*)'],
                                   condition="BKDFSL=%s" % sql_quote(uuid))[0][0]
    if rowcount < 1:
        raise RuntimeError("Internal Server error: insertation into ABK00 failed: uuid=%r" % uuid)
    elif rowcount > 1:
        # the race condition has hit - remove our entry and retry
        get_connection().delete('ABK00', 'BKDFSL=%s' % sql_quote(uuid))
        # sleep to avoid deadlocks see http://de.wikipedia.org/wiki/CSMA/CD#Das_Backoff-Verfahren_bei_Ethernet
        time.sleep(random.randint()/100.0)
        # recusively try again
        auftrag2softm(auftrag, belegtexte)
    else:
        sql = []
        for record in positionen + texte + adressen:
            sql.append(record.to_sql())
        sql.append(kopf.to_sql())
        for command in sql:
            print command
            get_connection().insert_raw(command)
        # remove "dateifuehrungsschluessel" and set recort active
        get_connection().update_raw("UPDATE ABK00 SET BKKZBA=0, BKDFSL='' WHERE BKVGNR=%s" 
                                % vorgangsnummer)
    
    return vorgangsnummer
    

class _MockAuftrag(object):
    """Represents an Auftrag."""
    pass
    

class _MockAddress(object):
    """Represents an Address."""
    pass
    

class _MockPosition(object):
    """Represents an orderline."""
    pass
    

class _GenericTests(unittest.TestCase):
    """Vermischte Tests."""
    
    def test_minimal_auftrag(self):
        """Test if the minimal possible Auftrag can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        self.assertEqual(kopf.to_sql(), "INSERT INTO ABK00 (BKABT, BKVGNR, BKDTKW, BKSBNR, BKVGPO, BKFNR, "
            "BKKDNR, BKAUFA) VALUES('1','123','1081230','1','0','01','   17200','')")
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])
        self.assertEqual(adressen, [])
    
    def test_simple_auftrag(self):
        """Test if a Auftrag with all headerfields but without extradata can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_min = datetime.date(2008, 12, 30)
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 31)
        auftrag.bestelldatum = datetime.date(2008, 12, 29)
        auftrag.kundenauftragsnr = '0012345'
        auftrag.infotext_kunde = 'infotext_kunde'
        auftrag.bestelltext = 'bestelltext'
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        self.assertEqual(kopf.to_sql(), "INSERT INTO ABK00 (BKABT, BKVGNR, BKDTKW, BKSBNR, BKVGPO, BKFNR, "
            "BKNRKD, BKKDNR, BKDTKD, BKAUFA) VALUES('1','123','1081231','1','0','01','0012345','   17200',"
            "'2008-12-29','')")
        self.assertEqual(positionen, [])
        self.assertEqual(texte[0].to_sql(), "INSERT INTO ABT00 (BTKZLF, BTVGNR, BTVGPO, BTTART, BTFNR, "
            "BTTX60, BTLFNR, BTKZAB, BTKZRG) VALUES('1','123','0','8','01','infotext_kunde','1','1','1')")
        self.assertEqual(texte[1].to_sql(), "INSERT INTO ABT00 (BTKZLF, BTVGNR, BTVGPO, BTTART, BTFNR, "
            "BTTX60, BTLFNR, BTKZAB, BTKZRG) VALUES('0','123','0','8','01','bestelltext','2','1','0')")
        self.assertEqual(adressen, [])
    
    def test_lieferadresse(self):
        """Tests if a Lieferadresse is successfully converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        auftrag.lieferadresse = _MockAddress()
        auftrag.lieferadresse.name1 = 'name1'
        auftrag.lieferadresse.name2 = 'name2'
        auftrag.lieferadresse.name3 = 'name3'
        auftrag.lieferadresse.ort = 'Rade'
        auftrag.lieferadresse.land = 'DE'
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        self.assertEqual(kopf.to_sql(), "INSERT INTO ABK00 (BKABT, BKVGNR, BKDTKW, BKSBNR, BKVGPO, BKFNR, "
            "BKKDNR, BKAUFA) VALUES('1','123','1081230','1','0','01','   17200','')")
        self.assertEqual(adressen[0].to_sql(), "INSERT INTO ABV00 (BVNAM2, BVNAM3, BVLKZ, BVVGNR, BVSTR, "
            "BVKZAD, BVKZBA, BVNAME, BVPLZ, BVORT, BVAART) VALUES('name2','name3','D','123','','1','',"
            "'name1','','Rade','1')")
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])

    def test_positionen(self):
        """Tests if orderlines coan be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        pos1 = _MockPosition()
        pos1.menge = 10
        pos1.artnr = '11111'
        pos2 = _MockPosition()
        pos2.menge = 20
        pos2.artnr = '22222/09'
        auftrag.positionen = [pos1, pos2]
        kopf, positionen, dummy, dummy = _auftrag2records(vorgangsnummer, auftrag)
        self.assertEqual(kopf.to_sql(), "INSERT INTO ABK00 (BKABT, BKVGNR, BKDTKW, BKSBNR, BKVGPO, BKFNR, "
            "BKKDNR, BKAUFA) VALUES('1','123','1081230','1','2','01','   17200','')")
        self.assertEqual(positionen[0].to_sql(), "INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, "
            "BAARTN, BAVGNR) VALUES('1080922','1','1','01','10','11111','123')")
        self.assertEqual(positionen[1].to_sql(), "INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, "
            "BAARTN, BAVGNR) VALUES('1080922','2','1','01','20','22222/09','123')")


if __name__ == '__main__':
    unittest.main()
