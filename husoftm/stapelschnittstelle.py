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
from husoftm.connection import get_connection
from husoftm.tools import date2softm, sql_quote, iso2land

from husoftm.stapleschnittstelle_const import ABK00, ABA00, ABT00, ABV00, ABK00


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
    return rows[0][0]+1


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
    if aobj_position.kundenartnr:
        text = Text()
        texte.append(text)
        text.vorgang = vorgangsnummer
        text.vorgangsposition = position.vorgangsposition
        text.textart = 8
        text.auf_lieferschein = 1
        text.auf_rechnung = 1
        text.text = "Kundenartikelnummer: %s" % aobj_position.kundenartnr
    

def _create_addressatz(adressen, vorgangsnummer, adresse, is_lieferadresse=True):
    """Fügt einen Zusatz-Addressatz hinzu."""
    adresse = Adresse()
    if is_lieferadresse:
        adresse.adressart = 1
    adresse.vorgang = vorgangsnummer
    adresse.name1 = getattr(adresse, 'name1', '')
    adresse.name2 = getattr(adresse, 'name2', '')
    adresse.name3 = getattr(adresse, 'name3', '')
    adresse.strasse = getattr(adresse, 'strasse', '')
    adresse.plz = getattr(adresse, 'plz', '')
    adresse.ort = getattr(adresse, 'ort', '')
    adresse.laenderkennzeichen = iso2land(getattr(adresse, 'ort', 'DE'))
    adressen.append(adresse)
    

def auftrag2softm(auftrag, belegtexte=[]):
    """Schreibt etwas, dass dem AuftragsFormat entspricht in SoftM."""
    
    # this should be done by the caller:
    # if kundenauftragsnummer_bekannt(auftragskennzeichen):
    #     print "Auftragskennzeichen %r wurde bereits verwendet." % auftragskennzeichen
    #     return None
    # if not schnittstelle_leer():
    #     print "In der Stapelschnittstelle befinden sich nicht verarbeitete Nachrichten."
    #    return None
    
    vorgangsnummer = getnextvorgang()
    kopf = Kopf()
    kopf.vorgang = vorgangsnummer
    kopf.kundenauftragsnr = auftrag.kundenauftragsnr
    kopf.kundennr = '%8s' % auftrag.kundennr.split('/')[0]
    if len(auftrag.kundennr.split('/')) > 1:
        # abweichende Lieferadresse in address-zusatzdatei
        kopf.lieferadresse = int(auftrag.kundennr.split('/')[1])
    
    kopf.bestelldatum = auftrag.bestelldatum
    
    kopf.auftragsart = ''
    kopf.sachbearbeiter = 1
    kopf.liefertermin = date2softm(auftrag.anlieferdatum_min)
    kopf.kundenwunschtermin = date2softm(auftrag.anlieferdatum_max)
    
    positionen = []
    texte = []
    
    _create_kopftext(texte, vorgangsnummer, auftrag.infotext_kunde, auftragsbestaetigung=1, lieferschein=1,
        rechnung=1)
    _create_kopftext(texte, vorgangsnummer, auftrag.bestelltext, auftragsbestaetigung=1, lieferschein=0,
        rechnung=0)
    
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
    
    # see http://blogs.23.nu/c0re/stories/18926/ for the aproach we try here to write data into SoftM.
    # But instead of using the date for our token we use the DFSL field
    uuid = hex((int(time.time() * 10000) 
                ^ (os.getpid() << 24) 
                ^ thread.get_ident()) 
                % 0xFFFFFFFFFF).rstrip('L')[2:]
    
    kopf.dateifuehrungsschluessel = uuid
    get_connection().server.update_adtastapel(vorgangsnummer, token='Ae.so=7e,S(')
    get_connection().insert_raw(kopf.to_sql(), token='Aes.o=j7eS(')
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
            get_connection().insert_raw(command, token='Aes.o=j7eS(')
        # remove "dateifuehrungsschluessel" and set recort active
        get_connection().update("UPDATE ABK00 SET BKKZBA=0, BKDFSL='' WHERE BKVGNR=%s" 
                                % vorgangsnummer, token='E~iy3*eej^')
    
    return vorgangsnummer
