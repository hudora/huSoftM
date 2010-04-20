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
import decimal
import huTools.world
import itertools
import os
import random
import textwrap
import thread
import time
import unittest
from huTools.unicode import deUmlaut
from husoftm.connection import get_connection
from husoftm.stapelschnittstelle_const import ABK00, ABA00, ABT00, ABV00
from husoftm.tools import date2softm, sql_quote, iso2land


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
        for rowid, rowconf in sorted(self.rowmapping.items()):
            name = rowconf['name']
            if rowconf.get('required', False) == True:
                felddict[rowid] = getattr(self, name)
            elif hasattr(self, name):
                felddict[rowid] = getattr(self, name)
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


def loesche_dfsl(vorgangsnr):
    get_connection().update_raw("UPDATE ABK00 SET BKDFSL='' WHERE BKVGNR=%s AND BKAUFN = 0 AND"
                                " BKSTAT <> 'X' AND BKKZBA <> 0" % vorgangsnr)


def loesche_vorgang(vorgangsnr):
    """Setzt einen Vorgang auf Status gelöscht'."""
    get_connection().update_raw("UPDATE ABK00 SET BKSTAT='X' WHERE BKVGNR=%s AND BKAUFN = 0 AND"
                                " BKSTAT <> 'X' AND BKKZBA <> 0" % vorgangsnr)
    get_connection().update_raw("UPDATE ABA00 SET BASTAT='X' WHERE BAVGNR=%s AND BAAUFN = 0 AND"
                                " BASTAT <> 'X' AND BAKZBA <> 0" % vorgangsnr)


def feststeckende_jobs():
    """Returns a list of jobs that got stuck in s17e."""
    rows = get_connection().query('ABK00', fields=['BKKDNR', 'BKVGNR', 'BKNRKD', 'BKKZBA'], condition="BKAUFN = 0 AND BKSTAT <> 'X' AND BKKZBA <> 0")
    fields = ('kundennr', 'vorgangsnr', 'kundenauftragsnr', 'fehlercode')
    return [dict(list(zip(fields, row))) for row in rows]


def vorgangsnummer_bekannt(vorgangsnummer):
    """Prüft, ob sich eine bestimmte Vorgangsnummer bereits im System befindet."""
    rows = get_connection().query('ABK00', fields=['BKVGNR'], condition="BKVGNR=%s"
                                  % sql_quote(vorgangsnummer))
    if rows:
        return True
    return False


def kundenauftragsnummer_bekannt(kundenauftragsnummer):
    """Prüft, ob eine Kunden-Auftragsnummer bereits verwendet wurde."""

    # Achtung: Kundenauftragsnummern muessen nicht eindeutig sein. Bspw. kann KundeA die gleiche
    # Kundenauftragsnummer vergeben wie bereits von KundeB vergeben worden ist.

    rows = get_connection().query('ABK00', fields=['BKVGNR'],
                                          condition="BKNRKD=%s" % sql_quote(deUmlaut(kundenauftragsnummer)))
    if rows:
        return True
    return False


def schnittstelle_leer():
    """Ermittelt, ob sich ungeprüfte Vorgänge in der stapelschnittstelle befinden."""

    rows = get_connection().query('ABK00', fields=['BKVGNR'], condition="BKAUFN = 0 AND BKKZBA = 0")
    if rows:
        return False
    return True


def address_transmitted(vorgangsnr):
    """Prüft, ob zu einem gegebenen Vorgang auch die Adresse übermittelt wurde.

    Dies kann zB. dann nicht erfolgt sein, wenn sich Sonderzeichen in den Adressen befanden.
    >>> address_transmitted(13247)
    True
    >>> address_transmitted(13147)
    False
    """
    rows = get_connection().query('ABV00', condition="BVVGNR=%s" % sql_quote(vorgangsnr))
    return bool(rows)


def get_auftragsnr(vorgangsnr):
    """Gibt die zu einer Vorgangsnummer gehörige Auftragsnummer zurück.

    Wenn es (noch) keine Auftragsnummer zu dieser Vorgangsnummer gibt, wird None zurückgegeben."""
    rows = get_connection().query('ABK00', fields="BKAUFN", condition="BKVGNR=%s" % sql_quote(vorgangsnr))
    if rows:
        return int(rows[0][0])
    return None


def _create_auftragstext(textart, vorgangsposition, texte, vorgangsnummer, text, auftragsbestaetigung,
                         lieferschein, rechnung):
    """Fügt einen Text zu einem Auftrag, entweder als Kopftext oder Positionstext, hinzu.

    Jede Zeile wird ggf. in Zeilen < 60 Zeichen aufgeteilt.
    Das Ergebnis wird in die Liste texte angehangen.
    """
    # split text at newlines
    texts = text.split('\n')
    # replace umlauts
    texts = [deUmlaut(txt) for txt in texts]
    # split text into chunks of 60 chars
    texts = [textwrap.wrap(txt, 60) for txt in texts]
    for line in itertools.chain(*texts):
        txtobj = Text()
        texte.append(txtobj)
        txtobj.vorgang = vorgangsnummer
        txtobj.vorgangsposition = vorgangsposition
        txtobj.textart = textart
        txtobj.auf_auftragsbestaetigung = auftragsbestaetigung
        txtobj.auf_lieferschein = lieferschein
        txtobj.auf_rechnung = rechnung
        txtobj.text = line
        txtobj.textnummer = len(texte)


def _create_kopftext(texte, vorgangsnummer, text, auftragsbestaetigung=1, lieferschein=1, rechnung=1):
    """Fügt einen Kopftext hinzu."""
    _create_auftragstext(8, 0, texte, vorgangsnummer, text, auftragsbestaetigung, lieferschein, rechnung)


def _create_positionstext(textart, vorgangsposition, texte, vorgangsnummer, text, auftragsbestaetigung=0,
    lieferschein=1, rechnung=1):
    """Fügt einen Positionstext hinzu."""
    _create_auftragstext(textart, vorgangsposition, texte, vorgangsnummer, text, auftragsbestaetigung,
        lieferschein, rechnung)


def _create_positionssatz(positionen, vorgangsnummer, aobj_position, texte):
    """Fügt einen Positionssatz ("orderline") hinzu."""
    position = Position()
    positionen.append(position)
    position.position = len(positionen)
    position.vorgang = vorgangsnummer
    position.vorgangsposition = len(positionen)
    position.bestellmenge = aobj_position.menge
    position.artikel = getattr(aobj_position, 'artnr', None)
    position.verkaufspreis = getattr(aobj_position, 'verkaufspreis', None)
    if hasattr(aobj_position, 'ean'):
        position.ean = aobj_position.ean
    position.erfassungsdatum = date2softm(datetime.date.today())
    if hasattr(aobj_position, 'kundenartnr') and aobj_position.kundenartnr:
        text = "Kundenartikelnummer: %s" % aobj_position.kundenartnr
        _create_positionstext(textart=8, vorgangsposition=position.vorgangsposition, texte=texte,
            vorgangsnummer=vorgangsnummer, text=text, auftragsbestaetigung=0, lieferschein=1, rechnung=1)

    if hasattr(aobj_position, 'text_vor_position') and aobj_position.text_vor_position:
        for text in aobj_position.text_vor_position:
            _create_positionstext(textart=7, vorgangsposition=position.vorgangsposition, texte=texte,
                vorgangsnummer=vorgangsnummer, text=text, auftragsbestaetigung=0, lieferschein=1, rechnung=1)

    if hasattr(aobj_position, 'text_nach_position') and aobj_position.text_nach_position:
        for text in aobj_position.text_nach_position:
            _create_positionstext(textart=8, vorgangsposition=position.vorgangsposition, texte=texte,
                vorgangsnummer=vorgangsnummer, text=text, auftragsbestaetigung=0, lieferschein=1, rechnung=1)


def _create_addressentries(adressen, vorgangsnummer, aobj):
    """Adds entries for invoice and deliveryaddress, if given for this order.

    Returns the country code of the invoice address w/ a fallback to country of delivery address.
    """
    land = 'DE'
    for addresstype in ['lieferadresse', 'rechnungsadresse']:
        aobj_adresse = getattr(aobj, addresstype, None)
        if not aobj_adresse:
            continue
        if (hasattr(aobj_adresse, 'name1') and hasattr(aobj_adresse, 'ort') and
                hasattr(aobj_adresse, 'land')) == False:
            continue
        adresse = Adresse()
        if addresstype == 'lieferadresse':
            adresse.adressart = 1
        adresse.vorgang = vorgangsnummer
        adresse.name1 = getattr(aobj_adresse, 'name1', '')
        adresse.name2 = getattr(aobj_adresse, 'name2', '')
        adresse.name3 = getattr(aobj_adresse, 'name3', '')
        adresse.avisieren = getattr(aobj_adresse, 'avisieren', '')
        adresse.strasse = getattr(aobj_adresse, 'strasse', '')
        adresse.plz = getattr(aobj_adresse, 'plz', '')
        adresse.ort = getattr(aobj_adresse, 'ort', '')
        land = getattr(aobj_adresse, 'land', 'DE')
        adresse.laenderkennzeichen = iso2land(land)
        adressen.append(adresse)
    return land


def _auftrag2records(vorgangsnummer, auftrag):
    """Convert a auftrag into records objects representing AS/400 SQL statements."""
    kopf = Kopf()
    kopf.vorgang = vorgangsnummer
    kopf.kundennr = '%8s' % auftrag.kundennr.split('/')[0]
    if len(auftrag.kundennr.split('/')) > 1:
        # abweichende Lieferadresse in address-zusatzdatei
        kopf.lieferadresse = int(auftrag.kundennr.split('/')[1])

    if hasattr(auftrag, 'herkunft'):
        kopf.herkunft = auftrag.herkunft

    if hasattr(auftrag, 'teillieferung_zulaessig'):
        kopf.teillieferung_zulaessig = str(int(auftrag.teillieferung_zulaessig))
    else:
        # für lagacy code, den default wert auf false setzen
        kopf.teillieferung_zulaessig = ""

    if hasattr(auftrag, 'abgangslager'):
        kopf.abgangslager = auftrag.abgangslager

    if hasattr(auftrag, 'kundenauftragsnr'):
        kopf.kundenauftragsnr = auftrag.kundenauftragsnr

    if hasattr(auftrag, 'bestelldatum') and auftrag.bestelldatum:
        kopf.bestelldatum = date2softm(auftrag.bestelldatum)
    else:
        kopf.bestelldatum = date2softm(datetime.date.today())

    kopf.auftragsart = ''
    if hasattr(auftrag, 'auftragsart') and auftrag.auftragsart:
        kopf.auftragsart = auftrag.auftragsart

    kopf.sachbearbeiter = 1

    if auftrag.anlieferdatum_max:
        kopf.kundenwunschtermin = date2softm(auftrag.anlieferdatum_max)
    else:
        kopf.kundenwunschtermin = ''

    if hasattr(auftrag, 'anlieferdatum_min') and auftrag.anlieferdatum_min:
        kopf.anliefertermin = date2softm(auftrag.anlieferdatum_min)
    else:
        kopf.anliefertermin = date2softm(datetime.date.today()) # FIXME +7 days eventually?

    positionen = []
    texte = []

    if hasattr(auftrag, 'infotext_kunde'):
        _create_kopftext(texte, vorgangsnummer, deUmlaut(auftrag.infotext_kunde), auftragsbestaetigung=1,
                         lieferschein=1, rechnung=1)
    if hasattr(auftrag, 'bestelltext'):
        _create_kopftext(texte, vorgangsnummer, deUmlaut(auftrag.bestelltext), auftragsbestaetigung=1,
                         lieferschein=0, rechnung=0)

    # Für Fixtermine die Uhrzeit (oder was immer im Fixterminfeld steht) als Kopftext übertragen und das
    # Fixtermin Flag setzen
    if hasattr(auftrag, 'fixtermin') and auftrag.fixtermin:
        txt_fixtermin = "FIX: %s" 
        if isinstance(auftrag.fixtermin, (datetime.date, datetime.datetime)):
            txt_fixtermin %= auftrag.fixtermin.strftime('%d-%m-%y %H:%M')
        elif isinstance(auftrag.fixtermin, datetime.time):
            txt_fixtermin %= auftrag.fixtermin.strftime('%H:%M')
        else:
            txt_fixtermin %= auftrag.fixtermin
        _create_kopftext(texte, vorgangsnummer, txt_fixtermin, auftragsbestaetigung=1,
                         lieferschein=0, rechnung=0)
        kopf.fixtermin = 1

    adressen = []

    # add Lieferadresse and Rechungsadresse and create eu country code if neccessary
    land = _create_addressentries(adressen, vorgangsnummer, auftrag)
    if land != 'DE' and huTools.world.in_european_union(land):
        kopf.eu_laendercode = land

    for aobj_position in auftrag.positionen:
        _create_positionssatz(positionen, vorgangsnummer, aobj_position, texte)
    kopf.vorgangspositionszahl = len(positionen)
    return kopf, positionen, texte, adressen


# TODO: das AuftragsFormat - https://cybernetics.hudora.biz/intern/wordpress/2008/09/antville-18807/
# ist nicht mehr aktuell, wir wollen auf
# https://github.com/hudora/CentralServices/blob/master/doc/ExtendedOrderProtocol.markdown
# umstellen. 
def auftrag2softm(auftrag, belegtexte=None):
    """Schreibt etwas, dass dem AuftragsFormat entspricht in SoftM."""

    # this should be done by the caller:
    # if kundenauftragsnummer_bekannt(auftragskennzeichen):
    #     print "Auftragskennzeichen %r wurde bereits verwendet." % auftragskennzeichen
    #     return None
    # if not schnittstelle_leer():
    #     print "In der Stapelschnittstelle befinden sich nicht verarbeitete Nachrichten."
    #    return None
    
    if not belegtexte:
        belegtexte = []
    
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
        raise RuntimeError("Internal Server error: insertation into ABK00 failed: "
                           "uuid=%r\n SQL Statement: %r" % (uuid, kopf.to_sql()))
    elif rowcount > 1:
        # the race condition has hit - remove our entry and retry
        get_connection().delete('ABK00', 'BKDFSL=%s' % sql_quote(uuid))
        # sleep to avoid deadlocks see http://de.wikipedia.org/wiki/CSMA/CD#Das_Backoff-Verfahren_bei_Ethernet
        time.sleep(random.random()/100.0)
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
        get_connection().update_raw("UPDATE ABK00 SET BKKZBA=0, BKDFSL='' WHERE BKVGNR=%s" % vorgangsnummer)

    return vorgangsnummer


def _get_attr(obj, key):
    """Get an object attribute or an dict key"""
    ret = getattr(obj, key, '')
    if (not ret) and hasattr(obj, 'get'):
        ret = obj.get(key, '')
    return ret


def _order2records(vorgangsnummer, order):
    """Convert a auftrag into records objects representing AS/400 SQL statements."""
    # see https://github.com/hudora/CentralServices/blob/master/doc/ExtendedOrderProtocol.markdown
    kopf = Kopf()
    kopf.vorgang = vorgangsnummer
    # kundennr Interne Kundennummer. Kann das AddressProtocol erweitern. Wenn eine kundennr angegeben ist und die per AddressProtocol angegebene Lieferadresse nicht zu der kundennr passt, handelt es sich um eine abweichende Lieferadresse.
    kopf.kundennr = '%8s' % _get_attr(order, 'kundennr').split('/')[0]
    # kundenauftragsnr - Freitext, den der Kunde bei der Bestellung mit angegeben hat, ca. 20 Zeichen.
    kopf.kundenauftragsnr = deUmlaut(_get_attr(order, 'kundenauftragsnr'))[:20] # max 20 Zeichen, sonst gehts nicht
    # TODO: brauchen wir die beiden daten?
    # kopf.erfassungsdatum = kopf.bestelldatum = date2softm(datetime.date.today())
    
    # wunschdatum_von und wunschdatum_bis - bilden wir NICHT ab.
    # anlieferdatum_von und anlieferdatum_bis Wenn das System keine Zeitfenster unterstützt, wird nur
    # anlieferdatum_von verwendet..
    if _get_attr(order, 'anlieferdatum_von'):
        kopf.anliefertermin = date2softm(_get_attr(order, 'anlieferdatum_von'))
    else:
        kopf.anliefertermin = date2softm(datetime.date.today()) # FIXME +7 days eventually?
    if _get_attr(order, 'anlieferdatum_bis'):
        kopf.kundenwunschtermin = date2softm(_get_attr(order, 'anlieferdatum_bis'))
    else:
        kopf.kundenwunschtermin = kopf.anliefertermin
    
    # versandkosten - Versandkosten kommen in Cent -> umwandeln in Euro
    if _get_attr(order, 'versandkosten'):
        kopf.versandkosten = decimal.Decimal(_get_attr(order, 'versandkosten'))
        kopf.versandkosten /= 100
        kopf.versandkosten = kopf.versandkosten.quantize(decimal.Decimal('0.12')) # Runden
        if kopf.versandkosten:
            kopf.lieferbedingung = 18

    # absenderadresse - (mehrzeiliger) String, der die Absenderadresse auf Versandpapieren codiert.

    if _get_attr(order, 'abgangslager'):
        kopf.abgangslager = _get_attr(order, 'abgangslager')

    positionen = []
    texte = []
    adressen = []
    # guid - Eindeutiger ID des Vorgangs, darf niemals doppelt verwendet werden
    _create_kopftext(texte, vorgangsnummer, "Referenz: %s" % _get_attr(order, 'guid'),
                     auftragsbestaetigung=0, lieferschein=0, rechnung=0)
    if _get_attr(order, 'erfasst_von'):
                 _create_kopftext(texte, vorgangsnummer, "erfasst von: %s" % _get_attr(order, 'erfasst_von'),
                                  auftragsbestaetigung=0, lieferschein=0, rechnung=0)

    # infotext_kunde - Freitext, der sich an den Warenempfänger richtet. Kann z.B. auf einem Lieferschein angedruckt werden. Der Umbruch des Textes kann durch das Backendsystem beliebig erfolgen, deshalb sollte der Text keine Zeilenumbrüche beinhalten.
    if _get_attr(order, 'infotext_kunde'):
        _create_kopftext(texte, vorgangsnummer, _get_attr(order, 'infotext_kunde'),
                         auftragsbestaetigung=1, lieferschein=1, rechnung=1)

    ## add Lieferadresse and Rechungsadresse and create eu country code if neccessary
    #land = _create_addressentries(adressen, vorgangsnummer, auftrag)
    #if land != 'DE' and huTools.world.in_european_union(land):
    #    kopf.eu_laendercode = land
    
    for order_position in  _get_attr(order, 'orderlines'):
        #_create_positionssatz(positionen, vorgangsnummer, aobj_position, texte)
        position = Position()
        positionen.append(position)
        position.position = len(positionen)
        position.vorgang = vorgangsnummer
        position.vorgangsposition = len(positionen)
        position.bestellmenge = _get_attr(order_position, 'menge')
        position.artikel = _get_attr(order_position, 'artnr')
        position.ean = _get_attr(order_position, 'ean')
        position.erfassungsdatum = date2softm(datetime.date.today())
        if _get_attr(order_position, 'infotext_kunde'):
            _create_positionstext(textart=8, vorgangsposition=position.vorgangsposition, texte=texte,
                vorgangsnummer=vorgangsnummer, text=_get_attr(order_position, 'infotext_kunde'),
                auftragsbestaetigung=1, lieferschein=1, rechnung=1)
        # orderline/guid - Eindeutiger ID der Position. GUID des Auftrags + Positionsnummer funktionieren ganz gut.
        # orderline/preis - Rechnungs-Preis der Orderline in Cent ohne Mehrwertsteuer.
    kopf.vorgangspositionszahl = len(positionen)
    return kopf, positionen, texte, adressen


def extended_order_protocol2softm(order):
    """Schreibt etwas, dass dem ExtendedOrderProtocol entspricht in SoftM.
    
    https://github.com/hudora/CentralServices/blob/master/doc/ExtendedOrderProtocol.markdown"""

    while True:
        # SoftM (Mister Hering) suggested updating Adtastapel as soon as possible to avoid
        # dupes - missing IDs would be no problem. update_adtastapel() is extremly hairy code
        vorgangsnummer = getnextvorgang()
        get_connection().update_adtastapel(vorgangsnummer)
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order)

        # see http://blogs.23.nu/c0re/stories/18926/ for the aproach we try here to write data into SoftM.
        # But instead of using the date for our token we use the DFSL field
        uuid = hex((int(time.time() * 10000)
                    ^ (os.getpid() << 24)
                    ^ thread.get_ident())
                    % 0xFFFFFFFFFF).rstrip('L')[2:]
        kopf.dateifuehrungsschluessel = uuid

        # Do something like "Retransmission Back-Off" on Ethernet for collision avoidance:
        # sleep for a random amount of time
        time.sleep(random.random() / 11.0)
        #check im somobdy else has been writing to the DB.
        if not vorgangsnummer_bekannt(vorgangsnummer):
            # no, so we can proceed
            break
        # else: retry while loop with a new vorgangsnummer
        time.sleep(random.random() / 7.0)

    # start writing data into the database
    get_connection().insert_raw(kopf.to_sql())
    rowcount = get_connection().query('ABK00', fields=['COUNT(*)'],
                                   condition="BKDFSL=%s" % sql_quote(uuid))[0][0]
    if rowcount < 1:
        raise RuntimeError("Internal Server error: insertation into ABK00 failed: "
                           "uuid=%r\n SQL Statement: %r" % (uuid, kopf.to_sql()))
    elif rowcount > 1:
        # the race condition has hit - remove our entry and retry
        get_connection().delete('ABK00', 'BKDFSL=%s' % sql_quote(uuid))
        # sleep to avoid deadlocks see http://de.wikipedia.org/wiki/CSMA/CD#Das_Backoff-Verfahren_bei_Ethernet
        time.sleep(random.random()/13.0)
        # recusively try again
        extended_order_protocol2softm(order)
    else:
        # we finally can insert
        sql = []
        for record in positionen + texte + adressen:
            sql.append(record.to_sql())
        sql.append(kopf.to_sql())
        for command in sql:
            print command
            get_connection().insert_raw(command)
        # remove "dateifuehrungsschluessel" and set recort active
        get_connection().update_raw("UPDATE ABK00 SET BKKZBA=0, BKDFSL='' WHERE BKVGNR=%s" % vorgangsnummer)

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


class _AuftragTests(unittest.TestCase):
    """Vermischte Tests für das Auftragsformat."""

    def test_minimal_auftrag(self):
        """Test if the minimal possible Auftrag can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKDTKD, BKKDNR)"
                   " VALUES('1','123','xtodayx','1081230','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)
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
        auftrag.fixtermin = datetime.datetime.now().time()
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKNRKD, BKLTFX, BKDTKD,"
            " BKKDNR) VALUES('1','123','1081230','1081231','1','01','0012345','1','1081229','   17200')")
        
        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte[0].to_sql(), "INSERT INTO ABT00 (BTKZLF, BTKZAB, BTTART, BTFNR, BTTX60,"
            " BTLFNR, BTKZRG, BTVGNR) VALUES('1','1','8','01','infotext_kunde','1','1','123')")
        self.assertEqual(texte[1].to_sql(), "INSERT INTO ABT00 (BTKZAB, BTTART, BTFNR, BTTX60, BTLFNR,"
            " BTVGNR) VALUES('1','8','01','bestelltext','2','123')")
        self.assertEqual(texte[2].to_sql(),
                "INSERT INTO ABT00 (BTKZAB, BTTART, BTFNR, BTTX60, BTLFNR, BTVGNR) VALUES('1','8','01',"
                    "'FIX: %s','3','123')" % auftrag.fixtermin.strftime('%H:%M'))
        self.assertEqual(adressen, [])

        auftrag.fixtermin = ''
        vorgangsnummer = 124
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKNRKD, BKDTKD, BKKDNR)"
            " VALUES('1','124','1081230','1081231','1','01','0012345','1081229','   17200')")
        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte[0].to_sql(), "INSERT INTO ABT00 (BTKZLF, BTKZAB, BTTART, BTFNR, BTTX60,"
            " BTLFNR, BTKZRG, BTVGNR) VALUES('1','1','8','01','infotext_kunde','1','1','124')")
        self.assertEqual(texte[1].to_sql(), "INSERT INTO ABT00 (BTKZAB, BTTART, BTFNR, BTTX60, BTLFNR,"
            " BTVGNR) VALUES('1','8','01','bestelltext','2','124')")

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
        auftrag.lieferadresse.avisieren = '+49 21 91 / 6 09 12-0'
        auftrag.lieferadresse.strasse = 'Nicht Vergessen Weg 1'
        auftrag.lieferadresse.ort = 'Rade'
        auftrag.lieferadresse.land = 'DE'
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKDTKD, BKKDNR)"
                   " VALUES('1','123','xtodayx','1081230','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)
        
        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVNAM3, BVKZAD, BVNAM4, BVVGNR, BVSTR, BVLKZ, BVNAME,"
            " BVORT, BVAART) VALUES('name2','name3','1','+49 21 91 / 6 09 12-0','123',"
            "'Nicht Vergessen Weg 1','D','name1','Rade','1')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])

    def test_rechnungsadresse(self):
        """Tests if a Rechnungsadresse is successfully converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        auftrag.rechnungsadresse = _MockAddress()
        auftrag.rechnungsadresse.name1 = 'name1'
        auftrag.rechnungsadresse.name2 = 'name2'
        auftrag.rechnungsadresse.name3 = 'name3'
        auftrag.rechnungsadresse.avisieren = '+49 21 91 / 6 09 12-0'
        auftrag.rechnungsadresse.strasse = 'Nicht Vergessen Weg 1'
        auftrag.rechnungsadresse.ort = 'Rade'
        auftrag.rechnungsadresse.land = 'DE'
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)

        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVNAM3, BVKZAD, BVNAM4, BVVGNR, BVSTR, BVLKZ, BVNAME,"
            " BVORT) VALUES('name2','name3','1','+49 21 91 / 6 09 12-0','123','Nicht Vergessen Weg 1'"
            ",'D','name1','Rade')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])

    def test_rechnungsadresse_eu(self):
        """Tests if a Rechnungsadresse from an european country is successfully converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        auftrag.rechnungsadresse = _MockAddress()
        auftrag.rechnungsadresse.name1 = 'name1'
        auftrag.rechnungsadresse.name2 = 'name2'
        auftrag.rechnungsadresse.name3 = 'name3'
        auftrag.rechnungsadresse.avisieren = '+49 21 91 / 6 09 12-0'
        auftrag.rechnungsadresse.strasse = 'Nicht Vergessen Weg 1'
        auftrag.rechnungsadresse.ort = 'Rade'
        auftrag.rechnungsadresse.land = 'SI'
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKEGCD, BKDTLT, BKDTKW, BKSBNR, BKVGNR, BKFNR, BKDTKD, BKKDNR)"
                   " VALUES('1','SI','xtodayx','1081230','1','123','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)

        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVNAM3, BVKZAD, BVNAM4, BVVGNR, BVSTR, BVLKZ, BVNAME,"
            " BVORT) VALUES('name2','name3','1','+49 21 91 / 6 09 12-0','123','Nicht Vergessen Weg 1'"
            ",'SLO','name1','Rade')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])

    def test_rechnungsadresse_non_eu(self):
        """Tests if a Rechnungsadresse from an outer-european country is successfully converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        auftrag.rechnungsadresse = _MockAddress()
        auftrag.rechnungsadresse.name1 = 'name1'
        auftrag.rechnungsadresse.name2 = 'name2'
        auftrag.rechnungsadresse.name3 = 'name3'
        auftrag.rechnungsadresse.avisieren = '+49 21 91 / 6 09 12-0'
        auftrag.rechnungsadresse.strasse = 'Nicht Vergessen Weg 1'
        auftrag.rechnungsadresse.ort = 'Rade'
        auftrag.rechnungsadresse.land = 'CH'
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)

        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVNAM3, BVKZAD, BVNAM4, BVVGNR, BVSTR, BVLKZ, BVNAME,"
            " BVORT) VALUES('name2','name3','1','+49 21 91 / 6 09 12-0','123','Nicht Vergessen Weg 1'"
            ",'CH','name1','Rade')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])

    def test_positionen_text(self):
        """Tests if text of positions can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        pos1 = _MockPosition()
        pos1.menge = 10
        pos1.artnr = '11111'
        pos1.text_vor_position = ['text for pos1', 'more text for pos1']
        pos2 = _MockPosition()
        pos2.menge = 20
        pos2.text_vor_position = ['text for pos2', 'more text for pos2']
        pos2.artnr = '22222/09'
        auftrag.positionen = [pos1, pos2]
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','   17200')")

        # pos 1
        txt0 = texte[0].to_sql()
        txt0_sql = ("INSERT INTO ABT00 (BTKZLF, BTVGNR, BTVGPO, BTTART, BTFNR, BTTX60, BTLFNR, BTKZRG)"
            " VALUES('1','123','1','7','01','text for pos1','1','1')")
        self.assertEqual(txt0, txt0_sql)
        txt1 = texte[1].to_sql()
        txt1_sql = ("INSERT INTO ABT00 (BTKZLF, BTVGNR, BTVGPO, BTTART, BTFNR, BTTX60, BTLFNR, BTKZRG)"
            " VALUES('1','123','1','7','01','more text for pos1','2','1')")
        self.assertEqual(txt1, txt1_sql)

        # pos 2
        txt2 = texte[2].to_sql()
        txt2_sql = ("INSERT INTO ABT00 (BTKZLF, BTVGNR, BTVGPO, BTTART, BTFNR, BTTX60, BTLFNR, BTKZRG)"
            " VALUES('1','123','2','7','01','text for pos2','3','1')")
        self.assertEqual(txt2, txt2_sql)
        txt3 = texte[3].to_sql()
        txt3_sql = ("INSERT INTO ABT00 (BTKZLF, BTVGNR, BTVGPO, BTTART, BTFNR, BTTX60, BTLFNR, BTKZRG)"
            " VALUES('1','123','2','7','01','more text for pos2','4','1')")
        self.assertEqual(txt3, txt3_sql)

    def test_positionen_artnr(self):
        """Tests if orderlines containing artnr can be converted to SQL."""
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
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','   17200')")

        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen[0].to_sql(), "INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, "
            "BAARTN, BAVGNR) VALUES('xtodayx','1','1','01','10','11111','123')".replace('xtodayx',
             date2softm(datetime.date.today())))
        self.assertEqual(positionen[1].to_sql(), "INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, "
            "BAARTN, BAVGNR) VALUES('xtodayx','2','1','01','20','22222/09','123')".replace('xtodayx',
             date2softm(datetime.date.today())))

    def test_positionen_ean(self):
        """Tests if orderlines containing ean can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        pos1 = _MockPosition()
        pos1.menge = 10
        pos1.ean = '11111'
        pos2 = _MockPosition()
        pos2.menge = 20
        pos2.ean = '22222'
        auftrag.positionen = [pos1, pos2]
        kopf, positionen, dummy, dummy = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','   17200')")

        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen[0].to_sql(),
                "INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAMNG, BAVGNR, BAFNR, BAEAN)"
                " VALUES('xtodayx','1','1','10','123','01','11111')".replace(
                    'xtodayx', date2softm(datetime.date.today())))
        self.assertEqual(positionen[1].to_sql(),
                "INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAMNG, BAVGNR, BAFNR, BAEAN)"
                " VALUES('xtodayx','2','1','20','123','01','22222')".replace(
                    'xtodayx', date2softm(datetime.date.today())))

    def test_zusatz_lieferadresse(self):
        """Tests that 'zusatzliche lieferadresse' (BKVANR) can be converted to sql"""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200/444' # <-- 004 ist die zusätzliche lieferadresse
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        pos1 = _MockPosition()
        pos1.menge = 10
        pos1.artnr = '11111'
        pos2 = _MockPosition()
        pos2.menge = 20
        pos2.artnr = '22222/09'
        auftrag.positionen = [pos1, pos2]
        kopf, positionen, dummy, dummy = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKVANR,"
            " BKKDNR) VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','444','   17200')")

        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)

    def test_abgangslager(self):
        """Test if the 'abgangslager' can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.abgangslager = '26'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)

        kpf_sql = ("INSERT INTO ABK00 (BKLGNR, BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('26','1','123','xtodayx','1081230','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])
        self.assertEqual(adressen, [])

    def test_herkunft(self):
        """Test if 'herkunft' can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.herkunft = 'E' #EDI
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKHERK, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','01','E','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])
        self.assertEqual(adressen, [])

    def test_teillieferung(self):
        """Test if 'teillieferung_zulaessig' can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.teillieferung_zulaessig = True
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)

        vorgangsnummer = 124
        auftrag.teillieferung_zulaessig = False
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','124','xtodayx','1081230','1','0','01','xtodayx','   17200')")
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])
        self.assertEqual(adressen, [])

    def test_auftragsart(self):
        """Test if 'auftragsart' can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.auftragsart = "WA"
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        auftrag.positionen = []
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKKDNR, BKDTKD, BKAUFA)"
            " VALUES('1','123','xtodayx','1081230','1','01','   17200','xtodayx','WA')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)

        vorgangsnummer = 124
        auftrag.auftragsart = 'Z2'
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKKDNR, BKDTKD, BKAUFA)"
            " VALUES('1','124','xtodayx','1081230','1','01','   17200','xtodayx','Z2')")
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])
        self.assertEqual(adressen, [])

    def test_positionen_verkaufspreis(self):
        """Tests if orderlines containing prices can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = _MockAuftrag()
        auftrag.kundennr = '17200'
        auftrag.anlieferdatum_max = datetime.date(2008, 12, 30)
        pos1 = _MockPosition()
        pos1.menge = 10
        pos1.ean = '11111'
        pos1.verkaufspreis = 10000.123
        pos2 = _MockPosition()
        pos2.menge = 20
        pos2.ean = '22222'
        pos2.verkaufspreis = 0.123
        auftrag.positionen = [pos1, pos2]
        kopf, positionen, dummy, dummy = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','   17200')")

        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)
        positionen[0].to_sql()
        self.assertEqual(positionen[0].to_sql(),
                "INSERT INTO ABA00 (BADTER, BAPREV, BAVGPO, BAABT, BAMNG, BAVGNR, BAFNR, BAEAN)"
                " VALUES('xtodayx','10000.123','1','1','10','123','01','11111')".replace(
                    'xtodayx', date2softm(datetime.date.today())))
        self.assertEqual(positionen[1].to_sql(),
                "INSERT INTO ABA00 (BADTER, BAPREV, BAVGPO, BAABT, BAMNG, BAVGNR, BAFNR, BAEAN)"
                " VALUES('xtodayx','0.123','2','1','20','123','01','22222')".replace(
                    'xtodayx', date2softm(datetime.date.today())))


class _OrderTests(unittest.TestCase):
    """Vermischte Tests für das ExtendedOrderFormat."""

    def test_minimal_order(self):
        """Test if the minimal possible Order can be converted to SQL."""
        vorgangsnummer = 123
        order = {'_id': '17200',
                 'anlieferdatum_von': u'2010-03-03',
                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A',
                 'kundennr': u'17200',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'10316',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-1',
                                 u'menge': 1,
                                 'name': 'Street Monster RX-1'},
                                {u'artnr': u'76614',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-2',
                                 u'menge': 1,
                                 'name': u'Yoga Matte mint gr\xfcn, 172x61x0,6 cm'},
                                 ],
                 }
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGPO, BKFNR, BKKDNR)"
                   " VALUES('1','123','1100303','1100303','1','1','3','01','   17200')")
        self.assertEqual(kopf.to_sql(), kpf_sql)
        text_sql = ("INSERT INTO ABT00 (BTVGNR, BTTART, BTFNR, BTTX60, BTLFNR)"
                    " VALUES('123','8','01','Referenz: VS6RRW2MYL4FZ3PPMVH4ZRFE3A','1')")
        self.assertEqual(len(texte), 1)
        self.assertEqual(texte[0].to_sql(), text_sql)
        pos_sql = ("INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, BAARTN, BAVGNR)"
                   " VALUES('xtodayx','1','1','01','1','14600/03','123')")
        pos_sql = pos_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(len(positionen), 3)
        self.assertEqual(positionen[0].to_sql(), pos_sql)
        self.assertEqual(adressen, [])

    def test_simple_order(self):
        """Test if the minimal possible Order can be converted to SQL."""
        vorgangsnummer = 123
        order = {'_id': '17200',
                 '_rev': '4-4bba80636c015f98e908c79c521e5124',
                 'sachbearbeiter': 'verkauf',
                 'softmid': '17200',
                 'iln': '4.00599800001e+12',
                 'anlieferdatum_von': u'2010-03-03',
                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A',
                 'infotext_kunde': u'',
                 'kundenauftragsnr': u'',
                 'kundennr': u'17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'J\xc3\xa4gerwald 13',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'65240',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-24',
                                 u'menge': 1,
                                 'name': 'Test'}],
                 'tel': '+49 2191 60912 10'}
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGPO, BKFNR, BKKDNR)"
                   " VALUES('1','123','1100303','1100303','1','1','2','01','   17200')")
        self.assertEqual(kopf.to_sql(), kpf_sql)
        text_sql = ("INSERT INTO ABT00 (BTVGNR, BTTART, BTFNR, BTTX60, BTLFNR)"
                    " VALUES('123','8','01','Referenz: VS6RRW2MYL4FZ3PPMVH4ZRFE3A','1')")
        self.assertEqual(len(texte), 1)
        self.assertEqual(texte[0].to_sql(), text_sql)
        pos_sql = ("INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, BAARTN, BAVGNR)"
                   " VALUES('xtodayx','1','1','01','1','14600/03','123')")
        pos_sql = pos_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(len(positionen), 2)
        self.assertEqual(positionen[0].to_sql(), pos_sql)
        self.assertEqual(adressen, [])

    def test_simple_infotext(self):
        """Test if the minimal possible Order can be converted to SQL."""
        vorgangsnummer = 123
        order = {'_id': '17200',
                 '_rev': '4-4bba80636c015f98e908c79c521e5124',
                 'sachbearbeiter': 'verkauf',
                 'softmid': '17200',
                 'iln': '4.00599800001e+12',
                 'anlieferdatum_von': u'2010-03-03',
                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A',
                 'infotext_kunde': (u'Dieser Text sollte in meheren Zeilen enden.\n'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'01234567890123456789012345678901234567890123456789'
                                    u'0123456789<-Hier ein Ümbruch. üäöß Keine Ümlaute!!!'),
                 'kundenauftragsnr': u'',
                 'kundennr': u'17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'J\xc3\xa4gerwald 13',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'65240',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-24',
                                 u'menge': 1,
                                 'name': 'Test'}],
                 'tel': '+49 2191 60912 10'}
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGPO, BKFNR, BKKDNR)"
                   " VALUES('1','123','1100303','1100303','1','1','2','01','   17200')")
        self.assertEqual(kopf.to_sql(), kpf_sql)
        text_sql = ("INSERT INTO ABT00 (BTVGNR, BTTART, BTFNR, BTTX60, BTLFNR)"
                    " VALUES('123','8','01','Referenz: VS6RRW2MYL4FZ3PPMVH4ZRFE3A','1')")
        self.assertEqual(len(texte), 13)
        self.assertEqual(texte[0].to_sql(), text_sql)
        pos_sql = ("INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, BAARTN, BAVGNR)"
                   " VALUES('xtodayx','1','1','01','1','14600/03','123')")
        pos_sql = pos_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(len(positionen), 2)
        self.assertEqual(positionen[0].to_sql(), pos_sql)
        self.assertEqual(adressen, [])

    def test_versandkosten(self):
        vorgangsnummer = 123
        order = {'_id': '17200',
                 '_rev': '4-4bba80636c015f98e908c79c521e5124',
                 'sachbearbeiter': 'verkauf',
                 'softmid': '17200',
                 'iln': '4.00599800001e+12',
                 'anlieferdatum_von': u'2010-03-03',
                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A',
                 'infotext_kunde': u'',
                 'kundenauftragsnr': u'',
                 'kundennr': u'17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'versandkosten': 1950, # in Cent
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'J\xc3\xa4gerwald 13',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'65240',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-24',
                                 u'menge': 1,
                                 'name': 'Test'}],
                 'tel': '+49 2191 60912 10'}
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKVSK , BKSBNR, BKKZTF, BKVGPO, BKFNR, BKX3LB, BKKDNR) "
                   "VALUES('1','123','1100303','1100303','19.50','1','1','2','01','18','   17200')")
        self.assertEqual(kopf.to_sql(), kpf_sql)

    def test_abgangslager(self):
        """Test if the 'abgangslager' can be converted to SQL."""
        vorgangsnummer = 123
        order = {'_id': '17200',
                 '_rev': '4-4bba80636c015f98e908c79c521e5124',
                 'sachbearbeiter': 'verkauf',
                 'softmid': '17200',
                 'iln': '4.00599800001e+12',
                 'anlieferdatum_von': u'2010-03-03',
                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A',
                 'infotext_kunde': u'',
                 'kundenauftragsnr': u'',
                 'kundennr': u'17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'abgangslager': '26',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'J\xc3\xa4gerwald 13',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'65240',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-24',
                                 u'menge': 1,
                                 'name': 'Test'}],
                 'tel': '+49 2191 60912 10'}
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order)

        kpf_sql = ("INSERT INTO ABK00 (BKLGNR, BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGPO, BKFNR, BKKDNR)"
                   " VALUES('26','1','123','1100303','1100303','1','1','2','01','   17200')")
        self.assertEqual(kopf.to_sql(), kpf_sql)


if __name__ == '__main__':
    unittest.main()
