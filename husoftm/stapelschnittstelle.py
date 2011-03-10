#!/usr/bin/env python
# encoding: utf-8
"""
stapelschnittstelle.py - Ansprechen der SoftM Auftrags-Stapelschnittstelle.

ACHTUNG: Die SoftM Stapel-Schnittstelle ist - dürftig dokumentiert. Das bedeutet, dass dieser Code grosse
Chancen bietet, nicht wie erwartet zu funktionieren. Er ist nur für experimentelle Zwecke geeignet - oder
für Leute mit starken Nerven. Aber wenn sie schwache Nerven hätten, würden sie kein SoftM einsetzen, oder?

Die SoftM Stapelschnittstelle auch von SoftM umfangreich genutzt: so ist die EDI-Schnittstelle eigentlich
nur ein Umsetzer, der die Daten in die Stapelschnittstelle schreibt. Mit der Stapelschnittstelle kann man
nahezu alle Prameter eines Auftrages beschreiben.

In der SoftM-GUI findet sich das ganze in "Stapelschnittstelle Vertrieb (A0425)"

Created by Maximillian Dornseif on 2007-05-16.
Copyright (c) 2007, 2008, 2010 HUDORA GmbH. All rights reserved.
"""

from cs.masterdata.address import addresshash, get_address
import datetime
import huTools.world
import itertools
import os
import random
import textwrap
import thread
import time
import unittest
import warnings
from huTools.monetary import cent_to_euro
from huTools.structured import make_struct
from huTools.unicode import deUmlaut
from husoftm.connection import get_connection
from husoftm.stapelschnittstelle_const import ABK00, ABA00, ABT00, ABV00
from husoftm.tools import date2softm, sql_quote, iso2land, pad, add_prefix, remove_prefix


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

            # format
            if rowid in felddict and hasattr(felddict[rowid], 'format'):
                fmt = rowconf['format']
                if fmt.upper().startswith('A'):
                    fieldlength = int(fmt[1:])
                    felddict[rowid] = felddict[rowid][:fieldlength]

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
    if not rows:
        raise RuntimeError('Fehler bei der Ermittlung der Vorgangsnummer')
    return rows[0][0] + 1


def loesche_dfsl(vorgangsnr):
    """Lösche alle Dateiführungsschlüssel für eine Vorgangsnr in der Tabelle der Auftragsköpfe"""
    get_connection().update_raw("UPDATE ABK00 SET BKDFSL='' WHERE BKVGNR=%s" % vorgangsnr)


def loesche_vorgang(vorgangsnr):
    """Setzt einen Vorgang auf Status gelöscht'."""
    get_connection().update_raw("UPDATE ABK00 SET BKSTAT='X' WHERE BKVGNR=%s AND BKAUFN = 0 AND"
                                " BKSTAT <> 'X' AND BKKZBA <> 0" % vorgangsnr)
    get_connection().update_raw("UPDATE ABA00 SET BASTAT='X' WHERE BAVGNR=%s AND BAAUFN = 0 AND"
                                " BASTAT <> 'X' AND BAKZBA <> 0" % vorgangsnr)


def feststeckende_jobs():
    """Returns a list of jobs that got stuck in stapelschnittstelle."""
    rows = get_connection().query('ABK00', fields=['BKKDNR', 'BKVGNR', 'BKNRKD', 'BKKZBA'],
                                  condition="BKAUFN = 0 AND BKSTAT <> 'X' AND BKKZBA <> 0")
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


def _create_addressentries(adressen, vorgangsnummer, auftrag):
    """Adds entries for invoice and delivery address, if given for this order.

    adressen is expected to be a list type where the created addressentries are appended.
    Returns the country code of the invoice address w/ a fallback to country of delivery address.
    """

    land = 'DE'
    auftrag = make_struct(auftrag)

    for adresstyp in ['lieferadresse', 'rechnungsadresse']:
        if not adresstyp in auftrag:
            continue
        adresse = getattr(auftrag, adresstyp)
        land = _create_addressentry(adressen, vorgangsnummer, adresse, adresstyp)
    return land


def _create_addressentry(adressen, vorgangsnummer, adresscontainer, adresstyp):

    adresscontainer = make_struct(adresscontainer, default='')

    if not (adresscontainer.name1 and adresscontainer.land and adresscontainer.ort):
        raise ValueError(u'Adresse unvollständig: %s' % adresscontainer)

    adresse = Adresse()
    if adresstyp == 'lieferadresse':
        adresse.adressart = 1

    # Kopiere Adresse
    adresse.vorgang = vorgangsnummer
    adresse.name1 = deUmlaut(adresscontainer.name1)
    adresse.name2 = deUmlaut(adresscontainer.name2)
    adresse.name3 = deUmlaut(adresscontainer.name3)
    adresse.avisieren = adresscontainer.avisieren
    adresse.strasse = deUmlaut(adresscontainer.strasse)
    adresse.plz = adresscontainer.plz
    adresse.ort = deUmlaut(adresscontainer.ort)
    land = adresscontainer.land

    # Füge SoftM-Länderkennzeichen ein
    adresse.laenderkennzeichen = iso2land(land)
    adressen.append(adresse)
    return land


def _auftrag2records(vorgangsnummer, auftrag):
    """Convert an Auftrag into records objects representing AS/400 SQL statements."""

    auftrag = make_struct(auftrag, default='')

    kopf = Kopf()
    kopf.vorgang = vorgangsnummer

    # Trage ggf. eine abweichende Lieferadresse (Index in Adress-Zusatzdatei) in Auftragskopf ein
    kundennr = auftrag.kundennr
    if '.' in kundennr:
        kundennr, lieferadresse = kundennr.split('.')
        kopf.lieferadresse = int(lieferadresse)

    # Entferne Präfix vor Kundennr
    if not kundennr.startswith('SC'):
        warnings.warn(u"Kundennr %s ohne Prefix (SC)" % auftrag.kundennr, DeprecationWarning)
    kundennr = remove_prefix(kundennr, 'SC')

    # Füge Auftragsnr im SoftM-Format ein (Padding mit Leerzeichen)
    kopf.kundennr = pad('BKKDNR', kundennr)

    if 'herkunft' in auftrag:
        kopf.herkunft = auftrag.herkunft

    if 'teillieferung_zulaessig' in auftrag:
        kopf.teillieferung_zulaessig = str(int(auftrag.teillieferung_zulaessig))
    else:
        # für legacy code, den Standardwert auf 'keine Teillieferung' setzen
        kopf.teillieferung_zulaessig = ''

    if 'abgangslager' in auftrag:
        kopf.abgangslager = auftrag.abgangslager

    if 'kundenauftragsnr' in auftrag:
        kopf.kundenauftragsnr = auftrag.kundenauftragsnr

    if 'bestelldatum' in auftrag:
        kopf.bestelldatum = date2softm(auftrag.bestelldatum)
    else:
        kopf.bestelldatum = date2softm(datetime.date.today())

    # XXX: Mögliche Werte?
    kopf.auftragsart = ''
    if hasattr(auftrag, 'auftragsart') and auftrag.auftragsart:
        kopf.auftragsart = auftrag.auftragsart

    # XXX: Nicht auch über auftrag steuerbar?
    kopf.sachbearbeiter = 1

    if auftrag.anlieferdatum_max:
        kopf.kundenwunschtermin = date2softm(auftrag.anlieferdatum_max)
    else:
        kopf.kundenwunschtermin = ''

    if 'anlieferdatum_min' in auftrag:
        kopf.anliefertermin = date2softm(auftrag.anlieferdatum_min)
    else:
        kopf.anliefertermin = date2softm(datetime.date.today())

    positionen = []
    texte = []

    if 'infotext_kunde' in auftrag:
        _create_kopftext(texte, vorgangsnummer, deUmlaut(auftrag.infotext_kunde),
                         auftragsbestaetigung=1, lieferschein=1, rechnung=1)
    if hasattr(auftrag, 'bestelltext'):
        _create_kopftext(texte, vorgangsnummer, deUmlaut(auftrag.bestelltext),
                         auftragsbestaetigung=1, lieferschein=0, rechnung=0)

    # Für Fixtermine die Uhrzeit (oder was immer im Fixterminfeld steht) als Kopftext übertragen und das
    # Fixtermin Flag setzen
    if auftrag.fixtermin:
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

    # add Lieferadresse and Rechnungsadresse and create EU country code if neccessary
    land = _create_addressentries(adressen, vorgangsnummer, auftrag)
    if land != 'DE' and huTools.world.in_european_union(land):
        kopf.eu_laendercode = land

    for aobj_position in auftrag.positionen:
        _create_positionssatz(positionen, vorgangsnummer, aobj_position, texte)
    kopf.vorgangspositionszahl = len(positionen)
    return kopf, positionen, texte, adressen


def auftrag2softm(auftrag, belegtexte=None):
    """Schreibt etwas, dass dem AuftragsFormat entspricht in SoftM."""

    warnings.warn("auftrag2softm() is obsolete use extended_order_protocol2softm() instead",
                  DeprecationWarning)
    if not belegtexte:
        belegtexte = []
    while True:
        vorgangsnummer = getnextvorgang()
        get_connection().update_adtastapel(vorgangsnummer)
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        uuid = hex((int(time.time() * 10000)
                    ^ (os.getpid() << 24)
                    ^ thread.get_ident())
                    % 0xFFFFFFFFFF).rstrip('L')[2:]
        kopf.dateifuehrungsschluessel = uuid
        time.sleep(random.random() / 10.0)
        if not vorgangsnummer_bekannt(vorgangsnummer):
            break
    get_connection().insert_raw(kopf.to_sql())
    rowcount = get_connection().query('ABK00', fields=['COUNT(*)'],
                                   condition="BKDFSL=%s" % sql_quote(uuid))[0][0]
    if rowcount < 1:
        raise RuntimeError("Internal Server error: insertation into ABK00 failed: "
                           "uuid=%r\n SQL Statement: %r" % (uuid, kopf.to_sql()))
    elif rowcount > 1:
        get_connection().delete('ABK00', 'BKDFSL=%s' % sql_quote(uuid))
        time.sleep(random.random() / 100.0)
        return auftrag2softm(auftrag, belegtexte)
    else:
        sql = []
        for record in positionen + texte + adressen:
            sql.append(record.to_sql())
        sql.append(kopf.to_sql())
        for command in sql:
            get_connection().insert_raw(command)
        get_connection().update_raw("UPDATE ABK00 SET BKKZBA=0, BKDFSL='' WHERE BKVGNR=%s" % vorgangsnummer)
    return vorgangsnummer


def _order2records(vorgangsnummer, order, auftragsart=None, abgangslager=None):
    """Erzeuge SQL statements fuur ein Objekt nach ExtendedOrderProtocol"""
    # see https://github.com/hudora/CentralServices/blob/master/doc/ExtendedOrderProtocol.markdown

    order = make_struct(order, default='')

    kopf = Kopf()
    kopf.vorgang = vorgangsnummer

    # kundennr ist die interne Kundennr. Kann das AddressProtocol erweitern.
    # Wenn eine Kundennr angegeben ist und die per AddressProtocol angegebene Lieferadresse nicht zu
    # der Kundennr passt, handelt es sich um eine abweichende Lieferadresse.
    kundennr = order.kundennr
    if '.' in kundennr:
        kundennr, lieferadresse = kundennr.split('.')
        kopf.lieferadresse = int(lieferadresse)

    # Entferne Präfix vor Kundennr
    if not kundennr.startswith('SC'):
        warnings.warn(u"Kundennr %s ohne Prefix (SC)" % order.kundennr, DeprecationWarning)
    kundennr = remove_prefix(kundennr, 'SC')

    # Füge Auftragsnr im SoftM-Format ein (Padding mit Leerzeichen)
    kopf.kundennr = pad('BKKDNR', kundennr)

    # kundenauftragsnr - Freitext, den der Kunde bei der Bestellung mit angegeben hat, max. 20 Zeichen.
    kopf.kundenauftragsnr = deUmlaut(order.kundenauftragsnr)[:20]

    # Ohne bestelldatum holpert es in SoftM
    kopf.erfassungsdatum = kopf.bestelldatum = date2softm(datetime.date.today())

    # wunschdatum_von und wunschdatum_bis - bilden wir NICHT ab.
    # anlieferdatum_von und anlieferdatum_bis Wenn das System keine Zeitfenster unterstützt, wird nur
    # anlieferdatum_von verwendet..
    if order.anlieferdatum_von:
        kopf.anliefertermin = date2softm(order.anlieferdatum_von)
    else:
        kopf.anliefertermin = date2softm(datetime.date.today())

    if order.anlieferdatum_bis:
        kopf.kundenwunschtermin = date2softm(order.anlieferdatum_bis)
    else:
        kopf.kundenwunschtermin = kopf.anliefertermin

    # versandkosten - Versandkosten kommen in Cent, für SoftM werden sie in Euro umgewandelt
    if order.versandkosten:
        kopf.versandkosten = cent_to_euro(order.versandkosten)
        if float(kopf.versandkosten):
            kopf.lieferbedingung = ' 18'  # XXX: Was bedeutet das?

    # absenderadresse - (mehrzeiliger) String, der die Absenderadresse auf Versandpapieren kodiert.

    if abgangslager:
        kopf.abgangslager = abgangslager
    if auftragsart:
        kopf.auftragsart = auftragsart

    positionen = []
    texte = []
    adressen = []
    # guid - Eindeutiger ID des Vorgangs, darf niemals doppelt verwendet werden
    _create_kopftext(texte, vorgangsnummer, "#:guid:%s" % order.guid,
                     auftragsbestaetigung=0, lieferschein=0, rechnung=0)
    if 'erfasst_von' in order:
        _create_kopftext(texte, vorgangsnummer, "erfasst von: %s" % order.erfasst_von,
                         auftragsbestaetigung=0, lieferschein=0, rechnung=0)

    # infotext_kunde - Freitext, der sich an den Warenempfänger richtet. Kann z.B. auf einem Lieferschein
    #  angedruckt werden. Der Umbruch des Textes kann durch das Backendsystem beliebig erfolgen, deshalb
    # sollte der Text keine Zeilenumbrüche beinhalten.
    if 'infotext_kunde' in order:
        _create_kopftext(texte, vorgangsnummer, order.infotext_kunde,
                         auftragsbestaetigung=1, lieferschein=1, rechnung=1)

    ## add Lieferadresse and Rechnungsadresse and create EU country code if necessary
    if addresshash(order):
        # Adresse mit unserer Kundendatenbank vergleichen.
        # Weichen sie voneinander ab, dann wird die Auftragsadresse mit in die Stapelschnittstelle übertragen.
        # Ansonsten ermittelt SoftM die Adresse anhand der Kundennr und Versandadressnr
        # und wir übertragen gar keine Adresse.
        kundenadresse = get_address(add_prefix(order.kundennr, 'SC'))
        if addresshash(kundenadresse) != addresshash(order):
            land = _create_addressentry(adressen, vorgangsnummer, order, 'lieferadresse')
            if land != 'DE' and huTools.world.in_european_union(land):
                kopf.eu_laendercode = land

    for order_position in order.orderlines:
        #_create_positionssatz(positionen, vorgangsnummer, aobj_position, texte)
        order_position = make_struct(order_position, default='')
        position = Position()
        position.erfassungsdatum = date2softm(datetime.date.today())
        positionen.append(position)
        position.position = len(positionen)
        position.vorgang = vorgangsnummer
        position.vorgangsposition = len(positionen)
        _create_positionstext(textart=8, vorgangsposition=position.vorgangsposition, texte=texte,
                              vorgangsnummer=vorgangsnummer, text='#:guid:%s' % order_position.guid,
                              auftragsbestaetigung=0, lieferschein=0, rechnung=0)
        position.artikel = order_position.artnr
        position.ean = order_position.ean
        position.bestellmenge = order_position.menge
        if not position.bestellmenge:
            raise RuntimeError("Menge nicht gegeben bei %s/%s" % (position.artikel, position.ean))
        if (not position.artikel) and (not position.ean):
            raise RuntimeError("Weder Artnr noch EAN gegeben bei %r" % order_position)

        # orderline/preis - Rechnungs-Preis der Orderline in Cent ohne Mehrwertsteuer.
        if order_position.preis:
            position.verkaufspreis = cent_to_euro(order_position.preis)

        if order_position.infotext_kunde:
            _create_positionstext(textart=8, vorgangsposition=position.vorgangsposition, texte=texte,
                                  vorgangsnummer=vorgangsnummer, text=order_position.infotext_kunde,
                                  auftragsbestaetigung=1, lieferschein=1, rechnung=1)
    kopf.vorgangspositionszahl = len(positionen)
    return kopf, positionen, texte, adressen


def extended_order_protocol2softm(order, auftragsart=None, abgangslager=None):
    """Schreibt etwas, dass dem ExtendedOrderProtocol entspricht, in SoftM.

    https://github.com/hudora/CentralServices/blob/master/doc/ExtendedOrderProtocol.markdown"""

    # TODO: check for duplicate guids like this:
    # this should be done by the caller:
    # if kundenauftragsnummer_bekannt(auftragskennzeichen):
    #     print "Auftragskennzeichen %r wurde bereits verwendet." % auftragskennzeichen
    #     return None
    # if not schnittstelle_leer():
    #     print "In der Stapelschnittstelle befinden sich nicht verarbeitete Nachrichten."
    #    return None
    # oder so:
    # -    orders = husoftm.auftraege.find_text(guidtext)
    #    if orders:
    #        raise RuntimeError('Auftrag mit guid %r bereits vorhanden: %r'
    #                            % (order['guid'], orders[0]['auftragsnr']))

    while True:
        # SoftM (Mister Hering) suggested updating Adtastapel as soon as possible to avoid
        # dupes - missing IDs would be no problem. update_adtastapel() is extremly hairy code
        vorgangsnummer = getnextvorgang()
        get_connection().update_adtastapel(vorgangsnummer)
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order, auftragsart, abgangslager)

        # see http://blogs.23.nu/c0re/stories/18926/ for the aproach we try here to write data into SoftM.
        # But instead of using the date for our token we use the DFSL field
        uuid = hex((int(time.time() * 10000)
                    ^ (os.getpid() << 24)
                    ^ thread.get_ident())
                    % 0xFFFFFFFFFF).rstrip('L')[2:]
        kopf.dateifuehrungsschluessel = uuid

        # Do something like "Retransmission Back-Off" on Ethernet for collision avoidance:
        # sleep for a random amount of time
        time.sleep(random.random() / 456.7)
        #check im somobdy else has been writing to the DB.
        if not vorgangsnummer_bekannt(vorgangsnummer):
            # no, so we can proceed
            break
        # else: retry while loop with a new vorgangsnummer
        time.sleep(random.random() / 17.1)

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
        # sleep to avoid deadlocks - http://de.wikipedia.org/wiki/CSMA/CD#Das_Backoff-Verfahren_bei_Ethernet
        time.sleep(random.random() / 13.3)
        # recusively try again
        return extended_order_protocol2softm(order, auftragsart, abgangslager)
    else:
        # we finally can insert
        sql = []
        for record in positionen + texte + adressen:
            sql.append(record.to_sql())
        sql.append(kopf.to_sql())
        for command in sql:
            get_connection().insert_raw(command)
        # remove "dateifuehrungsschluessel" and set recort active
        get_connection().update_raw("UPDATE ABK00 SET BKKZBA=0, BKDFSL='' WHERE BKVGNR=%s" % vorgangsnummer)

    return vorgangsnummer


class _AuftragTests(unittest.TestCase):
    """Vermischte Tests für das Auftragsformat."""

    def test_minimal_auftrag(self):
        """Test if the minimal possible Auftrag can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[])
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
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_min=datetime.date(2008, 12, 30),
                       anlieferdatum_max=datetime.date(2008, 12, 31),
                       bestelldatum=datetime.date(2008, 12, 29),
                       kundenauftragsnr='0012345',
                       infotext_kunde='infotext_kunde',
                       bestelltext='bestelltext',
                       positionen=[],
                       fixtermin=datetime.datetime.now().time())
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
                    "'FIX: %s','3','123')" % auftrag['fixtermin'].strftime('%H:%M'))
        self.assertEqual(adressen, [])

        auftrag['fixtermin'] = ''
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
        """Tests if a Lieferadresse is successfully converted to SQL.

        Checks also for field truncation.
        """
        vorgangsnummer = 123
        lieferadresse = dict(name1='Hier werden nur 40 Zeichen stehen: --->|<--- Weg',
                             name2='name2',
                             name3='name3',
                             avisieren='+49 21 91 / 6 09 12-0',
                             strasse='Nicht Vergessen Weg 1',
                             ort='Rade',
                             land='DE')
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[],
                       lieferadresse=lieferadresse
        )
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKDTKD, BKKDNR)"
                   " VALUES('1','123','xtodayx','1081230','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)

        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVNAM3, BVKZAD, BVNAM4, BVVGNR, BVSTR, BVLKZ, BVNAME,"
            " BVORT, BVAART) VALUES('name2','name3','1','+49 21 91 / 6 09 12-0','123',"
            "'Nicht Vergessen Weg 1','D','Hier werden nur 40 Zeichen stehen: --->|','Rade','1')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)
        self.assertEqual(positionen, [])
        self.assertEqual(texte, [])

    def test_rechnungsadresse(self):
        """Tests if a Rechnungsadresse is successfully converted to SQL."""
        vorgangsnummer = 123
        rechnungsadresse = dict(name1='name1',
                                name2='name2',
                                name3='name3',
                                avisieren='+49 21 91 / 6 09 12-0',
                                strasse='Nicht Vergessen Weg 1',
                                ort='Rade',
                                land='DE')
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       rechnungsadresse=rechnungsadresse,
                       positionen=[])
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
        rechnungsadresse = dict(name1='name1',
                                name2='name2',
                                name3='name3',
                                avisieren='+49 21 91 / 6 09 12-0',
                                strasse='Nicht Vergessen Weg 1',
                                ort='Rade',
                                land='SI')
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[],
                       rechnungsadresse=rechnungsadresse)

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
        rechnungsadresse = dict(name1='name1',
                                name2='name2',
                                name3='name3',
                                avisieren='+49 21 91 / 6 09 12-0',
                                strasse='Nicht Vergessen Weg 1',
                                ort='Rade',
                                land='CH')
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[],
                       rechnungsadresse=rechnungsadresse)
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
        pos1 = dict(menge=10, artnr='11111', text_vor_position=['text for pos1', 'more text for pos1'])
        pos2 = dict(menge=20, artnr='22222/09', text_vor_position=['text for pos2', 'more text for pos2'])
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[pos1, pos2])
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        # kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKKDNR)"
        #     " VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','   17200')")

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
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[dict(menge=10, artnr='11111'), dict(menge=20, artnr='22222/09')])
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
        """Tests if orderlines containing an EAN can be converted to SQL."""
        vorgangsnummer = 123
        pos1 = dict(menge=10, ean='11111')
        pos2 = dict(menge=20, ean='22222')
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[pos1, pos2])
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
        pos1 = dict(menge=10, artnr='11111')
        pos2 = dict(menge=20, artnr='22222/09')
        auftrag = dict(kundennr='SC17200.444',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[pos1, pos2])
        kopf, positionen, dummy, dummy = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKVGPO, BKFNR, BKDTKD, BKVANR,"
            " BKKDNR) VALUES('1','123','xtodayx','1081230','1','2','01','xtodayx','444','   17200')")

        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(kopf.to_sql(), kpf_sql)

    def test_abgangslager(self):
        """Test if the 'abgangslager' can be converted to SQL."""
        vorgangsnummer = 123
        auftrag = dict(kundennr='SC17200',
                       abgangslager='26',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[])
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
        auftrag = dict(kundennr='SC17200',
                       herkunft='E',  # EDI
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[])
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
        auftrag = {'kundennr': 'SC17200',
                   'teillieferung_zulaessig': True,
                   'anlieferdatum_max': datetime.date(2008, 12, 30),
                   'positionen': []}
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)

        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKFNR, BKDTKD, BKKDNR)"
            " VALUES('1','123','xtodayx','1081230','1','1','01','xtodayx','   17200')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)

        vorgangsnummer = 124
        auftrag['teillieferung_zulaessig'] = False
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
        auftrag = dict(kundennr='SC17200',
                       auftragsart='WA',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[])
        kopf, positionen, texte, adressen = _auftrag2records(vorgangsnummer, auftrag)
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKVGNR, BKDTLT, BKDTKW, BKSBNR, BKFNR, BKKDNR, BKDTKD, BKAUFA)"
            " VALUES('1','123','xtodayx','1081230','1','01','   17200','xtodayx','WA')")
        # insert date of today since this will be automatically done by _auftrag2records()
        kpf_sql = kpf_sql.replace('xtodayx', date2softm(datetime.date.today()))

        self.assertEqual(kopf.to_sql(), kpf_sql)

        vorgangsnummer = 124
        auftrag['auftragsart'] = 'Z2'
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
        pos1 = dict(menge=10, ean='11111', verkaufspreis=10000.123)
        pos2 = dict(menge=20, ean='22222', verkaufspreis=0.123)
        auftrag = dict(kundennr='SC17200',
                       anlieferdatum_max=datetime.date(2008, 12, 30),
                       positionen=[pos1, pos2])

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
                 'kundennr': u'SC17200',
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

        heute = date2softm(datetime.date.today())
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKDTER, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGNR, BKVGPO, BKFNR, BKDTKD, BKKDNR) "
                   "VALUES('1','%s','1100303','1100303','1','1','123','3','01','%s','   17200')") % (heute, heute)
        self.assertEqual(kopf.to_sql(), kpf_sql)
        text_sql = ("INSERT INTO ABT00 (BTVGNR, BTTART, BTFNR, BTTX60, BTLFNR)"
                    " VALUES('123','8','01','#:guid:VS6RRW2MYL4FZ3PPMVH4ZRFE3A','1')")
        #self.assertEqual(len(texte), 1)
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
                 'kundennr': u'SC17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'Jägerwald 13',
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

        heute = date2softm(datetime.date.today())
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKDTER, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGNR, BKVGPO, BKFNR, BKDTKD, BKKDNR) "
                   "VALUES('1','%s','1100303','1100303','1','1','123','2','01','%s','   17200')") % (heute, heute)
        self.assertEqual(kopf.to_sql(), kpf_sql)
        text_sql = ("INSERT INTO ABT00 (BTVGNR, BTTART, BTFNR, BTTX60, BTLFNR)"
                    " VALUES('123','8','01','#:guid:VS6RRW2MYL4FZ3PPMVH4ZRFE3A','1')")
        #self.assertEqual(len(texte), 1)
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
                 'infotext_kunde': (u'Dieser Text sollte in mehreren Zeilen enden.\n'
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
                 'kundennr': u'SC17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'Jägerwald 13',
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

        heute = date2softm(datetime.date.today())
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKDTER, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGNR, BKVGPO, BKFNR, BKDTKD, BKKDNR) "
                   "VALUES('1','%s','1100303','1100303','1','1','123','2','01','%s','   17200')") % (heute, heute)
        #self.assertEqual(len(texte), 13)
        self.assertEqual(kopf.to_sql(), kpf_sql)
        text_sql = ("INSERT INTO ABT00 (BTVGNR, BTTART, BTFNR, BTTX60, BTLFNR)"
                    " VALUES('123','8','01','#:guid:VS6RRW2MYL4FZ3PPMVH4ZRFE3A','1')")
        #self.assertEqual(len(texte), 13)
        self.assertEqual(texte[0].to_sql(), text_sql)
        pos_sql = ("INSERT INTO ABA00 (BADTER, BAVGPO, BAABT, BAFNR, BAMNG, BAARTN, BAVGNR)"
                   " VALUES('xtodayx','1','1','01','1','14600/03','123')")
        pos_sql = pos_sql.replace('xtodayx', date2softm(datetime.date.today()))
        self.assertEqual(len(positionen), 2)
        self.assertEqual(positionen[0].to_sql(), pos_sql)
        self.assertEqual(adressen, [])

    def test_versandkosten(self):
        """Test order with given shippping costs"""
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
                 'kundennr': u'SC17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': '-UMFUHR-',
                 'name3': '',
                 'versandkosten': 1950,  # in Cent
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'strasse': u'Jägerwald 13',
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
        heute = date2softm(datetime.date.today())
        kpf_sql = ("INSERT INTO ABK00 (BKABT, BKDTER, BKDTLT, BKDTKW, BKVSK , BKSBNR, BKKZTF, BKVGNR, BKVGPO, BKFNR, BKDTKD, BKKDNR, BKX3LB) "
                   "VALUES('1','%s','1100303','1100303','19.50','1','1','123','2','01','%s','   17200',' 18')") % (heute, heute)
        self.assertEqual(kopf.to_sql(), kpf_sql)

    def test_abgangslager_auftragsart(self):
        """Test if the 'abgangslager' and 'auftragsart' can be converted to SQL."""
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
                 'kundennr': u'SC17200',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': 'Anlieferung Modul',
                 'name3': '',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'softmid': '17200/002',
                 'softmstatus': '',
                 'strasse': u'J\xe4gerwald 15',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'65240',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-24',
                                 u'menge': 1,
                                 'name': 'Test'}],
                 'tel': '+49 2191 60912 10'}
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order, auftragsart='ME', abgangslager=26)

        heute = date2softm(datetime.date.today())
        kpf_sql = ("INSERT INTO ABK00 (BKLGNR, BKABT, BKDTER, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGNR, BKVGPO, BKFNR, BKKDNR, BKDTKD, BKAUFA) "
                   "VALUES('26','1','%s','1100303','1100303','1','1','123','2','01','   17200','%s','ME')") % (heute, heute)
        self.assertEqual(kopf.to_sql(), kpf_sql)
        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVSTR, BVKZAD, BVVGNR, BVLKZ, BVNAME, BVPLZ, BVORT, BVAART) "
                        "VALUES('Anlieferung Modul','Jaegerwald 15','1','123','D','HUDORA GmbH','42897','Remscheid','1')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)

    def test_abweichende_lieferadresse(self):
        """Test if addresses can be converted to SQL."""
        vorgangsnummer = 123
        order = {'_id': '17200',
                 '_rev': '4-4bba80636c015f98e908c79c521e5124',
                 'sachbearbeiter': 'verkauf',
                 'iln': '4.00599800001e+12',
                 'anlieferdatum_von': u'2010-03-03',
                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A',
                 'infotext_kunde': u'',
                 'kundenauftragsnr': u'',
                 'kundennr': u'SC17200.444',
                 'land': 'DE',
                 'name1': 'HUDORA GmbH',
                 'name2': 'Anlieferung Modul',
                 'name3': '',
                 'ort': 'Remscheid',
                 'plz': '42897',
                 'softmstatus': '',
                 'strasse': u'J\xe4gerwald 15',
                 'orderlines': [{u'artnr': u'14600/03',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-0',
                                 u'menge': 1,
                                 'name': 'HUDORA Big Wheel silber, 125 mm Rolle'},
                                {u'artnr': u'65240',
                                 'guid': 'VS6RRW2MYL4FZ3PPMVH4ZRFE3A-24',
                                 u'menge': 1,
                                 'name': 'Test'}],
                 'tel': '+49 2191 60912 10'}
        kopf, positionen, texte, adressen = _order2records(vorgangsnummer, order, auftragsart='ME', abgangslager=26)

        heute = date2softm(datetime.date.today())
        kpf_sql = ("INSERT INTO ABK00 (BKLGNR, BKABT, BKDTER, BKDTLT, BKDTKW, BKSBNR, BKKZTF, BKVGNR, BKVGPO, BKFNR, BKKDNR, BKDTKD, BKVANR, BKAUFA) "
                   "VALUES('26','1','%s','1100303','1100303','1','1','123','2','01','   17200','%s','444','ME')") % (heute, heute)
        self.assertEqual(kopf.to_sql(), kpf_sql)
        adressen_sql = ("INSERT INTO ABV00 (BVNAM2, BVSTR, BVKZAD, BVVGNR, BVLKZ, BVNAME, BVPLZ, BVORT, BVAART) "
                        "VALUES('Anlieferung Modul','Jaegerwald 15','1','123','D','HUDORA GmbH','42897','Remscheid','1')")
        self.assertEqual(adressen[0].to_sql(), adressen_sql)


if __name__ == '__main__':
    unittest.main()
