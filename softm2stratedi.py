#!/usr/bin/env python
# encoding: utf-8
"""
softm2stratedi.py

Created by Maximillian Dornseif on 2008-10-31.
Copyright (c) 2008 HUDORA. All rights reserved.
"""

import unittest
from husoftm.datenexportschnittstelle import *

def convert_transmissionhead(transmission_records, previous_output_records):
    rec000 = opbject() # interchangeheader000
    rec000.sender_iln = 
    rec000.empfaenger_iln = xh.dfue_partner
    rec000.erstellungsdatum = xh.erstellungs_datum
    rec000.erstellungszeit = xh.erstellungs_zeit
    # Fortlaufende achtstellige Sendenummer
    rec000.datenaustauschreferenz = XXX
    rec000.referenznummer = xh.dateiname
    rec000.anwendungsreferenz = xh.umgebung
    rec000.testkennzeichen = xh.testkennzeichen
    return [rec000]
    


def convert_invoice_head(invoice_records, previous_output_records):
    rec900 = object()
    f1 = invoicerecords['f1']
    f2 = invoicerecords['f2']

    rec100 = opbject()
    rec119_lierferaddr = object()
    rec119_rechnungsaddr = object()
    rec119_verkaeferaddr = object()
    # Eindeutige Nachrichtenreferenz des Absenders; laufende Nummer der Nachrichten im Datenaustausch
    # beginnt mit "1" und wird für jede Rechnung/Gutschrift innerhalb einer Übertragungsdatei
    # um 1 erhöht.
    rec100.referenz 
    
    rec100.belegnummer = f1.belegnummer
    rec100.belegdatum = f1.belegdatum
    
    # Lieferant
    rec119_verkaeferaddr.partnerart = 'SU'
    rec119_verkaeferaddr.iln = f1.eigene_iln_beim_kunden
    # TODO: was ist der Unterschied zwischen ustdid und steuernr?
    rec119_verkaeferaddr.ustdid = f1.ustdid_absender
    rec119_verkaeferaddr.steuernr = f1.ustdid_absender
    rec119_verkaeferaddr.weeenr = 'DE 70323035'
    rec119_verkaeferaddr.fax = '+49 2191 60912-50'
    rec119_verkaeferaddr.tel = '+49 2191 60912-0'

    # Warenempfänger
    rec119_lierferaddr.partnerart = 'DP'
    rec119_lierferaddr.iln = f2.iln_warenempfaenger
    rec119_lierferaddr.name1 = f2.liefer_name1
    rec119_lierferaddr.name2 = f2.liefer_name2
    rec119_lierferaddr.name3 = f2.liefer_name3
    rec119_lierferaddr.strasse1 = f2.liefer_strasse
    rec119_lierferaddr.plz = f2.liefer_plz
    rec119_lierferaddr.ort = f2.liefer_ort
    rec119_lierferaddr.land = f2.liefer_land
    rec119_lierferaddr.internepartnerid = f2.warenempfaenger

    # Rechnungsempfänfger
    rec119_rechnungsaddr.partnerart = 'IV'
    rec119_rechnungsaddr.iln = f1.iln_rechnungsempfaenger
    rec119_rechnungsaddr.internepartnerid = f1.rechnungsempfaenger
    rec119_rechnungsaddr.gegebenepartnerid = f1.lieferantennummer
    rec119_rechnungsaddr.ustdid = f1.ustdid_rechnungsempfaenger
    # rec119_rechnungsaddr.partnerabteilung
    
    # Nicht genutzte Felder aus SoftM
    # a1.Belegart'),
    # a1.Auftrag'),
    # a1.Auftragsdatum'),
    # a1.AB Druckdatum'),
    # a1.Kundenbestellnummer'),
    # a1.Kundenbestelldatum'),
    # a1.ILN Rechnungsempfänger'),
    # a1.Rechnungsempfänger'),
    # a1.USt-IDNr. RgEmpf'),
    # a1.eigene ILN beim RgEmpf'),
    # a1.unsere LiNr beim RgEmpf'),
    # a1.eigene USt-IDNr.'),
    # a1.ISO-WSL'),
    # a1.USt 1 für Skonto'),
    # a1.USt 2 für Skonto'),
    # a1.Skontofähig USt 1'),
    # a1.Skontofähig USt 2'),
    # a1.Skontotage 1'),
    # a1.Skonto 1'),
    # a1.Skontobetrag 1 USt 1'),
    # a1.Skontobetrag 1 USt 2'),
    # a1.Skontotage 2'),
    # a1.Skonto 2'),
    # a1.Skontobetrag 2 USt 1'),
    # a1.Skontobetrag 2 USt 2'),
    # a1.Skontotext'),
    # f1.liefertermin', fieldclass=DateField),
    # f1.lieferscheinnr', fieldclass=IntegerField),
    # f1.lieferscheindatum', fieldclass=DateField),
    # f1.kundenbestellnummer'),
    # f1.kundenbestelldatum', fieldclass=DateField),
    # f1.auftragsnr', fieldclass=IntegerField),
    # f1.auftragsdatum', fieldclass=DateField),
    # f1.'rechnungsliste', fieldclass=IntegerField),
    # f1.rechnungslistendatum', fieldclass=DateField),
    # f1.waehrung', fieldclass=FixedField, default='EUR',
    # f1.
    # f1.ust1_fuer_skonto', fieldclass=DecimalFieldNoDot, precision=2),
    # f1.ust2_fuer_skonto', fieldclass=DecimalFieldNoDot, precision=2),
    # f1.'Skontofähig USt 1'),
    # f1.'Skontofähig USt 2'),
    # f1.Skontodatum 1'),
    # f1.Skontotage 1'),
    # f1.Skonto 1'),
    # f1.'Skontobetrag 1 USt 1'),
    # f1.'Skontobetrag 1 USt 2'),
    # f1.Skontodatum 2'),
    # f1.Skontotage 2'),
    # f1.Skonto 2'),
    # f1.'Skontobetrag 2 USt 1'),
    # f1.'Skontobetrag 2 USt 2'),
    # f1.='Nettodatum'),
    # f1.valutatage', fieldclass=IntegerField),
    # f1.='valutadatum', fieldclass=DateField),
    # f1.Firma'), # , fieldclass='FixedField', default='01'),
    # f1.Abteilung'),
    # f1.'Bibliothek'),
    # f1.nettotage'),
    # f1.e='iln_besteller', fieldclass=EanField),
    # f1.e='Reserve', fieldclass=FixedField, default=' ' *18),
    # f1.Status', fieldclass=FixedField, default=' '),
    # f2.'Lagerbezeichnung'),
    # f2.versandart'),
    # f2.lieferbedingung'),
    # f2.verband', fieldclass=IntegerField),
    # f2.verband_iln', fieldclass=EanField),
    
    return [rec100, rec119_lierferaddr, rec119_rechnungsaddr, rec119_verkaeferaddr]




def convert_invoice_position():
    rec500 = opbject()
    rec500.positionsnummer = f3.positionsnr
    rec500.ean = f3.ean
    rec500.artnr_lieferant = f3.artnr
    rec500.artnr_kunde = f3.artnr_kunde
    rec500.artikelbezeichnung1 = f3.artikelbezeichnung[:35]
    rec500.artikelbezeichnung2 = f3.artikelbezeichnung[35:70]
    rec500.berechnete_menge = f3.menge
    rec500.menge_ohne_berechnung = 0
    # rec500.waehrung
    rec500.mwstsatz = f3.steuersatz
    # rec500.nettostueckpreis
    # rec500.bruttostueckpreis
    rec500.mengeneinheit = f3.mengeneinheit
    
    # MOA-5004 Nettowarenwert = Menge x Bruttopreis ./. Artikelrabatte bzw. Menge x Nettopreis (Rabatte sind im Preis eingerechnet) 
    # Bei Gutschriftspositionen ist der Nettowarenwert negativ einzustellen.
    rec500.nettowarenwert = f3.wert_netto
    # MOA-5004 Bruttowarenwert = Menge x Bruttopreis ohne MWSt., vor Abzug der Artikelrabatte
    rec500.bruttowarenwert = f3.wert_brutto
    
    # MOA-5004 Summe aller Zu- und Abschläge aus Satzart(en) 513 mit vorzeichengerechter Darstellung
    # rec500.summeabschlaege
    
    # Daten aus SoftM bisher nicht verwendet werden:
    # f3.zolltarifnummer
    # f3.artikelbezeichnung_kunde
    # f3.verkaufspreis
    # f3.verkaufspreis_vorzeichen
    # f3.Mengeneinheit Preis
    # f3.preisdimension
    # f3.Vorzeichen Positionswert
    # f3.mehrwertsteuer_kz,
    # f3.Steuerbetrag
    # f3.skontierfaehig
    # f3.ursprungsland


def convert_invoice_footer(invoice_records, previous_output_records):
    rec900 = object()
    f9 = invoicerecords['f9']
    
    rec900.rechnungsendbetrag = f9.gesamtbetrag
    rec900.mwst_gesammtbetrag = f9.Mehrwertsteuer
    rec900.nettowarenwert_gesammt = f9.nettowarenwert
    rec900.steuerpflichtiger_betrag = f9.steuerpflichtig1
    rec900.skontofaehiger_betrag = f9.skontofaehig
    # Ist das nur Zuschlaege oder Zuschlaege + Rabatte?
    rec900.zu_und_abschlage = f9.zuschlaege
    # Vorzeichen muss noch eingearbeitet werden.
    # f9.Vorzeichen Summe Zuschläge'),
    # rec900.gesammt_verkaufswert
    
    # Nicht genutzte Felder aus SoftM
    # f9.steuerpflichtig USt 2', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Skonto-Abzug', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Steuersatz 1', fieldclass=DecimalFieldNoDot, precision=2),
    # f9.Steuerbetrag 1', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Steuersatz 2', fieldclass=DecimalFieldNoDot, precision=2),
    # f9.Steuerbetrag 2', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Nettowarenwert 1', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Nettowarenwert 2', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Versandkosten 1', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Versandkosten 2', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Verpackungskosten 1', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Verpackungskosten 2', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Nebenkosten 1', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Nebenkosten 2', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Summe Rabatte', fieldclass=DecimalFieldNoDot, precision=3),
    # f9.Vorzeichen Summe Rabatte'),
    # f9.Kopfrabatt 1 in %'),
    # f9.Kopfrabatt 2 in %'),
    # f9.Vorzeichen Kopfrabatt 1'),
    # f9.Vorzeichen Kopfrabatt 2'),
    # f9.Kopfrabatt 1'),
    # f9.Kopfrabatt 2'),
    # f9.TxtSl Kopfrabatt 1'),
    # f9.TxtSl Kopfrabatt 2'),
    # f9.Kopfrabatt USt 1'),
    # f9.Kopfrabatt USt 2'),
    # f9.Gesamtgewicht brutto'),
    # f9.Gesamtgewicht netto'),
    # f9.Anzahl Positionen'),
    return [rec900]


def convert_invoice(softm_record_list, stratedi_records):
    
    softm_records = dict(softm_record_list)
    stratedi_records.append(convert_invoice_head(x, stratedi_records))
    
    # the now we have to extract the per invoice records from softm_record_list
    # every position starts with a F3 record
    tmp_softm_record_list = softm_record_list[:] # copy list
    
    # remove everything until we hit the first F3
    while tmp_softm_record_list[0][0] != 'F3':
        tmp_softm_record_list.pop(0)
    
    while tmp_softm_record_list:
        # slice of segment untill the next F3
        position = []
        while tmp_softm_record_list[0][0] != 'F3':
            position.append(tmp_softm_record_list.pop(0))
        
        # process position
        stratedi_records.append(convert_invoice_positions(position, stratedi_records))
    
    stratedi_records.append(convet_invoice_footer(x, stratedi_records))
    return stratedi_records

def convert(softm_record_list):
    """Convert a SoftM Transmission to StratEDI Format"""
    
    stratedi_records = []
    softm_records = dict(softm_record_list)
    stratedi_records.append(convert_transmissionhead(x, stratedi_records))
    stratedi_records.append(convert_invoice(x, stratedi_records))
    return stratedi_records

def read_softm():
    softm_record_list = parse_to_objects('INVOIC/4333936000001/RG00073.TXT')