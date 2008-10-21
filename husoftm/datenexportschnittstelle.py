#!/usr/bin/env python
# encoding: utf-8
"""
datenexportschnittstelle.py - Read the SoftM EDI Datenexportschnittstelle.

Based on  trunk/web/MoftS/lib/pySoftM/EDI.py

Created by Maximillian Dornseif on 2007-05-07.
Copyright (c) 2007, 2008 HUDORA GmbH. All rights reserved.

ACHTUNG: Die SoftM EDI-Schnittstelle ist - wie alles von SoftM - dürftig bis gar nicht dokumentiert. Das
bedeutet, dass dieser Code grosse Chancen bietet, nicht wie erwartet zu funktionieren. Er ist nur für
experimentelle Zwecke geeignet - oder für Leute mit starken Nerven.
Aber wenn sie schwache Nerven hätten, würden sie kein SoftM einsetzen, oder?

"""

# Benötigt das huProtocols toolkit von http://svn1.hosted-projects.com/hudora/public/huProtocols
import datetime
from edilib.recordbased import *
from pprint import pprint


doctext = """Diese Satzart enthält allgemeine Angaben zur empfangenen EDIFACT-Nachricht und kennzeichnet
jeweils den Beginn einer neuen Übertragung."""
FELDERXH = [
 # dict(length=35, startpos=1, endpos=35, name='uebertragungs_id'),
 dict(length=8, startpos=36, endpos=43, name='uebertragungs_datum',
      fieldclass=DateField, default=datetime.datetime.today),
 dict(length=4, startpos=44, endpos=47, name='uebertragungs_zeit',
      fieldclass=TimeField, default=datetime.datetime.now),
 dict(length=8, startpos=48, endpos=55, name='empfangsdatum',
      fieldclass=DateField, default=datetime.datetime.today),
 dict(length=4, startpos=56, endpos=59, name='empfangszeit',
      fieldclass=TimeField, default=datetime.datetime.now),
 # dict(length=35, startpos=60, endpos=94, name='logischer_dateiname'),
 dict(length=35, startpos=95, endpos=129, name='physischer_dateiname', choices=['XOO00']),
 dict(length=35, startpos=130, endpos=164, name='dfue_partner'),
 dict(length=8, startpos=165, endpos=172, name='nachrichtenart', choices=['      ']),
 dict(length=2, startpos=173, endpos=174, name='firma', choices=['01']),
 # dict(length=35, startpos=175, endpos=209, name='belegnummer'),
 dict(length=10, startpos=210, endpos=219, name='umgebung'),
 dict(length=10, startpos=220, endpos=229, name='sendestatus'),
 # dict(length=15, startpos=230, endpos=244, name='1.Res. 15 St.'),
 # dict(length=15, startpos=245, endpos=259, name='2.Res. 15 St.'),
 # dict(length=8, startpos=260, endpos=267, name='1.Res. 8 St.'),
 # dict(length=8, startpos=268, endpos=275, name='2.Res. 8 St.'),
 # dict(length=3, startpos=276, endpos=278, name='1.Res. 3 St.'),
 # dict(length=3, startpos=279, endpos=281, name='2.Res. 3 St.'),
 dict(length=2, startpos=282, endpos=283, name='testkennzeichen'),
 # dict(length=10, startpos=284, endpos=293, name='versionsnummer'),
 # dict(length=10, startpos=294, endpos=303, name='freigabenummer'),
 # dict(length=178, startpos=304, endpos=481, name='reserve_178'),
 dict(length=8, startpos=482, endpos=489, name='erstellungsdatum'),
 dict(length=6, startpos=490, endpos=495, name='erstellungszeit'),
 dict(length=1, startpos=496, endpos=496, name='status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERXH:
    feld['startpos'] = feld['startpos'] - 1
XHsatzklasse = generate_field_datensatz_class(FELDERXH, name='XHheader', length=496, doc=doctext)

doctext = """Kopfdaten (XOO00EF1) = Diese Satzart enthält die Kopfdaten einer Rechnung und kann beliebig
oft pro Übertragung vorkommen."""
FELDERF1 = [
 dict(length=3, startpos=1, endpos=3, name='Belegart'),
 dict(length=35, startpos=4, endpos=38, name='Belegnummer'),
 dict(length=8, startpos=39, endpos=46, name='Belegdatum'),
 dict(length=8, startpos=47, endpos=54, name='Liefertermin'),
 dict(length=35, startpos=55, endpos=89, name='Lieferschein'),
 dict(length=8, startpos=90, endpos=97, name='Lieferscheindatum'),
 dict(length=20, startpos=98, endpos=117, name='Kundenbestellnummer'),
 dict(length=8, startpos=118, endpos=125, name='Kundenbestelldatum'),
 dict(length=9, startpos=126, endpos=134, name='Auftrag'),
 dict(length=8, startpos=135, endpos=142, name='Auftragsdatum'),
 dict(length=17, startpos=143, endpos=159, name='ILN Rechnungsempfänger'),
 dict(length=17, startpos=160, endpos=176, name='Rechnungsempfänger'),
 dict(length=17, startpos=177, endpos=193, name='USt-IDNr. RgEmpf'),
 dict(length=17, startpos=194, endpos=210, name='eigene ILN beim RgEmpf'),
 dict(length=17, startpos=211, endpos=227, name='unsere LiNr beim RgEmpf'),
 dict(length=17, startpos=228, endpos=244, name='eigene USt-IDNr.'),
 dict(length=35, startpos=245, endpos=279, name='Rechnungsliste'),
 dict(length=8, startpos=280, endpos=287, name='Rechnungslistendatum'),
 dict(length=3, startpos=288, endpos=290, name='ISO-WSL'),
 dict(length=5, startpos=291, endpos=295, name='USt 1 für Skonto'),
 dict(length=5, startpos=296, endpos=300, name='USt 2 für Skonto'),
 dict(length=15, startpos=301, endpos=315, name='Skontofähig USt 1'),
 dict(length=15, startpos=316, endpos=330, name='Skontofähig USt 2'),
 dict(length=8, startpos=331, endpos=338, name='Skontodatum 1'),
 dict(length=3, startpos=339, endpos=341, name='Skontotage 1'),
 dict(length=5, startpos=342, endpos=346, name='Skonto 1'),
 dict(length=15, startpos=347, endpos=361, name='Skontobetrag 1 USt 1'),
 dict(length=15, startpos=362, endpos=376, name='Skontobetrag 1 USt 2'),
 dict(length=8, startpos=377, endpos=384, name='Skontodatum 2'),
 dict(length=3, startpos=385, endpos=387, name='Skontotage 2'),
 dict(length=5, startpos=388, endpos=392, name='Skonto 2'),
 dict(length=15, startpos=393, endpos=407, name='Skontobetrag 2 USt 1'),
 dict(length=15, startpos=408, endpos=422, name='Skontobetrag 2 USt 2'),
 dict(length=8, startpos=423, endpos=430, name='Nettodatum'),
 dict(length=3, startpos=431, endpos=433, name='Valutatage'),
 dict(length=8, startpos=434, endpos=441, name='Valutadatum'),
 dict(length=2, startpos=442, endpos=443, name='Firma'),
 dict(length=4, startpos=444, endpos=447, name='Abteilung'),
 dict(length=10, startpos=448, endpos=457, name='Bibliothek'),
 dict(length=3, startpos=458, endpos=460, name='Nettotage'),
 dict(length=17, startpos=461, endpos=477, name='ILN Besteller'),
 dict(length=18, startpos=478, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERF1:
    feld['startpos'] = feld['startpos'] - 1
F1satzklasse = generate_field_datensatz_class(FELDERF1, name='F1kopfdaten', length=496, doc=doctext)

doctext = 'XOO00EF2: Rechnungs-Kopf Lieferdaten'
FELDERF2 = [
 dict(length=17, startpos=1, endpos=17, name='ILN Warenempfänger'),
 dict(length=17, startpos=18, endpos=34, name='Warenempfänger'),
 dict(length=17, startpos=35, endpos=51, name='eigene ILN beim WaEmpf'),
 dict(length=17, startpos=52, endpos=68, name='unsere LiNr beim WaEmpf'),
 dict(length=17, startpos=69, endpos=85, name='ILN Lieferadresse'),
 dict(length=35, startpos=86, endpos=120, name='LfAdr: Name 1'),
 dict(length=35, startpos=121, endpos=155, name='LfAdr: Name 2'),
 dict(length=35, startpos=156, endpos=190, name='LfAdr: Name 3'),
 dict(length=35, startpos=191, endpos=225, name='LfAdr: Name 4'),
 dict(length=35, startpos=226, endpos=260, name='LfAdr: Strasse'),
 dict(length=3, startpos=261, endpos=263, name='LfAdr: Länderkennzeichen'),
 dict(length=9, startpos=264, endpos=272, name='LfAdr: Postleitzahl'),
 dict(length=35, startpos=273, endpos=307, name='LfAdr: Ort'),
 dict(length=30, startpos=308, endpos=337, name='Lagerbezeichnung'),
 dict(length=3, startpos=338, endpos=340, name='Versandart'),
 dict(length=3, startpos=341, endpos=343, name='Lieferbedingung'),
 dict(length=17, startpos=344, endpos=360, name='Verband'),
 dict(length=17, startpos=361, endpos=377, name='ILN Verband'),
 dict(length=118, startpos=378, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]


#    dict(length=8,  startpos=0,   endpos=  8, name='lieferdatum_fix', fieldclass=DateField),
#    dict(length=4,  startpos= 32, endpos= 36, name='lieferzeit_bis', fieldclass=TimeField),
#    dict(length=3,  startpos= 36, endpos= 39, name='valutatage', fieldclass=IntegerField),
#    dict(length= 5, startpos= 39, endpos= 44, name='rabatt1', fieldclass=DecimalField, precision=2,

# fix difference in array counting between SoftM and Python
for feld in FELDERF2:
    feld['startpos'] = feld['startpos'] - 1
F2satzklasse = generate_field_datensatz_class(FELDERF2, name='F2kopfdatenzusatz', length=496, doc=doctext)

doctext = "Rechnungs-Bankverbindung (XOO00EF8)"
FELDERF8 = [
 dict(length=35, startpos=1, endpos=35, name='Bankkonto-Nummer'),
 dict(length=15, startpos=36, endpos=50, name='Bankleitzahl'),
 dict(length=35, startpos=51, endpos=85, name='Name-1 der Bank'),
 dict(length=35, startpos=86, endpos=120, name='Name-2 der Bank'),
 dict(length=35, startpos=121, endpos=155, name='Straße'),
 dict(length=35, startpos=156, endpos=190, name='PLZ / Ort'),
 dict(length=200, startpos=191, endpos=390, name='Reserve 200'),
 dict(length=105, startpos=391, endpos=495, name='Reserve 105'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERF8:
    feld['startpos'] = feld['startpos'] - 1
F8satzklasse = generate_field_datensatz_class(FELDERF8, name='F8bankverbindung', length=496, doc=doctext)

doctext = 'B.6.9	Rechnungs-Endedaten (XOO00EF9)'
FELDERF9 = [
 dict(length=15, startpos=1, endpos=15, name='RG-Gesamtbetrag'),
 dict(length=15, startpos=16, endpos=30, name='Nettowarenwert'),
 dict(length=15, startpos=31, endpos=45, name='Skontofähig'),
 dict(length=15, startpos=46, endpos=60, name='steuerpflichtig USt 1'),
 dict(length=15, startpos=61, endpos=75, name='steuerpflichtig USt 2'),
 dict(length=15, startpos=76, endpos=90, name='Skonto-Abzug'),
 dict(length=15, startpos=91, endpos=105, name='Mehrwertsteuer'),
 dict(length=5, startpos=106, endpos=110, name='Steuersatz 1'),
 dict(length=15, startpos=111, endpos=125, name='Steuerbetrag 1'),
 dict(length=5, startpos=126, endpos=130, name='Steuersatz 2'),
 dict(length=15, startpos=131, endpos=145, name='Steuerbetrag 2'),
 dict(length=15, startpos=146, endpos=160, name='Nettowarenwert 1'),
 dict(length=15, startpos=161, endpos=175, name='Nettowarenwert 2'),
 dict(length=15, startpos=176, endpos=190, name='Versandkosten 1'),
 dict(length=15, startpos=191, endpos=205, name='Versandkosten 2'),
 dict(length=15, startpos=206, endpos=220, name='Verpackungskosten 1'),
 dict(length=15, startpos=221, endpos=235, name='Verpackungskosten 2'),
 dict(length=15, startpos=236, endpos=250, name='Nebenkosten 1'),
 dict(length=15, startpos=251, endpos=265, name='Nebenkosten 2'),
 dict(length=15, startpos=266, endpos=280, name='Summe Rabatte'),
 dict(length=1, startpos=281, endpos=281, name='Vorzeichen Summe Rabatte'),
 dict(length=15, startpos=282, endpos=296, name='Summe Zuschläge'),
 dict(length=1, startpos=297, endpos=297, name='Vorzeichen Summe Zuschläge'),
 dict(length=15, startpos=298, endpos=312, name='Kopfrabatt 1 in %'),
 dict(length=15, startpos=313, endpos=327, name='Kopfrabatt 2 in %'),
 dict(length=1, startpos=328, endpos=328, name='Vorzeichen Kopfrabatt 1'),
 dict(length=1, startpos=329, endpos=329, name='Vorzeichen Kopfrabatt 2'),
 dict(length=15, startpos=330, endpos=344, name='Kopfrabatt 1'),
 dict(length=15, startpos=345, endpos=359, name='Kopfrabatt 2'),
 dict(length=3, startpos=360, endpos=362, name='TxtSl Kopfrabatt 1'),
 dict(length=3, startpos=363, endpos=365, name='TxtSl Kopfrabatt 2'),
 dict(length=15, startpos=366, endpos=380, name='Kopfrabatt USt 1'),
 dict(length=15, startpos=381, endpos=395, name='Kopfrabatt USt 2'),
 dict(length=11, startpos=396, endpos=406, name='Gesamtgewicht brutto'),
 dict(length=11, startpos=407, endpos=417, name='Gesamtgewicht netto'),
 dict(length=4, startpos=418, endpos=421, name='Anzahl Positionen'),
 dict(length=74, startpos=422, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERF9:
    feld['startpos'] = feld['startpos'] - 1
F9satzklasse = generate_field_datensatz_class(FELDERF9, name='F9rechnungsendedaten', length=496, doc=doctext)

doctext = 'Rechnungs-Position (XOO00EF3)'
FELDERF3 = [
 dict(length=5, startpos=1, endpos=5, name='Position'),
 dict(length=35, startpos=6, endpos=40, name='Artikel'),
 dict(length=35, startpos=41, endpos=75, name='ArtikelNr Kunde'),
 dict(length=35, startpos=76, endpos=110, name='EAN'),
 dict(length=35, startpos=111, endpos=145, name='Zolltarifnummer'),
 dict(length=70, startpos=146, endpos=215, name='Artikelbezeichnung'),
 dict(length=70, startpos=216, endpos=285, name='Artikelbezeichnung Kunde'),
 dict(length=15, startpos=286, endpos=300, name='Menge'),
 dict(length=3, startpos=301, endpos=303, name='Mengeneinheit'),
 dict(length=15, startpos=304, endpos=318, name='Verkaufspreis'),
 dict(length=1, startpos=319, endpos=319, name='Vorzeichen Verkaufspreis'),
 dict(length=3, startpos=320, endpos=322, name='Mengeneinheit Preis'),
 dict(length=1, startpos=323, endpos=323, name='Preisdimension'),
 dict(length=15, startpos=324, endpos=338, name='Positionswert netto'),
 dict(length=15, startpos=339, endpos=353, name='Positionswert brutto'),
 dict(length=1, startpos=354, endpos=354, name='Vorzeichen Positionswert'),
 dict(length=2, startpos=355, endpos=356, name='Kz Mehrwertsteuer'),
 dict(length=5, startpos=357, endpos=361, name='Steuersatz in %'),
 dict(length=15, startpos=362, endpos=376, name='Steuerbetrag'),
 dict(length=1, startpos=377, endpos=377, name='Skontierfähig'),
 dict(length=11, startpos=378, endpos=388, name='Gewicht brutto'),
 dict(length=11, startpos=389, endpos=399, name='Gewicht netto'),
 dict(length=1, startpos=400, endpos=400, name='Mit Komponentenauflösung'),
 dict(length=5, startpos=401, endpos=405, name='Anzahl Komponenten'),
 dict(length=2, startpos=406, endpos=407, name='ISO Ursprungsland'),
 dict(length=88, startpos=408, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERF3:
    feld['startpos'] = feld['startpos'] - 1
F3satzklasse = generate_field_datensatz_class(FELDERF3, name='F3positionsdaten', length=496, doc=doctext)

doctext = 'Rechnungs-Position Rabatte (XOO00EF4)'
FELDERF4 = [
 dict(length=5, startpos=1, endpos=5, name='Position'),
 dict(length=15, startpos=6, endpos=20, name='Positionsrabatt Gesamt'),
 dict(length=15, startpos=21, endpos=35, name='Positionsrabatt 1 in %'),
 dict(length=1, startpos=36, endpos=36, name='Rabattkennzeichen 1'),
 dict(length=15, startpos=37, endpos=51, name='Rabattbetrag 1'),
 dict(length=1, startpos=52, endpos=52, name='Vorzeichen Rabatt 1'),
 dict(length=3, startpos=53, endpos=55, name='TxtSl Rabatt 1'),
 dict(length=15, startpos=56, endpos=70, name='Positionsrabatt 2 in %'),
 dict(length=1, startpos=71, endpos=71, name='Rabattkennzeichen 2'),
 dict(length=15, startpos=72, endpos=86, name='Rabattbetrag 2'),
 dict(length=1, startpos=87, endpos=87, name='Vorzeichen Rabatt 2'),
 dict(length=3, startpos=88, endpos=90, name='TxtSl Rabatt 2'),
 dict(length=15, startpos=91, endpos=105, name='Positionsrabatt 3 in %'),
 dict(length=1, startpos=106, endpos=106, name='Rabattkennzeichen 3'),
 dict(length=15, startpos=107, endpos=121, name='Rabattbetrag 3'),
 dict(length=1, startpos=122, endpos=122, name='Vorzeichen Rabatt 3'),
 dict(length=3, startpos=123, endpos=125, name='TxtSl Rabatt 3'),
 dict(length=15, startpos=126, endpos=140, name='Positionsrabatt 4 in %'),
 dict(length=1, startpos=141, endpos=141, name='Rabattkennzeichen 4'),
 dict(length=15, startpos=142, endpos=156, name='Rabattbetrag 4'),
 dict(length=1, startpos=157, endpos=157, name='Vorzeichen Rabatt 4'),
 dict(length=3, startpos=158, endpos=160, name='TxtSl Rabatt 4'),
 dict(length=15, startpos=161, endpos=175, name='Positionsrabatt 5 in %'),
 dict(length=1, startpos=176, endpos=176, name='Rabattkennzeichen 5'),
 dict(length=15, startpos=177, endpos=191, name='Rabattbetrag 5'),
 dict(length=1, startpos=192, endpos=192, name='Vorzeichen Rabatt 5'),
 dict(length=3, startpos=193, endpos=195, name='TxtSl Rabatt 5'),
 dict(length=15, startpos=196, endpos=210, name='Positionsrabatt 6 in %'),
 dict(length=1, startpos=211, endpos=211, name='Rabattkennzeichen 6'),
 dict(length=15, startpos=212, endpos=226, name='Rabattbetrag 6'),
 dict(length=1, startpos=227, endpos=227, name='Vorzeichen Rabatt 6'),
 dict(length=3, startpos=228, endpos=230, name='TxtSl Rabatt 6'),
 dict(length=15, startpos=231, endpos=245, name='Positionsrabatt 7 in %'),
 dict(length=1, startpos=246, endpos=246, name='Rabattkennzeichen 7'),
 dict(length=15, startpos=247, endpos=261, name='Rabattbetrag 7'),
 dict(length=1, startpos=262, endpos=262, name='Vorzeichen Rabatt 7'),
 dict(length=3, startpos=263, endpos=265, name='TxtSl Rabatt 7'),
 dict(length=15, startpos=266, endpos=280, name='Positionsrabatt 8 in %'),
 dict(length=1, startpos=281, endpos=281, name='Rabattkennzeichen 8'),
 dict(length=15, startpos=282, endpos=296, name='Rabattbetrag 8'),
 dict(length=1, startpos=297, endpos=297, name='Vorzeichen Rabatt 8'),
 dict(length=3, startpos=298, endpos=300, name='TxtSl Rabatt 8'),
 dict(length=35, startpos=301, endpos=335, name='Gebinde'),
 dict(length=35, startpos=336, endpos=370, name='Gebindebezeichnung'),
 dict(length=5, startpos=371, endpos=375, name='Gebindeanzahl Rechnung'),
 dict(length=15, startpos=376, endpos=390, name='Volumen'),
 dict(length=105, startpos=391, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERF4:
    feld['startpos'] = feld['startpos'] - 1
F4satzklasse = generate_field_datensatz_class(FELDERF4, name='F4positionsrabatte', length=496)

doctext = 'Rechnungsliste Position (XOO00ER2)'
FELDERER = [
 dict(length=17, startpos=1, endpos=17, name='ILN Rechnungsempfänger'),
 dict(length=17, startpos=18, endpos=34, name='Mitgliedsnummer'),
 dict(length=17, startpos=35, endpos=51, name='ILN Warenempfänger'),
 dict(length=9, startpos=52, endpos=60, name='Rechnungsliste'),
 dict(length=8, startpos=61, endpos=68, name='Rechnungslistendatum'),
 dict(length=5, startpos=69, endpos=73, name='Positionsnummer'),
 dict(length=9, startpos=74, endpos=82, name='Rechnung'),
 dict(length=8, startpos=83, endpos=90, name='Rechnungsdatum'),
 dict(length=8, startpos=91, endpos=98, name='Valutadatum'),
 dict(length=9, startpos=99, endpos=107, name='Lieferschein'),
 dict(length=8, startpos=108, endpos=115, name='Lieferdatum'),
 dict(length=9, startpos=116, endpos=124, name='Auftragsnummer'),
 dict(length=20, startpos=125, endpos=144, name='Kundenbestellnummer'),
 dict(length=8, startpos=145, endpos=152, name='Kundenbestelldatum'),
 dict(length=3, startpos=153, endpos=155, name='ISO-WSL'),
 dict(length=15, startpos=156, endpos=170, name='Warenwert gesamt'),
 dict(length=1, startpos=171, endpos=171, name='Vorzeichen Warenwert'),
 dict(length=15, startpos=172, endpos=186, name='Nebenkosten'),
 dict(length=1, startpos=187, endpos=187, name='Vorzeichen Nebenkosten'),
 dict(length=15, startpos=188, endpos=202, name='Verpackungskosten'),
 dict(length=1, startpos=203, endpos=203, name='Vorzeichen Verpackungskosten'),
 dict(length=15, startpos=204, endpos=218, name='Versandkosten'),
 dict(length=1, startpos=219, endpos=219, name='Vorzeichen Versandkosten'),
 dict(length=15, startpos=220, endpos=234, name='Skonto-Abzug'),
 dict(length=1, startpos=235, endpos=235, name='Vorzeichen Skonto-Abzug'),
 dict(length=2, startpos=236, endpos=237, name='Kz Mehrwertsteuer'),
 dict(length=5, startpos=238, endpos=242, name='Steuersatz in %'),
 dict(length=15, startpos=243, endpos=257, name='Steuerbetrag'),
 dict(length=1, startpos=258, endpos=258, name='Vorzeichen Steuerbetrag'),
 dict(length=15, startpos=259, endpos=273, name='Provision 1'),
 dict(length=1, startpos=274, endpos=274, name='Vorzeichen Prov halbe Steuer'),
 dict(length=15, startpos=275, endpos=289, name='Steuer zu Provision 1'),
 dict(length=1, startpos=290, endpos=290, name='Vorzeichen Steuer zu Prov 1'),
 dict(length=15, startpos=291, endpos=305, name='Provision 2'),
 dict(length=1, startpos=306, endpos=306, name='Vorzeichen Prov volle Steuer'),
 dict(length=15, startpos=307, endpos=321, name='Steuer zu Provision 2'),
 dict(length=1, startpos=322, endpos=322, name='Vorzeichen Steuer zu Prov 1'),
 dict(length=15, startpos=323, endpos=337, name='Rechnungsendbetrag'),
 dict(length=1, startpos=338, endpos=338, name='Vorzeichen Endbetrag'),
 dict(length=157, startpos=339, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERER:
    feld['startpos'] = feld['startpos'] - 1
ERsatzklasse = generate_field_datensatz_class(FELDERER, name='ERrechnungslisteposition',
                                              length=496, doc=doctext)

doctext = 'Auftrags-Kopf (XOO00EA1)'
FELDERA1 = [
 dict(length=3, startpos=1, endpos=3, name='Belegart'),
 dict(length=9, startpos=4, endpos=12, name='Auftrag'),
 dict(length=8, startpos=13, endpos=20, name='Auftragsdatum'),
 dict(length=8, startpos=21, endpos=28, name='AB Druckdatum'),
 dict(length=20, startpos=29, endpos=48, name='Kundenbestellnummer'),
 dict(length=8, startpos=49, endpos=56, name='Kundenbestelldatum'),
 dict(length=17, startpos=57, endpos=73, name='ILN Rechnungsempfänger'),
 dict(length=17, startpos=74, endpos=90, name='Rechnungsempfänger'),
 dict(length=17, startpos=91, endpos=107, name='USt-IDNr. RgEmpf'),
 dict(length=17, startpos=108, endpos=124, name='eigene ILN beim RgEmpf'),
 dict(length=17, startpos=125, endpos=141, name='unsere LiNr beim RgEmpf'),
 dict(length=17, startpos=142, endpos=158, name='eigene USt-IDNr.'),
 dict(length=3, startpos=159, endpos=161, name='ISO-WSL'),
 dict(length=5, startpos=162, endpos=166, name='USt 1 für Skonto'),
 dict(length=5, startpos=167, endpos=171, name='USt 2 für Skonto'),
 dict(length=15, startpos=172, endpos=186, name='Skontofähig USt 1'),
 dict(length=15, startpos=187, endpos=201, name='Skontofähig USt 2'),
 dict(length=3, startpos=202, endpos=204, name='Skontotage 1'),
 dict(length=5, startpos=205, endpos=209, name='Skonto 1'),
 dict(length=15, startpos=210, endpos=224, name='Skontobetrag 1 USt 1'),
 dict(length=15, startpos=225, endpos=239, name='Skontobetrag 1 USt 2'),
 dict(length=3, startpos=240, endpos=242, name='Skontotage 2'),
 dict(length=5, startpos=243, endpos=247, name='Skonto 2'),
 dict(length=15, startpos=248, endpos=262, name='Skontobetrag 2 USt 1'),
 dict(length=15, startpos=263, endpos=277, name='Skontobetrag 2 USt 2'),
 dict(length=60, startpos=278, endpos=337, name='Skontotext'),
 dict(length=2, startpos=338, endpos=339, name='Firma'),
 dict(length=4, startpos=340, endpos=343, name='Abteilung'),
 dict(length=10, startpos=344, endpos=353, name='Bibliothek'),
 dict(length=142, startpos=354, endpos=495, name='Reserve'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERA1:
    feld['startpos'] = feld['startpos'] - 1
A1satzklasse = generate_field_datensatz_class(FELDERA1, name='A1auftragskopf', length=496, doc=doctext)

doctext = 'Rechnungsliste Verband (XOO00ER1)'
FELDERR1 = [
 dict(length=17, startpos=1, endpos=17, name='ILN Verband'),
 dict(length=17, startpos=18, endpos=34, name='Abs.: eigene ILN'),
 dict(length=17, startpos=35, endpos=51, name='Abs.: unsere Nr.beim Verband'),
 dict(length=17, startpos=52, endpos=68, name='Abs.: UST-Identnummer'),
 dict(length=2, startpos=69, endpos=70, name='Firma'),
 dict(length=4, startpos=71, endpos=74, name='Abteilung'),
 dict(length=10, startpos=75, endpos=84, name='Bibliothek'),
 dict(length=200, startpos=85, endpos=284, name='Reserve 1'),
 dict(length=200, startpos=285, endpos=484, name='Reserve 2'),
 dict(length=11, startpos=485, endpos=495, name='Reserve 3'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERR1:
    feld['startpos'] = feld['startpos'] - 1
R1satzklasse = generate_field_datensatz_class(FELDERR1, name='R1verbandsrechnungsliste',
                                              length=496, doc=doctext)


doctext = 'Rechnungsliste Position (XOO00ER2)'
FELDERR2 = [
    dict(length=17, startpos=1, endpos=17, name='ILN Rechnungsempfänger'),
    dict(length=17, startpos=18, endpos=34, name='Mitgliedsnummer'),
    dict(length=17, startpos=35, endpos=51, name='ILN Warenempfänger'),
    dict(length=9, startpos=52, endpos=60, name='Rechnungsliste'),
    dict(length=8, startpos=61, endpos=68, name='Rechnungslistendatum'),
    dict(length=5, startpos=69, endpos=73, name='Positionsnummer'),
    dict(length=9, startpos=74, endpos=82, name='Rechnung'),
    dict(length=8, startpos=83, endpos=90, name='Rechnungsdatum'),
    dict(length=8, startpos=91, endpos=98, name='Valutadatum'),
    dict(length=9, startpos=99, endpos=107, name='Lieferschein'),
    dict(length=8, startpos=108, endpos=115, name='Lieferdatum'),
    dict(length=9, startpos=116, endpos=124, name='Auftragsnummer'),
    dict(length=20, startpos=125, endpos=144, name='Kundenbestellnummer'),
    dict(length=8, startpos=145, endpos=152, name='Kundenbestelldatum'),
    dict(length=3, startpos=153, endpos=155, name='ISO-WSL'),
    dict(length=15, startpos=156, endpos=170, name='Warenwert gesamt'),
    dict(length=1, startpos=171, endpos=171, name='Vorzeichen Warenwert'),
    dict(length=15, startpos=172, endpos=186, name='Nebenkosten'),
    dict(length=1, startpos=187, endpos=187, name='Vorzeichen Nebenkosten'),
    dict(length=15, startpos=188, endpos=202, name='Verpackungskosten'),
    dict(length=1, startpos=203, endpos=203, name='Vorzeichen Verpackungskosten'),
    dict(length=15, startpos=204, endpos=218, name='Versandkosten'),
    dict(length=1, startpos=219, endpos=219, name='Vorzeichen Versandkosten'),
    dict(length=15, startpos=220, endpos=234, name='Skonto-Abzug'),
    dict(length=1, startpos=235, endpos=235, name='Vorzeichen Skonto-Abzug'),
    dict(length=2, startpos=236, endpos=237, name='Kz Mehrwertsteuer'),
    dict(length=5, startpos=238, endpos=242, name='Steuersatz in %'),
    dict(length=15, startpos=243, endpos=257, name='Steuerbetrag'),
    dict(length=1, startpos=258, endpos=258, name='Vorzeichen Steuerbetrag'),
    dict(length=15, startpos=259, endpos=273, name='Provision 1'),
    dict(length=1, startpos=274, endpos=274, name='Vorzeichen Prov halbe Steuer'),
    dict(length=15, startpos=275, endpos=289, name='Steuer zu Provision 1'),
    dict(length=1, startpos=290, endpos=290, name='Vorzeichen Steuer zu Prov 1'),
    dict(length=15, startpos=291, endpos=305, name='Provision 2'),
    dict(length=1, startpos=306, endpos=306, name='Vorzeichen Prov volle Steuer'),
    dict(length=15, startpos=307, endpos=321, name='Steuer zu Provision 2'),
    dict(length=1, startpos=322, endpos=322, name='Vorzeichen Steuer zu Prov 1'),
    dict(length=15, startpos=323, endpos=337, name='Rechnungsendbetrag'),
    dict(length=1, startpos=338, endpos=338, name='Vorzeichen Endbetrag'),
    dict(length=157, startpos=339, endpos=495, name='Reserve'),
    dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERR2:
    feld['startpos'] = feld['startpos'] - 1
R2satzklasse = generate_field_datensatz_class(FELDERR2, name='R2rechnungslisteposition',
                                              length=496, doc=doctext)


doctext = 'Rechnungsliste Summe (XOO00ER3)'
FELDERR3 = [
 dict(length=5, startpos=1, endpos=5, name='Anzahl Positionen'),
 dict(length=15, startpos=6, endpos=20, name='Summe Rechnungsendbetrag'),
 dict(length=1, startpos=21, endpos=21, name='Vorzeichen Endbetrag'),
 dict(length=3, startpos=22, endpos=24, name='ISO-WSL'),
 dict(length=12, startpos=25, endpos=36, name='Umrechnungskurs'),
 dict(length=1, startpos=37, endpos=37, name='Faktor für Umrechnungskurs'),
 dict(length=200, startpos=38, endpos=237, name='Reserve 1'),
 dict(length=200, startpos=238, endpos=437, name='Reserve 2'),
 dict(length=58, startpos=438, endpos=495, name='Reserve 3'),
 dict(length=1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERR3:
    feld['startpos'] = feld['startpos'] - 1
R3satzklasse = generate_field_datensatz_class(FELDERR3, name='R3verbandsrechnungslistensummen',
                                              length=496, doc=doctext)

FELDERTEXT = [
 dict(length=60, startpos=  1, endpos= 60, name='Textzeile 1'),
 dict(length=60, startpos= 61, endpos=120, name='Textzeile 2'),
 dict(length=60, startpos=121, endpos=180, name='Textzeile 3'),
 dict(length=60, startpos=181, endpos=240, name='Textzeile 4'),
 dict(length=60, startpos=241, endpos=300, name='Textzeile 5'),
 dict(length=60, startpos=301, endpos=360, name='Textzeile 6'),
 dict(length=60, startpos=361, endpos=420, name='Textzeile 7'),
 dict(length=60, startpos=421, endpos=480, name='Textzeile 8'),
 dict(length=15, startpos=481, endpos=495, name='Reserve'),
 dict(length= 1, startpos=496, endpos=496, name='Status'),
]
# fix difference in array counting between SoftM and Python
for feld in FELDERTEXT:
    feld['startpos'] = feld['startpos'] - 1
TEXTsatzklasse = generate_field_datensatz_class(FELDERTEXT, name='generic_text', length=496)

def parseTest():
    fd = open('example/RL00381.TXT')
    satzresolver = dict(XH=XHsatzklasse,
                        F1=F1satzklasse,
                        F2=F2satzklasse,
                        F8=F8satzklasse,
                        F9=F9satzklasse,
                        F3=F3satzklasse,
                        F4=F4satzklasse,
                        ER=ERsatzklasse,
                        A1=A1satzklasse,
                        R1=R1satzklasse,
                        R2=R2satzklasse,
                        R3=R3satzklasse,
                        FP=TEXTsatzklasse,
                        FA=TEXTsatzklasse,
                        )
    for rawline in fd:
        # remove newline & EOF
        line = rawline.rstrip('\r\n').strip('\x1a')
        if not line:
            # skip empty lines
            continue
        # remove erstellungsdatum
        erstellungsdatum = line[519:]
        line = line[:519]
        # remove line-header
        line = line[19:]
        # pad line if it is to short now
        line = "% 500s" % line
        satzart, version, data = line[:2], line[2:4], line[4:]
        satzklasse = satzresolver.get(satzart, None)
        print satzart, version, repr(erstellungsdatum), len(line), len(data)
        if satzklasse:
            satz = satzklasse()
            satz.parse(data)
            pprint(satz.fields())
        else:
            print '...................................'
            print "unbelkannter Satz:", satzart, version
            print repr(rawline)
            print '^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^'

parseTest()
