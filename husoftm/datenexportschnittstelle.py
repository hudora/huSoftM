#!/usr/bin/env python
# encoding: utf-8
"""
EDI.py - Ansprechen der SoftM EDI Schnittstelle.

DIESER CODE WIRD ZUR ZEIT NICHT AKTIV GENUTZT. Siehe stattdessen stapelschnittstelle.py

ACHTUNG: Die SoftM EDI-Schnittstelle ist - wie alles von SoftM - dürftig bis gar nicht dokumentiert. Das
bedeutet, dass dieser Code grosse Chancen bietet, nicht wie erwartet zu funktionieren. Er ist nur für
experimentelle Zwecke geeignet - oder für Leute mit starken Nerven.
Aber wenn sie schwache Nerven hätten, würden sie kein SoftM einsetzen, oder?

Created by Maximillian Dornseif on 2007-05-07.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

# Benötigt das huProtocols toolkit von http://svn1.hosted-projects.com/hudora/public/huProtocols
import datetime
from huProtocols.recordbased import *
from pprint import pprint

doctext = """Diese Satzart enthält allgemeine Angaben zur empfangenen EDIFACT-Nachricht und kennzeichnet
jeweils den Beginn einer neuen Übertragung. Der Aufbau ist für alle eingehenden EDIFACT-Nachrichten identisch."""
FELDERXH = [
    dict(length=35, startpos=  0, endpos= 35, name='uebertragungs_id'),
    dict(length= 8, startpos= 35, endpos= 43, name='uebertragungs_datum',
         fieldclass=DateField, default=datetime.datetime.today),
    dict(length= 4, startpos= 43, endpos= 47, name='uebertragungs_zeit',
         fieldclass=TimeField, default=datetime.datetime.now),
    dict(length= 8, startpos= 47, endpos= 55, name='empfangsdatum',
         fieldclass=DateField, default=datetime.datetime.today),
    dict(length= 4, startpos= 55, endpos= 59, name='empfangszeit',
         fieldclass=TimeField, default=datetime.datetime.now),
    dict(length=35, startpos= 59, endpos= 94, name='logischer_dateiname'),
    dict(length=35, startpos= 94, endpos=129, name='physischer_dateiname'),
    dict(length=35, startpos=129, endpos=164, name='dfue_partner'),
    dict(length= 8, startpos=164, endpos=172, name='nachrichtenart', choices=['ORDERS'],
         doc='Gibt die Art der EDIFACT/EANCOM Nachricht an. Bisher ist nur ORDERS implementiert.'),
    #dict(length=15, startpos=172, endpos=187, name='1.Res. 15 St.'),
    #dict(length=15, startpos=187, endpos=202, name='2.Res. 15 St.'),
    #dict(length= 8, startpos=202, endpos=210, name='1.Res. 8 St.'),
    #dict(length= 8, startpos=210, endpos=218, name='2.Res. 8 St.'),
    #dict(length= 3, startpos=218, endpos=221, name='1.Res. 3 St.'),
    #dict(length= 3, startpos=221, endpos=224, name='2.Res. 3 St.'),
    #dict(length=271,startpos=224, endpos=495, name='Reserve 271'),
    dict(length= 1, startpos=495, endpos=496, name='status', 
         doc="""Bedeutung des Feldes ist unbekannt. Wird vermutlich bei der Weiterverarbeitung innerhalb der
         SoftM Suite verwendet."""),
    ]
XHsatzklasse = generate_field_datensatz_class(FELDERXH, name='XHheader', length=496, doc=doctext)

doctext = """Kopfdaten (XOI00EA1) = Diese Satzart enthält die Kopfdaten eines Auftrags und kann beliebig 
oft pro Übertragung vorkommen."""
FELDERA1 = [
    dict(length=35, startpos= 0,  endpos=35,  name='belegnummer'),
    dict(length=17, startpos=35,  endpos=52,  name='iln_kunde',     fieldclass=EanField,
         doc="""ILN des Kunden, der die Bestellung auslöst. Das Zusammenspiel mit der Kundennummer und
         Adresse ist unklar. Scheinbar ist die ein Pflichtfeld."""),
    dict(length=17, startpos=52,  endpos=69,  name='iln_lieferant', fieldclass=EanField,
         doc="""Hier scheint unsere eigene ILN hin zu gehören. Scheinbar ist die ein Pflichtfeld."""),
    dict(length=17, startpos=69,  endpos=86,  name='kundennummer'),
    dict(length=17, startpos=86,  endpos=103, name='lieferantennummer_kunde',
         doc="""Unsere Lieferantennummer beim Kunden ???"""),
    dict(length=35, startpos=103, endpos=138, name='name1'),
    dict(length=35, startpos=138, endpos=173, name='name2'),
    dict(length=35, startpos=173, endpos=208, name='strasse'),
    dict(length=3,  startpos=208, endpos=211, name='land'), # which notation?
    dict(length=9,  startpos=211, endpos=220, name='plz'),
    dict(length=35, startpos=220, endpos=255, name='ort'),
    dict(length=17, startpos=255, endpos=272, name='ustid'),
    dict(length=8,  startpos=272, endpos=280, name='belegdatum', fieldclass=DateField,
         default=datetime.date.today()),
    dict(length=8,  startpos=280, endpos=288, name='eingangsdatum', fieldclass=DateField),
    dict(length=17, startpos=288, endpos=305, name='sachbearbeiter_id'), # references what?
    dict(length=35, startpos=305, endpos=340, name='sachbearbeiter'), # freetext?
    dict(length=3,  startpos=340, endpos=343, name='iso'), # ???
    dict(length=4,  startpos=343, endpos=347, name='waehrungsbasis', choices=['EUR'], default='EUR'),
    dict(length=3,  startpos=347, endpos=350, name='belegart'), # ???
    dict(length=2,  startpos=350, endpos=352, name='function'), # , choices=['ORDERS']),
    dict(length=12, startpos=352, endpos=364, name='umrechnungskurs'),
    dict(length=2,  startpos=364, endpos=366, name='firma'),
    dict(length=4,  startpos=366, endpos=370, name='abteilung', fieldclass=IntegerField), # references what?
    dict(length=10, startpos=370, endpos=380, name='bibliothek'), # ???
]
A1satzklasse = generate_field_datensatz_class(FELDERA1, name='A1kopfdaten', length=496, doc=doctext)

# Kopfdaten – Zusatz (XOI00EA2)
# Diese Satzart enthält weitere Zusatzdaten zu den Kopfdaten eines Auftrags.
FELDERA2 = [
    dict(length=8,  startpos=0,   endpos=  8, name='lieferdatum_fix', fieldclass=DateField),
    dict(length=8,  startpos=8,   endpos= 16, name='lieferdatum_von', fieldclass=DateField),
    dict(length=8,  startpos=16,  endpos= 24, name='lieferdatum_bis', fieldclass=DateField),
    dict(length=4,  startpos= 24, endpos= 28, name='lieferzeit_fix',  fieldclass=TimeField),
    dict(length=4,  startpos= 28, endpos= 32, name='lieferzeit_von',  fieldclass=TimeField),
    dict(length=4,  startpos= 32, endpos= 36, name='lieferzeit_bis',  fieldclass=TimeField),
    dict(length=3,  startpos= 36, endpos= 39, name='valutatage',  fieldclass=IntegerField),
    dict(length= 5, startpos= 39, endpos= 44, name='rabatt1',  fieldclass=DecimalField, precision=2,
         doc="Rabat Prozentsatz"),
    dict(length= 3, startpos= 44, endpos= 47, name='rabattschluessel1', fieldclass=IntegerField),
    dict(length= 5, startpos= 47, endpos= 52, name='rabatt2',  fieldclass=DecimalField, precision=2,
         doc="Rabat Prozentsatz"),
    dict(length= 3, startpos= 52, endpos= 55, name='rabattschluessel2', fieldclass=IntegerField),
    dict(length= 5, startpos= 55, endpos= 60, name='rabatt3',  fieldclass=DecimalField, precision=2,
         doc="Rabat Prozentsatz"),
    dict(length= 3, startpos= 60, endpos= 63, name='rabattschluessel3', fieldclass=IntegerField),
    dict(length= 3, startpos= 63, endpos= 66, name='werbeaktion'),
    dict(length= 3, startpos= 66, endpos= 69, name='versandart'),
    dict(length= 3, startpos= 69, endpos= 72, name='lieferbedingung'),
    dict(length= 3, startpos= 72, endpos= 75, name='zahlungsbedingung'),
    dict(length= 3, startpos= 75, endpos= 78, name='referenzcode1'),
    dict(length=35, startpos= 78, endpos=113, name='referenz1'),
    dict(length= 3, startpos=113, endpos=116, name='referenzcode2'),
    dict(length=35, startpos=116, endpos=151, name='referenz2'),
    dict(length= 3, startpos=151, endpos=154, name='referenzcode3'),
    dict(length=35, startpos=154, endpos=189, name='referenz3'),
    dict(length=70, startpos=189, endpos=259, name='kopftext1'),
    dict(length=70, startpos=259, endpos=329, name='kopftext2'),
    dict(length= 1, startpos=495, endpos=496, name='status'),
]
A2satzklasse = generate_field_datensatz_class(FELDERA2, name='A2kopfdatenzusatz', length=496)

# Kopfdaten – Lieferadresse (XOI00EA3)
# Diese Satzart enthält die Lieferadresse zu einem Auftrag.
FELDERA3 = [
    dict(length=17, startpos=  0, endpos= 17, name='iln_warenempfaenger'),
    dict(length=17, startpos= 17, endpos= 34, name='nr_warenempfaenger_ltKd'),
    dict(length=17, startpos= 34, endpos= 51, name='nr_warenempfaenger_ltLi'),
    dict(length=35, startpos= 51, endpos= 86, name='lieferadresse_name1'),
    dict(length=35, startpos= 86, endpos=121, name='lieferadresse_name2'),
    dict(length=35, startpos=121, endpos=156, name='lieferadresse_name3'),
    dict(length=35, startpos=156, endpos=191, name='lieferadresse_name4'),
    dict(length=35, startpos=191, endpos=226, name='lieferadresse_strasse'),
    dict(length= 3, startpos=226, endpos=229, name='lieferadresse_land'),
    dict(length= 9, startpos=229, endpos=238, name='lieferadresse_plz'),
    dict(length=35, startpos=238, endpos=273, name='lieferadresse_ort'),
    dict(length= 5, startpos=273, endpos=278, name='lieferadresse_werk'),
    dict(length=17, startpos=278, endpos=295, name='abladestelle'),
]
A3satzklasse = generate_field_datensatz_class(FELDERA3, name='A3lieferadresse', length=496)

# Kopfdaten – Rechnungsempfänger (XOI00EA4)
# Diese Satzart enthält die Angaben zum Rechnungsempfänger eines Auftrags.
FELDERA4 = [
    dict(length=17, startpos=  0, endpos= 17, name='iln_rechnungsempfaenger'),
    dict(length=17, startpos= 17, endpos= 34, name='nr_rechnungsempfaenger_ltKd'),
    dict(length=17, startpos= 34, endpos= 51, name='nr_rechnungsempfaenger_ltKd'),
    dict(length=35, startpos= 51, endpos= 86, name='rechnungsadresse_name1'),
    dict(length=35, startpos= 86, endpos=121, name='rechnungsadresse_name2'),
    dict(length=35, startpos=121, endpos=156, name='rechnungsadresse_name3'),
    dict(length=35, startpos=156, endpos=191, name='rechnungsadresse_name4'),
    dict(length=35, startpos=191, endpos=226, name='rechnungsadresse_strasse'),
    dict(length= 3, startpos=226, endpos=229, name='rechnungsadresse_land'),
    dict(length= 9, startpos=229, endpos=238, name='rechnungsadresse_plz'),
    dict(length=35, startpos=238, endpos=273, name='rechnungsadresse_ort'),
]
A4satzklasse = generate_field_datensatz_class(FELDERA4, name='A4rechnungsadresse', length=496)

# Positionsdaten (XOI00EA5)
# Diese Satzart enthält die Positionsdaten zu einem Auftrag.
FELDERA5 = [
    #    T Lä length=Lä De  startpos=Pos, endpos=Pos
    #    y St length=By zi  startpos=von, endpos=bis Text
    dict(length=4,  startpos=0,   endpos=4,   name='position'),
    dict(length=35, startpos=4,   endpos=39,  name='artikelnummer'),
    dict(length=35, startpos=39,  endpos=74,  name='artikelnummer_kunde'),
    dict(length=35, startpos=74,  endpos=109, name='ean'),
    dict(length=35, startpos=109, endpos=144, name='name1'),
    dict(length=35, startpos=144, endpos=179, name='name2'),
    # menge is actually an 15,3 decimal field but we don't use fractions at HUDORA
    dict(length=15, startpos=179, endpos=194, name='menge', fieldclass=DecimalFieldNoDotZeropadded, precision=3),
    dict(length=3,  startpos=194, endpos=197, name='mengeneinheit', default='STK', choices=['STK']),
    dict(length=15, startpos=197, endpos=212, name='bruttopreis', fieldclass=DecimalField, precision=3),
    dict(length=15, startpos=212, endpos=227, name='nettopreis', fieldclass=DecimalField, precision=3),
    dict(length=15, startpos=227, endpos=242, name='verkaufspreis'),
    dict(length=3,  startpos=242, endpos=245, name='preisdimension'),
    dict(length=3,  startpos=245, endpos=248, name='preistyp'),
    dict(length=8,  startpos=248, endpos=256, name='lieferdatum_fix', fieldclass=DateField),
    dict(length=8,  startpos=256, endpos=264, name='lieferdatum_von', fieldclass=DateField),
    dict(length=8,  startpos=264, endpos=272, name='lieferdatum_bis', fieldclass=DateField),
    dict(length=4,  startpos=272, endpos=276, name='lieferzeit_fix',  fieldclass=TimeField),
    dict(length=4,  startpos=276, endpos=280, name='lieferzeit_von',  fieldclass=TimeField),
    dict(length=4,  startpos=280, endpos=284, name='lieferzeit_bis',  fieldclass=TimeField),
    dict(length=3,  startpos=284, endpos=287, name='versandart'),
    dict(length=2,  startpos=287, endpos=289, name='aktivitaet'),
    dict(length=35, startpos=289, endpos=324, name='text1'),
    dict(length=35, startpos=324, endpos=359, name='text2'),
    # dict(length=2  0  288  289 Aktivität       
]
A5satzklasse = generate_field_datensatz_class(FELDERA5, name='A5positionsdaten', length=496)

FELDERA8 = [
    # Endedaten (XOI00EA8)
    # Diese Satzart enthält Endsummen und Abstimmsummen pro Auftrag.
     dict(length=8, startpos=0, endpos=8, name='valutadatum', fieldclass=DateField),
     # dict(length=A     3  0    9   11 Kontr.art 1          
     # dict(length=S 15 15  3   12   26 Kontr.wert 1         
     # dict(length=S  3  3  0   27   29 Kontr.Eh 1           
     # dict(length=A     3  0   30   32 Kontr.art 2          
     # dict(length=S 15 15  3   33   47 Kontr.wert 2         
     # dict(length=S  3  3  0   48   50 Kontr.Eh 2           
     # dict(length=A     3  0   51   53 Kontr.art 3          
     # dict(length=S 15 15  3   54   68 Kontr.wert 3         
     # dict(length=S  3  3  0   69   71 Kontr.Eh 3           
     dict(length=35, startpos=71,  endpos=106, name='fusstext1'),
     dict(length=35, startpos=106, endpos=141, name='fusstext2'),
     dict(length=35, startpos=141, endpos=176, name='fusstext3'),
]
A8satzklasse = generate_field_datensatz_class(FELDERA8, name='A8endedaten', length=496)

# Texte (XOI00EA0)
# Diese Satzart enthält diverse Texte, abhängig von der Satzart:
# AK = Auftragskopftexte
# AP = Positionstexte
# AE = Auftragsendetexte
FELDERA0 = [
    dict(length=60, startpos=  1, endpos= 60, name='textzeile1'),
    dict(length=60, startpos= 61, endpos=120, name='textzeile2'),
    dict(length=60, startpos=121, endpos=180, name='textzeile3'),
    dict(length=60, startpos=181, endpos=240, name='textzeile4'),
    dict(length=60, startpos=241, endpos=300, name='textzeile5'),
    dict(length=60, startpos=301, endpos=360, name='textzeile6'),
    dict(length=60, startpos=361, endpos=420, name='textzeile7'),
    dict(length=60, startpos=421, endpos=480, name='textzeile8'),
    # dict(length=15, startpos=481, endpos=495, name='Reserve'),
    dict(length=1,  startpos=496, endpos=496, name='Status'),
]
A0satzklasse = generate_field_datensatz_class(FELDERA8, name='A0texte', length=496)


# Satzart Bezeichnung                   MK Häufigkeit
# XH      Headerinformationen           M  1-mal pro Übertragung
# A1      Kopfdaten                     M  n-mal pro Übertragung
# A2      Kopfdaten-Zusatz              M  1-mal pro Kopfdaten (A1)
# A3      Kopfdaten-Lieferadresse       K  1-mal pro Kopfdaten (A1)
# A4      Kopfdaten-Rechnungsempfänger  K  1-mal pro Kopfdaten (A1)
# AK      Auftrags-Kopftext             K  n-mal pro Kopfdaten (A1)
# A5      Positionsdaten                M  n-mal pro Kopfdaten (A1)
# A6      Position-Zu-Abschläge         K  n-mal pro Positionsdaten (A5)
# A7      Position-Packmittel           K  n-mal pro Positionsdaten (A5)
# AP      Positionstexte                K  n-mal pro Positionsdaten (A5)
# AE      Auftrags-Endetexte            K  n-mal pro Kopfdaten (A1)
# A8      Endedaten                     M  1-mal pro Kopfdaten (A1)

# In Regex Notation:
# XH (A1 A2 A3? A4? AK+? (A5 A6+? A7+? AP+?)+ AE+?)+ A8

class AuftragDatei:
    def generate(self):
        xh = XHsatzklasse()
        a1 = A1satzklasse()
        a1.kundennummer = '12644'
        a2 = A2satzklasse()
        a5 = A5satzklasse()
        a5.position = 1
        a5.artikelnummer = 14650
        a5.menge = 15
        a8 = A8satzklasse()
        out = []
        for record in [xh, a1, a2, a5, a8]:
            out.append(record.__class__.__name__[:2] + '' + record.serialize())
        print '\n'.join(out)

def parseTest():
    fd = open('lib/public/pySoftM/ORDERS.txt')
    satzresolver = dict(XH=XHsatzklasse,
                        A1=A1satzklasse,
                        A2=A2satzklasse,
                        A3=A3satzklasse,
                        A4=A4satzklasse,
                        A5=A5satzklasse,
                        A8=A8satzklasse,
                        )
    for line in fd:
        satzart, version, data = line[:2], line[2:4], line[4:]
        data = data.strip('\r\n')
        satzklasse = satzresolver.get(satzart, None)
        print satzart, version,
        if satzklasse:
            satz = satzklasse()
            satz.parse(data)
            pprint(satz.fields())
    
def nachschub(mengen, liefertermin):
    """Stellt einen Nachscubauftrag von Lager 26 (Geo) an Lager 100 ins System."""
    satzliste = []
    xh = XHsatzklasse()
    satzliste.append(xh)
    a1 = A1satzklasse()
    satzliste.append(a1)
    a1.iln_lieferant = '4005998000007'
    a1.iln_kunde = '4005998000007'
    a1.uebertragungs_id = 'Abruf zur Laneferung am %s' % liefertermin.strftime('%Y-%m-%d')
    # a1.lieferantennummer_kunde = '17200'
    # 'sachbearbeiter_id': ''
    # 'sachbearbeiter': ''
    a2 = A2satzklasse()
    satzliste.append(a2)
    if hasattr(liefertermin, 'strftime'):
        a2.kopftext1 = 'Abruf zur Laneferung am %s' % liefertermin.strftime('%Y-%m-%d')
        a2.lieferdatum_fix = liefertermin
        if  liefertermin.strftime('%H:%M') != '00:00':
            a2.kopftext1 = 'Abruf zur Lieferung %s' % liefertermin.strftime('%Y-%m-%d %H:%M')
            a2.lieferzeit_fix = liefertermin
    print a2.fields(), '\n'
    position = 0
    for menge, artnr in mengen:
        position += 1
        a5 = A5satzklasse()
        satzliste.append(a5)
        a5.position = position
        a5.menge = menge
        a5.artikelnummer = artnr
    a8 = A8satzklasse()
    satzliste.append(a8)
    a8.fusstext1 = 'Nachschubabruf, Mengen Fix!'
    out = []
    for record in satzliste:
        out.append(record.__class__.__name__[:2] + '  ' + record.serialize())
    return '\n'.join(out)

if __name__ == '__main__':
    # unittest.main()
    # AuftragDatei().generate()
    # parseTest()
    import ERP.nachschub
    abruf = ERP.nachschub.nachschub_mindestbestaende()
    mengen = abruf.vorschlaege[:34] 
    print nachschub([(x.menge, x.artnr) for x in mengen], datetime.datetime(2007, 05, 18, 10, 00))
