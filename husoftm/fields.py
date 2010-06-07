#!/usr/bin/env python
# encoding: utf-8
"""
fields.py - describes the SoftM table structure. Part of huSoftM.

See also http://cybernetics.hudora.biz/projects/wiki/SoftMtabellen

Created by Maximillian Dornseif on 2007-03-18. Based on code named "MoftS" from Summer 2005.
Copyright (c) 2007, 2008, 2009 HUDORA GmbH. All rights reserved.
"""

__revision__ = "$Revision$"

MAPPINGDIR = {
'ABV00': {# Adressdaten zu Aufträgen die in die Stapelschnittstelle geschrieben werden
          'BVVGNR': 'vorgang',
          #'BVLFDS': 'stapelsatznummer',
          'BVAART': 'adressart',
          'BVNAME': 'name1',
          'BVNAM2': 'name2',
          'BVNAM3': 'name3',
          'BVNAM4': 'avisieren',
          'BVSTR': 'strasse',
          'BVPLZ': 'plz',
          'BVORT': 'ort',
          'BVLKZ': 'laenderkennzeichen',
          'BVKZBA': 'bearbeitungskennzeichen',
          #'BVORTT': 'Ortszusatz',
          'BVKZAD': 'Adressaufbereitung',
        },

'AAK00': {# Auftragsköpfe
          # 'AKFNR ': 'Firma',
          'AKAUFN': 'auftragsnr',
          'AKSBNR': 'sachbearbeiter',
          # 'AKABT ': 'Abteilungs-nr',
          # 'AKAGRP': 'Abteilungs-Gruppe/Sparte',
          # 'AKFGRP': 'Firmen-Gruppe',
          'AKAUFA': 'art',
          # 'AKRANR': 'Rahmenvereinbarungs-Nr.',
          # 'AKKANR': 'Übergeordneter Auftrag',
          # 'AKFNRM': 'Mandanten-nr',
          'AKKDNR': 'kundennr_warenempf',
          # 'AKKDN3': 'Kunden-Nr/Verbraucher',
          # 'AKEGCD': 'Ländercode / EG',
          # 'AKUSTN': 'USt-Id-nr',
          # 'AKVANR': 'Versandadress-Nr.',
          # 'AKALS1': 'Alphasortierung/Warenempfän',
          # 'AKKDRG': 'Kundennr.Rechnungs-Empfänge',
          # 'AKALS2': 'Alphasortierung/Rech-Empfän',
          # 'AKVERB': 'Verband/Mischkonto',
          # 'AKABEM': 'Ab-Empfänger: R = Rg-Zahler',
          # 'AKRGEM': 'Rechnungsempfänger',
          'AKNRKD': 'auftragsnr_kunde',
          'AKDTKD': 'auftragsdatum_kunde',
          # 'AKDTLE': 'Leih-Datum',
          'AKDTKW': 'kundenwunsch_date',
          # 'AKJWKW': 'Kundenwunschtermin CJJWW',
          # 'AKPROB': 'Objekt/Projekt/Aktion',
          # 'AKDTVA': 'Valuta-Datum/Erfassung',
          # 'AKVALT': 'Abstandstage Valuta',
          'AKDTLT': 'liefer_date',
          'AKIK01': 'fixtermin',
          # 'AKJWLT': 'Liefertermin CJJWW',
          # 'AKX3ZB': 'Zahlungsbedingungs-Schlüsse',
          # 'AKX3VA': 'Versandarten-Schlüssel',
          # 'AKX3LB': 'Lieferbedingungs-Schlüssel',
          # 'AKVPKE': 'Verpackungskosten',
          # 'AKVSKE': 'Versandkosten',
          # 'AKNBKE': 'Nebenkosten',
          # 'AKX4NB': 'Textschl. Nebenkosten',
          # 'AKNBT1': 'Zus. Nebenkosten 1',
          # 'AKNTY1': 'Typ/Zus. Nebenkosten 1',
          # 'AKX4N1': 'Textschl. zus. Nebenkosten',
          # 'AKNBT2': 'Zus. Nebenkosten 2',
          # 'AKNTY2': 'Typ/Zus. Nebenkosten 2',
          # 'AKX4N2': 'Textschl. zus. Nebenkosten',
          # 'AKRBP1': 'Auftrags-Rabatt-%-1',
          # 'AKRBP2': 'Auftrags-Rabatt-%-2',
          # 'AKX3R1': 'Textschl. Rabatt 1',
          # 'AKX3R2': 'Textschl. Rabatt 2',
          # 'AKKZRX': 'Kz.Rabatt Brutto',
          # 'AKMWKZ': 'Umsatz-Steuerprofil',
          # 'AKWSL ': 'Währungs-Kennzeichen',
          # 'AKKURS': 'Wechsel-Kurs',
          # 'AKKUFA': 'Kursfaktor',
          # 'AKSPSL': 'Sprache',
          # 'AKVRT1': 'Vertreter 1',
          # 'AKVRT2': 'Vertreter 2',
          # 'AKPRZA': 'Aufteilungs-%-Satz',
          # 'AKBZRG': 'Bezogene RG-nr',
          'AKLGN1': 'auslieferungslager',
          'AKLGN2': 'zugangslager',
          # 'AKTRNR': 'Touren-nr',
          # 'AKLINR': 'Lieferant bei Strecke',
          # 'AKSPED': 'Spediteur',
          # 'AKKZBE': 'Steuerung Best.-Druck',
          # 'AKKZST': 'Kennzeichen Streckengeschäf',
          # 'AKKZWS': 'WZ/RE-Sperre',
          # 'AKKZIN': 'Kennzeichen interner Beleg',
          # 'AKKZBO': 'Steuerung Bonus-Abrechnung',
          'AKKZTF': 'teillieferung', # bzw. Teilfakturierung
          # 'AKKZSR': 'Kz.Drucken in Sammelrech=1',
          # 'AKKZZF': 'un-/versteuert',
          'AKKZVA': 'voll_ausgeliefert',
          # 'AKKZZU': 'Kz.Bevorzugte Zuteilung',
          # 'AKKZRK': 'Kz: Rückstand möglich',
          # 'AKKZSL': 'Kz.Sammel-Lieferschein',
          # 'AKKZNN': 'Kz.Nachnahme',
          # 'AKKZAR': 'Kz.Leihart',
          # 'AKABTP': 'Abt. für Prüfung KD-Zusatz',
          'AKDTER': 'erfassung_date',
          # 'AKZTER': 'Uhrzeit der Erfassung',
          # 'AKDTAE': 'Datum l. Änderung CJJMMTT',
          # 'AKZTAE': 'Uhrzeit letzte Änderung',
          # 'AKDTAB': 'Datum letzte Auftragsbestät',
          # 'AKDTLF': 'Datum letzter Lieferschein',
          # 'AKDTKB': 'Datum ltzt.Kommissionierbel',
          # 'AKDTFA': 'Datum letzte Rechnung/Gutsc',
          # 'AKKZAE': 'AB: nur geänderte Posit.',
          # 'AKFKZA': 'Formular-KZ AB',
          # 'AKFKZK': 'Formular-KZ Kommission.Bele',
          # 'AKFKZL': 'Formular-KZ Lieferschein',
          # 'AKFKZR': 'Formular-KZ Rechnung/Gutsch',
          # 'AKSFAA': 'AB Sofort',
          'AKSFAL': 'LF Sofort',
          'AKSFAK': 'Komm.-Beleg sofort',
          # 'AKSFAR': 'RG Sofort',
          # 'AKABCO': 'Anzahl AB',
          'AKKBCO': 'anzahl_kommissionierbelege',
          'AKLSCO': 'anzahl_liefscheine',
          # 'AKRGCO': 'Anzahl Rechnungen',
          # 'AKOQZO': 'Outqueue-Zuordnung',
          # 'AKGEB ': 'Gebiet',
          # 'AKBRAN': 'Branche',
          # 'AKPLZ':  'plz',
          'AKLKZ': 'land',
          # 'AKVRG1': 'Vertriebs-Gruppe 1',
          # 'AKVRG2': 'Vertriebs-Gruppe 2',
          # 'AKDSTK': 'Distrikt',
          # 'AKAWRT': 'Auftragswert',
          # 'AKSTOR': 'Kennzeichen Stornierung',
          # 'AKFORM': 'Rechnungsart',
          # 'AKMJBU': 'Buchungsmonat',
          # 'AKKZWR': 'Kz wertmäßig',
          # 'AKKZVS': 'Vertriebsstatus',
          # 'AKFA1A': 'Preisstufe',
          # 'AKFA2A': 'Fax-Zustellung',
          # 'AKFA4A': 'Herkunft',
          # 'AKKZRE': 'Retouren-Beleg ja/nein',
          # 'AKKZSH': 'Hrst Strecke',
          # 'AKDTRV': 'Rahmen: gültig ab',
          # 'AKDTRB': 'Rahmen: gültig bis',
          # 'AKKDN1': 'Adresse 1',
          # 'AKKDN2': 'Adresse 2',
          # 'AKZUOR': 'Zuordnung für dezentrale An',
          # 'AKDFSL': 'Dateiführungs-Schlüssel',
          # 'AKSTAT': 'satzstatus'
         },

'AAP00': { # Auftragspositionen
          'APMNG-APMNGF-APMNGG': 'menge_offen',
           # 'APFNR ': 'Firma',
           # 'APAUFN': 'Auftrags-nr',
           'APAUPO': 'position',
           'APSBNR': 'sachbearbeiter',
           # 'APABT':  'Abteilungs-nr',
           # 'APAGRP': 'Abteilungs-Gruppe/Sparte',
           # 'APFGRP': 'Firmen-Gruppe',
           # 'APFNRX': 'Best.-Führungsfirma',
           # 'APKDNR': 'Kunden-nr',
           # 'APKDRG': 'Kundennr.Rechnungs-Empfänge',
           # 'APVERB': 'Verband/Mischkonto',
           # 'APLINR': 'Lieferanten-nr',
           # 'APAUFA': 'AUFTRAGS-ART',
           # 'APHPTP': 'zugeordnete_position',
           # 'APRANR': 'rahmenauftrag',
           # 'APRAPO': 'rahmenposition',
           # 'APKANR': 'Übergeordneter Auftrag',
           # 'APANNR': 'Angebots-nr',
           # 'APANPO': 'Angebots-Positions-nr',
           # 'APBSTN': 'Bestell-nr',
           # 'APBSTP': 'Bestell-Position',
           # 'APLHNR': 'LH-Nr.',
           # 'APLHPO': 'Leihauftrags-Pos',
           'APVGNR': 'vorgangs_nr',
           'APVGPO': 'vorgangs_position',
           # 'APFAUN': 'Fertigungsauftrag',
           # 'APPROB': 'Objekt/Projekt/Aktion',
           'APARTN': 'artnr',
           # 'APARTG': 'Artikel-Gruppe',
           # 'APARTH': 'Artikel-Haupt-Gruppe',
           # 'APKZSO': 'Kennzeichen Sonderartikel',
           'APLGNR': 'lager',
           # 'APLGRP': 'Lagergruppe',
           # 'APLGPL': 'Lager-Platz',
           # 'APKZBE': 'Ohne Bestandsführung',
           # 'APKZSE': 'Serien-/Chargenartikel',
           'APKZKO': 'komponentenaufloesung',
           'APKZPS': 'Positionsbezogener Set',
           # 'APKZEP': 'KZ: Eigen-Produkt',
           # 'APKZMU': 'Muster-Artikel',
           'APMNG': 'bestellmenge',               # Die Menge, die der Kunde haben will
           'APMNGL': 'Menge zu liefern',
           'APMNGG': 'Menge/Liefersch.nichtfakt',
           'APMNGF': 'fakturierte_menge',          # Menge, die geliefert und fakturiert ist
           'APMNGR': 'Menge AFA35',
           'APMNGB': 'Bestellmenge in ME XLF',
           'APMESL': 'Mengen-Einheit',
           'APMEKZ': 'ME XLF/AAP',
           # 'APDEZI': 'Anzahl Dezimalstellen',
           # 'APVOLM': 'Inhalt VKE/Gebinde',
           # 'APGANZ': 'Bestellte Gebinde',
           # 'APGANF': 'Fakturierte Gebinde',
           # 'APGANG': 'Gebinde/Lief.nicht fakt',
           # 'APGANL': 'Gebinde/zu liefern',
           # 'APGANP': 'Gebinde verpackt',
           # 'APGANR': 'Gebinde AFA35',
           # 'APGEWI': 'Gewicht',
           'APPREV': 'verkaufspreis',
           # 'APPEBN': 'Ebene Preisfindung',
           # 'APKZPR': 'Preiskennzeichen Verkauf',
           # 'APMEPR': 'Preis-Einheit',
           # 'APFAPR': 'Faktor APMEH --> APMEPR',
           'APFWRT': 'fakturierter_wert',
           'APOWRT': 'offener_auftragswert',
           'APPREL': 'listenpreis',
           # 'APPREE': 'Einstands-Preis',
           # 'APPRKZ': 'Preiskennzeichen Lager',
           # 'APWSLE': 'Währungsschlüssel: EK',
           # 'APKURS': 'Wechselkurs EK',
           # 'APKUFA': 'Kursfaktor/EK',
           # 'APRBP1': 'Pos-Rabatt-1',
           # 'APRBP2': 'Pos-Rabatt-2',
           # 'APRBP3': 'Pos-Rabatt-3',
           # 'APRBP4': 'Pos-Rabatt-4',
           # 'APKZR1': 'Berechnung Rabatt 1',
           # 'APKZR2': 'Berechnung Rabatt 2',
           # 'APKZR3': 'Berechnung Rabatt 3',
           # 'APKZR4': 'Berechnung Rabatt 4',
           # 'APX3R1': 'Textschl. Rabatt 1',
           # 'APX3R2': 'Textschl. Rabatt 2',
           # 'APX3R3': 'Textschl. Rabatt 3',
           # 'APX3R4': 'Textschl. Rabatt 4',
           # 'APKZRU': 'Kz.Rabatt versteckt',
           # 'APKZRX': 'Kz.Rabatte Brutto',
           # 'APKZRB': 'Kz: kein Auftragsrabatt = 1',
           # 'APKZET': 'Steuerung Sortiments-Rabatt',
           # 'APKZSR': 'Sortiments-Rabatt gewährt',
           # 'APMWAR': 'Steuerart / Artikel',
           'APDTLT': 'liefer_date',
           # 'APJWLT': 'Liefertermin CJJWW',
           'APDTKD': 'kundenwunsch_date',
           # 'APJWKD': 'Kundenwunschtermin CJJWW',
           'APDTLN': 'neuliefer_date',
           # 'APJWLN': 'Liefertermin neu CJJWW',
           # 'APKZTE': 'Kz: Termin-Format: 1 =WW/JJ',
           # 'APKZAB': 'Position in AB gedruckt',
           'APKZAE': 'geaendert',
           # 'APKZST': 'Kennzeichen Streckengeschäf',
           # 'APKZFA': 'Rg-Freigabe',
           # 'APKZEK': 'Schnittstelle Einkauf',
           # 'APKZBD': 'Steuerung Best.-Druck',
           # 'APKZSP': 'KZ: Sperre für Übernahme',
           # 'APHERK': 'Herkunft',
           # 'APKZSS': 'Kz Stapelschnittstelle',
           # 'APFA1A': 'Lieferschein-Auslösung',
           # 'APKZLA': 'Lieferschein-Auslösung 2',
           # 'APLFKZ': 'Kz. Lfsch drucken',
           'APKZZL': 'zuteilungskennzeichen',
           'APKZFG': 'KB/LS-Feigabe',
           'APKZRE': 'Lieferschein -Auslösung',
           # 'APKZZU': 'Kz.Bevorzugte Zuteilung',
           'APKZLF': 'Kz:Im LfSchein andrucken',
           # 'APKZVL': 'Vorab-Lieferschein',
           # 'APKZRD': 'Kz:Pos in Rech.nicht andr.=',
           # 'APKZSF': 'Kz: In Sofort-Form andrucke',
           # 'APKZSV': 'Zusätzliches Sofortformular',
           # 'APKZSA': 'Kz: keine AE-Statistik = 1',
           # 'APKZSM': 'Kz:keine Mengen-Statistik=1',
           # 'APKZSU': 'Kz:keine Umsatzstatistik=1',
           # 'APKZBA': 'Kz: kein Auftragsbestand=1',
           # 'APKZRK': 'Kz: Rückstand erlaubt = 1',
           'APKZVA': 'voll_ausgeliefert',
           # 'APKZAF': 'Kz:Abruf aus Rahmenauftrag',
           # 'APKZRA': 'Kz:Rahmen-Pos',
           # 'APKZPA': 'Kz:Preisänderung = 1',
           'APKZTA': 'Kz:Terminänderung = 1',
           'APKZMA': 'Kz:Mengenänderung = 1',
           # 'APPRVK': 'Provisions-Kennzeichen',
           # 'APPWRT': 'Provisionswert',
           # 'APPRV1': 'Provisions-%-Satz 1',
           # 'APPRV2': 'Provisions-%-Satz 2',
           # 'APGATG': 'Garantie-Tage',
           # 'APX4TR': 'Transaktions-Code',
           # 'APKZBO': 'Kz Bonus',
           # 'APKZEE': 'Einstandspreis eingebbar',
           # 'APKTO ': 'Erlöskonto',
           # 'APKTOK': 'Kosten-Konto',
           # 'APKST ': 'Kostenstelle',
           # 'APKTR ': 'Kostenträger',
           # 'APZUOR': 'Zuordnung für dezentrale An',
           # 'APRES1': 'Firma/Herkunft',
           # 'APFA3A': 'Bestell-Zusammenfass./Dispo',
           'APFA4A': 'Teil-Zuteilungs-Verbot',
           # 'APKZBF': 'ZUSATZANGABE RAHMEN/ABRUF',
           # 'APDRKZ': 'Preisdruck',
           # 'APHLKZ': 'Kz Hochregal',
           # 'APHLCR': 'Chargenrein',
           # 'APPGSZ': 'Preisstufe',
           # 'APUAPR': 'Typ Preisfaktor',
           # 'APMEGB': 'Mengeneinheiten-Schlüssel',
           # 'APSFRG': 'Pos für SofortRg ausgewählt',
           # 'APKZDI': 'Beschaffungsschlüssel',
           # 'APKZVS': 'Vertriebsstatus',
           # 'APDTRV': 'Rahmen: gültig ab',
           # 'APDTRB': 'Rahmen: gültig bis',
           # 'APAUPS': 'bezogene Position',
           # 'APSNPF': 'StzNr Preisfindung',
           'APMG01': 'Menge 1',
           # 'APWR01': 'Preis 1',
           # 'APPR01': '%-Satz 1',
           'APDTZU': 'Datum Zuteilung',
           'APZTZU': 'Uhrzeit Zuteilung HHMMSS',
           'APJNZU': 'Jobnr Zuteilung',
           # 'APDTER': 'Datum Format CJJMMTT',
           # 'APZTER': 'Uhrzeit der Erfassung',
           # 'APDTAE': 'Datum Format CJJMMTT',
           # 'APZTAE': 'Uhrzeit letzte Änderung',
           # 'APSBAE': 'Sachb.letzte Änderung',
           # 'APDFSL': 'Dateiführungs-Schlüssel',
           # 'APSTAT': 'Satzstatus',
          },

'AAT00': {# Auftragsnr
          'ATAUFN': 'auftragsnr',
          'ATAUPO': 'auftragsposition',
          'ATTART': 'textart',
          'ATLFNR': 'nr',
          'ATTX60': 'text',
          'ATKZAB': 'andruck_ab',
          'ATKZLF': 'andruck_ls',
          'ATKZRG': 'andruck_re',
         },

'AKZ00': {# Kundenstamm für Auftragsverwaltung
          'KZKDNR': 'Kunden-nr',
          #'KZSBNR': 'zuständiger Sachbearbeiter',
          'KZVRT': 'vertreter',
          'KZGEB': 'gebiet',
          'KZBRAN': 'branche',
          'KZDSTK': 'distrikt',
          #'KZKZVB':' Kz. 'Kunde ist Verbraucher','
          #'KZKZZU': 'Bevorzugte Zuteilung',
          #'KZKZRK': 'Rückstand möglich',
          'KZX3LB': 'lieferbedingung',
          'KZX3VP': 'verpackungsvorschrift',
          'KZX3VS': 'versandart',
          #'KZVANR': 'Versand-Adress Nr.',
          #'KZEXPR': 'Expreßgut-Station',
          'KZLGNR': 'auslieferunglager',
          #'KZX3ZB': 'Zahl.-Bed-Schl.',
          #'KZKDRG': 'Rechnungs-Zahler',
          #'KZWSL ': 'Währungs-Kennzeichen',
          #'KZKZBO': 'Bonus-Kennzeichen',
          #'KZKZSL': 'Sammellieferschein',
          #'KZKZRV': 'Rechnung nur vollständig',
          #'KZKZSR': 'Sammelrechnung',
          #'KZKZAB': 'Steuerung AB-Andruck',
          #'KZABCO': 'Formular-Anz./Auftragsbest.',
          #'KZKBCO': 'Formularanz./ Komm.-Beleg',
          #'KZLSCO': 'Formularanz./ Lieferschein',
          #'KZRGCO': 'Formularanz./ Rechnung',
          #'KZTELF': 'tel',
          #'KZTFAX': 'fax',
          'KZINFO': 'sachbearbeiter',
          'KZDTAE': 'updated_at',
},

'ALK00': {# Lieferscheinköpfe
          'LKSANK': 'satznr',
          'LKSANB': 'bezogener_kopf',
          'LKSBNR': 'sachbearbeiter',
          'LKLGNR': 'lager',
          'LKX3VA': 'versandart',
          'LKKZTF': 'teilfakturiert',
          'LKKDRG': 'rechnungsempfaenger',
          'LKKDNR': 'warenempfaenger',
          'LKLFSN': 'lieferscheinnr',
          'LKDTLF': 'letzter_lieferschein_date',
          'LKZTLF': 'letzter_lieferschein_time',
          'LKDTST': 'lieferschein_storno_date',
          'LKKZSL': 'sammellieferschein',
          'LKKZLF': 'druckkennzeichen',
          'LKKBNR': 'kommissionierbelegnr',
          'LKDTKB': 'letzter_kommissionierbeleg_date',
          'LKZTKB': 'letzter_kommissionierbeleg_time',
          'LKLFDN': 'laufendenr',
          'LKBELN': 'belegnr_freigabe',
          # 'LKFGNR': 'Freigabenr AFA20
          # 'LKFGSB': 'Sachbearb. Freigabe
          'LKDTFG': 'freigabe_date',
          'LKZTFG': 'freigabe_time',
          'LKFGI1': 'freigabe_info1',
          'LKFGI2': 'freigabe_info2',
          #'LKFGLC': 'Anzahl Liefsch.'
          'LKAUFS': 'auftragsnr',
          'LKDTLT': 'liefer_date',
          'LKKZ02': 'hrl_status',
          'LKKZVA': 'alle_positionen_fakturiert',
          'LKDTER': 'erfassung_date',
          'LKZTER': 'erfassung_time',
          'LKDTAE': 'aenderung_date',
          'LKZTAE': 'aenderung_time',
          'LKDFSL': 'dateifuehrungsschluessel',
          'LKSTAT': 'satzstatus',
         },

'ALN00': {# Lieferscheinpositionen
          'LNSANK': 'satznr_kopf',
          'LNSANP': 'satznr',
          # 'LNKANR': 'Übergeordnete Auftrags-Nr.
          'LNAUFN': 'auftragsnr',
          'LNAUPO': 'auftrags_position',
          'LNARTN': 'artnr',
          'LNKZKO': 'setartikel',
          'LNBSTN': 'bestellnr',
          'LNBSTP': 'bestellposition',
          # 'LNVKE':  'verkaufseinheit', # ist immer leer
          'LNLGNR': 'lager',
          #'LNLGPL': 'lagerplatz', # ist immer leer
          #'LNKZBE': 'Kz:keine Bestandsführung =1
          'LNMNGO': 'menge_offen',
          'LNDTLT': 'liefertermin_date',
          #'LNJWLT': 'liefertermin_woche',
          'LNKZRK': 'rueckstand_erlaubt',
          #'LNKZZL': 'zuteilungskennzeichen',
          'LNKZZU': 'bevorzugte_zuteilung',
          'LNKDRG': 'rechnungsempfaenger',
          'LNKDNR': 'kundennr',
          'LNLFSN': 'lieferscheinnr',
          'LNMNGL': 'menge',
          'LNDTLF': 'lieferschein_date',
          'LNZTLF': 'lieferschein_time',
          #'LNKZVL': 'KZ. Vorab-Lieferschein
          #'LNKZLD': 'Kz: Im LF. andrucken
          #'LNKZL2': 'Kz: Im LF. andrucken
          #'LNKZLF': 'Kz: LF-Schein drucken
          #'LNKZFA': 'Kz:Pos.für Fakt.ausgewählt=
          #'LNKZFF': 'Kz: Pos ist fakturierbar =
          'LNDTST': 'storno_date',
          #'LNKZRE': 'lieferschein_ausloesung',
          'LNKBNR': 'kommissionierbelegnr',
          'LNMNGK': 'menge_komissionierbeleg',
          'LNDTKB': 'komissionierbeleg_date',
          'LNZTKB': 'komissionierbeleg_time',
          'LNKZKB': 'komissionierbeleg_drucken',
          'LNRGST': 'rechnungsstatus',
          'LNDTVS': 'versand_date',
          'LNSTOR': 'gutschrift',
          'LNBELP': 'kommissionierbeleg_position',
          #'LNFGNR': 'Freigabenr AFA20
          #'LNFNFA': 'Fehlernr FG. AFA20
          'LNLSTO': 'lieferscheinstorno',
          'LNMNGF': 'menge_fakturierung',
          'LNKZV2': 'voll_ausgeliefert',
          #'LNPSTA': 'Packstatus
          #'LNKZVS': 'Kz versandfertig
          #'LNPROG': 'Herkunftsprogramm
          #'LNKZUB': 'Übernahmekennzeichen
          #'LNKINF': 'Kennzeicheninfo
          #'LNAUPS': 'bezogene Position
          #'LNKZ03': 'hrl_status',
          #'LNRSN1': 'Druck-Kz für KB
          'LNDTER': 'erfassung_date',
          'LNZTER': 'erfassung_time',
          'LNDTAE': 'aenderung_date',
          'LNZTAE': 'aenderung_time',
          'LNDFSL': 'dateiführungsschluessel',
          'LNSTAT': 'satzstatus',
          'LNSBNR': 'sachbearbeiter_bearbeitung',
         },

'AFK00': # Rechnungsköpfe
     {
     'FKFNR ': 'firma',
     # 'FKABT ': 'Abteilungs-Nummer',
     # 'FKAGRP': 'Abteilungs-Gruppe/Sparte',
     # 'FKFGRP': 'Firmen-Gruppe',
     # 'FKSBNR': 'Sachbearbeiter-Nummer',
     'FKAUFN': 'auftragsnummer',
     # 'FKKANR': '!bergeordneter Auftrag',
     # 'FKRANR': 'Rahmenvereinbarungs-Nr.',
     'FKAUFA': 'auftragsart',
     # 'FKSRKO': 'KZ SAMMELRECHNUNGS-KOPF-SAT',
     'FKRGNR': 'rechnungsnr',
     # 'FKRGNI': 'IntRg',
     # 'FKRGLI': 'Rechnungslisten-Nr.',
     # 'FKBZRG': 'Bezogene Rg',
     'FKFORM': 'rechnungsart',
     'FKSTOR': 'storniert',
     # 'FKRART': 'Rechnungsart/Rg-Freigabe',
     # 'FKKZWE': 'Kz. wertm{~iger Vorgang',
     # 'FKDTRI': 'Rechnungsdatum/intern',
     # 'FKMJBU': 'Buchungsmonat CJJMM',
     # 'FKFNRM': 'Mandanten-Nummer',
     # 'FKKDNR': 'Kunden-Nr/Warenempf{nger',
     # 'FKKDRG': 'Kunden-Nr/Rechnungsempf{nge',
     # 'FKKDN3': 'Kunden-Nr/Verbraucher',
     # 'FKVERB': 'Kunden-Nr/Verband',
     # 'FKSPSL': 'Sprache',
     # 'FKBRAN': 'Branche',
     # 'FKGEB ': 'Gebiet',
     # 'FKDSTK': 'Distrikt',
     # 'FKPLZ ': 'Postleitzahl',
     # 'FKLKZ ': 'L{nderkennzeichen',
     # 'FKNRKD': 'Kunden-Auftrags-Nummer',
     # 'FKDTKD': 'Kunden-Auftrags-Datum',
     # 'FKALS1': 'Alphasortierung/Warenempf{n',
     # 'FKALS2': 'Kennzeichen ZFB',
     # 'FKVRT ': 'Vertreter 1',
     # 'FKVRT2': 'Vertreter 2',
     # 'FKPRZA': 'Aufteilungs-%-Satz',
     # 'FKLGN2': 'Zugangslager',
     # 'FKVANR': 'Versandadress-Nr.',
     # 'FKPROB': 'Objekt/Projekt/Aktion',
     # 'FKRBP1': 'Auftrags-Rabatt-%-1',
     # 'FKRBP2': 'Auftrags-Rabatt-%-2',
     # 'FKX3R1': 'Textschl. Rabatt 1',
     # 'FKX3R2': 'Textschl. Rabatt 2',
     # 'FKKZRX': 'Kz.Rabatt Brutto',
     # 'FKTRNR': 'Touren-Nummer',
     # 'FKKZTF': 'Kennzeichen Teilfakt/Lief.',
     # 'FKKZZF': 'un-/versteuert',
     # 'FKKZRK': 'Kz: R}ckstand m¦glich',
     # 'FKKZNN': 'Kz.Nachnahme',
     # 'FKKZAR': 'Kz.Leihart',
     # 'FKKZSR': 'Sammelrechnungs-Turnus',
     # 'FKKZSD': 'Auswahl: Druck in Sammel-Rg',
     'FKBRUT': 'brutto',
     # 'FKSBRT': 'Skontierf{higer Betrag/Brt',
     # 'FKSKTA': 'Skonto-Abzugsbetrag',
     'FKNETT': 'netto',
     # 'FKMWBT': 'Mehrwert-Steuer-Betrag',
     # 'FKNETR': 'Positionswert f}r Auftr.-Ra',
     # 'FKRBB1': 'Auftragsrabatt 1',
     # 'FKRBB2': 'Auftragsrabatt 2',
     # 'FKGERB': 'Gesamt-Rabatt',
     # 'FKNBK ': 'Nebenkosten',
     # 'FKNBT1': 'Zus. Nebenkosten 1',
     # 'FKNBT2': 'Zus. Nebenkosten 2',
     # 'FKVPK ': 'Verpackungskosten',
     # 'FKVSK ': 'Versandkosten',
     # 'FKWSL ': 'W{hrungs-Kennzeichen',
     # 'FKKURS': 'Wechsel-Kurs',
     # 'FKKUFA': 'Kursfaktor',
     # 'FKMWSA': 'Umsatz-Steuerprofil',
     # 'FKEGCD': 'L{ndercode / EG',
     # 'FKUSTN': 'USt-Id-Nummer',
     # 'FKSTSL': 'Umsatz-Steuerprofil',
     # 'FKDTVF': 'Valutadatum',
     # 'FKX3ZF': 'Zahlungsbed.-Schl}ssel',
     # 'FKX3VA': 'Versandart',
     # 'FKX4NB': 'Textschl. Nebenkosten/Erf.',
     # 'FKNTY1': 'Typ/Zus. Nebenkosten 1',
     # 'FKNTY2': 'Typ/Zus. Nebenkosten 2',
     # 'FKX4N1': 'Textschl. zus. Nebenkosten',
     # 'FKX4N2': 'Textschl. zus. Nebenkosten',
     # 'FKX3LB': 'Lieferbedingung',
     # 'FKFKZR': 'KZ Rechnung drucken',
     # 'FKRGCO': 'Anzahl Rechnungen',
     'FKDTVS': 'versand_date',
     # 'FKKZRE': 'Retouren-Beleg ja/nein',
     # 'FKKZUF': 'Auswahldaten -->  AAK00',
     # 'FKKZUS': 'Update Statistikdaten',
     # 'FKSANK': 'Satznummer Kopf',
     # 'FKPROG': 'Herkunftsprogramm',
     # 'FKSNLK': 'Satznummer ALK00',
     # 'FKFBEL': 'Beleg-Nummer f}r Freigabe',
     # 'FKFAUF': 'Auftrags-Nr. f}r Freigabe',
     'FKDTFA': 'druck_date',
     # 'FK30ST': 'Kz Fakt.Update Statistik',
     # 'FK30FI': 'Kz Fakt.Update FIBU',
     # 'FKKZBB': 'Status FIBU-Verbuchung',
     # 'FKKZRV': 'Kz Rechnungsvorb. erforderl',
     # 'FKKZKD': 'Nachtr. Erstellung / 9 = ja',
     # 'FKKZKS': 'Steuerung Kundenstrecke',
     # 'FKKZTL': 'Andruck in Tourenliste',
     # 'FKTRPO': 'Pos-Nummer/Tourenliste',
     # 'FKTRLN': 'Nr Tourenl.',
     # 'FKDTTL': 'Datum/Tourenliste',
     # 'FKLDLN': 'Nr Ladel.',
     # 'FKLDLP': 'PosNr Ladeliste',
     # 'FKKZLL': 'Kz. Ladeliste',
     # 'FKDTLL': 'Datum/Ladeliste',
     'FKKZIB': 'interner_beleg',
     # 'FKKZRL': 'Andruck in Rg-Liste',
     # 'FKKZPR': 'Abgleich  FIBU/Statistik',
     # 'FKKZSE': 'Ausg.SEDAS-Schnittstelle',
     # 'FKOQZO': 'Outqueue-Zuordnung',
     # 'FKZUOR': 'Zuordnung f}r dezentrale An',
     # 'FKKZ01': 'Packstatus Beleg',
     # 'FKKZ05': 'Sort SmlRg',
     # 'FKSORT': 'Sortierfeld',
     # 'FKSNRG': 'StzNr SmlRg',
     # 'FKDFSL': 'Dateif}hrungs-Schluessel',
     'FKSTAT': 'status',
     'FKDTER': 'erfassung_date',
     'FKZTER': 'erfassung_time',
     },


'AFU00': # Rechnungspositionen
        {
        'FUFNR': 'firma',
        'FURGNR': 'rechnungsnr',
        # 'FURGNI': 'Intern Rechnungs-Nr.',
        # 'FUAUFN': 'Auftrags-Nummer',
        # 'FUAUPO': 'Auftrags-Position',
        # 'FUPOZU': 'Zusatz-Position',
        # 'FUABT ': 'Abteilung/Auftrag',
        # 'FUKDNR': 'Kunden-Nr/Warenempf{nger',
        # 'FUKDRG': 'Kunden-Nr/Rechnungsempf{nge',
        # 'FUFNRX': 'Best.-F}hrungsfirma',
        'FUARTN': 'artnr',
        # 'FUARTG': 'Artikel-Gruppe',
        # 'FUARTH': 'Artikel-Haupt-Gruppe',
        # 'FULGNR': 'Lager',
        # 'FULGRP': 'Lagergruppe',
        # 'FULGPL': 'Lager-Platz',
        'FUMNG': 'menge',
        # 'FUMESL': 'MENGENEINHEIT/STAT.',
        # 'FUPNET': 'Positions-Netto-Wert',
        # 'FUPBUT': 'Positions-Brutto-Wert',
        # 'FUPORB': 'Positions-Rabatt-Betrag',
        # 'FUPREW': 'Wert/Ek',
        # 'FUPREV': 'Verkaufs-Preis',
        # 'FUPEBN': 'Ebene Preisfindung',
        # 'FUKZPR': 'Preiskennzeichen Verkauf',
        # 'FUMEPR': 'Preis-Einheit',
        # 'FUFAPR': 'Faktor APMEH --> APMEPR',
        # 'FUPREE': 'Einstands-Preis',
        # 'FUWSLE': 'W{hrungsschl}ssel: EK',
        # 'FUPRDL': 'Preiskennzeichen Lager',
        # 'FUKUFA': 'Kursfaktor/EK',
        # 'FUKURS': 'Wechselkurs EK',
        # 'FUMWAR': 'Steuerart / Artikel',
        # 'FUDEZI': 'Anzahl Dezimalstellen',
        # 'FUKZBE': 'Ohne Bestandsf}hrung',
        # 'FUKZST': 'Kennzeichen Streckengesch{f',
        # 'FUKZKO': 'Mit Komponentenaufl¦sung',
        # 'FUKZRB': 'Kz: kein Auftragsrabatt = 1',
        # 'FUKZRD': 'Kz:Pos in Rech.nicht andr.=',
        # 'FUKZRU': 'Kz.Rabatt versteckt',
        # 'FUKZRX': 'Kz.Rabatte Brutto',
        # 'FUKZSE': 'Serien-/Chargenartikel',
        # 'FUKZSO': 'Kennzeichen Sonderartikel',
        # 'FUKZPS': 'Positionsbezogener Set',
        # 'FUPRKZ': 'Preiskennzeichen Lager',
        # 'FUKZEP': 'KZ: Eigen-Produkt',
        # 'FUKZZU': 'Zuschlagssatz',
        # 'FUGANZ': 'Bestellte Gebinde',
        # 'FUVOLM': 'Volumen/Inhalt',
        # 'FUGEWI': 'Gewicht',
        # 'FULFSN': 'Lieferschein-Nr./7-stellig',
        # 'FUDTLF': 'Datum/LfS-Druck',
        # 'FUDTFA': 'Datum/Rechungsdruck',
        # 'FUKZRV': 'Kz Rechnungsvorb. erforderl',
        # 'FUFKZR': 'Kennzeichen Rechnung drucke',
        # 'FU30ST': 'Kz Fakt.Update Statistik',
        # 'FU30FI': 'Kz Fakt.Update FIBU',
        # 'FULBUF': 'Kz.Lagerbuchung fehlt',
        # 'FUSORT': 'Sortierfeld',
        # 'FUSANK': 'Satznummer Kopf',
        # 'FUSANP': 'Satznummer',
        # 'FUPROG': 'Herkunftsprogramm',
        # 'FUSNLN': 'Satznummer ALN00',
        # 'FUSNST': 'Satznummer Statistik',
        # 'FUSANR': 'Bezogene Bewegung',
        # 'FURES1': 'Reserve-Feld-01',
        # 'FURES2': 'Reserve-Feld-02',
        # 'FURES3': 'Packstatus BlgPos',
        # 'FURES5': 'Reserve-Feld-05',
        # 'FUBTR1': 'Reserve-Feld-Betrag 1',
        # 'FUBTR2': 'Reserve-Feld-Betrag 2',
        # 'FUKZ02': 'EstdPrs XSL',
        # 'FUAUPS': 'bezogene Position',
        # 'FUSNRG': 'StzNr SmlRg',
        # 'FUDTRI': 'Rechnungsdatum/intern',
        'FUDTER': 'erfassung_date',
        'FUZTER': 'erfassung_time',
        },


'ASK00': {# Set/Komponenten
           # 'SKFNR':  'firma',
           'SKARTN': 'artnr',
           # 'SKAUFN': 'Auftrags-nr',
           # 'SKAUPO': 'Auftrags-Position',
           'SKLFNR': 'laufende_nr',
           'SKKART': 'komponenten_artnr',
           'SKMENG': 'menge_im_set',
           # 'SKKZSE': 'KENNZ: SERIEN-NR.-ARTIKEL',
           # 'SKKZ01': 'AbwStddSet',
           # 'SKKZ02': 'Kz. ohne Bestandsf.',
           # 'SKKZ03': 'Auf/Ang',
           # 'SKPROZ': 'Anteil am Umsatz in %',
           # 'SKKZPD': 'Kz PrFnd',
           # 'SKHPTP': 'NatRbPos',
           # 'SKDTAE': 'updated_at_date',
           # 'SKZTAE': 'updated_at_time',
           },

'AVA00': {
         #'VAFNR':  Firma
         'VAKDNR': 'kundennr',
         'VAVANR': 'versandadresssnr',
         #'VAFGRP'  Firmen-Gruppe
         #'VAAGRP'  Abteilungs-Gruppe/Sparte
         #'VAABT'  Abteilungs-nr
         'VASANR': 'satznr', # gepackt
         #'VAX3VS'  Versandarten-Schlüssel
         #'VAX3LB'  Lieferbedingungs-Schlüssel
         #'VAINFO'  Interne Information
         #'VAVRT'  Vertreter-nr
         #'VAEXPR'  Expressgut-Station
         #'VATRNR'  Auslieferungstour
         'VAILN': 'iln',
         #'VADTER'  Datum Erfassung CJJMMTT
         #'VADTAE'  Datum l. Änderung CJJMMTT
         #'VADFSL'  Dateiführungs-Schlüssel
         'VASTAT': 'satzstatus',
       },
       
'BBU00': { # Buchungspositionen in der Buchhaltung
    # 'BUFNR':  'Firmennr. Sachbuchung',
    # 'BUBHKZ': 'Buchhaltungskennz. D,K,S',
    'BUPKTO': 'personenkonto',
    'BUKTO': 'konto_sachbuchhaltung',
    'BUBELN': 'belegnr',
    #'BUBELK': 'Beleg-Nummer Kreis',
    #'BUABKR': 'Abstimmkreis',
    'BUDTBL': 'beleg_date',
    #'BUMMBU': 'MM Bumo',
    #'BUJJBU': 'JJ Bumo',
    #'BUJHBU': 'JH Bumo',
    #'BUGJ':   'Gesch{ftsjahr',
    'BUBLRT': 'belegart',
    #'BUBUSL': 'externe Belegart',
    #'BUBLRA': 'Anzeigebelegart',
    #'BUBLRE': 'externe Belegart',
    'BUSOHA': 'soll_oder_haben', # 'S' oder 'H'
    'BUGKTO': 'gegenkonto_sachbuchhaltung',
    # 'BUGKSH': 'Gegenkonto-Soll/Haben',
    'BUOINF': 'op_info',
    'BUBTXT': 'Buchungstext',
    # 'BUOPSN': 'OP-Nr. f}r OP-f}hrendes Sac',
    'BUSTOR': 'S = Stornobuchung',
    #'BUSTRT': 'Steuer-Art',
    #'BUEGCD': 'L{ndercode / EG',
    #'BUSTSL': 'Steuer-Schl}ssel',
    #'BUSTKT': 'Steuerkonto Sachbuchhaltung',
    #'BUSTKZ': 'Steuerkennzeichen',
    #'BUSTBT': 'Steuer-Betrag',
    #'BUSTWB': 'Steuer in W{hrung',
    #'BUSTSH': 'Soll/Haben Steuer',
    # 'BUBUBT': 'buchungsbetrag Kto/Pers.-kt',
    'BUNEBT': 'buchungsbetrag_gegenkonto',
    'BUNEWB': 'buchungsbetrag',
    # 'BURGNR': 'rechnungsnr',
    # 'BURPOS': 'rechnungspos',
    # 'BUABZU': 'Abzugsbetrag',
    # 'BUABKZ': 'Art des Abzuges',
    # 'BUSCKE': 'Scheckeinr.Kz',
    # 'BUVKTO': 'Verbandskto. nr.',
    # 'BUMKTO': 'Mischkonto',
    # 'BUZAKZ': 'W{hrungsart',
    'BUWSL': 'waehrung',
    'BUWBBT': 'waehrungbetrag',
    'BUKURS': 'kurs',
    'BUKUFA': 'kursfaktor',
    # 'BUBDIM': 'Betragsdimension',
    # 'BUBUI1': 'Informationsfeld 1',
    # 'BUBUI2': 'Informationsfeld 2',
    # 'BUBUI3': 'Informationsfeld 3',
    # 'BUBUI4': 'Informationsfeld 4',
    # 'BUBUI5': 'Info-Kennz. 1',
    # 'BUBUI6': 'Info-Kennz. 2',
    # 'BUBUI7': 'Info Text',
    # 'BUBUI8': 'Info Betrag',
    # 'BUFNRA': 'An Firmen-Nr',
    'BUAUFN': 'auftragsnr',
    'BUAUPO': 'auftragspos',
    # 'BUHERK': 'Kz. Herkunft',
    # 'BUEBKZ': 'Kz. EB-Buchung',
    # 'BUOPAR': 'Art Automatikbuchung',
    # 'BUGRKZ': 'Zusammengeh¦rigkeit',
    # 'BUAUTO': 'Automatische Buchung',
    # 'BUJONP': 'Journalnummer',
    # 'BUJONS': 'Journalnr. Sachbuchhaltung',
    'BUUMKZ': 'Umsatzkz.',
    # 'BUKGRU': 'Umsatzkz.Steuerkto.',
    # 'BUKZK':  'Kennz. Buchung',
    # 'BUKZP':  'Kennz. Buchung',
    # 'BUKZG':  'Kennz. Buchung',
    # 'BUKZS':  'Kennz. Buchung',
    # 'BUBLAE': 'Beleg Anf./Ende',
    # 'BUASYN': 'Kennz.Asynchr.geb.',
    # 'BUWZKZ': 'W = Wechselzahlung',
    # 'BUDTWF': 'Wechself{lligkeit',
    # 'BUDTWS': 'Wertstellung Zahlung',
    # 'BUZPNR': 'Zahlungsplannummer',
    # 'BUZPOS': 'Zahlungsplanpos.',
    # 'BUKSKZ': 'Kostenstellen-Kennzeichen',
    # 'BUKST':  'Kostenstelle',
    # 'BUMMLK': 'MM Kst.rg',
    # 'BUJJLK': 'JJ Kst-rg.',
    # 'BUJHLK': 'JH Kst-rg.',
    # 'BUGJLK': 'GJ Kst-rg.',
    # 'BUKOBZ': 'Konzernbeziehung',
    # 'BUKZSB': 'Kennz. Schattenbuchung',
    # 'BUSPAR': 'Abteilungs-Gruppe/Sparte',
    # 'BUKZNB': 'Kennz. durch Nebenbuchh.',
    'BUUSR1': 'user_1', # name des Sachbaerbeiters
    # 'BUUSR2': 'User 2',
    # 'BUMBR':  'Member',
    'BUDTER': 'erfassung_date',
    'BUSBNR': 'herkunft',
    'BUSANK': 'Kopfsatznr.',
    'BUSPOP': 'Satznr.Pers.OP',
    # 'BUSSOP': 'Satznr.Sach-OP',
    'BUSANR': 'satznummer',
    # 'BUBTPW': 'Bruttobetrag PW',
    # 'BUNBPW': 'Nettobetrag PW',
    # 'BUSTPW': 'Steuerbetrag PW',
    # 'BUWSPW': 'waehrung_?', # weiter oben gibt es auch nochmal währungsfelder
    # 'BUKUPW': 'kurs__?',
    # 'BUFAPW': 'kursfaktor_?',
    # 'BUKZPW': 'W{hrungsart',
    # 'BUSKBT': 'Skontobetrag',
    # 'BUSKKZ': 'Skontokennzeichen',
    # 'BUSKSL': 'Skonto-Schl}ssel',
    # 'BUDKSL': 'Skt.Schl.Delcedere',
    'BUDTVA': 'valuta_date',
    'BUDTFL': 'faellig_date',
    'BUSABB': 'satznummer',
    # 'BUKGR2': 'K}rzungsgrund',
    'BUR1': 'buchungsbetrag',
    # 'BUR2':   'Feld 1 A',
    # 'BUR3':   'Feld 1 A',
    # 'BUR4':   'Feld 1 A',
    # 'BUR5':   'Feld 1 A',
    # 'BUR6':   'Feld 10 A',
    'BUR7': 'satz_date',
},


'BED00': { # Datei fuer die Zentrale-Meldung innerg. Warenverk.
          'EDFNR ': 'firmennr',
          'EDPKTO': 'personenkonto',
          'EDBEKZ': 'berichtigungskennzeichen',
          # 'EDJHBU': '???',
          'EDJJBU': 'buchungsjahr',
          'EDQUAR': 'meldequartal',
          'EDEGCF': 'egcd_fremde_firma',
          'EDUSTF': 'ustnr_fremde_firma',
          'EDEGCE': 'egcd_eigene_firma',
          'EDUSTE': 'ustnr_eigene_firma',
          'EDBEMG': 'bemessgrundlage',
          'EDKZ  ': 'warenbewegungskennzeichen',
          'EDKZDR': 'kennzeichen_dreiecksgeschaeft',
          'EDRAFF': 'geraffte_daten',
          'EDA01A': 'buchhaltungskennzeichen',
          # 'EDRBT ': 'reserve_betrag',
          'EDSTBT': 'steuerbetrag',
          },

# BOP00 - Offene Posten?
#     OPFNR               BOP00              Firmennummer               
#     OPBHKZ              BOP00              Buchhaltungskennz. D,K,S   
#     OPBLRT              BOP00              Belegart                   
#     OPBLRA              BOP00              Anzeigebelegart            
#     OPBUSL              BOP00              externe Belegart           
#     OPRGNR              BOP00              Rechnungs-Nummer           
#     OPRPOS              BOP00              Rechnungsposition          
#     OPRGLS              BOP00              Rechnungslistennr.         
#     OPPKTO              BOP00              Personenkonto KD/LIEF      
#     OPVKTO              BOP00              Verbandskto. nr.           
#     OPMKTO              BOP00              Mischkonto                 
#     OPKTO               BOP00              Konto-Sachbuchhaltung      
#     OPWSL               BOP00              W{hrungs-Kennzeichen       
#     OPAUSB              BOP00              Auszahlungsbetrag          
#     OPDTEB              BOP00              F{lligkeit Einbehalt        
#     OPSKBT              BOP00              Skontobetrag                
#     OPSKKZ              BOP00              Skontokennzeichen           
#     OPSKSL              BOP00              Skonto-Schl}ssel            
#     OPDKSL              BOP00              Skt.Schl.Delcedere          
#     OPZAHL              BOP00              Zahlangabe                  
#     OPBKLF              BOP00              Laufnr. vers. Banken/Konten 
#     OPMAHN              BOP00              Mahnabgabe                  
#     OPMSTU              BOP00              Mahnstufe                   
#     OPUKKZ              BOP00              Ungepr. Kreditorenrg.       
#     OPZPNR              BOP00              Zahlungsplannummer          
#     OPZPOS              BOP00              Zahlungsplanpos.            
#     OPDTVA              BOP00              Valutadatum                 
#     OPDTFL              BOP00              F{lligkeitsdatum            
#     OPDTLM              BOP00              Datum letzte Mahnung        
#     OPDTBL              BOP00              Belegdatum                  
#     OPMMBU              BOP00              MM Bumo                     
#     OPJJBU              BOP00              JJ Bumo                     
#     OPJHBU              BOP00              JH Bumo                     
#     OPGJBU              BOP00              Gesch{ftsjahr               
#     OPMMAU              BOP00              TT Ausgleich                
#     OPJJAU              BOP00              JJ Ausgleich                
#     OPJHAU              BOP00              JH Ausgleich                
#     OPGJAU              BOP00              Gesch{ftsjahr               
#     OPOINF              BOP00              OP-Information              
#     OPVRT               BOP00              Vertreter-Nummer            
#     OPABT               BOP00              Abteilungs-Nummer           
#     OPBRAN              BOP00              Branche                     
#     OPGEB               BOP00              Gebiet                      
#     OPAKAN              BOP00              Akt.Betrag Anbu             
#     OPSKZA              BOP00              Gew{hrter Skonto            
#     OPDSLD              BOP00              Durchschn. Saldo            
#     OPWZZA              BOP00              Zahlungswartezeit           
#     OPOPBT              BOP00              Betrag off. Posten          
#     OPOPWB              BOP00              W{hrungsbetrag              
#     OPOPPW              BOP00              Rechnungsbetrag             
#     OPOPSH              BOP00              Kennzeichen Soll/Haben = S/ 
#     OPRGBT              BOP00              Rechnungsbetrag             
#     OPRGWB              BOP00              W{hrungsbetrag              
#     OPRGPW              BOP00              Rechnungsbetrag             
#     OPRGSH              BOP00              Kennzeichen Soll/Haben = S/ 
#     OPKURS              BOP00              Wechsel-Kurs                
#     OPKUFA              BOP00              Kursfaktor                  
#     OPBDIM              BOP00              Betragsdimension            
#     OPZAKZ              BOP00              W{hrungsart                 
#     OPSPAR              BOP00              Abteilungs-Gruppe/Sparte    
#     OPKZSB              BOP00              Kennz. Schattenbuchung      
#     OPSRT1              BOP00              Sort-Kennz. 1               
#     OPSRT2              BOP00              Sort-Kennz. 2               
#     OPSRT3              BOP00              Sort Text                   
#     OPSRT4              BOP00              Sort Betrag                 
#     OPSRT5              BOP00              Sort 5/Anz.tr{ger           
#     OPSRT6              BOP00              Sort 6/Anz.kennz.           
#     OPKZRG              BOP00              KZ J/N                      
#     OPBUI1              BOP00              Informationsfeld 1          
#     OPBUI2              BOP00              Informationsfeld 2          
#     OPBUI3              BOP00              Informationsfeld 3          
#     OPBUI4              BOP00              Informationsfeld 4          
#     OPBUI5              BOP00              Info-Kennz. 1               
#     OPBUI6              BOP00              Info-Kennz. 2               
#     OPBUI7              BOP00              Info Text                   
#     OPBUI8              BOP00              Info Betrag                 
#     OPOPR1              BOP00              Feld 10 A                   
#     OPOPR2              BOP00              Feld 10 A                   
#     OPOPR3              BOP00              Feld 10 A                   
#     OPOPR4              BOP00              Feld 20 A                   
#     OPOPR5              BOP00              Feld 1 A                    
#     OPOPR6              BOP00              Feld 1 A                    
#     OPOPR7              BOP00              Buchungsbetrag              
#     OPSANR              BOP00              Satznummer                  
#     OPSAN1              BOP00              Satznummer             
#     OPWSPW              BOP00              W{hrungs-Kennzeichen   
#     OPKUPW              BOP00              Wechsel-Kurs           
#     OPFAPW              BOP00              Kursfaktor             
#     OPKZPW              BOP00              W{hrungsart            


'EBL00': { # Bestellköpfe
     #'BLFNR ': 'firma',
     'BLBSTN': 'bestellnr',
     #'BLFNRH': 'Firma / Herkunft',
     #'BLFNRZ': 'zentrale Einkaufsfirma',
     #'BLFNRR': 'Firma f}r Rahmen',
     #'BLBSRH': 'Nummer der Rahmenvereinb.',
     'BLSBNR': 'sachbearbeiter_erfassung',
     #'BLABT': 'Abteilungs-Nummer',
     #'BLAGRP': 'Abteilungs-Gruppe/Sparte',
     #'BLFGRP': 'Firmen-Gruppe',
     #'BLFNRK': 'Firmen-Nr. f}r Zugriff XLI0',
     'BLLINR': 'lieferant',
     #'BLRGST': 'abw. Rechnungssteller',
     #'BLEGCD': 'EG-Code Warenlieferant',
     'BLHRST': 'hersteller',
     #'BLVANR': 'Lieferadresse',
     'BLX3ZB': 'zahlungsbedingung',
     'BLX3VA': 'versandart',
     'BLX3LB': 'lieferbedingung',
     #'BLX3VS': 'Versicherung',
     # 'BLVPKE': 'Verpackungskosten/Erfassung',
     'BLVSKE': 'versandkosten_erfassung',
     'BLNBKE': 'nebenkosten_erfassung',
     'BLX4NB': 'nebenkosten_textschluessel',
     #'BLRBP1': 'Gesamt-Rabatt-%-1',
     #'BLX4R1': 'Textschl}ssel Rabatt 1',
     #'BLA1R1': 'Rabattart / Rabatt 1',
     #'BLRBP2': 'Gesamt-Rabatt-%-2',
     #'BLX4R2': 'Textschl}ssel Rabatt 1',
     #'BLA1R2': 'Rabattart / Rabatt 2',
     'BLWSL': 'waehrung',
     'BLKURS': 'kurs',
     'BLKUFA': 'kursfaktor',
     #'BLSPSL': 'Sprache',
     #'BLVPKB': 'Verpackungskosten/Berechnet',
     'BLVSKB': 'versandkosten_berechnet',
     'BLNBKB': 'nebenkosten_berechnet',
     'BLLIAU': 'auftragsnr_lieferant',
     #'BLSBN2': 'Zust{ndiger Eink{ufer',
     #'BLSBN3': 'Sachbearb./Unterschrift 1',
     #'BLSBN4': 'Sachbearb./Unterschrift 2',
     'BLSBN5': 'anfordernder_sachbearbeiter',
     'BLINFO': 'zusatzinformation',
     'BLKTO ': 'konto',
     #'BLKTR ': 'kostentr{ger',
     #'BLUKTR': 'Kostentr{gerunternummer',
     #'BLKST ': 'Kostenstelle',
     #'BLPROJ': 'Projekt',
     #'BLUPRJ': 'Projektunternummer',
     'BLLGNR': 'lager',
     'BLBTXT': 'buchungstext',
     #'BLGSCH': 'Art des Gesch{fts',
     #'BLVERF': 'Verfahren/INTRASTAT',
     'BLKZBA': 'art',
     #'BLKZDR': 'Kennzeichen Andruck',
     #'BLKZIE': 'Kz: Intern/Extern',
     #'BLKZSI': 'Kz: Sperre / inaktiv',
     #'BLKZBD': 'KZ: Bestellung drucken',
     #'BLKZLE': 'KZ: Liefererinnerung drucke',
     #'BLTGLE': 'Tg.Lief.Erinnerung',
     #'BLKZMA': 'KZ: Mahnung drucken',
     #'BLKZBR': '¢nd-Berecht.',
     'BLKZKS': 'kurssicherung',
     #'BLKZWU': 'wust_hinweis',
     'BLTYP': 'typ',
     #'BLKZRV': 'Rechnungsvorlage',
     #'BLKZVT': 'SS Eink/Auftrg',
     #'BLKZDT': 'Wunschtermin',
     #'BLDTWU': 'Wunschtermin',
     'BLDTBE': 'termin',
     'BLDTER': 'erfassung_date',
     'BLDTAE': 'aenderung_date',
     #'BLDTMA': 'Datum letzte Mahnung',
     'BLDTBD': 'druck_date',
     'BLDTFA': 'rechnungs_date',
     'BLPANZ': 'positionen_zahl',
     'BLPANV': 'positionen_voll_ausgeliefert',
     'BLPANF': 'positionen_voll_berechnet',
     #'BLDANZ': 'Druckz{hler Gesamt',
     #'BLDANA': 'Druckz{hler ¢nderung',
     'BLBWRT': 'bestell_wert',
     #'BLF1A4': 'Feld 4 1A',
     #'BLF10A': 'Feld 10A',
     #'BLFMG1': 'Reservefeld Menge 1 13P3',
     #'BLFMG2': 'Reservefeld Menge 2 13P3',
     #'BLFWR1': 'Reservefeld Wert 1 15P2',
     #'BLFWR2': 'Reservefeld Wert 2 15P2',
     #'BLF101': 'Reservefeld 1 10A',
     #'BLF102': 'Reservefeld 2 10A',
     #'BLF1A6': 'Reservefeld 6 1A',
     #'BLF1A7': 'Reservefeld 7 1A',
     #'BLF1A8': 'Reservefeld 8 1A',
     #'BLF1A9': 'Reservefeld 9 1A',
     #'BLF1A0': 'Reservefeld 10 1A',
     #'BLF1AA': 'Feld 11 1A',
     #'BLF1AB': 'Feld 12 1A',
     #'BLF1AC': 'Feld 13 1A',
     #'BLF1AD': 'Feld 14 1A',
     #'BLFDT1': 'Reservefeld Datum 1',
     #'BLFDT2': 'Reservefeld Datum 2',
     #'BLFK20': 'Kunden-Spez',
     'BLDFSL': 'dateifuehrungsschluessel',
     'BLSTAT': 'status'
     },

# BZT00: Bestellzusatztexte
#     ZTSANR              BZT00              Satznummer                
#     ZTLFNR              BZT00                                        
#     ZTTX60              BZT00                                        
#     ZTKZ1               BZT00                                        
#     ZTKZ2               BZT00                                        
#     ZTKZ3               BZT00                                        
#     ZTKZ4               BZT00                                        
#     ZTDTER              BZT00              Datum Erfassung CJJMMTT   
#     ZTDTAE              BZT00              Datum l. ¢nderung CJJMMTT 
#     ZTSBER              BZT00              Sachb. Erfassung          
#     ZTSBAE              BZT00              Sachb.letzte ¢nderung     
#     AXFNR               EAX00              Firma                       
#     AXARTN              EAX00              Artikel/Art-Gruppe          
#     AXSPSL              EAX00              Sprache                     
#     AXLFNR              EAX00              Lfd. Nummer                 
#     AXABT               EAX00              Abteilungs-Nummer           
#     AXAGRP              EAX00              Abteilungs-Gruppe/Sparte    
#     AXFGRP              EAX00              Firmen-Gruppe               
#     AXTX60              EAX00              Text / 60-stellig           
#     AXKZAB              EAX00              Andruck auf Bestell-Formula 
#     AXKZLF              EAX00              Andruck auf Wareneing-Beleg 
#     AXKZRG              EAX00              Interner Text               
#     AXKZ04              EAX00              Kennzeichen 04              
#     AXKZ05              EAX00              Kennzeichen 05              
#     AXKZ06              EAX00              Kennzeichen 06              
#     AXKZ07              EAX00              Kennzeichen 07              
#     AXKZ08              EAX00              Kennzeichen 08              
#     AXKZ09              EAX00              Kennzeichen 09              
#     AXKZ10              EAX00              Kennzeichen 10              
#     AXKZ11              EAX00              Kennzeichen 11              
#     AXKZ12              EAX00              Kennzeichen 12              
#     AXKZ13              EAX00              Kennzeichen 13              
#     AXKZ14              EAX00              Kennzeichen 14              
#     AXKZ15              EAX00              Kennzeichen 15              
#     AXDTAE              EAX00              Datum letzte ¢nderung       
#     AXDFSL              EAX00              Dateif}hrungs-Schl}ssel     
#     AXSTAT              EAX00              Satzstatus                  

'EBP00': { # Bestellpositions-Datei
          'BPMNGB-BPMNGL': 'menge_offen',
          # 'BPFNR ': 'Firma',
          'BPBSTN': 'bestellnr', # bei uns auch als P.O. bekannt
          'BPBSTP': 'bestellpos',
          # 'BPBSAB': 'Nummer des bez. Rahmenvert.',
          # 'BPBSAP': 'Pos-Nr des bez. Rahmenvert.',
          # 'BPTGAR': 'Tage bis Druck Abr-Pos',
          'BPSBNR': 'created_by',
          'BPSBAE': 'updated_by',
          # 'BPABT ': 'Abteilungs-Nummer',
          # 'BPAGRP': 'Abteilungs-Gruppe/Sparte',
          # 'BPFGRP': 'Firmen-Gruppe',
          'BPHPOS': 'hauptpositionnr',
          'BPBDIF': 'beschaffung_deckung', # Tg BeschffgT. - DeckgT.',
          # 'BPANNR': 'Anfrage',
          # 'BPANPO': 'Anfrageposition            ',
          'BPARTN': 'artnr',
          # 'BPLIAR': 'Lieferanten-Artikel-Nr     ',
          # 'BPARTG': 'Artikel-Gruppe             ',
          # 'BPARTH': 'Artikel-Haupt-Gruppe       ',
          'BPLGNR': 'lager',
          # 'BPWERK': 'Werksnummer                ',
          # 'BPEMPF': 'Empfangsstelle             ',
          # 'BPKZSO': 'KZ: Sonderartikel          ',
          # 'BPKZBE': 'KZ: Ohne Bestandsf}hrung   ', immer 0
          # 'BPFNRK': 'Fnr/Zugriff Lief-Adresse   ',
          'BPLINR': 'lieferant',
          # 'BPHRST': 'Hersteller                 ',
          # 'BPFNRZ': 'zentrale Einkaufsfirma     ',
          # 'BPFNRR': 'Firma f}r Rahmen',
          # 'BPHERK': 'KZ: Herkunft',
          # 'BPURSA': 'verursacher',
          # 'BPSBN2': 'einkaeufer',
          # 'BPSBN5': 'Anfordernder Sachbearbeiter',
          # 'BPINFN': 'informationsnummer',
          # 'BPINFA': 'informationsnummernart',
          # 'BPPRVO': 'Pr}f-Vorschriften-Nr',
           'BPMEER': 'mengeneinheit_rechnung?',
          # 'BPFAKT': 'Umrechn.-Faktor  ER-->BE',
          'BPMEBE': 'mengeneinheit_bestellung',
          # 'BPDEZI': 'Anzahl Dezimalstellen',
          # 'BPKTO ': 'Konto',
          # 'BPKTR ': 'Kostentr{ger',
          # 'BPUKTR': 'Kostentr{gerunternummer    ',
          # 'BPKST ': 'Kostenstelle               ',
          # 'BPPROJ': 'Projekt                    ',
          # 'BPUPRJ': 'Projektunternummer         ',
          # 'BPFABG': 'Leistungsart/FremdArbGang  ',
          # 'BPKZFG': 'Fremdarbeitsgangart        ',
          # 'BPX3VP': 'Verpackungsvorschrift      ',
          # 'BPLIAU': 'Lieferanten-Auftrags-Nummer',
          'BPWSL': 'waehrung_bestellpreis',
          'BPPREW': 'wert',
          #'BPKZWR': 'Preisdimension',
          'BPPRAK': 'tatsaechlicher_preis',
          # 'BPPREB': 'Bestaetigter Preis',
          # 'BPNANZ': 'Anzahl Rabatte',
          'BPPREH': 'bestell_preis_eur',
          'BPWRTF': 'abgerufener_positionswert',
          # 'BPWRTR': 'Summe Rabatte              ',
          # 'BPWRTN': 'nebenkosten',
          # 'BPMNGE': 'Bestell-Menge/Bestell-Eh   ',
          # 'BPMNG3': 'Menge vor ¢nder./Bestell-Eh',
          # 'BPMNG4': 'Best{tigte Menge/Best-Eh   ',
          'BPMNGB': 'bestellmenge',
          'BPMNGL': 'gelieferte_menge',
          # 'BPMNG1': 'Vorabgebuchte gelief. Menge',
          # 'BPMNGA': 'Reserviert f}r Auftr{ge    ',
          'BPMNGF': 'gebuchte_menge',
          # 'BPMNG2': 'nicht mehr verwendet       ',
          'BPMNGO': 'ausschuss_menge',
          # 'BPMNGQ': 'MengeQualLager',
          # 'BPMSTU': 'Mahnstufe',
          # 'BPKZMI': 'Mahn-Inhalt',
          # 'BPKZLA': 'Mahn Lief-AB',
          # 'BPTGLE': 'Tg.Lief.Erinnerung',
          # 'BPTYP ': 'Bestelltyp',
          # 'BPTART': 'Feld 2 1A',
          # 'BPKZVT': 'SStEinkaufAuftrag',
          # 'BPKZDT': 'wunsch_date',
          # 'BPKZFA': 'Art Fremdarbeitgang',
          # 'BPKZCZ': 'Chargenzertifikat',
          # 'BPKZEK': 'Feld 2 1/0',
          # 'BPKZNP': 'KZ Nebenposition',
          # 'BPKZPZ': 'Positionszusatz',
          # 'BPKZUL': 'Unlimitiert',
          # 'BPKZBP': 'KZ: Bestellbestand/Periode',
          # 'BPKZAB': 'KZ: Bestell-Abschlu~',
          # 'BPKZQU': 'KZ: Qualit{tspr}fung',
          # 'BPKZAE': 'KZ: Position ge{ndert',
          # 'BPKZAK': 'KZ: Abgeschlossen',
          # 'BPKZAR': 'KZ: Rahmen / Abruf',
          # 'BPKZDR': 'Bereits im Formular gedruck',
          # 'BPKZAD': 'KZ: Artikel-Nr andrucken',
          'BPKZGL': 'geliefert_kz',
          # 'BPPZGL': '%-Satz VollGeliefert',
          # 'BPKZTF': 'KZ: Teillieferung erlaubt',
          # 'BPKZUE': 'KZ: Unter-/!berlieferung',
          # 'BPPZUE': '%-Satz erlaubte !nter/!berL',
          # 'BPKZIE': 'KZ: Intern/Extern',
          # 'BPKZLE': 'KZ: Liefererinnerung drucke',
          # 'BPKZMA': 'KZ: In Mahnung andrucken',
          # 'BPKZPP': 'Kz: Pr}fpflichtig',
          # 'BPKZPR': 'Kz: Gesch{tzter Preis',
          # 'BPKZSP': 'Sperre f}r Bestellandruck',
          # 'BPKZWZ': 'KZ: Warenzugang nicht buche',
          # 'BPKZTX': 'KZ: Textart',
          # 'BPKZLT': 'KZ: Art Liefertermin',
          # 'BPKZMB': 'KZ: Materialbeistellung',
          # 'BPKZAL': 'KZ: EAL00 versorgen',
          # 'BPKZTA': 'KZ: im TA verarbeitet',
          # 'BPKZBM': 'KZ Dialog',
          'BPKZSG': 'streckengeschaeft',
          # 'BPKZSI': 'Kz: Sperre / inaktiv',
          # 'BPSPLT': 'KZ Splitting',
          # 'BPLIZF': 'KZ Lieferantenzertifizierun',
          # 'BPKEYI': 'Kd-Individueller Schl}ssel',
          'BPDTLT': 'liefer_date',
          # 'BPWWLT': 'Liefertermin/Woche',
          # 'BPDTRV': 'Rahmen g}ltig von',
          # 'BPDTRB': 'Rahmen g}ltig bis',
          # 'BPDTLE': 'Datum Liefererinnerung',
          # 'BPDTMA': 'Datum letzte Mahnung',
          # 'BPDTBD': 'Datum letzter Bestellandruc',
          # 'BPDTWU': 'wunsch2_date',
          # 'BPDTBE': 'termin1_date',
          # 'BPDTBL': 'termin2_date',
          'BPDTLZ': 'zugang_date',
          'BPDTER': 'erfassung_date',
          # 'BPDTAE': 'aenderung_date',
          # 'BPDTAL': 'Datum der Lieferanten-AB',
          'BPDFSL': 'dateifuehrungsschluessel',
          'BPSTAT': 'status',
           },
    

# Eingangsrechnungen
#     ERFNR               EER00              Firma                 
#     ERABT               EER00              Rg.-Pr}fungs-Abteilung
#     ERBSTN              EER00              Bestell-Nummer        
#     ERFNRK              EER00              Firma                 
#     ERRGST              EER00              abw. Rechnungssteller 
#     ERLINR              EER00              Lieferant / Bestellung
#     ERZRFN              EER00              ZentralEk: Firma      
#     ERZRLI              EER00              ZentralEk: RgSteller  
#     EREGCD              EER00              EG-Code Warenlieferant
#     ERFNRZ              EER00              zentrale Einkaufsfirma
#     ERFNRR              EER00              Firma f}r Rahmen      
#     ERDTWZ              EER00              Zugangsdatum          
#     ERLFSN              EER00              Lieferscheinnummer    
#     ERFORM              EER00              KZ:Rechnungs-Art      
#     ERBUMO              EER00              Buchungsmonat CJJMM     
#     ERDTBL              EER00              Belegdatum              
#     EROINF              EER00              OP-Info                 
#     ERBELN              EER00              Beleg-Nummer            
#     ERRGNR              EER00              Interne Rg.Nr           
#     ERBZRG              EER00              Bezogene Rg.-Nr         
#     ERSTSL              EER00              Steuer-Schl}ssel 1      
#     ERSTS2              EER00              Steuer-Schl}ssel 2      
#     ERSTSF              EER00              Steuer-Schl}ssel Fracht 
#     ERSTSZ              EER00              Steuer-Schl}ssel Zoll   
#     ERSKSL              EER00              Skonto-Schl}ssel        
#     ERDTVA              EER00              Valuta-Datum            
#     ERZAHL              EER00              Zahl-Sperre             
#     ERVCDE              EER00              Verfolgungscode         
#     ERINFO              EER00              Info                        
#     ERBUI1              EER00              Informationsfeld 1          
#     ERBUI2              EER00              Informationsfeld 2          
#     ERBUI3              EER00              Informationsfeld 3          
#     ERBUI4              EER00              Informationsfeld 4          
#     ERTCOD              EER00              PTT Teilnehmercode          
#     ERPFZI              EER00              PrfZf Mod                   
#     ERTLNR              EER00              PTT Teilnehmernummer        
#     ERREFN              EER00              CH:VESR-Nr                  
#     ERBKLF              EER00              Laufnr. vers. Banken/Konten 
#     ERDTRE              EER00              Rechnungseingangsdatum      
#     ERLEMO              EER00              Leistungsmonat              
#     ERSTMO              EER00              Storno / Buchungsmonat      
#     ERSTSB              EER00              Storno / Sachbearbeiter     
#     ERDTSA              EER00              Storno/Datum der Ausf}hrung 
#     ERWSL               EER00              W{hrungs-Kennzeichen        
#     ERKURS              EER00              Wechsel-Kurs                
#     ERKUFA              EER00              Kursfaktor                  
#     ERKUR2              EER00              Wechsel-Kurs                
#     ERKUF2              EER00              Kursfaktor                  
#     ERBTBT              EER00              Brutto-Betrag               
#     ERMWBT              EER00              Vorsteuer-Betrag 1          
#     ERMWB2              EER00              Vorsteuer-Betrag 2          
#     ERFRBT              EER00              Fracht-Betrag               
#     ERZOBT              EER00              Zoll-Betrag                 
#     ERNKBT              EER00              Nebenkosten                 
#     ERRBBT              EER00              Gesamt-Rabatt 1             
#     ERRBB2              EER00              Gesamt-Rabatt 2             
#     ERSKBZ              EER00              Skontof{higer Betrag      
#     ERSKBT              EER00              Skonto-Betrag             
#     ERMWNT              EER00              Vorsteuer-Betrag 1 NETTO  
#     ERMWN2              EER00              Vorsteuer-Betrag 2 NETTO  
#     ERFNBT              EER00              Fracht-Betrag NETTO       
#     ERZNBT              EER00              Zoll-Betrag NETTO         
#     ERKZRB              EER00              KZ:FIBU-S{tze erstellen   
#     ERKZRA              EER00              KZ: Gesamt-Rabatte umlegen
#     ERKZNT              EER00              KZ: Rechnung netto buchen 
#     ERKZFZ              EER00              KZ: Fracht/Zoll umlegen   
#     ERKZAN              EER00              KZ: Anlagen-Gut           
#     ERKZBN              EER00              KZ: Berlin                
#     ERKZSP              EER00              KZ J/N                    
#     ERKZEM              EER00              KZ: Einmal-Lieferant      
#     ERKZKS              EER00              KZ: Kurssicherung           
#     ERSBER              EER00              KZ: Sachbearbeiter/Erfassun 
#     ERKZBT              EER00              Sonderbetrag                
#     ERKZGK              EER00              GU K}rzPos                  
#     ERF1N2              EER00              Reservefeld 2 1.0           
#     ERF1N3              EER00              Reservefeld 3 1.0           
#     ERF1WR              EER00              Feld 1 - 15/2               
#     ERF3WR              EER00              Feld 3 - 15/2               
#     ERF4WR              EER00              Feld 4 - 15/2               
#     ERF5WR              EER00              Feld 5 - 15/2               
#     ERF6WR              EER00              Feld 6 - 15/2               
#     ERF1MG              EER00              Reservefeld Menge 1 13P3    
#     ERF2MG              EER00              Reservefeld Menge 2 13P3    
#     ERFA12              EER00              Reservefeld 2 10A           
#     ERFA13              EER00              Reservefeld 3 10A           
#     ERFA14              EER00              Reservefeld 4 10A           
#     ERF1A2              EER00              Reservefeld 2 1A            
#     ERF1A3              EER00              Reservefeld 3 1A            
#     ERF1A4              EER00              Reservefeld 4 1A            
#     ERF1A5              EER00              Reservefeld 5 1A            
#     ERF1A6              EER00              Reservefeld 6 1A            
#     ERFDT1              EER00              Reservefeld 1 Datum         
#     ERFDT2              EER00              Reservefeld 2 Datum         
#     ERFK20              EER00              Kunden-Spez                 
#     ERDTER              EER00              Datum Erfassung CJJMMTT     
#     ERDTST              EER00              Erstelldatum/Stapelbuchunge 
#     ERDTBU              EER00              Buchungsdatum in der FIBU   
#     ERDFSL              EER00              Dateif}hrungs-Schl}ssel     
#     ERSTAT              EER00              Satzstatus 


'ESL00': { # Stapelschnittstelle LAGER
    #'SLABT ': 'Abteilungs-Nummer',
    #'SLAGRP': 'Abteilungs-Gruppe/Sparte',
    #'SLFNR ': 'Firma',
    #'SLFGRP': 'Firmen-Gruppe',
    'SLARTN': 'artnr',
    #'SLARTG': 'Artikel-Gruppe',
    #'SLARTH': 'Artikel-Haupt-Gruppe',
    'SLLGNR': 'lager',
    'SLMNG ': 'menge', # "Einzelmenge"
    # 'SLMESL': 'mengeneinheit', inner 1
    # 'SLFAKT': 'Umrechn.-Faktor  ER-->BE', immer 1
    'SLPREW': 'wert',
    # 'SLKZWR': 'KZ Preisdimension', immer 9
    'SLWSLE': 'waehrung',
    'SLKURE': 'kurs',
    'SLKUFA': 'kursfaktor',
    # 'SLSBNR': 'sachbearbeiter_erfassung',
    'SLDEZI': 'Anzahl Dezimalstellen',
    # 'SLKZBE': 'KZ: Ohne Bestandsf}hrung', immer 0
    # 'SLKZEK': 'KZ: EK-Preis ist eingebbar', immer 0
    # 'SLKZSE': 'KZ: Serien-Nr./ Charge',
    'SLDTRG': 'rechnungs_date',
    'SLRGNR': 'rechnungsnr',
    'SLAUFN': 'auftragsnr',
    'SLAUPO': 'auftragspos',
    #'SLEKY1': 'Erzeuger: Key -1-',
    #'SLEKY2': 'Erzeuger: Key -2-',
    #'SLEKY3': 'Erzeuger: Key -3-',
    # 'SLSENR': 'Serien-/Chargennummer',
    # 'SLWSID': 'WS-ID f}r Herkunft', 'EINKAUF'
    'SLSAKZ': 'SA',
    # 'SLBWSL': 'bewegungs_schluessel', # immer 22
    'SLBELN': 'belegnr',
    'SLDTBL': 'beleg_date',
    # 'SLBUMO': 'Buchungsmonat CJJMM',
    # 'SLLKTR': 'Kostentr{ger',
    # 'SLKST ': 'Kostenstelle',
    # 'SLPROJ': 'Projekt',
    'SLINFO': 'Internes Info-Feld',
    # 'SLBZBW': 'Bezogene Lagerbewegung', immer 0
    # 'SLLGN2': 'Umlag: Empfangendes Lager',
    # 'SLLGPL': 'Lager-Platz', # immer leer
    # 'SLLGP2': 'Umlag: Empfangender Lg-Plat',
    #'SLFPRZ': 'Fracht-%-Satz', immer 0
    #'SLZPRZ': 'Zoll-%-Satz', immer  0
    # 'SLZARZ': 'Abwertungs-%-Satz', immer 0
    # 'SLZOBT': 'Zoll-Betrag', # immer 0
    # 'SLFRBT': 'Fracht-Betrag', immer 0
    'SLBSTN': 'bestellnr',
    'SLBSTP': 'bestellpos',
    'SLZUKZ': 'Zustand: A/R/F',
    'SLKZKO': 'KZ. mit Komponenten',
    #'SLKZVB': 'KZ: Ohne Update Vorab-Menge',
    #'SLKZIN': 'KZ: Inventur-Bewegung',
    #'SLMNGI': 'Menge als Zusatzinfo', immer 0
    #'SLWRTI': 'Wert als Zusatzinfo', # immer 0
    'SLLBW ': 'lagerbewegungsnr',
    'SLLWKO': 'lager_korrektur_wert', # 'Korrektur-Wert des Lagers',
    # 'SLLWFR': 'Korr.Wert Lg. / Frachtantei', immer 0
    # 'SLLWZO': 'Korr.Wert Lg. / Zollanteil', immer 0
    # 'SLSBAE': 'aenderung_sachbearbeiter',
    # 'SLDTAE': 'aenderung_date',
    'SLBTYP': 'typ',
    'SLDFSL': 'dateifuehrungsschluessel',
    'SLSTAT': 'satzstatus',
    'SLSANR': 'stapelsatznr'
},

#     VPFNR               EVP00              Firma                    
#     VPHERK              EVP00              KZ: Herkunft             
#     VPHKNR              EVP00              Herkunfts-Nummer         
#     VPHKPO              EVP00              Herkunfts-Position-Nummer
#     VPVGNR              EVP00              Vorgangs-Nummer          
#     VPVGPO              EVP00              Vorgangs-Positions-Nummer
#     VPBSTN              EVP00              BestellNr./AuftragsNr.   
#     VPBSTP              EVP00              Bestell-Position         
#     VPKZIE              EVP00              Kz: Intern/Extern        
#     VPCBSN              EVP00              Bestell-Nr.  / Copy      
#     VPCBSP              EVP00              Bestell-Pos. / Copy      
#     VPANNR              EVP00              Anfrage                  
#     VPANPO              EVP00              Anfrageposition          
#     VPABT               EVP00              Abteilung / Bestellung   
#     VPABTH              EVP00              Abteilung / Herkunft        
#     VPAGRP              EVP00              Abteilungs-Gruppe/Sparte    
#     VPFNRH              EVP00              Firma / Herkunft            
#     VPFNRZ              EVP00              zentrale Einkaufsfirma      
#     VPFNRR              EVP00              Firma f}r Rahmen            
#     VPTXHK              EVP00              KZ: Herkunft des Textes     
#     VPKETX              EVP00              Kz: Posit.-Texte eingesetzt 
#     VPURSA              EVP00              Verursacher                 
#     VPSBN2              EVP00              Zust{ndiger Eink{ufer       
#     VPSBN5              EVP00              Anfordernder Sachbearbeiter 
#     VPSBNR              EVP00              Sachbearbeiter / Stapelsatz 
#     VPSBDI              EVP00              Sachbearbeiter/ Disposition 
#     VPABDI              EVP00              Abteilung / Disposition     
#     VPABC               EVP00              ABC-Klassifizierung         
#     VPDIVA              EVP00              Variante f. Bestellvorschla 
#     VPARTN              EVP00              Artikel-Nummer              
#     VPSFFX              EVP00              Artikel Suffix              
#     VPEAN               EVP00              EAN-Nummer                  
#     VPLIAR              EVP00              Lief-Art                    
#     VPKZSO              EVP00              Kz: Sonderartikel = 1       
#     VPKZBE              EVP00              Kz: Ohne Bestandf}hrung     
#     VPARTG              EVP00              Artikel-Gruppe              
#     VPKEAG              EVP00              Kz: Art.-Gruppe eingesetzt  
#     VPARTH              EVP00              Artikel-Haupt-Gruppe        
#     VPKEHG              EVP00              Kz: Hpt.-Gruppe eingesetzt  
#     VPLGNR              EVP00              Lagernummer                 
#     VPKELG              EVP00              Kz: Lager eingesetzt        
#     VPWERK              EVP00              Werk                        
#     VPEMPF              EVP00              Empfangsstelle          
#     VPTFDI              EVP00              Teilefamilie/Dispo      
#     VPTFEK              EVP00              Teilefamilie/Eink.      
#     VPDIGP              EVP00              Disponenten-Gruppe      
#     VPEKGP              EVP00              Einkaufs-Gruppe         
#     VPKST               EVP00              Kostenstelle            
#     VPKTR               EVP00              Kostentr{ger            
#     VPUKTR              EVP00              Kostentr{ger UnterNr    
#     VPKTO               EVP00              Kontonummer             
#     VPINFN              EVP00              Informationsnummern-Feld
#     VPINFA              EVP00              Informationsart         
#     VPPROJ              EVP00              Projekt-Nummer          
#     VPUPRJ              EVP00              Unter-Projekt-Nummer    
#     VPZUD3              EVP00              Feld f}r 3.Zuordnung    
#     VPKZD3              EVP00              KZ: 3.Zuordnung             
#     VPKZAR              EVP00              KZ: Rahmen / Abruf          
#     VPBSAB              EVP00              Nummer des bez. Rahmenvert. 
#     VPBSAP              EVP00              Pos-Nr des bez. Rahmenvert. 
#     VPKERA              EVP00              Kz: Rahmen eingesetzt       
#     VPFABG              EVP00              Fremdarbeitsgang            
#     VPKZFG              EVP00              Kz. Fremdarbeitsgang        
#     VPHRST              EVP00              Hersteller                  
#     VPX3VP              EVP00              Verpackungsvorschrift       
#     VPLIAU              EVP00              Lieferanten-Auftrags-Nummer 
#     VPPREW              EVP00              Preis/Wert                  
#     VPKZWR              EVP00              Preisdimension              
#     VPWSL               EVP00              W{hrungs-Kennzeichen        
#     VPKZPR              EVP00              Kz: Gesch{tzter Preis       
#     VPKZRD              EVP00              KZ Rabatte               
#     VPKEPR              EVP00              Kz: Preis eingesetzt     
#     VPMGVB              EVP00              Vorschlagsmenge          
#     VPMNGE              EVP00              Bestell-Menge/Bestell-Eh 
#     VPMNGB              EVP00              Bestell-Menge/Bestandsf. 
#     VPFAKT              EVP00              Umrechn.-Faktor  ER-->BE 
#     VPMEER              EVP00              Mg.-Einheit/ Bestellung  
#     VPMEBE              EVP00              Mg.-Einheit/ Bestandsf}hr
#     VPKEME              EVP00              KZ: MEH eingesetzt       
#     VPKZBD              EVP00              KZ: Bestell-Druck        
#     VPKZDR              EVP00              KZ: Bestell-Druck        
#     VPKZAB              EVP00              KZ: Bestell-Abschlu~     
#     VPKZAL              EVP00              KZ: EAL00 versorgen      
#     VPKZMB              EVP00              KZ: Materialbeistellung  
#     VPKZQU              EVP00              KZ: Qualit{tspr}fung        
#     VPKEQU              EVP00              Kz: Qualit{ts-KZ gesetzt    
#     VPKZTF              EVP00              KZ: Teillieferung erlaubt   
#     VPKZUE              EVP00              KZ: Unter-/!berlieferung    
#     VPPZUE              EVP00              %-Satz erlaubte !nter/!berL 
#     VPPZGL              EVP00              %-Satz VollGeliefert        
#     VPKZST              EVP00              KZ: Streckengesch{ft        
#     VPKZSP              EVP00              KZ: Sperre f}r !bernahme    
#     VPKZRE              EVP00              KZ: Reorganisierbar         
#     VPKZMO              EVP00              KZ: Modifizierbar           
#     VPPMTX              EVP00              Kz. Text vorhanden          
#     VPKZBS              EVP00              Beschaffungs-Schl}ssel      
#     VPKZAD              EVP00              KZ: Artikel-Nr andrucken    
#     VPKZBT              EVP00              KZ Batch                    
#     VPKZAN              EVP00              Anfrage                     
#     VPKZTX              EVP00              Kz Texte                    
#     VPKZDI              EVP00              Im Dialog bearbeitet        
#     VPKZDT              EVP00              Format Wunschtermin         
#     VPKZLI              EVP00              Lieferanten{nderung m¦glich 
#     VPKZIV              EVP00              Interne Verarbeitung        
#     VPKZCZ              EVP00              Chargenzertifikat           
#     VPFNRK              EVP00              Fnr/Zugriff Lief-Adresse    
#     VPLINR              EVP00              Lieferanten-Nummer          
#     VPKELI              EVP00              Kz: Lieferant bestimmen     
#     VPVANR              EVP00              Lieferadresse               
#     VPSPSL              EVP00              Sprachenschl}ssel           
#     VPINQT              EVP00              Kz. Lieferanten-Zertif.     
#     VPDTLT              EVP00              Liefertermin                
#     VPLTWW              EVP00              Liefertermin / Woche        
#     VPKZLT              EVP00              KZ: Art Liefertermin        
#     VPKELT              EVP00              KZ: Liefertermin eingesetzt 
#     VPDTER              EVP00              Datum Erstellung            
#     VPDTAL              EVP00              Datum der Lieferanten-AB    
#     VPTGLE              EVP00              Tage Liefererinnerung       
#     VPAUFA              EVP00              Fertigung: Auftragsart      
#     VPZONR              EVP00              Fertigung: ZuordnungsNr.    
#     VPAVGR              EVP00              Fertigung: AV-Gruppe        
#     VPVNUM              EVP00              Fertigung: Versionsnummer   
#     VPLFNR              EVP00              Fertigung: lfd.Nr. Planung  
#     VPHRKF              EVP00              Fertigung: Herkunft         
#     VPFHRK              EVP00              Fertigung Herkunft          
#     VPKZGF              EVP00              Fertigung: Generierung      
#     VPKZFA              EVP00              Art Fremdarbeitsgang       
#     VPFNRA              EVP00              Auftragsfirma              
#     VPFZUO              EVP00              Fertigung Zuordnung        
#     VPFAU2              EVP00              Fertigungsauftragsnummer 2 
#     VPF1N1              EVP00              Feld 1 1/0                 
#     VPF3N1              EVP00              Feld 3 1/0                 
#     VPF5N1              EVP00              Feld 5 1/0                 
#     VPF1N6              EVP00              Feld 6 1/0                 
#     VPF1N7              EVP00              Feld 7 1/0                 
#     VPF1A9              EVP00              Reservefeld 9 1A           
#     VPF1A0              EVP00              Reservefeld 10 1A          
#     VPF1AA              EVP00              Feld 11 1A                 
#     VPF1AB              EVP00              Reservefeld 12 1A          
#     VPF1AC              EVP00              Reservefeld 13 1A          
#     VPF1AD              EVP00              Reservefeld 14 1A         
#     VPF1AE              EVP00              Reservefeld 15 1A         
#     VPF1AF              EVP00              Reservefeld 16 1A         
#     VPF1AG              EVP00              Reservefeld 17 1A         
#     VPF1AH              EVP00              Reservefeld 18 1A         
#     VPF1A5              EVP00              Reservefeld 1 5A          
#     VPF11A              EVP00              Reservefeld 1 10 A        
#     VPFDT1              EVP00              Reservefeld 1 Datum       
#     VPFDT2              EVP00              Reservefeld 2 Datum       
#     VPFA11              EVP00              Reservefeld 1 10A         
#     VPFA12              EVP00              Reservefeld 2 10A         
#     VPFWR1              EVP00              Reservefeld Wert 1 15P2   
#     VPFWR2              EVP00              Reservefeld Wert 2 15P2   
#     VPFWR3              EVP00              Reservefeld Wert 3 15P2   
#     VPFMG1              EVP00              Reservefeld Menge 1 13P3  
#     VPFMG2              EVP00              Reservefeld Menge 2 13P3  
#     VPFMG3              EVP00              Reservefeld Menge 3 13P3  
#     VPFK21              EVP00              Kunden-Spez 2             
#     VPFK22              EVP00              Kunden-Spez 3             
#     VPKZBG              EVP00              Kennzeichen Bearbeitung   
#     VPDTAE              EVP00              Datum letzte ¢nderung     
#     VPSBAE              EVP00              SB letzte ¢nderung        
#     VPDTPR              EVP00              Datum letzte Pr}fung      
#     VPUEBN              EVP00              Lfd.Nr. der !bernahme     
#     VPDFSL              EVP00              Dateif}hrungs-Schl}ssel   
#     VPSTAT              EVP00              Satzstatus                


'EWZ00': { # Bestellpositionszusatzinformationen
     # 'WZFGRP': 'Firmen-Gruppe              ', 
     # 'WZFNR ': 'Firma                      ', 
     # 'WZAGRP': 'Abteilungs-Gruppe/Sparte   ', 
     # 'WZABT ': 'Einkaufs-Abteilung         ', 
     # 'WZARTH': 'Artikel-Haupt-Gruppe       ', 
     # 'WZARTG': 'Artikel-Gruppe             ', 
     'WZARTN': 'artnr',
     'WZBSTN': 'bestellnr',
     'WZBSTP': 'bestellpos',
     # 'WZFNRK': 'Firma                      ', 
     # 'WZLINR': 'Lieferanten-Nummer         ', 
     # 'WZSBN2': 'SB: Zust{ndiger Eink{ufer  ', 
     # 'WZKZSE': 'KZ: Serien-Nr./ Charge     ', 
     # 'WZKZSO': 'KZ: Sonderartikel          ', 
     # 'WZKZBE': 'KZ: Ohne Bestandsf}hrung   ', immer 0 
     # 'WZKZLG': 'KZ:Lagerzugang Buchen      ', 
     # 'WZKZSP': 'KZ:Lagerzugang sperren     ', 
     # 'WZKZQU': 'KZ:Qualit{ts-Kontrolle     ', 
     # 'WZKZSM': 'KZ:Schlechtmengen-Info     ', 
     # 'WZMEER': 'Mengeneinheit/Erfassung    ', 
     # 'WZMEBE': 'Mengeneinheit/Bestandsf.   ', 
     # 'WZFAKT': 'Faktor: Erf. -> Bestf.     ', 
     'WZWVNR': 'warenvereinnahmungsnr',
     # 'WZSBWZ': 'SB: Zugangspr}fung         ', 
     #'WZBNWZ': 'Beleg-Nr./Zugang', 
     'WZDTWZ': 'zugang_date',
     # 'WZMJBU': 'Buchungs-Monat             ', 
     'WZLBWZ': 'lagerbewegung_zugang', # referenziert 
     # 'WZREKL': 'Reklamationsnummer         ', 
     # 'WZGBNR': 'Vertriebseinheit           ', 
     # 'WZRIMW': 'WZ-RefNr f}r Image         ', 
     # 'WZLFSN': 'lieferschennr', # scheinbar immer 0000000
     'WZLGNR': 'lager', 
     # 'WZLGPL': 'Lager-Platz                ', 
     # 'WZWERK': 'Werksnummer                ', 
     # 'WZANLP': 'Anlieferungs-Ort           ', 
     # 'WZMGWE': 'Gepr.Zugangs-Mg./Erf.Einh  ', 
     # 'WZMGWB': 'Gepr.Zugangs-Mg./ Bst.F}hrg', 
     # 'WZMGOB': 'Ausschu~-Mg. / Bestandsf.  ', 
     # 'WZKZQD': 'Qualit{tsdaten             ', 
     # 'WZSBRG': 'SB: Rechnungs-Pr}fung      ', 
     'WZRGNR': 'rechnungsnr', 
     'WZRGPO': 'rechnungsposnr', 
     'WZDTRG': 'rechnungs_date', 
     'WZLBWR': 'lagerbewegung_rechnung', 
     # 'WZKZRB': 'KZ:Rg. in FIBU buchen      ', 
     # 'WZKZBO': 'LiBonusVarbSts             ', 
     # 'WZKZRG': 'Pseudoechnung              ', 
     # 'WZKRZR': 'K}rzungsreferenz           ', 
     # 'WZKRZA': 'K}rzungsart                ', 
     # 'WZGDIR': 'Referenz f}r Differenz     ', 
     'WZPREA': 'tatsaechlicher_preis', 
     'WZPREW': 'bestell_preis', 
     # 'WZKZWR': 'Preisdimension             ', 
     # 'WZWSL ': 'Waehrungs-Kennzeichen', 
     'WZKURS': 'kurs_zugang', 
     'WZKUFA': 'kursfaktor_zugang', 
     'WZBUBT': 'buchungsbetrag', 
     # 'WZLWRB': 'Rabatt-Betrag/W{h          ', 
     # 'WZLWNB': 'Nebenkosten/W{h            ', 
     # 'WZLWFR': 'Fracht-Betrag/W{h          ', 
     # 'WZLWZO': 'Zoll-Betrag/W{h            ', 
     # 'WZSKBT': 'Skonto-Betrag/W{h          ', 
     # 'WZKZRA': 'KZ: Rabattaufteilung vorh. ', 
     # 'WZKZNT': 'KZ: Rechnung netto buchen  ', 
     'WZMGRE': 'menge_berechnet', 
     # 'WZMGRB': 'menge_berechnet', 
     # 'WZSTSL': 'Steuerschl}ssel            ', 
     # 'WZMWBT': 'Vorsteuerbetrag            ', 
     # 'WZKTR ': 'Kostentr{ger               ', 
     # 'WZUKTR': 'Kostentr{gerunternummer    ', 
     # 'WZPROJ': 'Projekt                    ', 
     # 'WZUPRJ': 'Projektunternummer         ', 
     # 'WZKST ': 'Kostenstelle               ', 
     'WZKTO': 'konto', 
     'WZBTXT': 'buchungstext', 
     'WZOPSN': 'opnr', 
     'WZART': 'art', 
     'WZDTER': 'erfassungs_date',
     'WZIINF': 'info_intern',
     # 'WZINFN': 'Informationsnummer         ', 
     # 'WZINFA': 'Informationsnummernart     ', 
     # 'WZF1A3': 'Reservefeld 3 1A           ', 
     # 'WZF1A4': 'Reservefeld 4 1A           ', 
     # 'WZF1A5': 'Reservefeld 5 1A           ', 
     # 'WZF1A6': 'Reservefeld 6 1A           ', 
     # 'WZF1A7': 'Reservefeld 7 1A           ', 
     # 'WZF1A8': 'Reservefeld 8 1A           ', 
     # 'WZF1N2': 'Reservefeld 2 1/0          ', 
     # 'WZF1N3': 'Reservefeld 3 1/0          ', 
     # 'WZF1N4': 'Reservefeld 4 1/0          ', 
     # 'WZF1N5': 'Reservefeld 5 1/0          ', 
     # 'WZF1N6': 'Reservefeld 6 1/0          ', 
     # 'WZFSN1': 'Reservefeld 1 Satznummer   ', 
     # 'WZFSN2': 'Reservefeld 2 Satznummer   ', 
     # 'WZFA11': 'Reservefeld 1 10A          ', 
     # 'WZFA12': 'Reservefeld 2 10A          ', 
     # 'WZFDT1': 'Reservefeld 1 Datum        ', 
     # 'WZFDT2': 'Reservefeld 2 Datum        ', 
     # 'WZFWR1': 'Reservefeld 1 Wert         ', 
     # 'WZFWR2': 'Reservefeld 2 Wert         ', 
     # 'WZFWR3': 'Reservefeld 3 Wert         ', 
     # 'WZFMG1': 'Reservefeld 1 Menge        ', 
     # 'WZFMG2': 'Reservefeld 2 Menge        ', 
     # 'WZFMG3': 'Reservefeld 3 Menge        ', 
     # 'WZFK20': 'Kunden-Spez                ', 
     'WZKZST': 'status', 
     #'WZMGST': 'Status Mg./ Bst.F}hrg      ', 
     'WZSTAT': 'satzstatus', 
     'WZSAN2': 'bezogener_satz_EWZ00',
     'WZSANR': 'satznummer_warenzugang', 
},

'ISA00': {# MyPL Schnittstelle: Komissionierbeleg
          'IAFNR': 'firma',
          'IALGNR': 'lagernr',
          'IAKBNR': 'kommissionierbelegnr',
          'IAAUFN': 'auftragsnr',
          'IADATE': 'anforderung_date',
          'IATIME': 'anforderung_time',
          'IADFSL': 'dateifuehrungsschluessel',
          'IASTAT': 'status',
          'IASANR': 'satznr',
         },

'ISB00': {# MyPL Schnittstelle: Lagerbuchungsschnittstelle
          'IBFNR': 'firma',
          'IBBWSL': 'bewegungsschluessel',
          'IBLGNR': 'lager',
          'IBARTN': 'artnr',
          'IBMNGB': 'menge',
          'IBINFO': 'info',
          'IBFCOD': 'letzter_fehlercode',
          'IBDFSL': 'dateifuehrungsschluessel',
          'IBSTAT': 'status',
         },

'ISK00': {# MyPL Schnittstelle: Warenzugang aus Umlagerung
          'IKFNR': 'firma',
          'IKKBNR': 'kommissionierbelegnr',
          'IKKPOS': 'kommissionierbelegposition',
          # TODO: inkonsistenz: in lagerschnittstelle.py wird dies folgendermassen definiert:
          # 'IKKBNR': 'komminr',
          # 'IKKPOS': 'kommiposition',
          'IKAUFN': 'auftragsnr',
          'IKAUPO': 'auftragsposition',
          'IKRMNG': 'rueckmeldemenge',
          'IKDATE': 'rueckmeldung_date',
          'IKTIME': 'rueckmeldung_time',
          'IKDFSL': 'dateifuehrungsschluessel',
          'IKSTAT': 'status',
          'IKSANR': 'satznr',
         },

'ISR00': {# MyPL Schnittstelle: KB-Rückmeldung
          'IRFNR': 'firma',
          'IRKBNR': 'kommissionierbelegnr',
          'IRKPOS': 'kommissionierbelegposition',
          'IRAUFN': 'auftragsnr',
          'IRAUPO': 'auftragsposition',
          'IRMENG': 'rueckmeldemenge',
          'IRKZNU': 'kennzeichen_nullen',
          'IRKZST': 'kennzeichen_stornieren',
          'IRFCOD': 'letzter_fehlercode',
          'IRDFSL': 'dateifuehrungsschluessel',
          'IRSTAT': 'status',
         },

'ISZ00': {# MyPL Schnittstelle: Warenzugang aus Auftrag
          'IZFNR ': 'firma',
          'IZBSTN': 'bestellnr',
          'IZWVNR': 'warenvereinnahmungsnr',
          'IZDTWZ': 'zugang_date',
          'IZTIME': 'zugang_time',
          'IZDFSL': 'dateifuehrungsschluessel',
          'IZSTAT': 'status',
          'IZSANR': 'satznr',
          },
 
# LSU00 - Lagerstatistik
# SUFNR    A         2  0    1    2 Firma
# SUARTN   A        20  0    3   22 Artikelnummer
# SULGNR   S    4    4  0   23   26 Lagerort
# SUJAHR   P    3    2  0   27   28 Jahr
# SUME01   P   11    6  3   29   34 Monatsendbestand Jan.
# SUME02   P   11    6  3   35   40 Monatsendbestand Feb.
# SUME03   P   11    6  3   41   46 Monatsendbestand M{rz
# SUME04   P   11    6  3   47   52 Monatsendbestand Apr.
# SUME05   P   11    6  3   53   58 Monatsendbestand Mai
# SUME06   P   11    6  3   59   64 Monatsendbestand Jun.
# SUME07   P   11    6  3   65   70 Monatsendbestand Jul.
# SUME08   P   11    6  3   71   76 Monatsendbestand Aug.
# SUME09   P   11    6  3   77   82 Monatsendbestand Sep.
# SUME10   P   11    6  3   83   88 Monatsendbestand Okt.
# SUME11   P   11    6  3   89   94 Monatsendbestand Nov. 
# SUME12   P   11    6  3   95  100 Monatsendbestand De
# SUSA01   P   13    7  3  101  107 Summe Abg{nge Jan.
# SUSA02   P   13    7  3  108  114 Summe Abg{nge Feb.
# SUSA03   P   13    7  3  115  121 Summe Abg{nge M{rz
# SUSA04   P   13    7  3  122  128 Summe Abg{nge Apr.
# SUSA05   P   13    7  3  129  135 Summe Abg{nge Mai
# SUSA06   P   13    7  3  136  142 Summe Abg{nge Jun.
# SUSA07   P   13    7  3  143  149 Summe Abg{nge Juli
# SUSA08   P   13    7  3  150  156 Summe Abg{nge Aug.
# SUSA09   P   13    7  3  157  163 Summe Abg{nge Sep.
# SUSA10   P   13    7  3  164  170 Summe Abg{nge Okt.
# SUSA11   P   13    7  3  171  177 Summe Abg{nge Nov.
# SUSA12   P   13    7  3  178  184 Summe Abg{nge Dez.
# SULGDR   P    7    4  3  185  188 Lagerdrehung
# SURCHW   P   11    6  3  189  194 Reichweite
# SUMMIN   P   11    6  3  195  200 Niedrigster Bestand
# SUMMAX   P   11    6  3  201  206 H¦chster Bestand
# SUANZB   P    5    3  0  207  209 Anzahl Bewegungen
# SUANZV   P    5    3  0  210  212 Anzahl Verbr{uche  
# SUANZM   P    2    2  0  213  214 Anzahl Monate
# SUSTAT   A         1  0  215  215 Satzstatus


'XAD00': {# Abweichende Lieferadressen
          'ADNAME': 'name1',
          'ADNAM2': 'name2',
          'ADNAM3': 'name3',
          'ADNAM4': 'name4',
          'ADSTR': 'strasse',
          'ADPLZ': 'plz',
          'ADORT': 'ort',
          'ADLKZ': 'laenderkennzeichen',
          #'ADORTT': 'ortsteil',
          'ADKZAD': 'adressaufbereitung',
         },

'XAR00': {# Artikelstamm
           'ARARTN': 'artnr',
           'ARSTAT': 'status',
           'ARARTH': 'artikelhauptgruppe',
           'ARARTG': 'artikelgruppe',
           'ARBEZ1': 'ARBEZ1',
           'ARBEZ2': 'ARBEZ2',
           'ARBEZ3': 'ARBEZ3',
           'ARGEWI': 'gewicht',
           'ARNGEW': 'gewicht_netto',
           'AREAN': 'ean',
           'ARMEBE': 'mengeneinheit',
           'ARZOGR': 'zollgruppe',
           'ARZTNR': 'statistischewarennr',
           'ARPREV': 'listenpreis',
           'ARMALG': 'laenge',
           'ARMABR': 'breite',
           'ARMATI': 'tiefe',
           'AREART': 'ersatzartikel',
           'ARMCOD': 'matchcode',
           #'ARMEBE': 'Mengeneinheit/Bestandsf.',
           #'ARMEER': 'Mengeneinheit/Erfassung',
           #'ARFAKT': 'Umrechn.-Faktor  ER-->BE',
           #'ARMEPR': 'Mengeneinheit/Preis',
           #'ARFAPR': 'Umrechn.-Faktor  PR-->BE',
           'ARABC': 'abc1',
           'ARABC2': 'abc2',
           'ARABC3': 'abc3',
           #'ARZOGR': 'Zoll-Grupppe-nr',
           'ARZTNR': 'zolltarifnr',
           'ARURLD': 'ursprungsland',
           #'ARPREV': 'Verkaufspreis',
           #'ARPRE2': 'Verkaufspreis 2',
           #'ARPREA': 'Verkaufspreis Ausland',
           #'ARPRI2': 'Vk-Preis/Inland - neu -',
           #'ARPRA2': 'Vk-Preis/Ausland - neu -',
           #'ARDTPI': 'Ab Datum Vk/Inland - neu -',
           #'ARDTPA': 'Ab Datum Vk/Ausland - neu -',
           #'ARPGSZ': 'Preisgruppen-Staffel-Zuordn',
           #'ARWSL' : 'Währungs-Kennzeichen',
           #'ARRBPR': 'Artikel-Rabatt-%-Satz',
           #'ARKZKW': 'Ohne wertmäßige Best.-Führ.',
           'ARKZKO': 'setartikel',
           'ARKZVK': 'verkaufsteil',
           #'ARKZNE': 'Kennzeichen Neuheit',
           #'ARDTNA': 'Auslieferungs-Datum/Neuheit',
           'ARKZNA': 'auslaufartikel',
           'ARKZAT': 'artikeltexte_vorhanden',
           'ARVKUB': 'volumen',
           'ARKZZS': 'zuteilungssperre',
           'ARINFO': 'info',
           'ARMGMA': 'mindestabnahmemenge',
           'ARCTAR': 'ersatzteil',
           'ARKZVP': 'ist_verpackung',
           #'ARKZWB': 'webshop',
           #'ARHAFO': 'handelsform',
           #'ARDAFO': 'darreichungsform',
           #'ARKZRG': 'andruck_in_rechnung',
           #'ARSBER': 'sachbearbeiter_erfassung',
           #'ARBPER': 'BenutzerPr.Erfassung',
           #'ARSBAE': 'Sachb.letzte Änderung',
           'ARBPAE': 'BenutzerPr.letzte Änderung',
           'ARDTER': 'erfassung_date',
           'ARDTAE': 'aenderung_date', },

'XCK00': { # Ablaufkontrolle
          'CKJNUM': 'jobnr',
          'CKBLNA': 'blockname',
          'CKJNAM': 'jobname',
          'CKUSER': 'benutzer',
          'CKWSID': 'bildschirmidentifikation',
          'CKFNR ': 'firmennr',
          'CKFCOD': 'firmencode',
          'CKSBNR': 'sachbearbeiternr',
          'CKBLST': 'blockstatus',
          'CKVART': 'verarbeitungsart',
          'CKRJOB': 'startjob',
          'CKJTYP': 'jobtyp',
          'CKANFD': 'start_date',
          'CKANFZ': 'start_time',
          'CKENDD': 'ende_date',
          'CKENDZ': 'ende_time',
          'CKECOD': 'beendigungscode',
          'CKBLMK': 'blockstepmarkierung',
          'CKJKEN': 'jobkennung',
          'CKDTAE': 'letzte_aenderung_date',
          'CKZTAE': 'letzte_aenderung_time',
          'CKDFSL': 'dateifuehrungsschluessel',
          'CKSTAT': 'satzstatus',
          },

'XED00': { # Intrastat-Meldungs-Datei
            'EDINKZ': 'kennziffer',
            'EDFORM': 'anmeldeform',
            'EDSITZ': 'sitz_versender_empfaenger',
            'EDMONA': 'anmeldemonat',
            'EDFIL1': 'fueller01',
            'EDPOS ': 'paginiernr',
            'EDFIL2': 'fueller02',
            'EDBLFA': 'bundesland_finanzamt',
            'EDSTNR': 'steuernr',
            'EDUSNR': 'unterscheidungsnr',
            'EDLCOD': 'bestimmungsland',
            'EDBLND': 'ursprungsbundesland',
            'EDGSCH': 'art_des_geschaefts',
            'EDVKZW': 'verkehrszweig',
            'EDFIL3': 'fueller03',
            'EDPORT': 'hafen_flughafen',
            'EDFIL4': 'fueller04',
            'EDSTWN': 'statistische_warennr',
            'EDFIL5': 'fueller05',
            'EDFIL6': 'fueller06',
            'EDVERF': 'statistisches_verfahren',
            'EDGEWI': 'einganmasse_in_kg',
            'EDMNGX': 'besondere_masseinheit',
            'EDFIL7': 'fueller07',
            'EDWRWA': 'warenwert_netto',
            'EDWRGU': 'statistischer_wert',
            'EDFIL8': 'fueller08',
            'EDBUMM': 'bezugsmonat',
            'EDBUJJ': 'bezugsjahr',
            'EDKREI': 'mineraloelerhebungspreis',
            'EDFIL9': 'fueller09',
          },

'XKD00': {# Kundenadressen
          'KDKDNR': 'kundennr',
          'KDNAME': 'name1',
          'KDNAM2': 'name2',
          'KDNAM3': 'name3',
          'KDNAM4': 'name4',
          'KDSTR': 'strasse',
          'KDPLZ': 'plz',
          'KDORT': 'ort',
          'KDLKZ': 'laenderkennzeichen',
          'KDTELF': 'tel',
          'KDTFAX': 'fax',
          'KDMOBI': 'mobil',
          'KDPSTF': 'postfach', # gelegentlich genutzt
          'KDPLZP': 'postfach_plz', # wenig genutzt
          #'KDORTP': 'Ortsname (Postfach-PLZ)', # gelegentlich genutzt
          #'KDPLZF': 'PLZ Firma', # sehr wenig genutzt
          # 'KDGESF': 'Geschäftsführer', # wenig genutzt
          'KDALSO': 'sortierfeld',
          # 'KDSPSL': 'Sprache', # Werte 1 & 2
          # 'KDWSL ': 'Währungs-Kennzeichen',
          'KDKGRP': 'kundengruppe', # alphanum
          'KDEMAL': 'mail',
          'KDHOME': 'url',
          'KDSANR': 'adressdatei_id',
          'KDDTER': 'erfassung_date',
          'KDDTAE': 'aenderung_date',
          #'KDSTAT': 'satzstatus',
          },

'XKS00': { # Kundenzusatzdaten
          'KSKDNR': 'kundennr',
          'KSLISP': 'liefersperre',
          'KSAWRT': 'offener_aftragswert',
          'KSAKWR': 'kreditlimit',
          'KSCOMP': 'company',
          'KSIAKZ': 'inland_ausland',
          'KSFNRK': 'interne_firmennr',
          'KSINFL': 'unsere_lieferantennumemr',
          'KCUSTN': 'ustid',
          'KSLIMI': 'kreditlimit2',
          'KSSKSL': 'skontoschluessel',
          'KSDKSL': 'delcredereschlüssel',
          'KSMASP': 'mahnsperre',
          'KSKDZA': 'lastschrift',
          'KSBOKZ': 'bonnitaet',
          # 'KSKLTA': 'Kd/Li-Texte anzeigen',
          # 'KSINF1': 'Inf. 1',
          'KCVERB': 'verband',
          #'KSKRED': 'Verbands-/Mischkto-Satz',
          'KCMGVB': 'mitgliednr',
          #'KCBBN':  'Bundeseinheitl.Betriebsnr.',
          #'KCBBS':  'Bundeseinh.Betriebsstell.Nr',
          #'KCVRKZ': 'Kz. "Rechnungsliste erst."',
          #'KCRGKZ': 'Kz. "Rechnung drucken"',
          #'KCKOIN': 'Kontoinhaber',
          #'KCHSTU': 'Auswertungsebene / KD-Hier',
          #'KCSKRL': 'Skto-Ausweis RGL Verband',
          #'KCEDIL': 'LS-Daten per EDI',
          #'KCPRIA': 'Inlands-/Auslandspreise',
          'KCE2IL': 'iln',
          },

'XPN00': { # Konditionen / Preise
    # 'PNFNR ': 'Firma',
    # 'PNANW ': 'Anwendung',
    'PNSANR': 'satznummer',
    # 'PNABK ': 'Sprung-Kriterium',
    'PNPRB': 'preis',
    # 'PNPROV': 'ProvisionsKZ',
    # 'PNKZTX': 'Textschl. Artikelbez.',
    # 'PNKZPR': 'Preisdimension',
    # 'PNSBER': 'Sachb. Erfassung',
    # 'PNX3RB': 'Rabatttextschl}ssel',
    # 'PNX4RB': 'allg. Textschl}ssel',
    # 'PNBPER': 'BenutzerPr.Erfassung',
    # 'PNDTER': 'Datum Erfassung CJJMMTT',
    # 'PNZTER': 'Uhrzeit der Erfassung',
    # 'PNSBAE': 'Sachb.letzte ¢nderung  ',
    # 'PNBPAE': 'BenutzerPr.letzte ¢nderung',
    # 'PNDTAE': 'Datum l. ¢nderung CJJMMTT',
    # 'PNZTAE': 'Uhrzeit letzte ¢nderung',
    # 'PNSTAT': 'Satzstatus',
    # 'PNDFSL': 'dateifuehrungsschluessel',

},

'XPR00': { # Koditionen / Preise
    'PRFNR': 'firma',
    'PRANW': 'anwendung', # 'E' == Einkauf
    # 'PRIB': 'Datenart',
    # 'PRANW1': 'Preismodul',
    'PRSANR': 'satznr_xpn00',
    # 'PRKTYP': 'Konditionstyp',
    # 'PRKDNR': 'Kunde',
    'PRARTN': 'artnr',
    'PRLINR': 'lieferant',
    # 'PRPROB': 'Objekt',
    # 'PRPROJ': 'Projekt',
    # 'PRVRT': 'Vertreter',
    # 'PRPRLK': 'Preisliste Kunde',
    # 'PRPRLL': 'Preisliste Lieferant',
    #'PRPGRP': 'Preisgruppe Vertrieb',
    # 'PRPGRL': 'Preisgruppe Einkauf',
    # 'PRARTG': 'Artikelgruppe',
    # 'PRARTH': 'Artikelhauptgruppe',
    # 'PRLGNR': 'Lager',
    # 'PRFABG': 'Leistungsart',
    # 'PRFKEY': 'freie Zuordnung',
    # 'PRABT': 'Abteilung',
    'PRDTVO': 'gueltig_ab_date',
    'PRDTBI': 'gueltig_bis_date',
    #'PRIAKZ': 'Inland/Ausland',
    #'PRKZBO': 'bonusw}rdig',
    #'PRKZSK': 'skontierf{hig',
    #'PRKZRB': 'Vorgangsrabatt',
    #'PRBART': 'Bonusartikel',
    #'PRTURN': 'Turnus',
    #'PRNART': 'Naturalrabattart',
    #'PRSTOP': 'Ende KZ',
    #'PRKZSM': 'Sortimentsrabatt',
    #'PRKZRL': 'Rabatt l¦schen',
    #'PRHKAR': 'Herkunft',
    #'PRHKNR': 'Kalkulations-Nr.',
    'PRWSL': 'waehrung',
    #'PRMESL': 'Mengeneinheit',
    #'PRSANO': 'SatzNr Preisdatei Original',
    #'PRKZKA': 'KZ Massenkonditions{nderung',
    #'PRSBER': 'Sachb. Erfassung',
    #'PRBPER': 'BenutzerPr.Erfassung',
    #'PRDTER': 'Datum Erfassung CJJMMTT',
    #'PRZTER': 'Uhrzeit der Erfassung',
    #'PRSBAE': 'Sachb.letzte ¢nderung',
    #'PRBPAE': 'BenutzerPr.letzte ¢nderung',
    #'PRDTAE': 'Datum l. ¢nderung CJJMMTT',
    #'PRZTAE': 'Uhrzeit letzte ¢nderung',
    # 'PRRSA1': 'Reserve 1 1A',
    # 'PRRSA2': 'Reserve 2 1A',
    # 'PRRSA3': 'Reserve 3 1A',
    # 'PRRSA4': 'Reserve 4 1A',
    # 'PRRSA5': 'Reserve 5 1A',
    # 'PRRSA6': 'Reserve 6 10A',
    # 'PRRSA7': 'Reserve 7 10A',
    # 'PRRSN1': 'Reserve 1 1S0',
    # 'PRRSN2': 'Reserve 2 1S0',
    # 'PRRSN3': 'Reserve 3 1S0',
    # 'PRRSN4': 'Reserve 4 1S0',
    # 'PRRSN5': 'Reserve 5 1S0',
    # 'PRIK01': 'Indv1 1A',
    # 'PRIK02': 'Indv2 1A',
    # 'PRIK03': 'Indv3 1A',
    # 'PRIK04': 'Indv4 1A',
    # 'PRIK05': 'Indv5 1A',
    # 'PRIK06': 'Indv1 3A',
    # 'PRIK07': 'Indv2 3A',
    # 'PRIK08': 'Indv3 3A ',
    # 'PRIK09': 'Indv4 3A',
    # 'PRIK10': 'Indv5 3A',
    # 'PRIK11': 'Indv 10A',
    # 'PRIK12': 'Indv 20A',
    'PRSTAT': 'Satzstatus',
    #'PRDFSL': 'Dateif}hrungs-Schl}ssel',
},

'XXA00': { # Adressen von Kunden und Lieferanten
        # 'XAFNR ': Firma
        'XAKDNR': 'kundennr',
        # 'XAKZKD': Kz Kunde
        'XALINR': 'lieferantennr',
        # 'XAKZLI': Kz Lieferant
        # 'XAKZIT': Kz Interessent
        # 'XANRRS': Freie nr
        # 'XAKZRS': Kz Adress-Art
        # 'XAKZT1': Adresstyp 1
        # 'XAKZT2': Adresstyp 2
        'XANAME': 'name1',
        'XANAM2': 'name2',
        'XANAM3': 'name3',
        'XANAM4': 'name4',
        'XASTR ': 'strasse',
        'XAPLZ ': 'plz',
        # 'XAPLZP': PLZ Postfach
        # 'XAPLZF': PLZ Firma
        # 'XAPLZI': Postleitzahl/international
        'XAORT ': 'ort',
        # 'XAORTT': Ortsteil/Info
        'XALKZ ': 'laenderkennzeichen',
        # 'XAPSTF': Postfach
        # 'XAORTP': Ortsname (Postfach-PLZ)
        # 'XAGESF': Gesch{ftsf}hrer
        'XATELF': 'tel',
        # 'XATELX': Telex-nr
        'XATFAX': 'fax',
        # 'XAALSO': Alpha-Sortierfeld
        # 'XASPSL': Sprache
        # 'XAWSL ': W{hrungs-Kennzeichen
        # 'XAFGRP': Firmen-Gruppe
        # 'XAKGRP': Kunden-Gruppe
        # 'XALGRP': Lieferanten-Gruppe
        # 'XAISIC': ISIC-Schl}ssel
        # 'XARPMK': RPM-Kreis
        # 'XARPMS': RPM-Segment
        # 'XAGDE ': Gemeindeschl}ssel
        # 'XAHEA ': Herkunftsart
        # 'XAHESP': Herkunftsspezifikation
        'XASANR': 'satznr',
        # 'XADTSP': Datum "Gesperrt bis"
        # 'XASBSP': Sachbearbeiter Sperre
        # 'XAKZSP': Kennzeichen Gesperrt
        # 'XAGRSP': Grund f}r Sperre
        # 'XATOFN': Niederlassungsk}rzel
        # 'XATOFK': TOF Kundennr
        # 'XATOFB': Barcodenr TOF
        # 'XAVRT ': Vertreter f}r Interessenten
        'XAEMAL': 'mail',
        'XAMOBI': 'mobil',
        # 'XAHOME': Homepage
        # 'XANAM5': 5. Namenszeile
        # 'XANAM6': 6. Namenszeile
        # 'XASTR2': Stra~enzusatz
        # 'XASBER': Sachb. Erfassung
        # 'XABPER': BenutzerPr.Erfassung
        'XADTER': 'erfassung_date',
        # 'XASBAE': Sachb.letzte ¢nderung
        # 'XABPAE': BenutzerPr.letzte ¢nderung
        'XADTAE': 'aenderung_date',
        # 'XADFSL': Dateif}hrungs-Schl}ssel
        'XASTAT': 'satzstatus',
       },
    
'XLB00': { # Lagerbewegungen
        #'LBFGRP': 'Firmen-Gruppe',
        #'LBFNR ': 'Firma',
        #'LBAGRP': 'Abteilungs-Gruppe/Sparte',
        #'LBABT ': 'Abteilungs-Nummer',
        #'LBARTH': 'Artikel-Haupt-Gruppe',
        #'LBARTG': 'Artikel-Gruppe',
        'LBARTN': 'artnr',
        #'LBSENR': 'Serien-/Chargennummer',
        # 'LBSBNR': 'sachbearbeiter_erfassung',
        # 'LDWSID': 'herkunft', # z.B. 'EINKAUF'
        # 'LBSAKZ': 'SA',
        'LBBWSL': 'bewegungsschluessel', # 22 = Warenzugang
        'LBBELN': 'belegnummer', # == Nummer in der Fibu
        'LBDTBL': 'beleg_date',
        'LBBUMO': 'buchungsmonat', # format: CJJMM
        #'LBLKTR': 'Kostentr{ger',
        #'LBKST ': 'Kostenstelle',
        #'LBPROJ': 'Projekt',
        # 'LBKZVB': 'vorab_bewertet', # immer 0
        # 'LBKZPR': 'KZ Protokoll-Druck ist offe', # scheinbar immer 1
        # 'LBKZAK': 'KZ im Artikelkonto gedruckt', # scheinbar 0
        # 'LBKZOP': 'KZ OP-Satz', - scheinbar immer 0
        'LBLGNR': 'lager',
        #'LBLGPL': 'Lager-Platz',
        #'LBLGRP': 'Lagergruppe',
        # 'LBLGL2': 'ziel_lager',
        # 'LBLGP2': 'Umlag.: Empfangender Lager Platz',
        # 'LBLGNE': 'Transit: Endlager', # immer 0
        'LBPREW': 'wert',
        # 'LBKZWR': 'bewegungswert_angegeben_kz', # scheinbar immer 9
        'LBWSL': 'waehrung',
        'LBKURS': 'kurs',
        'LBKUFA': 'kursfaktor',
        # 'LBRPRZ': 'Rabatt-%-Satz',
        # 'LBFPRZ': 'fracht_prozent', # % - Satz scheinbar immer 0
        # 'LBZPRZ': 'zoll_prozent', # % - Satz scheinbar immer 0
        # 'LBZARZ': 'abwertung_prozent', # % - Satz - scheinbar immer 0
        'LBLWER': 'wert_erfassung',
        'LBLWAK': 'wert_aktuell',
        # 'LBLWKO': 'lagerwert_korrektur', # scheinbar immer identisch mit LBLWAK
        'LBLWRT': 'lagerwert_vor_buchung',
        'LBMNGE': 'bewegungsmenge', # 'Bewegungs-Menge lt. Eingabe',
        # 'LBMEER': 'Mengeneinheit/Erfassung', # immer 1
        # 'LBFAKT': 'Umrechn.-Faktor  ER-->BE', # immer 1
        'LBMNGB': 'menge',
        'LBMNGZ': 'zugangsmenge', # Zugangsmenge nach Umlagerung',
        # 'LBMNGL': 'Menge noch am Lager', # FIFO - Menge? - scheinbar immer 0
        # 'LBMNGO': 'Menge Vorab-Bewertet', # scheinbar immer 0
        # 'LBMNGS': 'Abgangsmenge mit Statistik', - scheinbar immer 0
        # 'LBMGOP': 'Menge als offener Posten', - scheinbar immer 0
        'LBMGLP': 'bestand_vor_buchung',
        'LBMGKO': 'bestandsaenderung', # Bestandsänderung am Lager',
        #'LBBSTN': 'bestellnr',
        #'LBBSTP': 'bestellpos',
        # 'LBFNRK': 'Firmen-Nr./Lieferant',
        # 'LBLINR': 'lieferantennr',
        # 'LBZUKZ': 'Zustand: A/R/F', Zeicheinbar immer ''
        # 'LBAUFN': 'auftragsnr', # warenvereinnamungsnummer bei Zugängen
        # 'LBAUPO': 'auftragspos',
        # 'LBKZKO': 'KZ. Mit Komponenten', # immer 6?
        'LBDTER': 'erfassung_date',
        'LBDTAE': 'aenderung_date',
        'LBINFO': 'info',
        # 'LBART ': 'art', # bisher 0 und 1 beobachtet
        'LBBTYP': 'typ',
        'LBDFSL': 'dateifuehrungsschluessel',
        'LBSTAT': 'satzstatus',
        'LBSAN2': 'bezogene_bewegung',
        'LBSANR': 'satznummer',
    },

'XSB00': { # Sachbearbeiter
        'SBSBNR': 'id',
        'SBNAME': 'name',
    },

'XTY00': { # Versandarten
          'TYFNR': 'firma',
          'TYANW': 'anwendung',
          'TYVSAR': 'versandart',
          'TYVSBE': 'bezeichnung',
          'TYBSTX': 'kurzbezeichnung',
          'TYBFWG': 'versandweg',
          # 'TYKZTL': 'Tourenliste drucken',
          # 'TYKZLL': 'Ladeliste drucken',
          # 'TYKZSS': 'Speditionsschein drk',
          # 'TYKZPL': 'Paketliste drucken',
          # 'TYKZPA': 'Paketaufkleber drucken',
          # 'TYKZPT': 'Paketaufklebertyp',
          # 'TYTACD': 'Tarifcode',
          # 'TYKZLK': 'Zusaetzliche LfSnKopie',
          # 'TYFNRK': 'Firma Spediteur',
          # 'TYLINR': 'Spediteur',
          # 'TYHINW': 'Besondere Zustellart',
          # 'TYVZWE': 'Verkehrsweg',
          # 'TYKZSP': 'LfSn per EDI an Spediteur',
          # 'TYAVN1': 'Aufruf Packdialog',
          # 'TYNRKR': 'Paketnummernkreis',
          # 'TYKZTS': 'TOF-Sonderdienst',
          # 'TYVRBP': 'Rabattprozentsatz',
          # 'TYVRX3': 'Textschluessel Rabatt',
          # 'TYVRIN': 'Rabattkennzeichen',
          # 'TYPORT': 'Hafen/Flughafen',
          # 'TYVWGW': 'Verkehrsweg',
          # 'TYRSK1': 'Reserve-Kennzeichen 1',
          # 'TYRSK2': 'Reserve-Kennzeichen 2',
          # 'TYRSK3': 'Reserve-Kennzeichen 3',
          # 'TYRSK4': 'Reserve-Kennzeichen 4',
          # 'TYRSK5': 'Reserve-Kennzeichen 5',
          # 'TYRSK6': 'Reserve-Kennzeichen 6',
          # 'TYRSK7': 'Reserve-Kennzeichen 7',
          # 'TYRSK8': 'Reserve-Kennzeichen 8',
          # 'TYRSK9': 'Reserve-Kennzeichen 9',
          # 'TYRES1': 'Reservefeld 1',
          # 'TYRES2': 'Reservefeld 2',
          # 'TYRES3': 'Reservefeld 3',
          # 'TYRES4': 'Reservefeld 4',
          # 'TYRES5': 'Reservefeld 5',
          # 'TYRES6': 'Reservefeld 6',
          # 'TYRES7': 'Reservefeld 7',
          # 'TYDTER': 'Datum Erfassung CJJMMTT',
          # 'TYSBER': 'Sachb. Erfassung',
          # 'TYBPER': 'BenutzerPr.Erfassung',
          # 'TYDTAE': 'Datum l. ¢nderung CJJMMTT',
          # 'TYSBAE': 'Sachb.letzte ¢nderung',
          # 'TYBPAE': 'BenutzerPr.letzte ¢nderung',
          # 'TYDFSL': 'Dateifuehrungs-Schluessel',
          'TYSTAT': 'satzstatus'},

# SMKDIFP    XLF00      XLF00F01   Lagerbestands-F}hrungs-Datei    
# LFFNR    A         2  0    1    2 Firma
# LFARTN   A        20  0    3   22 Artikel-Nummer
# LFLGNR   S    4    4  0   23   26 Lager
# LFLGPL   A         6  0   27   32 Lager-Platz
# LFABT    S    4    4  0   33   36 Abteilungs-Nummer
# LFAGRP   A         2  0   37   38 Abteilungs-Gruppe/Sparte
# LFFGRP   A         2  0   39   40 Firmen-Gruppe
# LFARTG   A         6  0   41   46 Artikel-Gruppe
# LFARTH   A         6  0   47   52 Artikel-Haupt-Gruppe
# LFLGRP   S    4    4  0   53   56 Lagergruppe
# LFLGPH   A         6  0   57   62 Haupt-Lagerplatz
# LFFNRM   A         2  0   63   64 Mandanten-Nummer
# LFLINR   A         8  0   65   72 Hauptlieferant
# LFLIAR   A        20  0   73   92 Lieferanten-Artikel-Nr
# LFKWWB   P    3    2  0   93   94 Wiederbeschaffungszeit TTT  
# LFKWEZ   P    3    2  0   95   96 Eindeckzeit in TTT
# LFPREE   P   15    8  2   97  104 Letzter Einkaufspreis
# LFPRVR   P   15    8  2  105  112 Verrechnungspreis
# LFPRBD   P   15    8  2  113  120 Buchdurchschnittspreis
# LFPRV2   P   15    8  2  121  128 Verrechnungspreis 2
# LFLWRT   P   15    8  2  129  136 Wertm{ssiger LagerBestand
# LFLWZM   P   15    8  2  137  144 monatl. Zug{nge / wertm{ssi
# LFLWZJ   P   15    8  2  145  152 J{hrl. Zug{nge / wertm{ssig
# LFLWAM   P   15    8  2  153  160 Monatl. Abg{nge / wertm{ssi
# LFLWAJ   P   15    8  2  161  168 J{hrl. Abg{nge / Wertm{ssig
# LFLWVO   P   15    8  2  169  176 Wert vorab-bewertet
# LFMGLP   P   11    6  3  177  182 Buchbestand
# LFMGNP   P   11    6  3  183  188 Zugeteilt f}r Fertigung
# LFMGSP   P   11    6  3  189  194 Sperr-Bestand
# LFMGZM   P   13    7  3  195  201 Monatl. Zug{nge / Menge
# LFMGZJ   P   13    7  3  202  208 J{hrl. Zug{nge / Menge
# LFMGAM   P   13    7  3  209  215 Monatl. Abg{nge / Menge Ver
# LFMGAJ   P   13    7  3  216  222 J{hrl. Abg{nge / Menge Verk
# LFMGFM   P   13    7  3  223  229 Monatl. Abg{nge / Menge Fer
# LFMGFJ   P   13    7  3  230  236 J{hrl. Abg{nge / Menge Fert
# LFMGDU   P   11    6  3  237  242 Durchschnittlicher Bestand
# LFMGVO   P   11    6  3  243  248 Menge Vorab-bewertet
# LFMGST   P   11    6  3  249  254 Menge Statistik / neg.Bst
# LFMGAU   P   11    6  3  255  260 Auftrags-Bestand / Gesamt
# LFMGAR   P   11    6  3  261  266 Auftr.-Bstd./Wiederb.-Zeit
# LFMGAE   P   11    6  3  267  272 Auftr.-Bstd./Eindeck-Zeit
# LFMGA1   P   11    6  3  273  278 Vorab- Menge: Fakturierung
# LFMGA2   P   11    6  3  279  284 Vorab- Menge: Frei
# LFMGA3   P   11    6  3  285  290 Verbuchte Mg: Fakturierung
# LFMGA4   P   11    6  3  291  296 Verbuchte Mg: Frei
# LFMGAL   P   11    6  3  297  302 Res.-Menge / Bestell-Bstd.
# LFMGAN   P   11    6  3  303  308 Bestell-Bstd. Eindeckzeit
# LFMGBL   P   11    6  3  309  314 Bestell-Bestand
# LFMGBN   P   11    6  3  315  320 Bestell-Bstd. Wiederb.-Zeit
# LFMGB1   P   11    6  3  321  326 Vorab- Menge: Einkauf
# LFMGB2   P   11    6  3  327  332 Vorab- Menge: Fertigung
# LFMGB3   P   11    6  3  333  338 Verbuchte Mg: Einkauf
# LFMGB4   P   11    6  3  339  344 Verbuchte Mg: Fertigung    
# LFMGK1   P   11    6  3  345  350 Fertigung: Reserv. Mg. WBZ
# LFMGK2   P   11    6  3  351  356 Fertigung: Reserv. Mg. EDZ
# LFMGK3   P   11    6  3  357  362 Fertigung: Gesamt-Reservier 
# LFMGK4   P   11    6  3  363  368 Zugeteilt für Lieferschein  
# LFMGOP   P   11    6  3  369  374 Gesamte OP-Menge            
# LFMGQU   P   11    6  3  375  380 Bestand in QltPrf           
# LFMGQ2   P   11    6  3  381  386 Bestand in QltPrf 2         
# LFLWA1   P   15    8  2  387  394 Gesamter Auftragswert       
# LFLWA2   P   15    8  2  395  402 Lieferschein-Wert           
# LFLWA3   P   15    8  2  403  410 vorabvermerkter Wert        
# LFNARB   P    3    2  0  411  412 Numerisches Arbeitsfeld     
# LFDTIN   P    7    4  0  413  416 Datum letzte Inventur       
# LFKZIN   S    1    1  0  417  417 KZ Inventur-Sperre          
# LFMGIB   P   11    6  3  418  423 Bestand bei Inventur-Beginn 
# LFLWIB   P   15    8  2  424  431 Lagerwert nach Inventur     
# LFEKIB   P   15    8  2  432  439 LEK bei Inventur-Beginn     
# LFBELI   P    9    5  0  440  444 Beleg-Nr/Inventur           
# LFMGIN   P   11    6  3  445  450 Inventur-Bestand            
# LFDTIE   P    7    4  0  451  454 Datum der Inventurliste     
# LFKZWR   S    1    1  0  455  455 Preisdimension Lager        
# LFKZIA   S    1    1  0  456  456 Kz. Inventurabwicklung      
# LFKZCS   S    1    1  0  457  457 KZ Chargenzuordnung         
# LFKZPS   S    1    1  0  458  458 KZ Programmsperre           
# LFDTLZ   P    7    4  0  459  462 Datum letzter Lagerzugang   
# LFDTLA   P    7    4  0  463  466 Datum letzter Lagerabgang   
# LFF1A1   A         1  0  467  467 Reservefeld 1 1A            
# LFF1A2   A         1  0  468  468 Reservefeld 2 1A            
# LFF1A3   A         1  0  469  469 Reservefeld 3 1A            
# LFF1A4   A         1  0  470  470 Reservefeld 4 1A            
# LFF1A5   A         1  0  471  471 Reservefeld 5 1A            
# LFFDT1   P    7    4  0  472  475 Reservefeld 1 Datum         
# LFFDT2   P    7    4  0  476  479 Reservefeld 2 Datum         
# LFFA10   A        10  0  480  489 Reservefeld 1 10A           
# LFFWR1   P   15    8  2  490  497 Reservefeld Wert 1 15P2     
# LFFWR2   P   15    8  2  498  505 Reservefeld Wert 2 15P2  
# LFFWR3   P   15    8  2  506  513 Reservefeld Wert 3 15P2  
# LFFWR4   P   15    8  2  514  521 Reservefeld Wert 4 15P2  
# LFFMG1   P   13    7  3  522  528 Reservefeld Menge 1 13P3 
# LFFMG2   P   13    7  3  529  535 Reservefeld Menge 2 13P3 
# LFFMG3   P   13    7  3  536  542 Reservefeld Menge 3 13P3 
# LFFMG4   P   13    7  3  543  549 Reservefeld Menge 4 13P3 
# LFFK20   A        20  0  550  569 Kundenindividuelles Feld 
# LFDTAE   P    7    4  0  570  573 Datum letzte Änderung    
# LFKZAK   S    1    1  0  574  574 KZ Aktualität            
# LFDFSL   A        10  0  575  584 Dateiführungs-Schlüssel  
# LFSTAT   A         1  0  585  585 Satzstatus               
}

# decides which fields should be convertet to decimal, 2 digits
DECIMALIZE2 = set(['BUR1', 'BUBUBT', 'BUNEBT', 'BUNEWB', 'BUABZU', 'BUWBBT',
                   'LBPREW', 'LBLWER', 'LBLWAK', 'LBLWKO', 'LBLWRT', 
                   'BPPREW', 'BPPRAK', 'BPPREH', 'BPWRTF', 'BLKURS',
                   'PNPRB',
                   'SLPREW', 'SLWRTI', 'SLFRBT', 'SLZOBT', 'SLPREW', 'SLLWKO', 'SLLWFR', 'SLLWZO',
                   'WZBUBT', 'WZPREA', 'WZPREW'])

# maps datefield to related timefield for generating datetime objects
DATETIMEDIR = {'LKDTLF': 'LKZTLF', # letzter_lieferschein
               'LKDTKB': 'LKZTKB', # letzter_kommissionierbeleg
               'LKDTER': 'LKZTER', # erfassung
               'LKDTAE': 'LKZTAE', # aenderung
               'LNDTLF': 'LNZTLF', # lieferschein
               'LNDTER': 'LNZTER', # erfassung
               'LNDTAE': 'LNZTAE', # aenderung
               'IKDATE': 'IKTIME',
               'LNDTKB': 'LNZTKB',
               'IZDTWZ': 'IZTIME',
               'SKDTAE': 'SKZTAE', # komponenten
               'CKANFD': 'CKANFZ', # XCK00 start
               'CKENDD': 'CKENDZ', # XCK00 ende
               }

# Fields which need padding before beeing used in SQL queries
PADDINGFIELDS = {
    'AKKDNR': "%8s",
}