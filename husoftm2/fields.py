#!/usr/bin/env python
# encoding: utf-8
"""
fields.py - describes the SoftM table structure. Part of huSoftM.

See also http://cybernetics.hudora.biz/projects/wiki/SoftMtabellen

Created by Maximillian Dornseif on 2007-03-18. Based on code named "MoftS" from Summer 2005.
Copyright (c) 2007, 2008, 2009, 2011 HUDORA GmbH. All rights reserved.
"""


MAPPINGDIR = {
'ABV00': {  # Adressdaten zu Aufträgen die in die Stapelschnittstelle geschrieben werden
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

'AAK00': {  # Auftragsköpfe
          # 'AKFNR ': 'Firma',
          'AKAUFN': 'auftragsnr',
          'AKSBNR': 'sachbearbeiter',
          # 'AKABT ': 'Abteilungs-nr',
          # 'AKAGRP': 'Abteilungs-Gruppe/Sparte',
          # 'AKFGRP': 'Firmen-Gruppe',
          'AKAUFA': 'art',
          # Als Auftragsart darf nur ein Wert angegeben werden, der als zulässige Auftragsart in der
          # Parameterdatei hinterlegt ist. Sie steuert, wie der Auftrag im System abgewickelt werden soll.
          # So kann u.a. auftragsartenabhängig festgelegt werden:
          # * Soll die Auftragsbest‰tigung gedruckt werden?
          # * Muss die bezogene Rechnung angegeben werden? (z.B. bei Gutschriften oder Nachbelastungen)
          # * Ist Streckengesch‰ft zul‰ssig?
          # * Soll die Kreditlimitprüfung durchgeführt werden?
          # * Sind Sofortformulare möglich bzw. Pflicht?
          # * Sind Sammelbelege möglich (Lieferschein, Rechnung)?
          # * Welche Schnittstellen (Lager, Statistik, FiBu, Kostenrechnung) sollen versorgt werden?
          # Zum Teil werden diese Festlegungen auch verwendet, um Eingabefelder vorzubesetzen.
          # 'AKRANR': 'Rahmenauftrag',
          # Die Angabe ist nur gefüllt, wenn bei der Auftragserfassung auf eine bereits bestehende
          # Rahmenvereinbarung Bezug genommen und die Rahmenauftragsnummer für den gesamten Auftrag
          # angegeben wurde.
          # 'AKKANR': 'Übergeordneter Auftrag',
          # Soll für einen Auftrag ein Bezug zu einem bereits bestehenden Hauptauftrag hergestellt
          # werden, so ist hier die Nummer dieses Hauptauftrags einzugeben. Intern wird eine neue
          # Auftragsnummer vergeben. Die Kopfdaten des neuen Auftrags werden mit denen des
          # Hauptauftrags vorbesetzt. Die Angabe 'Hauptauftrag' ist nur dann gefüllt, wenn bei der
          # Auftragserfassung eine entsprechende manuelle Eingabe erfolgte. Falls für einen Auftrag
          # ein Bezug zu einem bereits bestehenden 'Hauptauftrag' hergestellt werden soll, so ist
          #  hier die Nummer dieses Hauptauftrags einzugeben. Intern wird eine neue Auftragsnummer
          # vergeben. Die Kopfdaten des neuen Auftrags werden mit denen des Hauptauftrags vorbesetzt.
          # 'AKFNRM': 'Mandanten-nr',
          'AKKDNR': 'kundennr_warenempf',
          # 'AKKDN3': 'Kunden-Nr/Verbraucher',
          # 'AKEGCD': 'Ländercode / EG',
          # 'AKUSTN': 'USt-Id-nr',
          # EU-einheitliche Steuernummer (USt-IdNr, VAT-Nr). Beim Druck der USt-IdNr ist zus‰tzlich noch
          # der EU-L‰ndercode mitzudrucken.
          'AKVANR': 'versandadressnr',
          # Dreistellige Nummer, unter der zus‰tzliche Lieferadressen zum Kunden verwaltet werden. Falls
          # für den Auftrag eine abweichende Lieferadresse verwendet werden soll, die in der
          # Lieferadressdatei gespeichert ist, kann die Nummer der Lieferadresse hier angegeben werden.
          # Die Nummer kann auch über F5=Lieferadressübersicht ausgewählt werden.
          # 'AKALS1': 'Alphasortierung/Warenempfän',
          'AKKDRG': 'kundennr_rechnungsempf',
          # Das Feld wird mit dem Rechnungszahler lt. Kundenzusatzdatei vorbesetzt. Die
          # Rechnungszahler-Kundennummer muss in den Datei XKD00, XKS00 und AKZ00 gespeichert sein.
          # Wird ein abweichender Rechnungszahler angegeben, dann wird dessen Adresse bei der
          # Kopfdateneingabe als Rechnungsadresse angezeigt. Als Lieferadresse wird in diesem Fall die
          # Adresse lt. Kundennummer 1 angezeigt, falls keine Lieferadresse angegeben wurde.
          # Falls ein abweichender Rechnungszahler angegeben wird, werden folgende Daten aus dem
          # Rechnungszahler geholt.
          # Kundensaldensatz: Kreditlimit
          # Kundenzusatz: Liefersperre Adresse änderbar, Kz Sammelrechnung, Zahlungsbedingung, Rabatte
          # 'AKALS2': 'Alphasortierung/Rech-Empfän',
          # 'AKVERB': 'Verband/Mischkonto',
          # Bei Zugehörigkeit des Kunden/Lieferanten zu einem Verband oder Mischkonto ist hier die Nummer
          # des Verbandes oder des Mischkontos einzutragen. Bei einem Mischkonto vom Typ N ist hier die
          # Debitorennummer einzutragen.
          # 'AKABEM': 'Ab-Empfänger: R = Rg-Zahler',
          # Das Feld wird mit der Angabe lt. Auftragsart vorbesetzt, es wird nur benötigt, falls
          # Warenempfänger und Rechnungsempfänger unterschiedlich sind.
          # *BLANK      Warenempfänger,  R Rechnungsempfänger, V Lieferadresse
          # 'AKRGEM': 'Rechnungsempfänger',
          # *BLANK Rechnungszahler
          # 1      Warenempf‰nger - Als Rechnungsadresse wird die Adresse des Warenempfängers gedruckt.
          'AKNRKD': 'auftragsnr_kunde',
          # 'AKDTKD': 'auftragsdatum_kunde',
          # Datum, das der Kunde bei der Auftragserteilung mitgeteilt Das Datum ist als Information
          # für den Kunden gedacht.
          # 'AKDTLE': 'Leih-Datum',
          # 'AKDTKW': 'kundenwunsch_date',
          # Termin, zu dem der Kunde die Ware erhalten soll. Für unbestimmte Termine oder
          # Rahmenkonditionen kann der Sonderwert '999999' eingegeben werden. Dieser Termin kann
          # die Konditionsermittlung beeinflussen. Der Termin kann über die automatische
          # Terminermittlung bei der Erfassung vorbesetzt werden.
          # 'AKJWKW': 'Kundenwunschtermin CJJWW',
          # 'AKPROB': 'Objekt/Projekt/Aktion'
          # Zusätzliches Zugriffskriterium bei der Konditionsermittlung in der Auftragsbearbeitung.
          # 'AKDTVA': 'Valuta-Datum/Erfassung',
          # Bei Offenen Posten ist das Valutadatum das Datum, ab dem die Fälligkeit gerechnet wird.
          # Wird kein Skontoschlüssel eingegeben, entspricht das Valutadatum dem Fälligkeitsdatum.
          # Wird ein Skontoschlüssel eingegeben, ist das Valutadatum das Datum, ab dem die
          # Fälligkeiten gemäß Skontoschlüssel berechnet werden.
          # 'AKVALT': 'Abstandstage Valuta',
          'AKDTLT': 'liefer_date',
          # Termin, zu dem die Ware das Lager verlassen soll.
          # 'AKIK01': 'fixtermin',
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
          # Sollen statt Rabatten Zuschl‰ge eingegeben werden, so sind die Prozentsätze negativ einzugeben.
          # Mögliche Vorbesetzungen:
          # Keine Vorbesetzung
          # Rabatt lt. Kundenzusatz Rechnungsempfänger
          # Rabatt lt. Versandart Warenempfänger
          # Rabatt lt. Preisdatei Waren- bzw. Rechnungsempfänger
          # 'AKRBP2': 'Auftrags-Rabatt-%-2',
          # 'AKX3R1': 'Textschl. Rabatt 1',
          # In der Validierungsdatei kann zu diesem Rabattschlüssel eine Rabattgruppe hinterlegt werden.
          # Außerdem kann dort für Positionsrabatte angegeben werden, ob es sich um einen Rabatt handelt,
          # der den Positionswert vermindert, oder um einen Zuschlag, der ihn erhöht.
          # 'AKX3R2': 'Textschl. Rabatt 2',
          # 'AKKZRX': 'Kz.Rabatt Brutto',
          # Festlegung, wie der zweite Auftragsrabatt berechnet werden soll. PTYP Tabelle
          # *ZERO Rabatt 2 auf Positionswert Netto
          # 1     Rabatt 2 auf Positionswert Brutto
          # 'AKMWKZ': 'Umsatz-Steuerprofil',
          # 'AKWSL ': 'Währungs-Kennzeichen',
          # Vorbesetzung mit dem Währungsschlüssel lt. Kundenzusatz- bzw. Kundenadressatz des
          # Rechnungsempfängers. Währungsschlüssel müsse im Parametersatz 'Währungen' angelegt werden.
          # 'AKKURS': 'Wechsel-Kurs',
          # Mit diesem Umrechnungsfaktor können die Angaben in Auftragswährung (Preise, Rabatte und
          # Nebenkosten) in die Hauptwährung der Firma umgerechnet werden:
          # Wert (Hauptwährung) := Wert (Vorgangswährung) * Umrechnungsfaktor ggf. modifiziert um den
          # Währungsfaktor
          # Wenn die Bezugswährung für Wechselkurse (im Regelfall "EUR") mit der Hauptwährung der Firma
          # übereinstimmt, dann ist dieser Umrechnungsfaktor identisch mit dem Wechselkurs.
          # 'AKKUFA': 'Kursfaktor',
          # Festlegung, auf wieviele Fremdw‰hrungseinheiten sich der Wechselkurs bezieht:
          # *ZERO Kurs pro Fremdw‰hrungseinheit
          # 1     Kurs pro 10 Fremdw‰hrungseinheiten
          # 2     Kurs pro 100 Fremdw‰hrungseinheiten
          # 3     Kurs pro 1000 Fremdw‰hrungseinheiten
          # 'AKSPSL': 'Sprache',
          # Mit dem Sprachschlüssel l‰sst sich steuern, welche Texte in den Formularen gedruckt werden.
          # Voraussetzung dafür ist, dass die zu druckenden Texte (z.B. Versand-, Liefer- und
          # Zahlungsbedingungen) in der entsprechenden Sprache hinterlegt sind.
          # Gültige Eingaben müssen in der Validierungsdatei unter dem zugehörigen Prüftyp hinterlegt sein.
          # 'AKVRT1': 'Vertreter 1',
          # 'AKVRT2': 'Vertreter 2',
          # 'AKPRZA': 'Aufteilungs-%-Satz',
          # ei Provisionssplitting (Vertreter 2 ist angegeben) wird hier festgelegt, welchen Prozentanteil
          # der fälligen Provision der Vertreter 1 erhalten soll.
          # 'AKBZRG': 'Bezogene RG-nr',
          # Das Feld wird nur angezeigt, wenn lt. Auftragsart die Angabe einer bezogenen Rechnungsnummer
          # erfoderlich ist (z.B. Gut- schriftsaufträge). Die hier eingegebene bezogene Rechnungsnummer
          # wird nach der Fakturierung des Auftrags an die Buchhaltung übergeben.
          # Dient als Vorbesetzung f ̧r Erfassung der Auftragsposition (statt Hauptlager aus Artikelstamm).
          # 'AKLGN1': 'auslieferungslager',
          'AKLGN2': 'zugangslager',
          # Bei Umbuchungsauftr‰gen und Leihauftr‰gen ist hier das empfangende Lager angegeben.
          # 'AKTRNR': 'Touren-nr',
          # 'AKLINR': 'Lieferant bei Strecke',
          # Bei Streckenaufträgen mit Streckenart = 3 bzw. 4 muss bei der Positionsbearbeitung ein
          # Lieferant angegeben werden. Wird der Lieferant bereits im Auftragskopf eingegeben, dann wird
          # dieser Wert als Vorbesetzung für die Positionserfassung verwendet.
          # 'AKSPED': 'Spediteur',
          # 'AKKZBE': 'Steuerung Best.-Druck',
          # 'AKKZST': 'Kennzeichen Streckengeschäf',
          # Eine Eingabe ist nur zulässig, wenn für die oben angegebene Auftragsart Streckengeschäft
          # zul‰ssig ist. Streckengeschäftsaufträge laufen generell ohne Bestandsführung, es werden jedoch
          # Lieferscheine gedruckt.
          # *ZERO       kein Streckengesch‰ft
          # 1   Streckengesch‰ft ohne Bestandsf ̧hrung
          # 2   Kundenindividuell
          # 3   Bestellware direkt an Kunden
          #     Die Rechnungsfreigabe erfolgt mit der Rechnungsprüfung der zugehörigen Bestellung
          # 4   Bestellware über Lager an Kunden
          #     Die Rechnungsfreigabe erfolgt mit dem Warenzugang der zugehörigen Bestellung
          # 'AKKZWS': 'WZ/RE-Sperre',
          # 'AKKZIN': 'Kennzeichen interner Beleg',
          # Über dieses Kennzeichen kˆnnen Vorg‰nge als interne Belege gekennzeichnet werden.
          # Bei internen Belegen wird im Rechnungsformular im Anschriftsbereich der Hinweis
          # "interner Beleg / nicht verschicken gedruckt"
          # 'AKKZBO': 'Steuerung Bonus-Abrechnung',
          'AKKZTF': 'teillieferung_erlaubt',  # bzw. Teilfakturierung
          # Bei Teillieferung zul‰ssig = nein wird der Kommissionierbeleg bzw. Lieferschein erst dann
          # gedruckt, wenn alle Positionen des Auftrags lieferbar sind.
          # 'AKKZSR': 'Kz.Drucken in Sammelrech=1',
          # 'AKKZZF': 'un-/versteuert',
          'AKKZVA': 'voll_ausgeliefert',
          # Sind alle Positionen eines Auftrags geliefert, so steht das Kennzeichen auf 1 andernfalls auf 0.
          # 'AKKZZU': 'Kz.Bevorzugte Zuteilung',
          # 'AKKZRK': 'Kz: Rückstand möglich',
          # Festlegung, ob Rückstand erlaubt ist. Ist Rückstand nicht erlaubt, so werden auch nicht
          # vollständig lieferbare Positionen nach Fakturierung auf 'voll ausgeliefert' gesetzt.
          # 'AKKZSL': 'Kz.Sammel-Lieferschein',
          # 'AKKZNN': 'Kz.Nachnahme',
          # 'AKKZAR': 'Kz.Leihart',
          # 'AKABTP': 'Abt. für Prüfung KD-Zusatz',
          'AKDTER': 'AAK_erfassung_date',
          'AKZTER': 'AAK_erfassung_time',
          'AKDTAE': 'AAK_aenderung_date',
          'AKZTAE': 'AAK_aenderung_time',
          # 'AKDTAB': 'Datum letzte Auftragsbestät',
          # 'AKDTLF': 'lieferschein_date'  # 'Datum letzter Lieferschein',
          # 'AKDTKB': # 'Datum ltzt. Kommissionierbel',
          # 'AKDTFA': 'rechnung_date  # Fakturierungsdatum
          # Datum der Vorbereitung der letzten Rechnung/Gutschrift. Das tatsächliche Datum des letzten
          # Rechnungs-/Gutschriftsdrucks steht in der Rechnungskopfdatei.
          # 'AKKZAE': 'AB: nur geänderte Posit.',
          # Auftragsbest‰tigung nur für geänderte Positionen drucken
          # 'AKFKZA': 'Formular-KZ AB',
          # 'AKFKZK': 'Formular-KZ Kommission.Bele',
          # 'AKFKZL': 'Formular-KZ Lieferschein',
          # 'AKFKZR': 'Formular-KZ Rechnung/Gutsch',
          # 'AKSFAA': 'AB Sofort',
          # 'AKSFAL': 'LF Sofort',
          # 'AKSFAK': 'Komm.-Beleg sofort',
          # 'AKSFAR': 'RG Sofort',
          # 'AKABCO': 'Anzahl AB',
          # 'AKKBCO': 'anzahl_kommibelege',
          # 'AKLSCO': 'anzahl_liefscheine',
          # 'AKRGCO': 'anzahl_rechnungen',
          # 'AKOQZO': 'Outqueue-Zuordnung',
          # 'AKGEB ': 'Gebiet',
          # 'AKBRAN': 'Branche',
          # 'AKPLZ':  'plz',
          # 'AKLKZ': 'land',
          # 'AKVRG1': 'Vertriebs-Gruppe 1',
          # 'AKVRG2': 'Vertriebs-Gruppe 2',
          # 'AKDSTK': 'Distrikt',
          # 'AKAWRT': 'Auftragswert',
          # Angabe in Hauptwährung. Summe der offenen Positionswerte dieses Auftrags.
          # 'AKSTOR': 'Kennzeichen Stornierung',
          # Vorgangsart - Festlegung, ob es sich bei der Rechnungsschreibung um eine Gutschrift oder
          # Stornierung handelt. Der Eintrag wird aus der Auftragsart übernommen.
          #  *ZERO Gutschrift, 1 Rechnung
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
          # 'AKKDN1': 'Adresse 1', # Adressnummer
          # 'AKKDN2': 'Adresse 2',
          # 'AKZUOR': 'Zuordnung für dezentrale An',
          # 'AKDFSL': 'Dateiführungs-Schlüssel',
          'AKSTAT': 'AAK_status'
          # 'AKRSA6': 'Satznummer Ansprechpartner',
         },

'AAP00': {  # Auftragspositionen
          'APMNG-APMNGF-APMNGG': 'menge_offen',
          # Für die Artikelverfügbarkeit muss scheinbar diese "offene Menge" verwendet werden:
          'APMNG-APMNGF': 'menge_offen2',
           # 'APFNR ': 'Firma',
           'APAUFN': 'auftragsnr',
           'APAUPO': 'position',
           # 'APSBNR': 'sachbearbeiter',
           # 'APABT':  'Abteilungs-nr',
           # 'APAGRP': 'Abteilungs-Gruppe/Sparte',
           # 'APFGRP': 'Firmen-Gruppe',
           # 'APFNRX': 'Best.-Führungsfirma',
           'APKDNR': 'warenempfaenger',
           # 'APKDRG': 'Kundennr.Rechnungs-Empfänge',
           # 'APVERB': 'Verband/Mischkonto',
           # 'APLINR': 'Lieferanten-nr',
           'APAUFA': 'auftragsart',
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
           # 'APVGNR': 'vorgangs_nr',
           # 'APVGPO': 'vorgangs_position',
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
           # 'APKZKO': 'komponentenaufloesung',
           # 'APKZPS': 'Positionsbezogener Set',
           # 'APKZEP': 'KZ: Eigen-Produkt',
           # *ZERO       Eigenprodukt
           # 1           Handelsware
           # 2           Werbematerial
           # 'APKZMU': 'Muster-Artikel',
           # *ZERO       Normalposition
           # 1           Musterartikel
           # 2           kostenlose Lieferung
           # 3           Naturalrabatt
           'APMNG': 'bestellmenge',               # Die Menge, die der Kunde haben will
           # Mengenangaben
           # Die offene (noch nicht fakturierte) Menge ergibt sich aus:
           #      APMNG - APMNGF
           # Die offene (noch nicht gelieferte) Menge ergibt sich aus:
           #      APMNG - APMNGF - APMNGG
           # Die offene (noch nicht zugeteilte) Menge ergibt sich aus:
           #      APMNG - APMNGF - APMNGG - APMNGL
           #      (diese Menge ist auch f}r den Auftragsbestand zu
           #      ber}cksichtigen)
           # Die zur Fakturierung freigegebene Mengen werden nicht mehr auf
           # Auftragspositionsebene verwaltet, sie stehen in der ALN00.
           # Grund: Zu einer Position k¦nnen mehrere Lieferscheine existieren,
           # die noch nicht fakturiert sind.
           # Die zugeteilte Menge errechnet sich aus:
           #      APMNGL +  APMNGG - Summe LNMNGR (zur Fakturierung
           #                                      freigegebene, aber noch
           #                                      nicht fakturierte
           #                                      Mengen dieser Pos)
           #   Menge, die im nächsten Kommissionierbeleg oder Lieferschein
           #   (abhängig von der Parameterisierung) gedruckt werden soll.
           # 'APMNGL': 'Menge zu liefern',
           'APMNGL+APMNGG': 'menge_zugeteilt',
           # 'APMNGG': 'Menge/Liefersch.nichtfakt',
           # Menge, }ber die bisher Lieferscheine geschrieben wurden und die
           # noch nicht fakturiert wurde. (Es k¦nnen mehrere Lieferscheine
           # geschrieben worden sein|) Beim Lieferscheindruck wird diese
           # Menge erh¦ht, bei der Fakturierung und beim Lieferscheinstorno
           # vermindert. Achtung: Wird in der Lieferscheinr}ckmeldung eine
           # vom Beleg abweichende Menge freigegeben, so wird dieses
           # Mengenfeld erst bei der Fakturierung korrigiert. Grund: Vor der
           # Fakturierung kann die R}ckmeldung jederzeit zur}ckgenommen
           # werden. Dieses Feld verhindert bis dahin die erneute
           # Stapelzuteilung| (!ber den Parameter PADMGG wird gesteuert, ob
           # evtl. doch sofort nach der R}ckmeldung eine erneute Zuteilung
           # m¦glich ist.)
           'APMNGF': 'fakturierte_menge',          # Menge, die geliefert und fakturiert ist
           # Bereits fakturierte Menge aller Rechnungen f}r diese
           # Auftragsposition. Unber}cksichtigt bleiben Proforma- und
           # Vorabrechnungen, sowie die aus der Lieferscheinr}ckmeldung
           # abrufbaren wertm{~igen Rechnungsarten. Bei Rahmenpositionen
           # stellt die fakturierte Menge die Summe der bisher abgerufenen
           # 'APMNGR': 'Menge AFA35',
           # 'APMNGB': 'Bestellmenge in ME XLF',
           # 'APMESL': 'Mengen-Einheit',
           # 'APMEKZ': 'ME XLF/AAP',
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
           # 'APFWRT': 'fakturierter_wert',
           # Der fakturierte Positionswert.
           # Bei Rahmenpositionen der Wert der abgerufenen Positionen.
           # 'APOWRT': 'offener_auftragswert',
           # Der noch nicht fakturierte Positionswert.
           # Bei Rahmenpositionen der Wert der noch nicht abgerufenen
           # Positionen.
           # 'APPREL': 'listenpreis',
           # Der Preis aus dem Artikelstamm oder der Preisdatei kann nicht
           # ge{ndert werden.
           # 'APPREE': 'Einstands-Preis',
           # Die Definition, welcher Einstandspreis angezeigt werden soll,
           # wird in den Firmenparametern Teil 1 (XPX00E01/PXBWKZ) festgelegt.
           # 'APPRKZ': 'Preiskennzeichen Lager',
           # 'APWSLE': 'Währungsschlüssel: EK',
           # 'APKURS': 'Wechselkurs EK',
           # 'APKUFA': 'Kursfaktor/EK',
           # 'APRBP1': 'Pos-Rabatt-1',
           # Abh{ngig vom Rabattkennzeichen kann es sich um einen Betrag,
           # einen Prozentsatz, einen Betrag pro Menge oder einen Betrag pro
           # Gebindeanzahl handeln.
           # Die Festlegung, ob es sich um einen Rabatt oder um einen
           # Zuschlag handelt, erfolgt auf zwei Arten:
           # 1.) Eine negative Eingabe wird im Normalfall als Zuschlag
           #     behandelt.
           # 2.) !ber den Rabatttextschl}ssel kann bereits definiert werden,
           #     ob es sich um einen Zu- oder Abschlag handelt. Die
           #     Festlegung bei der Rabattart wird links neben dem
           #     Rabatteingabefeld angezeigt: + f}r Zuschl{ge
           #                                     - f}r Abschl{ge
           #    Vorbesetzung mit dem Rabatt lt. Artikel- bzw. Preisdatei.
           #    Wieviele Rabatte angeboten werden, kann im Parametersatz
           #    'Steuerung Auftrag' festgelegt werden (Feld PABRAB)
           #    Rabatt 1 abh{ngig vom Rabattkennzeichen 1
           # 'APRBP2': 'Pos-Rabatt-2',
           # 'APRBP3': 'Pos-Rabatt-3',
           # 'APRBP4': 'Pos-Rabatt-4',
           # 'APKZR1': 'Berechnung Rabatt 1',
           # *ZERO       Rabatt in Prozent
           # 1           Rabatt als Betrag
           # 2           Rabatt als Betrag pro Menge
           #             abh{ngig von Preisdimension (z.B. Rabatt per 100)
           # 3           Rabatt als Betrag pro Gebindeanzahl
           #             nur erlaubt, falls mit Gebinde gearbeitet wird.
           # 'APKZR2': 'Berechnung Rabatt 2',
           # 'APKZR3': 'Berechnung Rabatt 3',
           # 'APKZR4': 'Berechnung Rabatt 4',
           # 'APX3R1': 'Textschl. Rabatt 1',
           # In der Validierungsdatei kann zu diesem Rabattschl}ssel eine
           # Rabattgruppe hinterlegt werden. Au~erdem kann dort f}r
           # Positionsrabatte angegeben werden, ob es sich um einen Rabatt
           # handelt, der den Positionswert vermindert, oder um einen
           # Zuschlag, der ihn erh¦ht.
           # 'APX3R2': 'Textschl. Rabatt 2',
           # 'APX3R3': 'Textschl. Rabatt 3',
           # 'APX3R4': 'Textschl. Rabatt 4',
           # 'APKZRU': 'Kz.Rabatt versteckt',
           # *ZERO       Rabatte werden ausgewiesen
           #  1           Rabatte werden n. ausgewiesen
           #              Rabatte werden in den Formularen nicht ausgewiesen. Al
           #              Verkaufspreis wird der um den Rabatt verminderte
           #              Verkaufspreis gedruckt.
           #            # 'APKZRX': 'Kz.Rabatte Brutto',
           # *ZERO       Rabatt auf Nettopreis
           #             Folgerabatte (falls in Prozent) werden immer vom berei
           #             verringerten Wert gerechnet
           #             Beispiel: 100 EUR - 10% = 90, 90 - 10% = 81 EUR
           # 1           Rabatt auf Bruttopreis
           #             Folgerabatte (falls in Prozent) werden immer vom Brutt
           #             gerechnet
           #             Beispiel: 100 EUR - 10% - 10% = 80 EUR
           # 'APKZRB': 'Kz: kein Auftragsrabatt = 1', Festlegung, ob ein
           #            Auftragsrabatt f}r die Position g}ltig ist.
           # 'APKZET': 'Steuerung Sortiments-Rabatt',
           # 'APKZSR': 'Sortiments-Rabatt gewährt',
           # 'APMWAR': 'Steuerart / Artikel',
           'APDTLT': 'liefer_date',
           # Termin, zu dem die Ware das Lager verlassen soll.
           # F}r unbestimmte Termine oder Rahmenkonditionen kann der
           # Sonderwert '999999' eingegeben werden. Dieser Termin steuert den
           # Zeitpunkt der Zuteilung und der Belegschreibung. Au~erdem kann
           # er die Konditionsermittlung und die Berechnung des Kreditlimits
           # beeinflussen.
           # 'APJWLT': 'Liefertermin CJJWW',
           # 'APDTKD': 'kundenwunsch_date',
           # Termin, zu dem der Kunde die Ware erhalten soll.
           # F}r unbestimmte Termine oder Rahmenkonditionen kann der
           # Sonderwert '999999' eingegeben werden. Dieser Termin steuert den
           # Zeitpunkt der Zuteilung und beeinflusst die Konditionsermittlung.
           # 'APJWKD': 'Kundenwunschtermin CJJWW',
           # 'APDTLN': 'neuliefer_date',
           # Bei Positionen mit R}ckstandsverwaltung wird nach der ersten
           # Teillieferung der 'Liefertermin' durch den 'Liefertermin neu'
           # ersetzt. F}r unbestimmte Termine oder Rahmenkonditionen kann der
           # Sonderwert '999999' eingegeben werden.
           # 'APJWLN': 'Liefertermin neu CJJWW',
           # 'APKZTE': 'Kz: Termin-Format: 1 =WW/JJ',
           # 'APKZAB': 'Position in AB gedruckt',
           # 'APKZAE': 'geaendert',
           # Dieses Kennzeichen wird in der Auftrags{nderung gesetzt, wenn
           # sich folgende Daten ge{ndert haben:
           #      Auftragskopf: Liefertermin (+ Fnkt 'Kopfdaten }bernehmen')
           #      AuftragsPos : generell
           #      Auftragstext: Texte aus Auftragspositionsauskunft oder wenn der/die
           #      Auftragskopf/-position gel¦scht wird.
           # *ZERO       keine ¢nderung seit letztem AB-Druck
           # 1           ge{ndert nach letztem AB-Druck
           # 'APKZST': 'Kennzeichen Streckengeschäf',
           # Festlegung der Streckenabwicklung f}r diese Position.
           # *ZERO       keine Strecke
           # 1           Strecke ohne Bestellung
           # 2           Strecke ohne Bestellung
           # 3           direkte Strecke
           #             Es wird eine Bestellung im Einkauf erzeugt, die Ware g
           #             direkt vom Lieferanten an den Kunden. Die Fakturierung
           #             den Kunden wird bei der Buchung der Lieferanten-
           #             eingangsrechnung automatisch aktiviert.
           # 4           Strecke }ber Lager
           #             Es wird eine Bestellung im Einkauf erzeugt, die Ware g
           #             vom Lieferanten ans Lager und von dort an den Kunden.
           #             Liefermengenzuteilung erfolgt nach der Buchung des War
           #             zugangs.
           # 'APKZFA': 'Rg-Freigabe',
           # 'APKZEK': 'Schnittstelle Einkauf',
           # 'APKZBD': 'Steuerung Best.-Druck',
           # 'APKZSP': 'KZ: Sperre für Übernahme',
           # 'APHERK': 'Herkunft',
           # 'APKZSS': 'Kz Stapelschnittstelle',
           # 'APFA1A': 'Lieferschein-Auslösung',
           # Es werden nur Positionen mit Kennzeichen 'Kommissionierbeleg
           # drucken' im Kommissionierschein gedruckt. Das Kennzeichen wird
           # gesetzt:
           #   - bei der Auftragsbearbeitung, falls der Termin erreicht und
           #     die Ware verf}gbar ist
           #   - bei der Freigabe von zugeteilten R}ckst{nden f}r die
           #     Kommissionierbeleg-Schreibung
           #   - f}r R}ckstandspositionen beim Druck des Kommissionierbelegs,
           #     wenn f}r einen Kunden der Kommissionierbeleg f}r einen neu
           #     erfassten Auftrag gedruckt wird und die Funktion 'Pr}fung
           #     R}ckst{nde beim Kommissionierbelegdruck' (PARPKB in APX00E04
           #     aktiviert ist
           # *BLANK      keine Lieferscheinausl¦sung
           # L           R}ckstandsposition mit Zuteilung
           # M           Manuell freigegebene, zugeteilte
           #             R}ckstandposition
           # N           neue Position, zugeteilt bei Erfassung
           #             bzw. bei Erreichen des entsprechenden Termins
           # 'APKZLA': 'Lieferschein-Auslösung 2',
           # 'APLFKZ': 'Kz. Lfsch drucken',
           # *ZERO       Lieferschein nicht drucken
           # 1           LfSn wird gedruckt, wenn LfTrm erreicht
           #             Lieferschein wird gedruckt, wenn der Liefertermin erre
           #             ist. Liegt der Liefertermin innerhalb des Lieferschein
           #             raumes, so wird das Kennzeichen automatisch gesetzt. L
           #             der Liefertermin au~erhalb dieses Zeitraums, so ist ei
           #             Liefermengeneingabe nur zul{ssig, wenn dies lt. Parame
           #             sierung (Steuerung Auftragsbearbeitung: Kz Zuteilung)
           #             m¦glich ist.
           # 2           LfSn obwohl LfTrm nicht erreicht
           #             Der Lieferschein soll geschrieben werden, obwohl der L
           #             termin noch nicht erreicht ist (manueller LfSnAbruf)
           # 3           LfSn drucken weil Zugang
           #              Der Lieferschein muss geschrieben werden, weil die
           #             Bestellung, in der die Auftragspositionsmenge reservie
           #             wurde, am Lager zugegangen ist.
           # 'APKZZL': 'zuteilungskennzeichen',
           # Herkunft der Zuteilung
           # PTYP Tabelle
           # *ZERO       Zuteilung aus Auftragserfassung
           # 1           Zuteilung per Automatik
           # 2           Zuteilung per Dialog
           # 3           Zuteilung per Automatik + Lager{nderung
           #             Nur m¦glich, wenn die Funktion 'Alternativlager' aus
           #             Steuerung Auftrag 1 (PABGA2 <> ' ') eingesetzt ist.
           # 'APKZFG': 'KB/LS-Feigabe',
           # 'APKZRE': 'Lieferschein -Auslösung',
           # 'APKZZU': 'Kz.Bevorzugte Zuteilung',
           # 'APKZLF': 'Kz:Im LfSchein andrucken',
           # 'APKZVL': 'Vorab-Lieferschein',
           # 'APKZRD': 'Kz:Pos in Rech.nicht andr.=',
           # 'APKZSF': 'Kz: In Sofort-Form andrucke',
           # 'APKZSV': 'Zusätzliches Sofortformular',
           # 'APKZSA': 'Kz: keine AE-Statistik = 1',
           # 'APKZSM': 'Kz:keine Mengen-Statistik=1',
           # 'APKZSU': 'Kz:keine Umsatzstatistik=1',
           # 'APKZBA': 'Kz: kein Auftragsbestand=1',
           # 'APKZRK': 'Kz: Rückstand erlaubt = 1',
           # Festlegung, wie die Position im Fall einer Teillieferung
           # behandelt werden soll.
           # Bei R}ckstandsverwaltung = Nein wird bei der
           # Lieferscheinr}ckmeldung die Angabe 'voll ausgeliefert' mit 'Ja'
           # vorbesetzt. Bei der Auftragserfassung wird das Kennzeichen in
           # der Position mit der Angabe lt. Auftragskopf vorbesetzt. Bei
           # Auslaufartikeln wird der Wert generell mit 'Nein' vorbesetzt.
           'APKZVA': 'voll_ausgeliefert',
           # Die Festlegung 'Position voll ausgeliefert' erfolgt bei der
           # Lieferscheinr}ckmeldung. Eine Position gilt auch dann als voll
           # ausgeliefert, wenn die fakturierte Menge gr¦~er oder gleich der
           # bestellten Menge ist  Das Kennzeichen 'Voll ausgeliefert' wird
           # nur aufgrund manueller Eingaben gesetzt.
           # PTYP Tabelle
           # *ZERO       Position ist noch offen
           # 1           Position voll ausgeliefert
           # 'APKZAF': 'Kz:Abruf aus Rahmenauftrag',
           # 'APKZRA': 'Kz:Rahmen-Pos',
           # Wertm{~ige Rahmenpositionen:
           # Rahmenpositionen mit der Bestellmenge 1 werden als wertm{~ige
           # Rahmen gef}hrt. Bei Abrufen werden der offene und fakturierte
           # Positionswert ganz normal vermindert. Die fakturierte Menge wird
           # aber erst dann erh¦ht (0 -> 1), wenn der offene Positionswert 0
           # ist  Erst dadurch wird die Position als erledigt gekennzeichnet.
           # PTYP Tabelle
           # *ZERO       kein Rahmen
           # 1           Rahmen
           # 'APKZPA': 'Kz:Preisänderung = 1',
           # Wird gesetzt bei nachtr{glichen Preis{nderungen
           # PTYP Tabelle
           # *ZERO       Preis seit Erfassung nicht ge{ndert
           # 1           Preis wurde ge{ndert
           # 9           Preis aus Bonus
           # 'APKZTA': 'Kz:Terminänderung = 1',
           # Festlegung, ob Termin ge{ndert wurde.
           # Wird bei nachtr{glichen Liefertermin{nderungen gesetzt.
           # PTYP Tabelle
           # *ZERO       Termin seit Erfassung nicht ge{ndert
           # 1           Termin wurde ge{ndert
           # 'APKZMA': 'Kz:Mengenänderung = 1',
           # Wird gesetzt bei nachtr{glichen Mengen{nderungen
           # PTYP Tabelle
           # *ZERO       Menge seit Erfassung nicht ge{ndert
           # 1           Menge wurde ge{ndert
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
           'APFA4A': 'teilzuteilungsverbot',
           # Information }ber die Teilzuteilung
           # PTYP Tabelle
           # *BLANK      Teilzuteilung erlaubt
           # *ZERO       Teilzuteilung erlaubt
           # 1           Teilzuteilung verboten
           #             Bei Zuteilungen aus externer Beschaffung (Einkauf bzw.
           #             Fertigung) wird dieses Kennzeichen nicht ber}cksichtig
           #             d.h. die Zuteilung erfolgt generell, auch bei Teilmeng
           # 2           Teilzuteilung folgt
           #             Es folgen noch weitere Teilzuteilungen aus der externe
           #             Beschaffung.
           # 3           Zuteilung abgeschlossen
           #             Die letzte Teilzuteilung aus der externen Beschaffung
           #             erfolgt.
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
           # Wird ein Set bei der Erfassung aufgel¦st, d.h. je Komponente
           # eine eigene Position angelegt, so sollen f}r den Druck diese
           # Positionen als zusammengeh¦riger Block erkennbar sein.
           # Beim Setartikel stimmen Position und bezogene Position }berein.
           # PTYP nicht aktiv
           # 'APSNPF': 'StzNr Preisfindung',
           # 'APMG01': 'Menge 1',
           # 'APWR01': 'Preis 1',
           # 'APPR01': '%-Satz 1', Marge in Prozent
           #                   (Verkaufspreis - Einstandspreis) x 100
           # Berechnung:       --------------------------------------
           #                                 Verkaufspreis
           # Bei der Marge f}r einen kompletten Vorgang sind f}r die Angaben
           # Verkaufspreis und Einstandspreis die entsprechenden Summen }ber
           # alle Positionen einzusetzen.
           'APDTZU': 'AAP_zuteilung_date',
           'APZTZU': 'AAP_zuteilung_time',
           # 'APJNZU': 'Jobnr Zuteilung',
           'APDTER': 'AAP_erfassung_date',
           'APZTER': 'AAP_erfassung_time',
           'APDTAE': 'AAP_aenderung_date',
           'APZTAE': 'AAP_aenderung_time',
           # 'APSBAE': 'Sachb.letzte Änderung',
           # 'APDFSL': 'Dateiführungs-Schlüssel',
           'APSTAT': 'AAP_status',
          },

'AAT00': {  # Auftrags-Texte
          'ATAUFN': 'auftragsnr',
          'ATAUPO': 'auftragsposition',
          # 1           Benutzer aus B2B Shop
          # 2           Abweichende Artikelbezeichnung
          #             Pos: ATTX60 Stelle  1-60:  abweichende Artikelbezeichn
          #             Kopf: individuell verwendet
          # 4           Zusatztexte
          #             (keine Anzeige im AAU05 / kein Druck auf Formularen)
          #             (Eingabe z.B. im AIS31FE: Umsatzauskunft-Detailanzeige
          #             ? wird anscheinend nirgends verwendet/unterst}tzt ?
          # 5           Statistische Warennummer
          #             ATTX60 Stelle 45-60:  Statistische Warennummer bei SoA
          #             (vor Release 6.2 war die Statistische WaNr unter Texta
          #             Feld ATTX60, Stelle 45-60 gespeichert)
          # 6           Texte f}r ¢nderungs-AB aus Einkauf
          # 7           Faxnummer (Position = 0)
          # 7           Auftragstexte vor Position
          # 8           Auftragsanfangstexte (Position = 0)
          # 8           Auftragstexte nach Position
          # 9           Auftragsendetexte
          'ATTART': 'textart',
          'ATLFNR': 'nr',
          'ATTX60': 'text',
          # 0 nicht in AB drucken
          # 1 in AB drucken
          # 2 undokumentiert
          'ATKZAB': 'andruck_ab',
          # 0 nicht auf KB/LS drucken
          # 1 auf KB/LS drucken
          # 2 nur auf KB drucken
          # 3 nur auf LS drucken
          'ATKZLF': 'andruck_ls',
          # 0 nicht auf RG drucken
          # 1 auf RG drucken
          'ATKZRG': 'andruck_re',
         },

'AKZ00': {  # Kundenstamm für Auftragsverwaltung
          'KZKDNR': 'Kunden-nr',
          #'KZSBNR': 'zuständiger Sachbearbeiter',
          'KZVRT': 'vertreter',
          'KZGEB': 'gebiet',
          'KZBRAN': 'branche',
          'KZDSTK': 'distrikt',
          'KZPREL': 'preisliste',
          #'KZKZVB':' Kz. 'Kunde ist Verbraucher','
          #'KZKZZU': 'Bevorzugte Zuteilung',
          #'KZKZRK': 'Rückstand möglich',
          'KZX3LB': 'lieferbedingung',
          'KZX3VP': 'verpackungsvorschrift',
          'KZX3VS': 'versandart',
          #'KZVANR': 'Versand-Adress Nr.',
          #'KZEXPR': 'Expreßgut-Station',
          'KZLGNR': 'auslieferunglager',
          # Der zahlungsbedingung_schluessel ist ein Verweis auf XKP00.KPINHA
          # Achtung! Der Wert kann nicht direkt z.B. in einer Subquery oder einem JOIN verwendet werden,
          # sondern muss mit Leerzeichen aufgefüllt werden: husoftm2.tools.pad('KPINHA', value)
          'KZX3ZB': 'zahlungsbedingung_schluessel',
          'KZRBPR': 'kundenrabatt',
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
          'KZINFO': 'betreuer',
          'KZDTAE': 'updated_at',
},

'ALK00': {  # Lieferscheinköpfe
          'LKSANK': 'satznr',
          'LKSANB': 'bezogener_kopf',
          # Die Angabe ist nur gefüllt, falls mit der Variante 'Kommissionierbeleg vor Lieferschein'
          # gearbeitet wird. Für jeden Lieferschein wird zusätzlich zum Kommissionierbeleg-Kopfsatz
          # ein eigener Kopfsatz angelegt, die ALN00-Sätze für den Lieferschein werden aber nicht
          # dupliziert.
          #'LKSBNR': 'sachbearbeiter', # Wird bei der Stapelfreigabe mit 999999  ̧berschrieben.
          'LKLGNR': 'lager',
          # 'LKX3VA': 'versandart',
          #'LKKZTF': 'teilfakturiert',
          'LKKDRG': 'rechnungsempfaenger',
          'LKKDNR': 'warenempfaenger',
          'LKLFSN': 'lieferscheinnr',
          # Wird für automatisch erstellte Lieferscheine bei der Lieferscheinschreibung (AFA37) mit dem
          # Tagesdatum, und für manuell erstellte Lieferscheine bei der Erfassung Versand-/Speditionsdaten
          # (AVS10) mit Datum 0 gefüllt.
          # Ist bei uns in der Praxis gerne mal 0
          'LKDTLF': 'ALK_lieferschein_date',
          'LKZTLF': 'ALK_lieferschein_time',
          #'LKDTST': 'lieferschein_storno_date',
          # Das Datum des letzten Belegstornos wird erst eingetragen, wenn alle Positionen des Belegs
          # storniert worden sind. Es kann nur der Beleg storniert werden, der in der Belegfreigabe
          # auch freigegeben werden kann. Je nach Parameterisierung ist das der Kommissionierbeleg oder
          # der Lieferschein.
          #'LKKZSL': 'sammellieferschein',
          #'LKKZLF': 'druckkennzeichen',
          'LKKBNR': 'kommibelegnr',
          'LKDTKB': 'kommibeleg_date',
          'LKZTKB': 'kommibeleg_time',  # ist bei uns fast immer 999999
          # 999999: Rechnungsfreigabe im Stapel erfolgt 888888: Belegfreigabe im Stapel erfolgt
          #'LKLFDN': 'laufendenr',
          # Die lfd. Nummer wird pro Kunde vergeben. Sie wird die Kommissionierbelegdaten für
          # unterschiedliche Lieferadressen des gleichen Kunden zu trennen. Die Vergabe der lfd. Nummer
          # erfolgt bei der Vorbereitung des Kommissionierbelegs bzw. Lieferscheins (AFA35). Beim Wechsel
          # der Auftragsnummer wird geprüft, ob in den Dateien ALD/ALK bereits Daten existieren, die in
          # Warenempfänger und Lieferadresse mit den Angaben des zu bearbeitenden Auftrags
          # übereinstimmen. Wird keine Übereinstimmung gefunden, dann wird eine neue lfd. Nummer vergeben.
          #'LKBELN': 'belegnr_freigabe',
          # Lieferschein vor Fakturierfreigabe: Lieferscheinnummer
          # Lieferschein nach Fakturierfreigabe: Kommissionierbelegnummer
          #'LKFGNR': 'Freigabenr AFA20
          #'LKFGSB': 'Sachbearb. Freigabe
          'LKDTFG': 'freigabe_date',
          'LKZTFG': 'freigabe_time',
          #'LKFGI1': 'freigabe_info1',
          #'LKFGI2': 'freigabe_info2',
          #'LKFGLC': 'Anzahl Liefsch.'
          'LKAUFS': 'auftragsnr',
          # Nummer des Auftrags In diesem Feld steht die zu dem Lieferschein gehörende Auftragsnummer.
          # Bei Sammellieferscheinen wird die Auftragsnummer der ersten Lieferscheinposition gespeichert.
          'LKDTLT': 'anliefer_date',
          # Achtung: wenn die Folgende zeile auskommentiert wird, muss das EDIhub Admin Interface angepasst
          # werden.
          #'LKKZ02': 'hrl_status',
          # 'LKKZVA': 'alle_positionen_fakturiert',
          # *ZERO noch nicht alle Pos fakturiert
          # 1     alle Positionen sind fakturiert
          'LKDTER': 'ALK_erfassung_date',
          'LKZTER': 'ALK_erfassung_time',
          'LKDTAE': 'ALK_aenderung_date',
          'LKZTAE': 'ALK_aenderung_time',   # scheinbar immer null
          'LKDFSL': 'ALK_dfsl',
          # 'LKSTAT': 'satzstatus',
         },


'ALN00': {  # Positionsdatei für KB/Lieferschein
          # Die Lieferscheinpositionsdatei enthält die Detailinformationen zu den einzelnen Lieferungen
          # einer Auftragsposition. Für den Mengen- und Wertabgleich zwischen Auftrag und
          # Lagerbestandsführung werden folgende Sätze NICHT berücksichtigt:
          #     * Rechnungsvorbereitung ist gelaufen (LNRGST = 3)
          #     * Lieferscheinposition ohne Bestandsf ̧hrung (LNKZBE = 1)
          #
          # Für den Mengen- und Wertabgleich zwischen Auftrag und Lagerbestandsführung werden die Werte
          # wie folgt ermittelt:
          #     * Zugeteilte Menge: Lieferscheinmenge (LNMNGL), falls Position fakturierbar ist (LNKZFF=1)
          #     * Offener Lieferwert: Lieferscheinwert (LNLWA1), falls Position fakturierbar ist (LNKZFF=1)
          #     * Vorabgebuchte Menge: Zu fakturierende Menge (LNMNGR), falls Position für Fakturierung
          #       ausgewählt ist (LNRGST = 2)
          #     * Auftragsmenge: Differenz aus Lieferscheinmenge (LNMNGL) und zu fakturierender Menge
          #       (LNMNGR), falls Position für Fakturierung ausgew‰hlt ist (LNKZRV = 2)
          #
          # Die Ermittlung dieser Werte pro Auftragspositon erledigt das Programm AFA06.
          'LNSANK': 'satznr_kopf',  # Satznummer des Kommissionierbelegs bzw. Lieferscheins in ALK00.
          # PAKZKB = 0: Satznummer des Lieferscheins
          # PAKZKB = 1: Satznummer des Kommissionierbelegs und des Lieferscheins (ein gemeinsamer Kopfsatz)
          # PAKZKB = 2: Satznummer des Kommissionierbelegs
          'LNSANP': 'satznr',
          # 'LNKANR': 'Übergeordnete Auftrags-Nr.
          # Soll für einen Auftrag ein Bezug zu einem bereits bestehenden Hauptauftrag hergestellt werden,
          # so ist hier die Nummer dieses Hauptauftrags einzugeben. Intern wird eine neue Auftragsnummer
          # vergeben. Die Kopfdaten des neuen Auftrags werden mit denen des Hauptauftrags vorbesetzt.
          # Die Angabe 'Hauptauftrag' ist nur dann gefüllt, wenn bei der Auftragserfassung eine
          # entsprechende manuelle Eingabe erfolgte.
          'LNAUFN': 'auftragsnr',
          'LNAUPO': 'auftrags_position',
          'LNARTN': 'artnr',
          'LNKZKO': 'setartikel',  # *ZERO Normalartikel, 1 Setartikel
          # Die folgenden zwei Felder sind nie befüllt
          #'LNBSTN': 'bestellnr',
          #'LNBSTP': 'bestellposition',
          # 'LNVKE':  'verkaufseinheit', # ist immer leer
          'LNLGNR': 'lager',
          #'LNLGPL': 'lagerplatz', # ist immer leer
          #'LNKZBE': 'Kz:keine Bestandsführung =1
          'LNMNGO': 'menge_offen',
          'LNDTLT': 'ALN_anliefer_date',
          # Bei der Stapelfreigabe werden nur Lieferscheine freigegeben, deren Liefertermin nicht nach
          # dem Freigabedatum liegt.
          #'LNJWLT': 'liefertermin_woche',  # Liefertermin im Format HJJWW (Jahrhundert/Jahr/Woche)
          'LNKZRK': 'rueckstand_erlaubt',
          # Festlegung, wie die Position im Fall einer Teillieferung behandelt werden soll. Bei
          # Rückstandsverwaltung = Nein wird bei der Lieferscheinrückmeldung die Angabe 'voll ausgeliefert'
          # mit 'Ja' vorbesetzt. Bei der Auftragserfassung wird das Kennzeichen in der Position mit der
          # Angabe lt. Auftragskopf vorbesetzt. Bei Auslaufartikeln wird der Wert generell mit 'Nein'
          # vorbesetzt.
          #'LNKZZL': 'zuteilungskennzeichen',
          # *ZERO, Zuteilung aus Auftragserfaassung 1, per Automatik, 2 per Dialog
          'LNKZZU': 'bevorzugte_zuteilung',
          'LNKDRG': 'rechnungsempfaenger',
          'LNKDNR': 'kundennr',
          'LNLFSN': 'ALN_lieferscheinnr',
          'LNMNGL': 'menge',  # Menge lt. Lieferschein
          # Die folgenden zwei Felder sind nicht immer befüllt
          'LNDTLF': 'ALN_lieferschein_date',
          'LNZTLF': 'ALN_lieferschein_time',
          #'LNKZVL': 'KZ. Vorab-Lieferschein
          #'LNKZLD': 'Kz: Im LF. andrucken
          #'LNKZL2': 'Kz: Im LF. andrucken
          #'LNKZLF': 'Kz: LF-Schein drucken
          #'LNKZFA': 'Kz:Pos.für Fakt.ausgewählt=
          #'LNKZFF': 'Kz: Pos ist fakturierbar =
          'LNDTST': 'storno_date',
          #'LNKZRE': 'lieferschein_ausloesung',
          'LNKBNR': 'kommibelegnr',
          'LNMNGK': 'menge_komissionierbeleg',
          'LNDTKB': 'komissionierbeleg_date',
          'LNZTKB': 'komissionierbeleg_time',
          # Kommissionierbeleg drucken
          # *ZERO Kommissionierbeleg nicht drucken
          # 1 Kommissionierbeleg vorbereitet
          # 2 Kommissionierbeleg gedruckt
          'LNKZKB': 'komissionierbeleg_drucken',
          # *ZERO Rechnung noch nicht bearbeitet
          # 1 Lieferung freigegeben
          # 2 Lieferschein gedruckt
          # 3 Rechnung gedruckt
          'LNRGST': 'rechnungsstatus',
          # Ist wohl das tatsächliche Versand-/Rückmelde-Datum bei Kommibelegen, entspricht
          # daher dem Leiferschein-Datum.
          'LNDTVS': 'versand_date',
          'LNSTOR': 'gutschrift',  # Rechnung/Gutschrift
          # *ZERO Lieferscheinkopf für Normalauftrag
          # 1     Lieferscheinkopf für Gutschriftsauftrag
          'LNBELP': 'kommibeleg_position',
          #'LNFGNR': 'Freigabenr AFA20
          #'LNFNFA': 'Fehlernr FG. AFA20
          'LNLSTO': 'lieferscheinstorno',
          'LNMNGF': 'menge_fakturierung',
          #  Bei LNKZFA = 0: fakturierte Menge,
          # Bei LNKZFA <> 0: zu fakturierende Menge
          'LNKZV2': 'voll_ausgeliefert',
          #'LNPSTA': 'Packstatus
          #'LNKZVS': 'Kz versandfertig
          #'LNPROG': 'Herkunftsprogramm
          #'LNKZUB': 'Übernahmekennzeichen
          #'LNKINF': 'Kennzeicheninfo
          #'LNAUPS': 'bezogene Position
          #'LNKZ03': 'hrl_status',
          #'LNRSN1': 'Druck-Kz für KB
          'LNDTER': 'ALN_erfassung_date',
          'LNZTER': 'ALN_erfassung_time',
          'LNDTAE': 'ALN_aenderung_date',
          'LNZTAE': 'ALN_aenderung_time',
          # Dieses Feld wird von dem Programm, das eine Änderung an Daten dieses Satzes vornimmt,
          # während des Änderungsvorgangs mit der Bildschirmidentifikation des ‰ndernden Bildschirms belegt:
          'LNDFSL': 'ALN_dfsl',
          'LNSTAT': 'satzstatus',
          # *BLANK Satz ist aktiv
          # X Satz steht zum Lôschen an (auf Satz kann in den Anwendungsprogr. nicht mehr zugegriffen werden)
          'LNSBNR': 'sachbearbeiter_bearbeitung',
          'LNLWA2': 'wert',
         },

'AFK00':  # Rechnungsköpfe
     {
     'FKFNR ': 'firma',
     # 'FKABT ': 'Abteilungs-Nummer',
     # 'FKAGRP': 'Abteilungs-Gruppe/Sparte',
     # 'FKFGRP': 'Firmen-Gruppe',
     'FKSBNR': 'sachbearbeiternr',
     'FKAUFN': 'auftragsnr',
     #'FKKANR': 'uebergeordneter Auftrag',
     # 'FKRANR': 'Rahmenvereinbarungs-Nr.',
     'FKAUFA': 'auftragsart',
     # 'FKSRKO': 'KZ SAMMELRECHNUNGS-KOPF-SAT',
     'FKRGNR': 'rechnungsnr',
     'FKRGNI': 'IntRg',
     # 'FKRGLI': 'Rechnungslisten-Nr.',
     #'FKBZRG': 'Bezogene Rg',
     'FKFORM': 'rechnungsart',
     'FKSTOR': 'storniert',
     # 'FKRART': 'Rechnungsart/Rg-Freigabe',
      'FKKZWE': 'Kz. wertm{~iger Vorgang',
     # 'FKDTRI': 'Rechnungsdatum/intern',
     # 'FKMJBU': 'Buchungsmonat CJJMM',
     # 'FKFNRM': 'Mandanten-Nummer',
     'FKKDNR': 'kundennr_warenempfaenger',
     'FKKDRG': 'kundennr_rechnungsempfaenger',
     'FKKDN3': 'Kunden-Nr/Verbraucher',
     'FKVERB': 'verbandsnr',
     # 'FKSPSL': 'Sprache',
     # 'FKBRAN': 'Branche',
     # 'FKGEB ': 'Gebiet',
     # 'FKDSTK': 'Distrikt',
     # 'FKPLZ ': 'Postleitzahl',
     # 'FKLKZ ': 'L{nderkennzeichen',
     'FKNRKD': 'kundenauftragsnr',
     # 'FKDTKD': 'Kunden-Auftrags-Datum',
     # 'FKALS1': 'Alphasortierung/Warenempf{n',
     # 'FKALS2': 'Kennzeichen ZFB',
     'FKVRT ': 'vertreter',
     # 'FKVRT2': 'Vertreter 2',
     # 'FKPRZA': 'Aufteilungs-%-Satz',
     # 'FKLGN2': 'Zugangslager',
     'FKVANR': 'Versandadress-Nr.',
     # 'FKPROB': 'Objekt/Projekt/Aktion',
     'FKRBP1': 'auftragsrabatt1p',
     'FKRBP2': 'auftragsrabatt2p',
     'FKX3R1': 'rabatttext1',
     'FKX3R2': 'rabatttext2',
     #'FKRBB1': 'auftragsrabatt1',
     #'FKRBB2': 'auftragsrabatt2',
     'FKGERB': 'auftragsrabatt',
     # 'FKKZRX': 'Kz.Rabatt Brutto',
     # 'FKTRNR': 'Touren-Nummer',
     # 'FKKZTF': 'Kennzeichen Teilfakt/Lief.',
     # 'FKKZZF': 'un-/versteuert',
     # 'FKKZRK': 'Kz: R}ckstand m¦glich',
     # 'FKKZNN': 'Kz.Nachnahme',
     # 'FKKZAR': 'Kz.Leihart',
     # 'FKKZSR': 'Sammelrechnungs-Turnus',
     #'FKKZSD': 'Auswahl: Druck in Sammel-Rg',
     'FKBRUT': 'zu_zahlen',
     'FKSBRT': 'skontierfaehiger_betrag',
     'FKSKTA': 'skontobetrag',
     'FKNETT': 'warenwert',
     'FKMWBT': 'rechnung_steueranteil',
     # 'FKNETR': 'Positionswert f}r Auftr.-Ra',
     # 'FKNBK ': 'Nebenkosten',
     # 'FKNBT1': 'Zus. Nebenkosten 1',
     # 'FKNBT2': 'Zus. Nebenkosten 2',
     # 'FKVPK ': 'Verpackungskosten',
     'FKVSK': 'versandkosten',
     'FKWSL ': 'waehrung',
     # 'FKKURS': 'Wechsel-Kurs',
     # 'FKKUFA': 'Kursfaktor',
     # 'FKMWSA': 'Umsatz-Steuerprofil',
     # 'FKEGCD': 'L{ndercode / EG',
     'FKUSTN': 'ustdid',
     # 'FKSTSL': 'Umsatz-Steuerprofil',
     'FKDTVF': 'valuta_date',
     'FKX3ZF': 'zahlungsbedingung',
     # 'FKX3VA': 'Versandart',
     # 'FKX4NB': 'Textschl. Nebenkosten/Erf.',
     # 'FKNTY1': 'Typ/Zus. Nebenkosten 1',
     # 'FKNTY2': 'Typ/Zus. Nebenkosten 2',
     # 'FKX4N1': 'Textschl. zus. Nebenkosten',
     # 'FKX4N2': 'Textschl. zus. Nebenkosten',
     # 'FKX3LB': 'Lieferbedingung',
     #'FKFKZR': 'KZ Rechnung drucken',
     # 'FKRGCO': 'Anzahl Rechnungen',
     'FKDTVS': 'versand_date',
     # 'FKKZRE': 'Retouren-Beleg ja/nein',
     #'FKKZUF': 'Auswahldaten -->  AAK00',
     # 'FKKZUS': 'Update Statistikdaten',
     #'FKSANK': 'Satznummer Kopf',
     #'FKPROG': 'Herkunftsprogramm',
     #'FKSNLK': 'Satznummer ALK00',
     #'FKFBEL': 'Beleg-Nummer f}r Freigabe',
     #'FKFAUF': 'Auftrags-Nr. f}r Freigabe',
     'FKDTFA': 'druck_date',
     # 'FK30ST': 'Kz Fakt.Update Statistik',
     # 'FK30FI': 'Kz Fakt.Update FIBU',
     #'FKKZBB': 'verbuchungfibu',
     #'FKKZRV': 'Kz Rechnungsvorb. erforderl',
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
     'FKKZRL': 'Andruck in Rg-Liste',
     # 'FKKZPR': 'Abgleich  FIBU/Statistik',
     'FKKZSE': 'Ausg.SEDAS-Schnittstelle',
     # 'FKOQZO': 'Outqueue-Zuordnung',
     # 'FKZUOR': 'Zuordnung f}r dezentrale An',
     # 'FKKZ01': 'Packstatus Beleg',
     'FKKZ05': 'Sort SmlRg',
     # 'FKSORT': 'Sortierfeld',
     'FKSNRG': 'StzNr SmlRg',
     # 'FKDFSL': 'Dateif}hrungs-Schluessel',
     'FKSTAT': 'status',
     'FKDTER': 'erfassung_date',
     'FKZTER': 'erfassung_time',
     },

'AFU00':  # Rechnungspositionen
        {
        #'FUFNR': 'firma',
        #'FURGNR': 'rechnungsnr',
        # 'FURGNI': 'Intern Rechnungs-Nr.',
        #'FUAUFN': 'auftragsnr',
        'FUAUPO': 'auftragsposition',
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
        'FUPNET': 'warenwert',
        'FUPBUT': 'wert_brutto',
        'FUPORB': 'abschlag',
        #'FUPREW': 'Wert/Ek',
        'FUPREV': 'rechungsbetrag',
        # 'FUPEBN': 'Ebene Preisfindung',
        # 'FUKZPR': 'Preiskennzeichen Verkauf',
        # 'FUMEPR': 'Preis-Einheit',
        # 'FUFAPR': 'Faktor APMEH --> APMEPR',
        #'FUPREE': 'Einstands-Preis',
        # 'FUWSLE': 'W{hrungsschl}ssel: EK',
        # 'FUPRDL': 'Preiskennzeichen Lager',
        # 'FUKUFA': 'Kursfaktor/EK',
        # 'FUKURS': 'Wechselkurs EK',
        # 'FUMWAR': 'Steuerart / Artikel',
        # 'FUDEZI': 'Anzahl Dezimalstellen',
        # 'FUKZBE': 'Ohne Bestandsf}hrung',
        'FUKZST': 'kennzeichen_streckengeschaeft',
        'FUKZKO': 'komponentenaufloesung',
        # Im Parametersatz: Steuerung Auftragsbearbeitung kann festgelegt
        # werden, ob ein Set bestandsgef}hrt sein kann oder nicht. Ist ein
        # Set bestandsgef}hrt, so kann auf Positionsebene entschieden
        # werden, ob das Set in Komponenten aufgel¦st wird.
        # *ZERO       Set nicht in Komponenten aufl¦sen
        #             d.h. Set wird wie normaler Artikel behandelt.
        # 1           Set in Komponenten aufl¦sen
        #             Bestandsf}hrung auf Komponentenebene (Standard)
        'FUKZRB': 'Kz: kein Auftragsrabatt = 1',
        'FUKZRD': 'Kz:Pos in Rech.nicht andr.=',
        'FUKZRU': 'Kz.Rabatt versteckt',
        'FUKZRX': 'Kz.Rabatte Brutto',
        # 'FUKZSE': 'Serien-/Chargenartikel',
        # 'FUKZSO': 'Kennzeichen Sonderartikel',
        # 'FUKZPS': 'Positionsbezogener Set',
        # Bei Auftragspositionen mit Setartikeln ist hier hinterlegt, ob
        # f}r die Position eine spezielle Setkomponentendefinition ein-
        # gegeben wurde.
        # PTYP Tabelle
        # *ZERO       kein positionsbezogener Set
        # 1           positionsbezogener Set
        # 'FUPRKZ': 'Preiskennzeichen Lager',
        # 'FUKZEP': 'KZ: Eigen-Produkt',
        'FUKZZU': 'Zuschlagssatz',
        # 'FUGANZ': 'Bestellte Gebinde',
        # 'FUVOLM': 'Volumen/Inhalt',
        # 'FUGEWI': 'Gewicht',
        'FULFSN': 'lieferscheinnr',
        # 'FUDTLF': 'Datum/LfS-Druck',
        # 'FUDTFA': 'Datum/Rechnungsdruck',
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
        'FUDTRI': 'rechnungs_date',
        #'FUDTER': 'erfassung_date',
        #'FUZTER': 'erfassung_time',
        },

'AKA00': {  # Kunden-Artikel
            # 'KAFNR': 'firma',
            'KAKDNR': 'kundennr',
            # 'KAKGRP': 'kundengruppe',
            'KAARTN': 'artnr',
            'KAKART': 'kundenartnr',
            'KABEZE': 'bezeichnung',
            # 'KAKZLI': 'listungskennzeichen',
            # 'KAKZSP': 'sperren',
            # 'KAGATG': 'Garantietage',
            # 'KAPREV': 'empfohlener Verkaufspreis',
            # 'KAPRE2': 'empfohlener Verkaufspreis neu',
            'KADTPR': 'gueltig_ab_date',
            # 'KAKZEU': 'Euro-Kennzeichen',
            # 'KADTPC': 'Datum der letzten PRICAT-Übertragung',
            # 'KAZTPC': 'Uhrzeit der letzten PRICAT-Übertragung',
            # 'KAINFO': 'infofeld',
            # 'KADFSL': 'Dateifuehrungsschlüssel',
            'KASTAT': 'AKA00_status',
            # 'KAAGRP': 'Abteilungsgruppe / Werk',
            # 'KAABT': 'Abteilung',
            # 'KAFGRP': 'Firmengruppe',
            # 'KAARTG': 'Artikelgruppe',
         },

'ASK00': {  # Set/Komponenten
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
         'VASANR': 'satznr',  # gepackt
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

'BBU00': {  # Buchungspositionen in der Buchhaltung
    # 'BUFNR':  'Firmennr. Sachbuchung',
    # 'BUBHKZ': 'Buchhaltungskennz. D,K,S',
    'BUPKTO': 'personenkonto',
    'BUKTO': 'konto_sachbuchhaltung',
    'BUBELN': 'belegnr',
    #'BUBELK': 'Beleg-Nummer Kreis',
    #'BUABKR': 'Abstimmkreis',
    'BUDTBL': 'beleg_date',
    #'BUMMBU': 'MM Bumo',
    'BUJJBU': 'buchungsjahr',
    #'BUJHBU': 'JH Bumo',
    #'BUGJ':   'Gesch{ftsjahr',
    'BUBLRT': 'belegart',
    #'BUBUSL': 'externe Belegart',
    #'BUBLRA': 'Anzeigebelegart',
    #'BUBLRE': 'externe Belegart',
    'BUSOHA': 'soll_oder_haben',  # 'S' oder 'H'
    'BUGKTO': 'gegenkonto_sachbuchhaltung',
    # 'BUGKSH': 'Gegenkonto-Soll/Haben',
    'BUOINF': 'op_info',
    'BUBTXT': 'buchungstext',
    # 'BUOPSN': 'OP-Nr. f}r OP-f}hrendes Sac',
    'BUSTOR': 'S = Stornobuchung',
    #'BUSTRT': 'Steuer-Art',
    #'BUEGCD': 'L{ndercode / EG',
    #'BUSTSL': 'Steuer-Schl}ssel',
    'BUSTKT': 'steuerkonto_sachbuchhaltung',
    #'BUSTKZ': 'Steuerkennzeichen',
    #'BUSTBT': 'Steuer-Betrag',
    #'BUSTWB': 'Steuer in W{hrung',
    #'BUSTSH': 'Soll/Haben Steuer',
    # 'BUBUBT': 'buchungsbetrag Kto/Pers.-kt',
    'BUNEBT': 'buchungsbetrag_gegenkonto',
    'BUNEWB': 'buchungsbetrag',
    'BURGNR': 'rechnungsnr',
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
    'BUUSR1': 'user_1',  # name des Sachbaerbeiters
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


'BED00': {  # Datei fuer die Zentrale-Meldung innerg. Warenverk.
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

'BOP00': {  # Offene Posten
          # 'OPFNR': 'Firmennummer',
          # 'OPBHKZ': 'Buchhaltungskennz. D,K,S',
          # 'OPBLRT': 'Belegart',
          # 'OPBLRA': 'Anzeigebelegart',
          # 'OPBUSL': 'externe Belegart',
          # 'OPRGNR': 'Rechnungs-Nummer',
          # 'OPRPOS': 'Rechnungsposition',
          # 'OPRGLS': 'Rechnungslistennr.',
          'OPPKTO': 'Personenkonto KD/LIEF',
          # 'OPVKTO': 'Verbandskto. nr.',
          # 'OPMKTO': 'Mischkonto',
          # 'OPKTO': 'Konto-Sachbuchhaltung',
          # 'OPWSL': 'W{hrungs-Kennzeichen',
          # 'OPAUSB': 'Auszahlungsbetrag',
          # 'OPDTEB': 'F{lligkeit Einbehalt',
          # 'OPSKBT': 'Skontobetrag',
          # 'OPSKKZ': 'Skontokennzeichen',
          # 'OPSKSL': 'Skonto-Schl}ssel',
          # 'OPDKSL': 'Skt.Schl.Delcedere',
          # 'OPZAHL': 'Zahlangabe',
          # 'OPBKLF': 'Laufnr. vers. Banken/Konten',
          # 'OPMAHN': 'Mahnabgabe',
          # 'OPMSTU': 'Mahnstufe',
          # 'OPUKKZ': 'Ungepr. Kreditorenrg.',
          # 'OPZPNR': 'Zahlungsplannummer',
          # 'OPZPOS': 'Zahlungsplanpos.',
          # 'OPDTVA': 'Valutadatum',
          # 'OPDTFL': 'F{lligkeitsdatum',
          # 'OPDTLM': 'Datum letzte Mahnung',
          # 'OPDTBL': 'Belegdatum',
          # 'OPMMBU': 'MM Bumo',
          # 'OPJJBU': 'JJ Bumo',
          # 'OPJHBU': 'JH Bumo',
          # 'OPGJBU': 'Gesch{ftsjahr',
          # 'OPMMAU': 'TT Ausgleich',
          # 'OPJJAU': 'JJ Ausgleich',
          # 'OPJHAU': 'JH Ausgleich',
          # 'OPGJAU': 'Gesch{ftsjahr',
          # 'OPOINF': 'OP-Information',
          # 'OPVRT': 'Vertreter-Nummer',
          # 'OPABT': 'Abteilungs-Nummer',
          # 'OPBRAN': 'Branche',
          # 'OPGEB': 'Gebiet',
          # 'OPAKAN': 'Akt.Betrag Anbu',
          # 'OPSKZA': 'Gew{hrter Skonto',
          # 'OPDSLD': 'Durchschn. Saldo',
          # 'OPWZZA': 'Zahlungswartezeit',
          # 'OPOPBT': 'Betrag off. Posten',
          # 'OPOPWB': 'W{hrungsbetrag',
          # 'OPOPPW': 'Rechnungsbetrag',
          # 'OPOPSH': 'Kennzeichen Soll/Haben = S/',
          # 'OPRGBT': 'Rechnungsbetrag',
          # 'OPRGWB': 'W{hrungsbetrag',
          # 'OPRGPW': 'Rechnungsbetrag',
          'OPRGSH': 'kennzeichen_soll_haben',
          # 'OPKURS': 'Wechsel-Kurs',
          # 'OPKUFA': 'Kursfaktor',
          # 'OPBDIM': 'Betragsdimension',
          # 'OPZAKZ': 'W{hrungsart',
          # 'OPSPAR': 'Abteilungs-Gruppe/Sparte',
          # 'OPKZSB': 'Kennz. Schattenbuchung',
          # 'OPSRT1': 'Sort-Kennz. 1',
          # 'OPSRT2': 'Sort-Kennz. 2',
          # 'OPSRT3': 'Sort Text',
          # 'OPSRT4': 'Sort Betrag',
          # 'OPSRT5': 'Sort 5/Anz.tr{ger',
          # 'OPSRT6': 'Sort 6/Anz.kennz.',
          # 'OPKZRG': 'KZ J/N',
          # 'OPBUI1': 'Informationsfeld 1',
          # 'OPBUI2': 'Informationsfeld 2',
          # 'OPBUI3': 'Informationsfeld 3',
          # 'OPBUI4': 'Informationsfeld 4',
          # 'OPBUI5': 'Info-Kennz. 1',
          # 'OPBUI6': 'Info-Kennz. 2',
          # 'OPBUI7': 'Info Text',
          # 'OPBUI8': 'Info Betrag',
          # 'OPOPR1': 'Feld 10 A',
          # 'OPOPR2': 'Feld 10 A',
          # 'OPOPR3': 'Feld 10 A',
          # 'OPOPR4': 'Feld 20 A',
          # 'OPOPR5': 'Feld 1 A',
          # 'OPOPR6': 'Feld 1 A',
          # 'OPOPR7': 'Buchungsbetrag',
          # 'OPSANR': 'Satznummer',
          # 'OPSAN1': 'Satznummer',
          # 'OPWSPW': 'W{hrungs-Kennzeichen',
          # 'OPKUPW': 'Wechsel-Kurs',
          # 'OPFAPW': 'Kursfaktor',
          # 'OPKZPW': 'W{hrungsart',
},


'EBL00': {  # Bestellköpfe
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
     'BLDTAE': 'EBL_aenderung_date',
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

'XLI00': {  # Lieferanten-Adress-Datei
          #'LIFNR': 'Firma',
          #'LIKDNR': 'Kunden-Nummer',
          #'LIKZKD': 'Kz Kunde',
          'LILINR': 'lieferantennr',
          #'LIKZLI': 'Kz Lieferant',
          #'LIKZIT': 'Kz Interessent',
          #'LINRRS': 'Freie Nummer',
          #'LIKZRS': 'Kz Adress-Art',
          #'LIKZT1': 'Adresstyp 1',
          #'LIKZT2': 'Adresstyp 2',
          'LINAME': 'name1',
          'LINAM2': 'name2',
          'LINAM3': 'name3',
          'LINAM4': 'name4',
          'LISTR': 'strasse',
          'LIPLZ': 'plz',
          #'LIPLZP': 'PLZ Postfach',
          #'LIPLZF': 'PLZ Firma',
          #'LIPLZI': 'plz internat',
          'LIORT': 'ort',
          #'LIORTT': 'ortsteil',
          'LILKZ': 'laenderkennzeichen',
          #'LIPSTF': 'postfach',
          #'LIORTP': 'Ortsname (Postfach-PLZ)',
          #'LIGESF': 'Geschäftsführer',
          'LITELF': 'tel',
          #'LITELX': 'telex',
          'LITFAX': 'fax',
          #'LIALSO': 'Alpha-Sortierfeld',
          #'LISPSL': 'Sprache',
          #'LIWSL': 'Währungs-Kennzeichen',
          #'LIFGRP': 'Firmen-Gruppe',
          #'LIKGRP': 'Kunden-Gruppe',
          #'LILGRP': 'Lieferanten-Gruppe',
          #'LIISIC': 'ISIC-Schlüssel',
          #'LIRPMK': 'RPM-Kreis',
          #'LIRPMS': 'RPM-Segment',
          #'LIGDE': 'Gemeindeschlüssel',
          #'LIHEA': 'Herkunftsart',
          #'LIHESP': 'Herkunftsspezifikation',
          #'LIKZDR': 'Reserve 1',
          #'LIKZR2': 'Reserve 2',
          #'LIKZR3': 'Reserve 3',
          #'LIKXR4': 'Reserve 4',
          #'LIKZR5': 'Reserve 5',
          #'LIRES6': 'Reserve 6',
          #'LIRXS7': 'Reserve 7',
          #'LIRES8': 'Reserve 8',
          #'LIRES9': 'Reserve 9',
          #'LISANR': 'Satznummer Adressdatei',
          #'LIDTSP': 'Datum "Gesperrt bis"',
          #'LISBSP': 'Sachbearbeiter Sperre',
          #'LIKZSP': 'Kennzeichen Gesperrt',
          #'LIGRSP': 'Grund für Sperre',
          #'LITOFN': 'TOF Niederlassung',
          #'LITOFK': 'TOF Kundennummer',
          #'LITOFB': 'TOF Barcode',
          #'LIVRT': 'Vertreter für Interessenten',
          'LIEMAL': 'email',
          'LIMOBI': 'mobil',
          'LIHOME': 'web',
          #'LINAM5': '5. Namenszeile',
          #'LINAM6': '6. Namenszeile',
          #'LISTR2': 'Straßenzusatz',
          #'LISBER': 'Sachb. Erfassung',
          #'LIBPER': 'BenutzerPr.Erfassung',
          # 'LIDTER': 'XLI_erfassung_date',
          #'LISBAE': 'Sachb.letzte Änderung',
          #'LIBPAE': 'BenutzerPr.letzte Änderung',
          # 'LIDTAE': 'XLI_aenderung_date',
          #'LIDFSL': 'Dateiführungs-Schlüssel',
          #'LISTAT': 'Satzstatus'
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

'EBP00': {  # Bestellpositions-Datei
          'BPMNGB-BPMNGL': 'menge_offen',
          # 'BPFNR ': 'Firma',
          'BPBSTN': 'bestellnr',  # bei uns auch als P.O. bekannt
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
          'BPBDIF': 'beschaffung_deckung',  # Tg BeschffgT. - DeckgT.',
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
          'BPKZAB': 'bestell_abschluss_kz',
          # 'BPKZQU': 'KZ: Qualit{tspr}fung',
          # 'BPKZAE': 'KZ: Position ge{ndert',
          'BPKZAK': 'abgeschlossen_kz',
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

'EBT00': {  # Bestelltexte
          #'BTFNR': 'Firma',
          'BTBSTN': 'bestellnr',
          'BTBSTP': 'bestellposnr',
          'BTTART': 'textart',
          'BTLFNR': 'laufende_nr',
          #'BTSBNR': 'Sachb. Erfassung',
          #'BTABT' : 'Abteilungs-Nummer',
          #'BTAGRP': 'Abteilungs-Gruppe/Sparte',
          #'BTFGRP': 'Firmen-Gruppe',
          'BTTX60': 'text',
          'BTKZ01': 'andruck_auf_bestellung',
          #'BTKZ02': 'Kz: Andruck WE-Beleg',
          #'BTKZ03': 'Kz: frei 1',
          #'BTKZ04': 'Kz: frei 2',
          #'BTKZ05': 'Kennzeichen 05',
          #'BTKZ06': 'Kennzeichen 06',
          #'BTKZ07': 'Kennzeichen 07',
          #'BTKZ08': 'Kennzeichen 08',
          #'BTKZ09': 'Kennzeichen 09',
          #'BTKZ10': 'Kennzeichen 10',
          #'BTKZ11': 'Kennzeichen 11',
          #'BTKZ12': 'Kennzeichen 12',
          #'BTKZ13': 'Kennzeichen 13',
          #'BTKZ14': 'Kennzeichen 14',
          #'BTKZ15': 'Kennzeichen 15',
          #'BTDTER': 'Datum Erfassung CJJMMTT',
          #'BTDTAE': 'Datum l. Änderung CJJMMTT',
          #'BTDFSL': 'Dateiführungs-Schlüssel',
          #'BTSTAT': 'satzstatus',
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


'ESL00': {  # Stapelschnittstelle LAGER
    #'SLABT ': 'Abteilungs-Nummer',
    #'SLAGRP': 'Abteilungs-Gruppe/Sparte',
    #'SLFNR ': 'Firma',
    #'SLFGRP': 'Firmen-Gruppe',
    'SLARTN': 'artnr',
    #'SLARTG': 'Artikel-Gruppe',
    #'SLARTH': 'Artikel-Haupt-Gruppe',
    'SLLGNR': 'lager',
    'SLMNG ': 'menge',  # "Einzelmenge"
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
    'SLLWKO': 'lager_korrektur_wert',  # 'Korrektur-Wert des Lagers',
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


'EWZ00': {  # geprüfte Warenzugänge
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
     'WZLBWZ': 'lagerbewegung_zugang',  # referenziert
     # 'WZREKL': 'Reklamationsnummer         ',
     # 'WZGBNR': 'Vertriebseinheit           ',
     # 'WZRIMW': 'WZ-RefNr f}r Image         ',
     # 'WZLFSN': 'lieferschennr', # scheinbar immer 0000000
     'WZLGNR': 'lager',
     # 'WZLGPL': 'Lager-Platz                ',
     # 'WZWERK': 'Werksnummer                ',
     # 'WZANLP': 'Anlieferungs-Ort           ',
     # 'WZMGWE': 'gepruefte_zugangsmenge',
     'WZMGWB': 'menge',
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
     # 'WZMGRE': 'Berechnete Menge',
     # 'WZMGRB': 'Berechnete Menge Bestandsführung',
     # 'WZSTSL': 'Steuerschl}ssel            ',
     # 'WZMWBT': 'Vorsteuerbetrag            ',
     # 'WZKTR ': 'Kostenträger               ',
     # 'WZUKTR': 'Kostenträgerunternummer    ',
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
     # 'WZFMG1': 'Reservefeld 1 Menge',
     # 'WZFMG2': 'Reservefeld 2 Menge',
     # 'WZFMG3': 'Reservefeld 3 Menge',
     # 'WZFK20': 'Kunden-Spez                ',
     'WZKZST': 'status',
     #'WZMGST': 'Status Mg./ Bst.F}hrg      ',
     'WZSTAT': 'satzstatus',
     'WZSAN2': 'bezogener_satz_EWZ00',
     'WZSANR': 'satznummer_warenzugang',
},

'ISA00': {  # MyPL Schnittstelle: Komissionierbeleg
          'IAFNR': 'firma',
          'IALGNR': 'lagernr',
          'IAKBNR': 'kommibelegnr',
          'IAAUFN': 'auftragsnr',
          'IADATE': 'anforderung_date',
          'IATIME': 'anforderung_time',
          'IADFSL': 'dateifuehrungsschluessel',
          'IASTAT': 'status',
          'IASANR': 'satznr',
         },

'ISB00': {  # MyPL Schnittstelle: Lagerbuchungsschnittstelle
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

'ISK00': {  # MyPL Schnittstelle: Warenzugang aus Umlagerung
          'IKFNR': 'firma',
          'IKKBNR': 'komminr',
          'IKKPOS': 'kommiposition',
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

'ISR00': {  # MyPL Schnittstelle: KB-Rückmeldung
          'IRFNR': 'firma',
          'IRKBNR': 'kommibelegnr',
          'IRKPOS': 'kommibelegposition',
          'IRAUFN': 'auftragsnr',
          'IRAUPO': 'auftragsposition',
          'IRMENG': 'rueckmeldemenge',
          'IRKZNU': 'kennzeichen_nullen',
          'IRKZST': 'kennzeichen_stornieren',
          'IRFCOD': 'letzter_fehlercode',
          'IRDFSL': 'dateifuehrungsschluessel',
          'IRSTAT': 'status',
         },

'ISZ00': {  # MyPL Schnittstelle: Warenzugang aus Auftrag
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


'XAD00': {  # Abweichende Lieferadressen / Bestelladdressdatei
          # ADANW - Anwendung  -> scheinbar immer A oder E
          'ADRGNR': 'nr',  # Auftrags/Rechnungs/Bestellnummer-Nummer usw.
          #  * Auftragsnummer f}r ADAART 0/1
          #  * Rechnungsnummer f}r ADAART 2/3
          #  * Bestellnummer f}r ADAART 5/6/7/8
          #  * Satznummer Rechnungskopf f}r ADAART 4/9
          #  * Satznummer Lieferscheinkopf f}r ADAART 12
          #  * Angebotsnummer f}r ADAART 13/14
          # Falls SoftM Auftrag und SoftM FiBu eingesetzt sind, wird bei der
          # Fakturierung von Auftr{gen f}r Einmalkunden f}r die FiBu der
          # Satz mit dem Satzschl}ssel FA/RGNR/2 im Fakturierungsupdate
          # erzeugt.
          # F}r das L¦schen der S{tze mit ADAART = 0/1 ist ausschlie~lich
          # SoftM Auftrag zust{ndig. Die S{tze mit ADAART = 2/3 m}ssen
          # von SoftM FiBu gel¦scht werden.
          # ADAART - Adressart
          #  *ZERO       Rechnungsadresse Auftrag (Auftrag)
          #  01          Lieferadresse Auftrag (Auftrag)
          #  02          Adresse f}r OP/Einmaldebitor
          #  03          Adresse f}r OP/Einmalkreditor
          #  04          Rechnungsadresse Faktura (Auftrag)
          #  05          Lieferantenadresse Bestellung (Einkauf)
          #  06          Lieferadresse Bestellung (Einkauf)
          #  08          Lieferadresse Anfrage (Einkauf)
          #  09          Lieferadresse Faktura (Auftrag)
          #  12          Lieferadresse Lieferschein (Auftrag)
          #  13          Rechnungsadresse Angebot (Auftrag)
          #  14          Lieferadresse Angebot (Auftrag)
          #  18          Abweichende Rechnungsanschrift (Einkauf)
          # ADPKTO   Personenkonto
          #          Je nach Anwendung wird das Personenkonto unterschiedlich gef}llt.
          #          SoftM Einkauf:
          #          ADAART = 18:  Rechnungssteller aus der Rechnung (Datei EER00)
          #          ADAART = 5/6: Lieferant aus der Bestellung (Datei EBL00)
          #          ADAART = 8:   leer"
          #          SoftM Auftrag:
          #          Bei Angebotsadressen ist das Personenkonto der Angebotskunde.
          #          In allen anderen F{llen ist das Personenkonto leer.
          'ADNAME': 'name1',
          'ADNAM2': 'name2',
          'ADNAM3': 'name3',
          # 'ADNAM4': 'name4',
          'ADSTR': 'strasse',
          'ADPLZ': 'plz',
          'ADORT': 'ort',
          'ADLKZ': 'laenderkennzeichen',
          # 'ADKZAD': 'adressaufbereitung', - fuer legacy Daten
         },

'XAR00': {  # Artikelstamm
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
           'ARZTNR': 'statistischewarennr',  # 'zolltarifnr',
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

'XCK00': {  # Ablaufkontrolle
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

'XED00': {  # Intrastat-Meldungs-Datei
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

'XKD00': {  # Kundenadressen - ist eine "Logische Datei (View)" basierend auf der XXA00
          'KDKDNR': 'kundennr',
          'KDNAME': 'name1',
          'KDNAM2': 'name2',
          'KDNAM3': 'name3',
          # 'KDNAM4': 'name4',
          'KDSTR': 'strasse',
          'KDPLZ': 'plz',
          'KDORT': 'ort',
          'KDLKZ': 'laenderkennzeichen',
          'KDTELF': 'tel',
          'KDTFAX': 'fax',
          # 'KDMOBI': 'mobil',
          # 'KDPSTF': 'postfach',      # gelegentlich genutzt
          # 'KDPLZP': 'postfach_plz',  # wenig genutzt
          #'KDORTP': 'Ortsname (Postfach-PLZ)', # gelegentlich genutzt
          #'KDPLZF': 'PLZ Firma', # sehr wenig genutzt
          # 'KDGESF': 'Geschäftsführer', # wenig genutzt
          # 'KDALSO': 'sortierfeld',
          # 'KDSPSL': 'Sprache', # Werte 1 & 2
          # 'KDWSL ': 'Währungs-Kennzeichen',
          'KDKGRP': 'kunden_gruppe',
          'KDEMAL': 'mail',
          # 'KDHOME': 'url',
          # 'KDSANR': 'adressdatei_id',
          'KDDTER': 'XKD_erfassung_date',
          'KDDTAE': 'XKD_aenderung_date',
          'KDSTAT': 'satzstatus',
          },

'XKS00': {  # Kundenzusatzdaten
          'KSKDNR': 'kundennr',
          'KSLISP': 'liefersperre',
          'KSAWRT': 'offener_aftragswert',
          # 'KSAKWR': 'kreditlimit',
          # 'KSCOMP': 'company',
          # 'KSIAKZ': 'inland_ausland',
          # 'KSFNRK': 'interne_firmennr',
          'KSINFL': 'unsere_lieferantennr',
          'KCUSTN': 'ustid',
          'KSLIMI': 'kreditlimit2',
          'KSSKSL': 'skontoschluessel',
          # 'KSDKSL': 'delcredereschlüssel',
          # 'KSMASP': 'mahnsperre',
          # 'KSKDZA': 'lastschrift',
          # 'KSBOKZ': 'bonnitaet',
          # 'KSKLTA': 'Kd/Li-Texte anzeigen',
          # 'KSINF1': 'Inf. 1',
          'KCVERB': 'verbandsnr',
          #'KSKRED': 'Verbands-/Mischkto-Satz',
          'KCMGVB': 'mitgliedsnr',
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

'XPN00': {  # Konditionen / Preise
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
    # 'PNSBAE': 'Sachb.letzte Änderung  ',
    # 'PNBPAE': 'BenutzerPr.letzte Änderung',
    # 'PNDTAE': 'Datum l. Änderung CJJMMTT',
    # 'PNZTAE': 'Uhrzeit letzte Änderung',
    # 'PNSTAT': 'Satzstatus',
    # 'PNDFSL': 'dateifuehrungsschluessel',

},

'XPR00': {  # Konditionen / Preise
    'PRFNR': 'firma',
    'PRANW': 'anwendung',  # 'E' == Einkauf
    # 'PRIB': 'Datenart',
    # 'PRANW1': 'Preismodul',
    'PRSANR': 'satznr_xpn00',
    # 'PRKTYP': 'Konditionstyp',
    'PRKDNR': 'kunde',
    'PRARTN': 'artnr',
    'PRLINR': 'lieferant',
    # 'PRPROB': 'Objekt',
    # 'PRPROJ': 'Projekt',
    # 'PRVRT': 'Vertreter',
    'PRPRLK': 'preisliste_kunde',
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

'XXA00': {  # Adressen von Kunden und Lieferanten
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
        'XAKGRP': 'kunden_gruppe',
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
        'XADTAE': 'XXA_aenderung_date',
        # 'XADFSL': Dateif}hrungs-Schl}ssel
        'XASTAT': 'satzstatus',
       },

'XXC00': {
          # Kunden-/Lieferantenzusatz
          # Hier werden zus{tzliche Daten f}r den Kunden oder Lieferanten
          # hinterlegt. Diese Datei wird sowohl von der Warenwirtschaft als
          # auch vom Rechnungswesen ben¦tigt  und enth{lt wichtige Daten,
          # wie z.B. MWSt-Zuordnung, EU-USt-IdNr etc.
          'XCADAR': 'art',
          # Festlegung, ob die Adressnummer eine Kunden- oder eine
          # Lieferantennummer enth{lt.
          # D           Kunde/Debitor
          # K           Lieferant/Kreditor
          'XCADNR': 'XXC_nr',  # Kunde/Lieferant
          'XCLISP': 'liefersperre',
          #          *ZERO       keine Liefersperre
          #          1           Liefersperre
          # XCFGRP   Firmengruppe                                    A    2     13   14
          'XCAWRT': 'offener_auftragswert',
          #          In diesem Feld sind die offenen Positionswerte aus der
          #          Auftragspositionsdatei aufsummiert. Allerdings k¦nnen aufgrund
          #          der Parameterisierung bestimmte Auftragsarten und Positionen
          #          dabei unber}cksichtigt bleiben.
          #          Die wichtigsten sind:
          #           Auftragspositionen mit Kreditlimitsperre bleiben generell
          #           unber}cksichtigt.
          #           Rahmenpositionen werden nur ber}cksichtigt, falls lt. Steuerung
          #           Auftrag 'Rahmen f}r Kreditlimit = ja' (PABNKL = 0).
          #           Positionen werden nur ber}cksichtigt, falls lt. Auftragsart
          #           'Kreditlimitpr}fung = ja' (PAAFA4 = leer/0/1).
          #  XCAKWR   Offener Auftragswert f}r Kreditlimit            P   15 2   23   30
          #           Je nach Parameterisierung der Kreditlimitpr}fung ergibt sich der
          #           relevante Positionswert aus:
          #             a) offener Positionswert               (KLmt bei Erfassung)
          #             b) Positionswert der zugeteilten Menge (KLmt bei Zuteilung)
          #           In diesem Feld sind die entsprechenden Positionswerte aus der
          #           Auftragspositionsdatei aufsummiert. Allerdings k¦nnen aufgrund
          #           der Parameterisierung bestimmte Auftragsarten und Positionen
          #           dabei unber}cksichtigt bleiben.
          #           Die wichtigsten sind:
          #           Auftragspositionen mit Kreditlimitsperre bleiben generell
          #          unber}cksichtigt.
          #          Rahmenpositionen werden nur ber}cksichtigt, falls lt. Steuerung
          #          Auftrag 'Rahmen f}r Kreditlimit = ja' (PABNKL = 0).
          #          Positionen werden nur ber}cksichtigt, falls lt. Auftragsart
          #          'Kreditlimitpr}fung = ja' (PAAFA4 = leer/0).
          # XCCOMP   Company-Kennzeichen                             A    3     31   33
          # XCIAKZ   Inland/Ausland                                  S    1 0   34   34
          #          Festlegung, ob Lieferant am Inlands- bzw. Auslandzahlungsverkehr
          #          teilnimmt.
          #          Bei Kunden wird normalerweise immer eine 0 angegeben.
          #          F}r Mischkonten kann die Auswahl 1 bzw. 2 angegeben werden.
          #          PTYP Tabelle
          #          *ZERO       Inlandszahlungsverkehr / Lastschriften
          #                      Der Lieferant wird }ber den Inlandszahlungsverkehr
          #
          #                       abgewickelt.
          #                       Der Kunde kann am Lastschriftverfahren bzw. an der
          #                       debitorischen Auszahlungen teilnehmen.
          #           1           Auslandszahlungsverkehr
          #           2           Inlands- und Auslandszahlungsverkehr
          #                       Alle OP's in Hauptw{hrung werden im Inlandszahlungsver
          #                       beglichen. Alle OP's in W{hrungen werden im Auslands-
          #                       zahlungsverkehr bezahlt.
          #  XCKZAD   Adresse {nderbar                                S    1 0   35   35
          #           *ZERO       Adresse ist nicht {nderbar
          #           1           Adresse ist {nderbar
          'XCINFL': 'unsere_lieferantennr',
          # Falls bekannt, kann hier die Lieferantennummer eingetragen werden, die uns der Kunde zugewiesen
          # hat.
          #  XCEGCD   EU-L{ndercode                                   A    2     48   49
          'XCUSTN': 'ustid',
          # EU-einheitliche Steuernummer (USt-IdNr, VAT-Nr).
          # Beim Druck der USt-IdNr ist zus{tzlich noch der EU-L{ndercode
          # mitzudrucken.
          # Die Umsatzsteueridentifikationsnummer hat folgenden Aufbau:
          #  Land              ]   L{nge   ] Numerisch
          #  ------------------]-----------]--------------------------------
          #  Belgien           ]       9   ] ja
          #  D{nemark          ]       8   ] ja
          #  Deutschland       ]       9   ] ja   Die Stelle 09 ist eine
          #                    ]           ]      Pr}fziffer }ber die Stellen
          #                    ]           ]      01 bis 08. Das System setzt
          #                    ]           ]      die Pr}fziffer automatisch
          #                    ]           ]      ein und l¦scht die Stellen
          #                    ]           ]      10 bis 12.
          #                    ]           ]
          #  Spanien           ]       9   ] ja   die erste und letzte
          #                    ]           ]      Stelle bzw. die erste oder
          #                    ]           ]      die letzte Stelle kann ein
          #                    ]           ]      Buchstabe sein
          #                    ]           ]
          #  Griechenland      ]       8   ] ja
          #  Frankreich        ]      11   ] ja
          #  Irland            ]       8   ] nein die zweite Stelle kann
          #                    ]           ]      und die letzte Stelle muss
          #                    ]           ]      ein Buchstabe sein
          #  Italien           ]      11   ] ja
          #  Luxemburg         ]       8   ] ja
          #  Niederlande       ]      12   ] nein die drittletzte Stelle ist
          #                    ]           ]      der Buchstabe B
          #  Portugal          ]       9   ] ja
          #  Gro~britannien    ] 9 oder 12 ] ja
          #  \sterreich        ]       9   ] nein erste Stelle = U
          #  Finnland          ]       8   ] ja
          #  Schweden          ]      12   ] ja
          #  XCKZUS   Kennzeichen USt-IdNr                            S    1 0   62   62
          #           *ZERO       keine USt-IdNr, da au~erhalb EU/Inland
          #           1           Kd/Li innerhalb EU, mit USt-IdNr
          #           2           Kd/Li innerhalb EU, USt-IdNr unklar
          #           3           Kd/Li innerhalb EU, ohne USt-IdNr
          #           4           wie 3, jedoch Privatperson
          #  XCMWKZ   Steuerprofil                                    S    2 0   63   64
          #  XCLIMI   Kreditlimit                                     P   15 2   65   72
          #           Angabe in Hauptw{hrung.
          #           Die Angabe bei einem Kunden ist Voraussetzung f}r eine
          #           Kreditlimitpr}fung im Bereich der Auftragsbearbeitung.
          #  XCSKSL   Skontoschl}ssel                                 S    3 0   73   75
          #           Angabe des Skontoschl}ssels des Kunden bzw. Lieferanten.
          #           Ist kein Skontoschl}ssel angegeben, wird f}r die Finanzbuch-
          #           haltung der entsprechende Vorbesetzungsskontoschl}ssel aus dem
          #           Parameter Debitoren bzw. Kreditoren verwendet.
          #  XCDKSL   Delkredereschl}ssel                             S    3 0   76   78
          #           Angabe des Skontoschl}ssels des Kunden bzw. Lieferanten.
          #           Ist kein Skontoschl}ssel angegeben, wird f}r die Finanzbuch-
          #           haltung der entsprechende Vorbesetzungsskontoschl}ssel aus dem
          #           Parameter Debitoren bzw. Kreditoren verwendet.
          # XCMASP   Mahnsperre Kunde                                A    1     79   79
          #          *BLANK      keine Mahnsperre
          #          N           sonstige
          #          1           gesperrt f}r 1 Mahnlauf
          #          2           gesperrt f}r 2 Mahnl{ufe
          #          3           gesperrt f}r 3 Mahnl{ufe
          #          4           gesperrt f}r 4 Mahnl{ufe
          #          5           gesperrt f}r 5 Mahnl{ufe
          #          6           gesperrt f}r 6 Mahnl{ufe
          #          7           gesperrt f}r 7 Mahnl{ufe
          #          8           gesperrt f}r 8 Mahnl{ufe
          #          9           gesperrt f}r 9 Mahnl{ufe
          # XCKDZA   Teilnahme am Lastschriftverfahren               A    1     80   80
          #           Festlegung, ob/wie der Kunde am Lastschriftverfahren teilnimmt
          #           Hinweis zum Einsatz in der Schweiz:
          #           Die Kennzeichen 1 bis 3 sind nur aktiv, sofern die Felder PXVESR
          #           (VESR-Codierung 'CH') im Firmenparameter und PAAVKZ (VESR-
          #           Codierung 'CH') im Auftragsartenparameter ebenfalls auf 1
          #           gesetzt wurden.
          #           PTYP Tabelle
          #           *BLANK      kein LastschriftenVrf / VESR-Einzahlung
          #           A           Abbuchungsverfahren
          #                       Die OP's des Kunden werden }ber den Lastschriftenlauf
          #                       Kunden eingezogen. Auf den Datentr{ger (im Rahmen des
          #                       Lastschriftenlaufes) wird der Textschl}ssel 04 f}r
          #                       Lastschrift (Abbuchungsverfahren) geschrieben.
          #           D           direct debit
          #                       Kunde wird im Lastschriftenlauf nur ber}cksichtigt, we
          #                       beim Aufruf das Kennzeichen 'D' selektiert wird.
          #           L           Lastschriftverfahren
          #                       Die OP's des Kunden werden }ber den Lastschriftenlauf
          #                       Kunden eingezogen. Auf den Datentr{ger (im Rahmen des
          #                       Lastschriftenlaufes) wird der Textschl}ssel 05 f}r
          #                       Lastschrift (Einzugserm{chtigungsverfahren) geschriebe
          #           1           VESR-Einzahlungsscheine (Nettobetr{ge)
          #                       Wird eine 1 gesetzt, so erstellt das Fakturadruckprogr
          #                       zus{tzlich einen ESR/VESR-Einzahlungsschein (Netto-Net
          #           2           VESR-Einzahlungsscheine (Bruttobetr{ge)
          #                       Wird eine 2 gesetzt, so erstellt das Fakturadruckprogr
          #                       zus{tzlich einen ESR/VESR-Einzahlungsschein (Brutto-Br
          #           3           VESR-Einzahlungsscheine (Nto/BtoBetr{ge)
          #                       Wird eine 3 gesetzt, so erstellt das Fakturadruckprogr
          #                       zus{tzlich einen ESR/VESR-Einzahlungsschein (Netto-Bru
          #  XCBOKZ   Bonit{t                                         A    1     81   81
          #           *BLANK      ohne Bonit{t
          #  XCKLTA   Kunden-/Lieferantentexte                        A    1     82   82
          # 'XCINF1':  # Infofeld 1
          'XCVERB': 'verbandskonto',  # Verbands-/Mischkonto
          #           Bei Zugeh¦rigkeit des Kunden/Lieferanten zu einem Verband oder
          #           Mischkonto ist hier die Nummer des Verbandes oder des Misch-
          #           kontos einzutragen.
          #           Bei einem Mischkonto vom Typ N ist hier die Debitorennummer ein-
          #           zutragen.
          #  XCKRED   Verbands-/Mischkontosatz                        A    1     92   92
          #           REFFLD  XREF       XREFF01    XXKRED
          #           COLHDG  Vbnd/MKpf
          #           Festlegung, ob es sich bei diesem Konto um einen Verbandskopf-
          #           satz oder um einen Mischkontenkopfsatz handelt.
          #           Verband:
          #             Die Eingabe V bedeutet, dass ein Verbandskopfsatz angelegt
          #             wird. Die Mitglieder werden durch die Eingabe der
          #             Verbandskopfsatznummer diesem Verband zugeordnet.
          #           Mischkonto:
          #             Die Eingabe M bedeutet, dass ein Mischkontenkopfsatz angelegt
          #             wird. Die Mitglieder werden durch die Eingabe der
          #             Mischkontenkopfsatznummer diesem Mischkonto zugeordnet.
          #           Die Eingabe N bedeutet, dass ein Debitor und ein Kreditor zu
          #           einem Mischkonto zusammengefasst werden sollen.
          #           Die Mischkontennummer beim Debitor muss mit der Debitorennummer
          #           gef}llt werden. Bleibt sie leer, wird sie vom Programm
          #
          #            eingesetzt.
          #            Die Mischkontennummer beim Kreditor muss ebenfalls mit dieser
          #            Debitorennummer gef}llt werden.
          #            Die Debitorennummer und die Kreditorennummer m}ssen identisch
          #            sein. Debitor=Kreditor=Mischkonto
          #            *BLANK      kein Verbandssatz
          #            M           Mischkontensatz mit Kopfsatz
          #            N           Mischkontensatz ohne Kopfsatz
          #            V           Verbandssatz
          'XCMGVB': 'mitgliedsnr',  # Mitgliedsnummer beim Verband
          #   XCSEDA   Rechnungsdatenaustausch per EDIFACT             S    1 0  101  101
          #            *ZERO       kein Datenaustausch
          #            1           nimmt an SEDAS teil
          #                        Ist nur erlaubt, wenn in der Auftragssteuerung Teil 1
          #                       SEDAS-Schnittstelle aktiviert ist.
          #           2           Rechnungsdaten per EDIFACT }bermitteln
          #                       Ist nur erlaubt, wenn in der Auftragssteuerung Teil 1
          #                       EDIFACT-Schnittstelle aktiviert ist.
          #  XCBBN    Bundeseinheitliche Betriebsnummer               S    8 0  102  109
          #  XCBBS    Bundeseinheitliche Betriebsstellennummer        S    6 0  110  115
          #  XCKDRZ   Rechenzentrum                                   A    8    116  123
          #  XCVRKZ   Rechnungsliste erstellen                        S    1 0  129  129
          #           *ZERO       keine Rechnungsliste
          #           1           Rechnungsliste ohne SEDAS
          #           2           Rechnungsliste und  SEDAS
          #           3           keine Rechnungsliste, nur SEDAS
          #  XCRGKZ   Rechnung drucken                                S    1 0  130  130
          #            *ZERO       Rg nicht drucken, kein SEDAS
          #            1           Rg drucken, kein SEDAS
          #            2           Rg und SEDAS drucken
          #            3           Rg nicht drucken, nur SEDAS
          #   XCKOIN   Kontoinhaber                                    A   40    131  170
          #   XCPAUO   Artikelunabh{ngige Objektpreise                 S    1 0  171  171
          #            Festlegung, ob und f}r welche Rabattarten (normale Preise /
          #            Rabatte und Naturalrabatte) Konditionen in der Preisdatei vor-
          #            handen sind. Dabei sind nur artikelabh{ngige bzw. unabh{ngige, s.
          #            o.) Konditionen einer Kundennummer und eines Objektes von Bedeu-
          #            tung. Dieses Kennzeichen dient zur Performance-Verbesserung bei
          #            der Konditionsermittlung.
          #           *ZERO       keine Konditionen
          #           1           normale Konditionen
          #           2           Naturalrabatte
          #           3           Naturalrabatte und normale Konditionen
          #  XCPAAO   Artikelabh{ngige Objektpreise                   S    1 0  172  172
          #           Festlegung, ob und f}r welche Rabattarten (normale Preise /
          #           Rabatte und Naturalrabatte) Konditionen in der Preisdatei vor-
          #           handen sind. Dabei sind nur artikelabh{ngige bzw. unabh{ngige, s.
          #           o.) Konditionen einer Kundennummer und eines Objektes von Bedeu-
          #           tung. Dieses Kennzeichen dient zur Performance-Verbesserung bei
          #           der Konditionsermittlung.
          #           *ZERO       keine Konditionen
          #           1           normale Konditionen
          #           2           Naturalrabatte
          #           3           Naturalrabatte und normale Konditionen
          #  XCPAU    Artikelunabh{ngige Preise                       S    1 0  173  173
          #           Festlegung, ob und f}r welche Rabattarten (normale Preise /
          #           Rabatte und Naturalrabatte) Konditionen in der Preisdatei vor-
          #           handen sind. Dabei sind nur artikelabh{ngige (bzw. unabh{ngige,
          #           s.o.) Konditionen mit Angabe einer Kundennummer aber ohne Angabe
          #           eines Objektes von Bedeutung. Dieses Kennzeichen dient zur
          #           Performance-Verbesserung bei der Konditionsermittlung.
          #           *ZERO       keine Konditionen
          #           1           normale Konditionen
          #           2           Naturalrabatte
          #           3           Naturalrabatte und normale Konditionen
          #  XCPAA    Artikelabh{ngige Preise                         S    1 0  174  174
          #           Festlegung, ob und f}r welche Rabattarten (normale Preise /
          #           Rabatte und Naturalrabatte) Konditionen in der Preisdatei vor-
          #           handen sind. Dabei sind nur artikelabh{ngige (bzw. unabh{ngige,
          #           s.o.) Konditionen mit Angabe einer Kundennummer aber ohne Angabe
          #           eines Objektes von Bedeutung. Dieses Kennzeichen dient zur
          #           Performance-Verbesserung bei der Konditionsermittlung.
          #           *ZERO       keine Konditionen
          #           1           normale Konditionen
          #           2           Naturalrabatte
          #           3           Naturalrabatte und normale Konditionen
          #  XCHSTU   Hierarchiebereich Kunde/Lieferant               P    3 0  175  176
          #           Alle Organisationseinheiten werden einem Hierarchiebereich
          #           zugeordnet. Der Hierarchiebereich unterscheidet OrgEh in Bezug
          #           auf ihre Stellung in der Hierarchie. Einer Hierarchie k¦nnen
          #           alle Hierarchiebereiche oder nur einige zugeordnet sein. Dabei
          #           werden die gew}nschten Hierarchiebereiche den Hierarchieebenen
          #           der Hierarchie zugeordnet. Ein Bereich kann in unterschiedlichen
          #           Hierarchien unterschiedlichen Ebenen zugeordnet sein. Hierin
          #           liegt der eigentliche Grund, weshalb die Zuordnung einer
          #           Hierarchieebene nicht direkt bei der OrgEh erfolgen kann.
          #           Es wird empfohlen, Hierarchiebereiche in 10-er Schritten
          #           anzulegen, um flexibel auf Erweiterungen der hierarchischen
          #           Struktur reagieren zu k¦nnen.
          #           PGM  UPX02CL
          #  XCRGST   Rechnungssteller                                A    8    237  244
          #           Wenn der Rechnungssteller vom Warenlieferanten abweicht, so ist
          #           hier der Kreditor/Lieferant zu hinterlegen, welcher die Rechnung
          #           stellt. Diese Angabe wird in der Bestellverwaltung und
          #           Rechnungspr}fung vorbesetzt.
          #  XCKZBO   Bonit{t                                         A    1    245  245
          #           Das Bonit{tskennzeichen wird nur von SoftM Auftrag ausgewertet.
          #           Um die Bonit{tspr}fung zu aktivieren, ist zum einen ein
          #           Kreditlimit einzutragen und zum anderen die Bonit{t zu pflegen.
          #           Das Bonit{tskennzeichen kann vom hierzu berechtigten Anwender
          #           in der SoftM Dokumentation erweitert bzw. ge{ndert werden.
          #           !ber das Bonit{tskennzeichen kann gesteuert werden, dass
          #           a) Auftr{ge des Kunden unabh{ngig vom verf}gbaren Kreditlimit
          #              generell in die Kreditlimitsperre laufen,
          #           b) differenzierte Liefersperren gesetzt werden k¦nnen.
          #           *BLANK      Kunde hat Bonit{t
          #           F           F{llige Posten
          #           K           Klage l{uft
          #  XCSKRL   Skontoausweis auf RgListe                       A    1    246  246
          #           REFFLD  XREF       XREFF01    XXA1
          #           COLHDG  Skonto RgL
          #           Festlegung, ob auf Rechnungslisten und SEDAS-!bertragungen f}r
          #           Verb{nde Skonto ausgewiesen wird.
          #           *ZERO       Skonto nicht ausweisen
          #           1           Skonto ausweisen
          #                       Der Skontobetrag wird bezogen auf den Warenwert zzgl.
          #                       Mehrwertsteuer ausgewiesen.
          #  XCEDIL   Lieferscheindatenaustausch per EDIFACT          S    1 0  247  247
          #           Zus{tzlich zum Lieferscheindruck k¦nnen wahlweise die
          #           Lieferscheindaten per EDIFACT }bermittelt werden.
          #           *ZERO       ohne EDIFACT
          #           1           mit EDIFACT
          #                       Ist nur erlaubt, wenn in der Auftragssteuerung Teil 1
          #                       EDIFACT-Schnittstelle aktiviert ist.
          #  XCPRIA   Inland/Ausland Konditionsermittlung             S    1 0  248  248
          #           Festlegung, ob Inlands- oder Auslandskonditionen gelten.
          #           *ZERO       Inlandskonditionen
          #           1           Auslandskonditionen
          #  XCWSLN   W{hrung                                         A    3    249  251
          #  XCDTWN   W{hrung neu g}ltig ab                           P    7 0  252  255
          #  XCE2RP   Schl}ssel                                       A    4    259  262
          'XCE2IL': 'iln',
          },

'XLB00': {  # Lagerbewegungen
        #'LBFGRP': 'Firmen-Gruppe',
        #'LBFNR ': 'Firma',
        #'LBAGRP': 'Abteilungs-Gruppe/Sparte',
        #'LBABT ': 'Abteilungs-Nummer',
        #'LBARTH': 'Artikel-Haupt-Gruppe',
        #'LBARTG': 'Artikel-Gruppe',
        'LBARTN': 'artnr',
        #'LBSENR': 'Serien-/Chargennummer',
        'LBSBNR': 'sachbearbeiter_erfassung',
        'LDWSID': 'herkunft',  # z.B. 'EINKAUF'
        # 'LBSAKZ': 'SA',
        'LBBWSL': 'bewegungsschluessel',  # 22 = Warenzugang
        'LBBELN': 'belegnummer',          # == Nummer in der Fibu
        'LBDTBL': 'beleg_date',
        'LBBUMO': 'buchungsmonat',        # format: CJJMM
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
        # 'LBKZWR': 'bewegungswert_angegeben_kz', # scheinbar immer 9 = Gesamtwert
        'LBWSL': 'waehrung',
        'LBKURS': 'kurs',
        'LBKUFA': 'kursfaktor',
        # 'LBRPRZ': 'Rabatt-%-Satz', - scheinbar immer 0
        # 'LBFPRZ': 'fracht_prozent', # % - Satz scheinbar immer 0
        # 'LBZPRZ': 'zoll_prozent', # % - Satz scheinbar immer 0
        'LBZARZ': 'abwertung_prozent',  # % - Satz - scheinbar immer 0
        'LBLWER': 'wert_erfassung',
        'LBLWAK': 'wert_aktuell',
        'LBLWKO': 'lagerwert_korrektur',  # scheinbar immer identisch mit LBLWAK
        'LBLWRT': 'lagerwert_vor_buchung',
        'LBMNGE': 'bewegungsmenge',       # 'Bewegungs-Menge lt. Eingabe',
        # 'LBMEER': 'Mengeneinheit/Erfassung', # immer 1
        # 'LBFAKT': 'Umrechn.-Faktor  ER-->BE', # immer 1
        'LBMNGB': 'menge',
        'LBMNGZ': 'zugangsmenge',  # Zugangsmenge nach Umlagerung',
        # 'LBMNGL': 'Menge noch am Lager', # FIFO - Menge? - scheinbar immer 0
        # 'LBMNGO': 'Menge Vorab-Bewertet', # scheinbar immer 0
        # 'LBMNGS': 'Abgangsmenge mit Statistik', - scheinbar immer 0
        # 'LBMGOP': 'Menge als offener Posten', - scheinbar immer 0
        'LBMGLP': 'bestand_vor_buchung',
        'LBMGKO': 'bestandsaenderung',    # Bestandsänderung am Lager',
        'LBBSTN': 'bestellnr',
        'LBBSTP': 'bestellpos',
        # 'LBFNRK': 'Firmen-Nr./Lieferant',
        # 'LBLINR': 'lieferantennr',
        # 'LBZUKZ': 'Zustand: A/R/F', Zeicheinbar immer ''
        'LBAUFN': 'auftragsnr',  # Warenvereinnamungsnummer bei Zugängen
        'LBAUPO': 'auftragspos',
        'LBKZKO': 'mit_komponenten',  # immer 0 oder 6?
        'LBDTER': 'erfassung_date',
        'LBDTAE': 'aenderung_date',
        'LBINFO': 'info',
        'LBART ': 'art',  # bisher 0 1, und 13 beobachtet
        'LBBTYP': 'typ',
        #'LBDFSL': 'dateifuehrungsschluessel',
        'LBSTAT': 'satzstatus',
        'LBSAN2': 'bezogene_bewegung',
        'LBSANR': 'satznummer',
    },

'XSB00': {  # Sachbearbeiter
        'SBSBNR': 'id',
        'SBNAME': 'name',
    },

'XTY00': {  # Versandarten
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
          # 'TYDTAE': 'Datum l. Änderung CJJMMTT',
          # 'TYSBAE': 'Sachb.letzte Änderung',
          # 'TYBPAE': 'BenutzerPr.letzte Änderung',
          # 'TYDFSL': 'Dateifuehrungs-Schluessel',
          'TYSTAT': 'satzstatus'},

'XKP00': {  # Validierungsdateien
          #'KPFNR': 'Firma',
          # 'KPANW': 'anwendung',
          # bekannte Werte für KPTYP: ZAHLBEDA - Zahlungsbedingungen
          'KPTYP': 'typ',
          'KPINHA': 'inhalt1',
          'KPINH2': 'inhalt2',
          'KPINH3': 'inhalt3',
          # 'KPLNG': 'laenge',
          # 'KPDART': 'art',
          'KPDTAE': 'XKP_aenderung_date',
          'KPBSTX': 'kurztext',
          'KPTXT2': 'langtext',
          # 'KPTX30': 'bezeichnung',
},

'XTK00': {  # Stichtagskurse (Währungskurse)
    # 'TKFNR': 'Firma',
    # 'TKKZPW': 'waehrungskennzeichen',
    'TKDTST': 'stichtag_date',
    # 'TKKART': 'Zu verwendender Kurs',
    'TKKUR1': 'wechselkurs',
    # 'TKKUR2': 'verrechnungskurs',
    # 'TKKUFA': 'kursfaktor',
    # *ZERO Kurs pro Fremdwährungseinheit
    # 1     Kurs pro 10 Fremdwährungseinheiten
    # 2     Kurs pro 100 Fremdwährungseinheiten
    # 3     Kurs pro 1000 Fremdwährungseinheiten
    # 'TKR5': 'reserve5_wechselkurs',
    # 'TKR6': 'reserve6_wechselkurs',
    # 'TKR7': 'reserve7_datum',
    'TKWSL': 'waehrungskennzeichen',
},

# SMKDIFP    XLF00      XLF00F01   Lagerbestands-führungs-Datei
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
                   'FKBRUT', 'FKNETT', 'FUPNET', 'FUPBUT',
                   'SLPREW', 'SLWRTI', 'SLFRBT', 'SLZOBT', 'SLPREW', 'SLLWKO', 'SLLWFR', 'SLLWZO',
                   'WZBUBT', 'WZPREA', 'WZPREW',
                   'FKRBB1', 'FKRBB2', 'FKMWBT', 'FKVSK', 'FKSBRT', 'FKGERB',
                   'FUPORB', 'FUPREV'])


# Decimal fields with the same precision like in SoftM
DECIMALIZE = set(['TKKUR1', 'TKKUR2'])


# maps datefield to related timefield for generating datetime objects
DATETIMEDIR = {
               'AKDTAE': 'AKZTAE',
               'AKDTER': 'AKZTER',
               'APDTZU': 'APZTZU',
               'APDTER': 'APZTER',
               'APDTAE': 'APZTAE',
               'CKANFD': 'CKANFZ',  # XCK00 start
               'CKENDD': 'CKENDZ',  # XCK00 ende
               'FKDTER': 'FKZTER',  # Rechnungskopf erfassung_date
               'FUDTER': 'FUZTER',  # Rechnungsposition erfassung_date
               'IKDATE': 'IKTIME',
               'IZDTWZ': 'IZTIME',
               'LKDTAE': 'LKZTAE',  # Lieferschein Änderung
               'LKDTER': 'LKZTER',  # Lieferschein Erfassung
               'LKDTKB': 'LKZTKB',  # letzter_kommibeleg
               'LKDTLF': 'LKZTLF',  # letzter_lieferschein
               'LNDTAE': 'LNZTAE',  # aenderung
               'LNDTER': 'LNZTER',  # erfassung
               'LNDTKB': 'LNZTKB',
               'LNDTLF': 'LNZTLF',  # lieferschein
               'SKDTAE': 'SKZTAE',  # komponenten
              }


# Fields which need padding before beeing used in SQL queries
PADDINGFIELDS = {
    'AKKDNR': '%8s',
    'BBGKTO': '%8s',
    'BBKST': '%10s',
    'BBKTO': '%8s',
    'BBPKTO': '%8s',
    'BUGKTO': '%8s',
    'BUPKTO': '%8s',
    'FKKDNR': '%8s',
    'KAKDNR': '%8s',
    'KDKDNR': '%8s',
    'KPINHA': '%10s',
    'KSKDNR': '%8s',
    'KZKDNR': '%8s',
    'KZVRT': '%8s',
    'LILINR': '%8s',
    'LKKDNR': '%8s',
    'OPPKTO': '%8s',
    'PRKDNR': '%8s',
    'XALINR': '%8s',
}
