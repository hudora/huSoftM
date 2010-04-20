#!/usr/bin/env python
# encoding: utf-8
"""
stapelschnittstelle_const.py - description of the SoftM Stapelschnittstelle. alles weitere in
stapelschnittstelle.py

Created by Maximillian Dornseif on 2008-09-19. Copyright (c) 2008 HUDORA. All rights reserved.
"""

# das Datenmodell, was hier definiert wird, ist deutlich reicher, als es momentan genutzt wird. Es ist
# geplant, sowas wie in huProtocols.recordbased zu implementieren.

# ABK sind die Auftragsköpfe. Für jeden Auftrag gibt es einen Eintrag in ABK00 und N Einträge in ABA00.
ABK00 = {
'BKFNR': dict(name='firma', format='A2', required=True, default='01',
               doc='''Prüfung erfolgt gegen den Firmenparameter (XF-Satz) Steuerung Auftragsbearbeitung.
                      Gültige Werte sind: *FIRM gültige Firma'''),
'BKABT': dict(name='abteilung', format='S4.0', required=True, default=1,
               doc='Prüfung: Abteilungsdatei'''),
'BKVGNR': dict(name='vorgang', format='S9.0', required=True,
               doc='''Dazu haben wir folgenden Hinweis bekommen: "Die zu vergebenen Vorgangsnummern werden
                      in dem Datenbereich 'ADTASTAPEL' abgelegt. Ablesbar mit dem Befehl:
                      DSPDTAARA DTAARA(ADTASTAPEL). Um doppelte Sätze in den Schnittstellendateien zu
                      verhindern wird auch gegen diesen Datenbereich geprüft." Der Inhalt des Datenbereiches
                      scheint aus meiner (md) Sicht auf unserem System inkonsistent.'''),
'BKKDNR': dict(name='kundennr', format='A8', required=True,
               doc='''kundennr - wird in der restlichen SoftM Dokumentation als "Warenempfänger Kunde"
                      betitelt.'''),
'BKVGPO': dict(name='vorgangspositionszahl', format='S5.0', required=True,
               doc='''Anzahl Vorgangspositionen) meint die noch nicht übernommenen Positionen für diesen
                      Vorgang. Vermutung: Muss beim erstellen mit der Gesammtzahl der Positionen vorbelegt
                      werden.'''),
'BKLFDS': dict(name='stapelsatznummer', format='S11.0',
               doc='''Lfd. Nummer Stapelsatz Nummer des Satzes aus der Originalstapeldatei, die zur
                      Erstellung des Kopfsatzes führte. Ist in userem System in der Regel NULL.'''),
'BKAUFA': dict(name='auftragsart', format='A2', default='',
               doc='''Als Auftragsart darf nur ein Wert angegeben werden, der als zulässige Auftragsart in
                      der Parameterdatei hinterlegt ist. Sie steuert, wie der Auftrag im System abgewickelt
                      werden soll. So kann u.a. auftragsartenabhängig festgelegt werden:
                      - Soll die Auftragsbestätigung gedruckt werden?
                      - Muss die bezogene Rechnung angegeben werden? (z.B. bei Gutschriften/Nachbelastungen)
                      - Ist Streckengeschäft zulässig?
                      - Soll die Kreditlimitprüfung durchgeführt werden?
                      - Sind Sofortformulare möglich bzw. Pflicht?
                      - Sind Sammelbelege möglich (Lieferschein, Rechnung)?
                      - Welche Schnittstellen (Lager, Statistik, FiBu, Kostenrechnung) sollen versorgt werden?
                      Zum Teil werden diese Festlegungen auch verwendet, um Eingabefelder vorzubesetzen.
                      Falls keine Auftragsart angegeben ist, dann wird die Standard-Auftragsart für im
                      Stapelübernommene Aufträge lt. Parameter 'Steuerung Stapelschnittstelle' eingesetzt.
                      Typische Werte bei HUDORA: U=Umlagerungsauftrag, WA=WerbeAuftrag'''),
'BKNRKD': dict(name='kundenauftragsnr', format='A20',
               doc='''AAK00.AKNRKD Kundenauftrag - Nummer, unter der der Auftrag beim Kunden geführt wird.
                      Diese Information wird in Auftragsformularen gedruckt. Zusätzlich ist die Suche eines
                      Auftrags über die Kundenauftragsnummer möglich. Automatisch erstellte Aufträge
                      erhalten eine besondere Kennung.
                      Auftrag aus EDI: 'EDI*' + Auftrag
                      Auftrag aus Retouren: 'RETOURENANF.' + Retoure
                      Umlagerung aus Dispo: Verursacher laut SSt Einkauf
                      Auftrag aus Bestellung: 'BSTN:' + Bestellnummer
                      (md) Nachschubauftrag von Nachschieber: N: + Anliefertermin
                      Auftragserfassung: Falls hier eine Eingabe erfolgt, wird sofort in die Erfassung der
                      Positionen verzweigt. Das Format für die Kopfdateneingabe wird übersprungen
                      (s. Stichwort "schnelle Eingabe"). Wahlweise Angabe, keine Prüfung.'''),
'BKLGNR': dict(name='abgangslager', format='S4.0',
               doc='''Wahlweise Angabe, Prüfung über Lagerstammdatei LLG00. Die Vorbesetzung erfolgt
                      laut Kundensatz.'''),
'BKLGNZ': dict(name='zugangslager', format='S4.0',
               doc='''Fertigungszugangslager Falls es sich bei einem Vorgang um eine Umlagerung handelt, kann
                      hier die Nummer des empfangenden Lagers angegeben werden.'''),
'BKDTKD': dict(name='bestelldatum', format='P7.0',
               doc='''AAK00.AKDTKD Datum, das der Kunde bei der Auftragserteilung mitgeteilt hat. Das Datum
                      ist als Information für den Kunden gedacht. Wahlweise Angabe,
                      Prüfung auf formale Richtigkeit. Wird zum Teil auch "Kundenauftragsdatum" genannt'''),
'BKDTKW': dict(name='kundenwunschtermin', format='P7.0', default='',
               doc='''AAK00.AKDTKW Termin, zu dem der Kunde die Ware erhalten soll. Für unbestimmte Termine
                      oder Rahmenkonditionen kann der Sonderwert '999999' eingegeben werden. Dieser Termin
                      kann die Konditionsermittlung beeinflussen. Der Termin kann über die automatische
                      Terminermittlung bei der Erfassung vorbesetzt werden.'''),
'BKJWKW': dict(name='kundenwunschtermin_woche', format='P5.0',
               doc='''AAK00.AKJWKW Termin, zu dem der Kunde die Ware erhalten soll. Für unbestimmte Termine
                      oder Rahmenkonditionen kann der Sonderwert '9999' eingegeben werden.'''),
'BKDTLT': dict(name='anliefertermin', format='P7.0',
               doc='''AAK00.AKDTLT Termin, zu dem die Ware den Kunden erreichen soll. Für unbestimmte
                      Termine oder Rahmenkonditionen kann der Sonderwert '999999' eingegeben werden. Dieser
                      Termin steuert den Zeitpunkt der Zuteilung und der Belegschreibung. Außerdem kann er
                      die Konditionsermittlung und die Berechnung des Kreditlimits beeinflussen. Der Termin
                      kann über die automatische Terminermittlung bei der Erfassung vorbesetzt werden.'''),
'BKJWLT': dict(name='liefertermin_woche', format='P5.0',
               doc='''AAK00.AKJWLT Termin, zu dem die Ware das Lager verlassen soll. Für unbestimmte Termine
                      oder Rahmenkonditionen kann der Sonderwert '9999' eingegeben werden. Dieser Termin
                      steuert den Zeitpunkt der Zuteilung und der Belegschreibung. Außerdem kann er die
                      Konditionsermittlung und die Berechnung des Kreditlimits beeinflussen.'''),
'BKLTFX': dict(name='fixtermin', format='P3.0',
               doc='''Auch "Termin Fix" genannt. Feld hat nur Info-Charakter. * Im Standard nicht/noch nicht
                      unterstützt * - Aber auf dem HUDORA System. Gültige Werte sind:
                      *ZERO kein fixer Termin
                      001 fixer Termin'''),
'BKDTVA': dict(name='valuta_erfassung', format='P7.0',
               doc='''Diese Angabe erfolgt wahlweise. Es erfolgt eine formale Prüfung und eine logische, ob
                      das Datum gößer als das Tagesdatum ist.'''),
'BKHERK': dict(name='herkunft', format='A1',
               doc='''AAK00.AKFA4A Gültige Eingaben müssen in der Validierungsdatei unter dem zugehörigen
               Prüftyp hinterlegt sein. Hier kann ein Kürzel eingetragen werden, das die Herkunft des
               Auftrags kennzeichnet (z.B. F=Fax, T=Telefon, usw.).'''),
'BKANNR': dict(name='Angebot', format='P7.0',
               doc='''Für Aufträge aus dem Einkauf (BKF20A 4-8 = '*XEA*') steht hier die
                      verursachende Bestellnummer'''),
'BKVANR': dict(name='lieferadresse', format='P3.0',
               doc='Dreistellige Nummer, unter der zusätzliche Lieferadressen zum Kunden verwaltet werden.'),
'BKX3VA': dict(name='Versandart Auftrag', format='A3',
               doc='''Gültige Eingaben müssen in der Versandartendatei hinterlegt sein. Wahlweise Angabe.
               Vorbesetzung: Die Versandart laut Kundenzusatz des Warenempfängers/laut Lieferadresse'''),
'BKX3LB': dict(name='lieferbedingung', format='A3',
               doc='''Vorbesetzung: Lieferbedingung lt. Kundenzusatz des Warenempfängers'''),
'BKX3ZB': dict(name='Zahlungsbedingung', format='A3',
               doc='''Vorbesetzung: Zahlungsbedingung lt. Kundenzusatz des RgEmpfängers'''),
'BKKZSP': dict(name='Mit Übernahmesperre', format='S1.0',
               doc='''Gültige Werte sind:
                      *ZERO Vorgang kann übernommen werden
                      1 Übernahme nicht erlaubt'''),
'BKDTER': dict(name='erfassungsdatum', format='P7.0',
               doc='''Datum der Erfassung/Erstellung des Datensatzes.'''),
'BKOQZO': dict(name='Druckerzuordnung', format='A4',
               doc='''Prüfttyp für Validierung: PRT Falls vorgangsbezogen festgelegt werden soll, wo die
                      Formulare für diesen Vorgang gedruckt werden sollen, dann muss hier das
                      entsprechende Druckerkürzel eingeben werden.'''),
'BKKZTF': dict(name='teillieferung_zulaessig', format='S1.0', default=1,
               doc='''AAK00.AKKZTF Bei Teillieferung zulässig = nein wird der Kommissionierbeleg
                      bzw. Lieferschein erst dann gedruckt, wenn alle Positionen des Auftrags lieferbar sind.
                      Gültige Werte sind:
                      *ZERO Teillieferung nicht zulässig
                      1 Teillieferung zulässig'''),
'BKAUFN': dict(name='auftrag', format='P7.0',
               doc='''Nummer des Auftrags Angabe wird per Programm gefüllt, falls der Auftrag fehlerfrei war
                      und in den Auftragsbestand übernommen werden konnte. Es wird die vom System bei der
                      Übernahme vergebene Auftragsnummer hinterlegt.'''),
'BKAWRT': dict(name='Auftragswert', format='P15.2',
               doc='''Angabe in Hauptwährung.'''),
'BKEGCD': dict(name='eu_laendercode', format='A2',
               doc='''ISO ANZFELDART: UPTYP10 Hier ist der Sitz des Unternehmens zu hinterlegen. Für das
                      eigene Heimatland muss das Feld leer bleiben, da sonst eine steuerliche Behandlung wie
                      EU-Ausland durchgeführt würde.'''),
'BKSBNR': dict(name='sachbearbeiter', format='S6.0', default='1', required=True,
               doc='''Prüfung erfolgt gegen die Sachbearbeiterdatei, falls ungleich 0, wenn ein
                      entsprechender Ersatzwert Konfiguriert ist.'''),
'BKUSTN': dict(name='USt-IdNr', format='A12',
               doc='''EU-einheitliche Steuernummer (USt-IdNr, VAT-Nr). Beim Druck der USt-IdNr ist zusätzlich
                      noch der EU-Ländercode mitzudrucken.'''),
'BKRBP1': dict(name='Auftragsrabatt 1 in %', format='P5.2',
               doc='''Wahlweise Angabe (muss kleiner als 100 sein). Die Vorbesetzung erfolgt lt. Rabatt
                      aus Kundenzusatz.'''),
'BKRBP2': dict(name='Auftragsrabatt 2 in %', format='P5.2',
               doc='''Wahlweise Angabe (muss kleiner als 100 sein).'''),
'BKVRT': dict(name='Vertreter', format='A8',
               doc='''Wahlweise Angabe, Prüfung über Vertreterdatei XVR00 Vorbesetzung: Verteter
                      laut Kundenzusatz'''),
'BKWSL ': dict(name='waehrung', format='A3',
               doc='''Wahlweise Angabe, Prüfung über Parameterdatei Währungsangaben. Keine Angabe: Der
                      Währungsschlüssel lt. Kundenzusatz des Rg- Empfängers wird bei der Übernahme
                      eingesetzt.'''),
'BKKURS': dict(name='Wechselkurs', format='P9.6',
               doc='''Bei fehlender Angabe wird der Kurs bei der Übernahme eingesetzt.'''),
'BKKUFA': dict(name='Kursfaktor', format='P1.0',
               doc='''Festlegung, auf wieviele Fremdwährungseinheiten sich der Wechselkurs bezieht:
                      Gültige Werte sind:
                      *ZERO Kurs pro Fremdwährungseinheit
                      1 Kurs pro 10 Fremdwährungseinheiten
                      2 Kurs pro 100 Fremdwährungseinheiten
                      3 Kurs pro 1000 Fremdwährungseinheiten'''),
'BKX3R1': dict(name='Textschlüssel Rabatt 1', format='A3',
               doc='''In der Validierungsdatei kann zu diesem Rabattschlüssel eine Rabattgruppe hinterlegt
                      werden. Außerdem kann dort für Positionsrabatte angegeben werden, ob es sich um einen
                      Rabatt handelt, der den Positionswert vermindert, oder um einen Zuschlag, der
                      ihn erhöht.'''),
'BKX3R2': dict(name='Textschlüssel Rabatt 2', format='A3',
               doc='''In der Validierungsdatei kann zu diesem Rabattschlüssel eine Rabattgruppe hinterlegt
                      werden. Außerdem kan dort für Positionsrabatte angegeben werden, ob es sich um einen
                      Rabatt handelt, der den Positionswert vermindert, oder um einen Zuschlag, der
                      ihn erhöht.'''),
'BKKZRX': dict(name='Rabattkz Brutto', format='S1.0',
               doc='''Gültige Werte sind:
                      *ZERO Auftragsrabatt 2 auf (Wert - Rabatt 1)
                      1 Auftragsrabatt auf rabattfähigen PosWrt
                      Auftragsrabatt wird auf den rabattfähigen Positionswert gerechnet'''),
'BKKZNN': dict(name='Mit Nachnahme', format='S1.0',
               doc='''Gültige Werte sind: *ZERO keine Nachnahme 1 mit Nachnahme'''),
'BKMWKZ': dict(name='Steuerprofil/MWSt-Art', format='S2.0',
               doc='''AAK00.AKMWKZ Über das Mehrwertsteuerprofil erfolgt die Zuordnung des gültigen
                      Steuersatzes bzw. -schlüssels in der Auftragsbearbeitung und Finanzbuchhaltung.
                      Die zulässigen Steuerprofile sind in der Parameterdatei, Satzart Steuerprofile,
                      hinterlegt. Der Satzschlüssel ist Fa/X/H/Mxx mit xx = Steuerprofil lt.
                      Kundenstamm. Im Rahmen der Auftragsbearbeitung wird das Steuerprofil bei der
                      Erfassung mit dem Wert lt. Kundenstamm vorbesetzt und kann auftragsbezogen
                      überschrieben werden. Bei der Fakturierung wird der Mehrwertsteuersatz über die
                      Angaben Steuerprofil und Umsatzsteuerkennzeichen lt. Artikel ermittelt. Der dem
                      Steuersatz zugeordnete Schlüssel wird an die Buchhaltung übergeben. Bei einer
                      Mehrwertsteueränderung ist nur die Zuordnung Steuer- profil / Steuerschlüssel
                      und Sätze in der Parameterdatei zu ändern. Die Kennzeichen im Kundenstamm und im
                      Auftragskopf bleiben unverändert. Vorbesetzung mit dem Steuerprofil/MWSt-Art
                      lt. Rechnungsempfän- ger. Falls im Kundensaldensatz des Rechnungszahlers
                      der Wert 'Interne Firma (KSFNRK)' ungleich leer oder 00 ist, dann wird
                      das Mehrwertsteuerkennzeichen mit 0 vorbesetzt. Das Steuerprofil ist nur änderbar,
                      wenn im Parameter 'Mehrwertsteuerangaben' die entsprechenden Profile zugeordnet
                      sind (z.B. altes <-> neues Profil bei einer MWSt-Umstellung)'''),
'BKVPK ': dict(name='Verpackungskosten', format='P15.2'),
'BKVSK ': dict(name='versandkosten', format='P15.2'),
'BKNBT1': dict(name='Nebenkosten', format='P15.2'),
'BKX4N1': dict(name='Textschl zu Nebenko', format='A4',
               doc='''Gültige Eingaben müssen in der Textbausteindatei hinterlegt sein.'''),
'BKNBT2': dict(name='Nebenkosten 2', format='P15.2',
               doc=''' Im Standard nicht/noch nicht unterstützt *'''),
'BKNTY2': dict(name='Typ Nebenkosten 2', format='P1.0',
               doc=''' Im Standard nicht/noch nicht unterstützt *'''),
#'BKX4N2': dict(name='Textschl zu Nebenko2', format='A4',
#               doc='''Gültige Eingaben müssen in der Textbausteindatei hinterlegt sein.
#                      * Im Standard nicht/noch nicht unterstützt *'''),
# 'BKNBT3': dict(name='Nebenkosten', format='P15.2',
#               doc=''' Im Standard nicht/noch nicht unterstützt *'''),
# 'BKX4N3': dict(name='Textschl zu Nebenko3', format='A4',
#                doc='''Gültige Eingaben müssen in der Textbausteindatei hinterlegt sein.
#                       * Im Standard nicht/noch nicht unterstützt *'''),
# 'BKNTY3': dict(name='Typ Nebenkosten 3', format='P1.0',
#                doc=''' Im Standard nicht/noch nicht unterstützt *'''),
'BKTRNR': dict(name='Auslieferungstour', format='A5',
               doc='''Prüfttyp für Validierung: TOUR Nummer der Auslieferungstour. Dient der Zuordnung
                      des Kunden zu einer Tour. Die Tour wird bei der Erfassung eines Kundenauftrags
                      vorbesetzt und kann geändert werden. Sie wird in der Versandabwicklung zum
                      Erstellen einer Tourenliste verwendet.'''),
'BKPROB': dict(name='Objekt/Aktion', format='A10',
               doc='''Prüfttyp für Validierung: OBJ Zusätzliches Zugriffskriterium bei der
                      Konditionsermittlung in der Auftragsbearbeitung.
                      * Objekte: Über die Angabe eines Objekts können zeitraumbezogene Konditionen
                      definiert werden. Sie werden dann berücksichtigt, wenn:
                      a) bei der Auftragserfassung das entsprechende Objekt angegeben wird;
                      b) in der Auftragsart die automatische Objektfindung ausgewählt wurde
                         (PAAOBJ = W oder O), und für den Stichtag ein gültiges Objekt vorhanden ist.
                         Falls für denselben Zeitraum und dieselben Zuordnungsbegriffe eine objektbezogene
                         Kondition gültig ist, dann ersetzt diese die entsprechende nicht-objektbezogene
                         Kondition. Die zulässigen Objekte sind in der Validierungsdatei hinterlegt. Dort
                         kann auch eine Zeitspanne für die Gültigkeit des Objekts hinterlegt werden. Die
                         Kondition darf diese Zeitspanne nicht über- oder unterschreiten.
                      * Projekte: Projektbezogene Preise werden (im Gegensatz zum Objekt) grundsätzlich
                      nur berücksichtigt, wenn in der Auftragserfassung das entsprechende Projekt angegeben
                      wird. Die zulässigen Projekte sind in der Projektstammdatei hinterlegt. Die
                      SoftM Kostenrechnung muss eingesetzt sein.'''),
# 'BKZUOR': dict(name='Zuordnung dezent Anw', format='A10',
#                doc='''Reserviert, wenn SoftM Außendienst eingesetzt.'''),
# 'BKDTPG': dict(name='Neue Preise gültig ab', format='P7.0',
#                doc='''Funktion 'Preise gültig ab' im Standard noch nicht unterstützt. Für die Generierung
#                       von Leihaufträgen ist hier der Rückgabetermin anzugeben.'''),
# 'BKBSTN': dict(name='BestellnummerStrecke', format='P7.0',
#                doc='''* Im Standard nicht/noch nicht unterstützt *'''),
# 'BKLINR': dict(name='Lieferant Strecke', format='A8',
#                doc='''* Im Standard nicht/noch nicht unterstützt *'''),
'BKBZRG': dict(name='Bezogene Rechnung', format='P9.0',
               doc='''Bei Gutschriftsvorgängen kann hier die bezogene Rechnungsnummer übergeben werden.'''),
'BKLFSN': dict(name='Lieferschein', format='P7.0',
               doc='''Für Vorgänge, die direkt in die Fakturierungsschnittstelle übernommen werden sollen,
                      kann hier die Lieferscheinnummer übergeben werden.'''),
# 'BKRGNR': dict(name='Rechnung', format='P9.0',
#                doc='''Für Vorgänge, die direkt in die Fakturierungsschnittstelle übernommen werden
#                       sollen, kann hier die Rechnungsnummer übergeben werden.
#                       * Im Standard nicht/noch nicht unterstützt *'''),
'BKKZBA': dict(name='bearbeitungskennzeichen', format='S1.0',
               doc='''Festlegung des Bearbeitungsstandes. Gültige Werte sind:
                      *ZERO nicht geprüft
                      1 geprüft, Warnungen aufgetreten
                      2 geprüft, Fehler aufgetreten
                      3 geprüft, kann übernommen werden
                      4 wurde übernommen
                      Komplett übernommene Vorgänge werden sofort im Anschluß an die Übernahme
                      logisch gelöscht.

                      Empfehlung von SoftM um Race-Conditions zu vermeiden: wenn ein Datensatz in die
                      Schnittstelle geschrieben wird, BKKZBA=4 setzen, bis alle Positionen geschrieben sind,
                      dann BKKZBA auf 0 setzen.
                      Vergleiche Fall 213: Stapelschnittstelle von Python aus ansprechen.'''),
'BKKZB1': dict(name='Bearbeitung Teil 1', format='S1.0',
               doc='''Gültige Werte sind:
               2 geprüft, schwere Fehler
               3 geprüft, in Ordnung
               *ZERO nicht geprüft
               1 geprüft, nur Warnungen'''),
'BKKZB2': dict(name='Bearbeitung Teil 2', format='S1.0',
               doc='''ABK00.BKKZB1 Gültige Werte sind:
                      2 geprüft, schwere Fehler
                      3 geprüft, in Ordnung
                      *ZERO nicht geprüft
                      1 geprüft, nur Warnungen'''),
'BKVGPU': dict(name='Anzahl übern Pos', format='S5.0'),
# 'BKTXTU': dict(name='Anzahl übern Vorgangst', format='P5.0', doc='''Nicht verwendet'''),
'BKVGPF': dict(name='Anzahl fehlerh Pos', format='S5.0',
               doc='''Nur fehlerfreie Positionen können übernommen werden.'''),
'BKTXTF': dict(name='Anzahl fehlerh Vorgangst', format='P5.0'),
'BKKZ01': dict(name='Vorgang aus Shop', format='S1.0',
               doc='''Für Vorgänge aus dem Shop wird eine zusätzliche Datei ABB00 versorgt.
                      Gültige Werte sind:
                      *ZERO kein Vorgang aus Shop
                      1 Vorgang aus Shop'''),
'BKKZ02': dict(name='Tour einsetzen', format='S1.0',
               doc='''Gültige Werte sind: *ZERO Tour aus Übergabe 1 Tour eingesetzt'''),
'BKKZ03': dict(name='Steuerung Übernahme', format='S1.0',
               doc='''Bearbeitungszustand vor letzter Übernahme, benötigt für den Wiederanlauf AAU29.
                      Gültige Werte sind:
                      *ZERO Funktion nicht eingesetzt
                      1 Funktion eingesetzt'''),
'BKVGAR': dict(name='vorgangsart', format='A4',
               doc='''APX00ADG45.PXPKEY Es gibt besondere Vorgangsarten (#RET, *AAK, *XEA, *ITS, *CSA),
                      die sich vom Standardparameter nicht nur durch die Parameterisierung unterscheiden.
                      Für alle anderen Vorgangsarten (*NO*BLANK) kann über einen eigenen Parametersatz
                      eine unterschiedliche Verarbeitung in SSt Auftrag erreicht werden, z.B.
                      Angebot erzeugen. Gültige Werte sind:
                      *BLANK Standardparameter
                      #RET Auftrag aus Retourenverwaltung
                      *AAK Änderung eines existierenden Auftrags
                      *XEA Auftrag aus Bestellung (XEA00)
                      *ITS Auftrag aus Intershop
                      *CSA Auftrag aus B2B /CSA
                      *OFF Auftrag aus Offline-Warenkorb
                      *TST Übernahme Altaufträge in neue Vorgänge
                      *NO*BLANK eigener Standardparameter'''),
# 'BKTXTO': dict(name='Anzahl Vorgangstexte', format='P5.0', doc='''Nicht verwendet'''),
'BKKE01': dict(name='Auftragsart eingesetzt', format='S1.0',
               doc='''Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
                      *ZERO StandardauftragsA lt. StplSSt einsetzen
                      1 AufA bei StapelPrf eingesetzt
                      2 AufA bei DialogKorr auf 0 ändern
                      3 AufA 0 bei StplPrf nicht überschreiben'''),
'BKKE02': dict(name='Nummer Lieferadr einges', format='S1.0',
               doc='''Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
                      *ZERO LfAdr lt. KdStamm/Warenempfänger
                      1 LfAdr bei Stapelprüfung eingesetzt
                      2 LfAdr bei DialogKorr auf 0 gesetzt
                      3 LfAdr 0 bei StpPrf nicht überschreiben'''),
'BKKE03': dict(name='Wunschtermin eingesetzt', format='S1.0',
               doc='''Gültige Werte sind: *ZERO Wunschtermin aus Übergabe 1 Wunschtermin eingesetzt'''),
'BKKE04': dict(name='Liefertermin eingesetzt', format='S1.0',
               doc='''Gültige Werte sind: *ZERO Liefertermin aus Übergabe 1 Liefertermin eingesetzt'''),
'BKKE05': dict(name='Rabatt eingesetzt', format='S1.0',
               doc='''Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
                      *ZERO Aktuelle Rabatte lt. Kundenstamm
                      1 Rabatte bei Stapelprüfung ergänzen
                      2 Rabatte bei DialogKorr auf 0 ändern
                      3 Rabatte nicht ergänzen'''),
'BKKE06': dict(name='Vertreter eingesetzt', format='S1.0',
               doc='''Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
                      *ZERO Vertreter aus KdStamm/WarenEmpf
                      1 Vertreter bei Stapelprüfung eingesetzt
                      2 Vertreter bei DialogKorr auf 0 geändert
                      3 Vertreter bei StapelPrf nicht ändern'''),
'BKKE07': dict(name='Lagernummer eingesetzt', format='S1.0',
               doc='''Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
                      *ZERO LagerNr lt. KdStamm/Warenempfänger
                      1 LagerNr bei Stapelprüfung eingesetzt
                      2 LagerNr bei DialogKorr auf 0 gesetzt
                      3 LagerNr 0 bei StpPrf nicht überschreiben'''),
'BKKE08': dict(name='Versandart eingesetzt', format='S1.0',
               doc='''Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
                      *ZERO Versandart aus Übergabe
                      1 Versandart bei Stapelprüfung eingesetzt
                      2 Versandart bei DialogKorr auf 0 gesetzt
                      3 Versandart nicht überschreiben'''),
'BKDFSL': dict(name='dateifuehrungsschluessel', format='A10',
               doc='''Dieses Feld wird von dem Programm, das eine Änderung an Daten dieses Satzes vornimmt,
                      während des Änderungsvorgangs mit der Bildschirmidentifikation des ändernden
                      Bildschirms belegt.'''),
'BKSTAT': dict(name='Satzstatus', format='A1',
               doc='''Gültige Werte sind: *BLANK Satz ist aktiv X Satz steht zum Löschen an (auf diesen
                      Satz kann in den Anwendungsprogrammen nicht mehr zugegriffen werden'''),
# 'BKKE09': dict(name='Lieferbedingung eingesetzt', format='S1.0',
#                 Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
#                 *ZERO LieferBed aus Übergabe
#                 1 LieferBed bei Stapelprüfung eingesetzt
#                 2 LieferBed bei DialogKorr auf 0 gesetzt
#                 3 LieferBed nicht überschreiben'''),
# 'BKKE10': dict(name='Zahlungsbedingung eingesetzt', format='S1.0',
#                  Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
#                  *ZERO ZahlungsBed aus Übergabe
#                  1 ZahlungsBed bei Stapelprüfung eingesetzt
#                  2 ZahlungsBed bei DialogKorr auf 0 gesetzt
#                  3 ZahlungsBed nicht überschreiben'''),
# 'BKKE11': dict(name='Rabatttext 1 eingesetzt RbTxt egsz  S   1.0
#                Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
#                *ZERO Rabatttext aus Übergabe
#                1 Rabatttext bei Stapelprüfung eingesetzt
#                2 Rabatttext bei DialogKorr auf 0 gesetzt
#                3 Rabatttext nicht überschreiben'''),
# 'BKKE12': dict(name='Rabatttext 2 eingesetzt', name='S1.0',
#                 BKKE11 Die Werte 1 und 2 werden intern verwaltet. Gültige Werte sind:
#                 *ZERO Rabatttext aus Übergabe
#                 1 Rabatttext bei Stapelprüfung eingesetzt
#                 2 Rabatttext bei DialogKorr auf 0 gesetzt
#                 3 Rabatttext nicht überschreiben '''),
# 'BKKE13': dict(name='Endepreise eingesetzt'   S   1.0
#               Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKE14': dict(name='Kz Nachnahme eingesetzt S   1.0 324 324
#                 Gültige Werte sind: *ZERO Kz Nachnahme bei Stapelprüfung ersetzen
#                 1 Kz Nachnahme bei Stapelprüfung ersetzt 3 Kz Nachnahme nicht ändern'''),
# 'BKKE15': dict(name='Kennzeichen Kz J/N  S   1.0 325 325 XREF. XREFF01. XXKZ
#                Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKE16': dict(name='Kennzeichen Kz J/N  S   1.0 326 326 XREF. XREFF01. XXKZ
#                 Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKE17': dict(name='Kennzeichen Kz J/N S 1.0 327 327 XREF. XREFF01. XXKZ
#                 Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKE18': dict(name='Kennzeichen Kz J/N  S   1.0 328 328 XREF. XREFF01. XXKZ
#                 Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKE19': dict(name='Kennzeichen Kz J/N  S   1.0 329 329 XREF. XREFF01. XXKZ
#                 Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKE20': dict(name='Kennzeichen Kz J/N  S   1.0 330 330 XREF. XREFF01. XXKZ
#                 Gültige Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt'''),
# 'BKKZSR': dict(name='Mit Sortimentsrabatt Kz SrtmRb  S   1.0 331 331 XREF. XR
#                 Wird generell bei der Prüfung eingesetzt, keine Eingabe. Gültige Werte sind:
#                 *ZERO kein Sortimentsrabatt 1 Sortimentsrabatt'''),
# 'BKKZA1': dict(name='Rabatt 1 auftragswertabhängig Rb1 wrtabh    S   1.0 332
#                 Wird generell bei der Prüfung eingesetzt, keine Eingabe Gültige Werte sind:
#                 *ZERO Rabatt nicht auftragswertabhängig 1 Rabatt auftragswertabhängig'''),
# 'BKKZA2': dict(name='Rabatt 2 auftragswertabhängig Rb2 wrtabh    S   1.0 333
#                   ABK00. ABK00F01. BKKZA1 Wird generell bei der Prüfung eingesetzt, keine Eingabe
#                  Gültige Werte sind: *ZERO Rabatt nich auftragswertabhängig
#                  1 Rabatt auftragswertabhängig'''),
'BKKDRG': dict(name='rechnungsempfaenger', format='A8',
               doc='Wird generell bei der Prüfung eingesetzt, keine Eingabe.'),
'BKKDPR': dict(name='konditionskunde', format='A8',
               doc='''Kunde für Konditionsermittlung. "Wird generell bei der Prüfung eingesetzt,
                      keine Eingabe."
                     Scheinbar wir dieses Feld tatsächlich NICHT eingesetzt, die SoftM Dokumentation
                     scheint hier fehlerhaft zu sein.'''),
# 'BKIAKZ': dict(name='Inland/Ausland In/Asld  S   1.0 350 350 XREF. XREFF01. XXIAKZ
#                    Festlegung, ob Lieferant am Inlands- bzw. Auslandzahlungsverkehr teilnimmt. Bei
#                    Kunden wird normalerweise immer eine 0 angegeben. Für Mischkonten kann die Auswahl
#                    1 bzw. 2 angegeben werden. Wird generell bei der Prüfung eingesetzt, keine Eingabe
#                    Gültige Werte sind: *ZERO Inlandszahlungsverkehr / Lastschriften Der Lieferant wird
#                    über den Inlandszahlungsverkehr abgewickelt. Der Kunde kann am Lastschriftverfahren
#                    bzw. an der debitorischen Auszahlungen teilnehmen. 1 Auslandszahlungsverkehr
#                    2 Inlands- und Auslandszahlungsverkehr Alle OP's in Hauptwährung werden im
#                    Inlandszahlungsverkehr beglichen. Alle OP's in Währungen werden im
#                    Auslandszahlungsverkehr bezahlt.'''),
# 'BKKZLH': dict(name='Mit Leihgebindeverwaltung LhGbdVrwa S   1.0 351 351 XREF. XREF
#                    F01. XXKZ Wird generell bei der Prüfung eingesetzt, keine Eingabe. Gültige Werte sind:
#                    *ZERO ohne Leihgebindeverwaltung
#                    1 mit Leihgebindeverwaltung Abgegebene und zurückerhaltene Leihgebinde werden für
#                    den Kunden erfasst. Der Bestand an Leihgebinden bei diesem Kunden kann als Liste
#                    gedruckt oder am Bildschirm abgerufen werden.'''),
# 'BKPREL': dict(name='Kundengruppe KundenGrp  A   3   352 354 XREF. XREFF01. XXKGRP
#                    Prüfttyp für Validierung:   KGR Zusammenfassung von Kunden zu verschiedenen Gruppen.
#                    Wird generell bei der Prüfung eingesetzt, keine Eingabe.'''),
# 'BKDTAE': dict(name='Änderungsdatum ÄndDatum P   7.0 355 358 XREF. XREFF01. XXDTAE
#                    Datum der letzten Änderung/Korrektur'''),
# 'BKDTPR': dict(name='Datum letzte Prüfung Dt ltz Prf P   7.0 359 362 XREF. XREFF01.
#                     XXDT
# 'BKSBAE': dict(name='Sachbearbeiter Änderung Sb Änd  S   6.0 375 380 XREF. XREFF01.
#                     XXSBAE Das Feld wird per Programm bei der Korrektur fehlerhafter Stapelsätze mit der
#                     Sachbearbeiternummer lt. Anmeldung gefüllt.
# 'BKFNRB': dict(name='Firma Fa    A   2   Gültige Werte sind: *FIRM gültige Firma
# 'BKIK01': dict(name='Feld 1A Feld 1A A   1   383 383 XREF. XREFF01. XXA1
# 'BKIK02': dict(name='Feld 1A Feld 1A A   1   384 384 XREF. XREFF01. XXA1
# 'BKIK03': dict(name='Feld 1A Feld 1A A   1   385 385 XREF. XREFF01. XXA1
# 'BKIK04': dict(name='Feld 1A Feld 1A A   1   386 386 XREF. XREFF01. XXA1
# 'BKIK05': dict(name='Feld 1A Feld 1A A   1   387 387 XREF. XREFF01. XXA1
# 'BKIK06': dict(name='Reservefeld RsvFld  A   3   388 390 XREF. XREFF01. XXA3
# 'BKIK07': dict(name='Reservefeld RsvFld  A   3   391 393 XREF. XREFF01. XXA3
# 'BKIK08': dict(name='Reservefeld RsvFld  A   3   394 396 XREF. XREFF01. XXA3
# 'BKIK09': dict(name='Reservefeld RsvFld  A   3   397 399 XREF. XREFF01. XXA3
# 'BKIK10': dict(name='Reservefeld RsvFld  A   3   400 402 XREF. XREFF01. XXA3
# 'BKIK11': dict(name='Reservefeld RsvFld  A   10  403 412 XREF. XREFF01. XXA10
# 'BKIK12': dict(name='Reservefeld RsvFld  A   20  413 432 XREF. XREFF01. XXA20
}


ABA00 = {
'BAVGNR': dict(name='vorgang', format='S9.0', required=True, key=True,
               doc='''Über die Vorgangsnummer wird die Verbindung zu den Kopfdaten in der Datei
               ABK00 hergestellt. (Pflichtfeld)'''),
'BAVGPO': dict(name='vorgangsposition', format='S5.0', required=True, key=True,
               doc='''Lfd. Positionsnumerierung innerhalb eines Vorgangs.'''),
'BAFNR': dict(name='firma', format='A2', required=True, default='01',
               doc='''Prüfung erfolgt gegen den Firmenparameter (XF-Satz)'''),
'BAABT': dict(name='abteilung', format='S4.0', required=True, default=1,
               doc='''Prüfung erfolgt gegen die Abteilungsdatei.'''),
'BAARTN': dict(name='artikel', format='A20', required=True,
               doc='''Zur Festlegung des Artikels ist die Artikelnummer anzugeben.'''),
'BAMNG': dict(name='bestellmenge', format='P11.3', required=True),
'BADTLT': dict(name='anliefertermin', format='P7.0', # required=True,
               doc='''APDTLT Termin, zu dem die Ware das Lager verlassen soll. Für unbestimmte Termine oder
                      Rahmenkonditionen kann der Sonderwert '999999' eingegeben werden. Dieser Termin
                      steuert den Zeitpunkt der Zuteilung und der Belegschreibung. Außerdem kann er die
                      Konditionsermittlung und die Berechnung des Kreditlimits  beeinflussen.'''),
'BAJWLT': dict(name='liefertermin_kw', format='P5.0',
               doc='''Termin, zu dem die Ware das Lager verlassen soll. Für unbestimmte Termine oder
               Rahmenkonditionen kann der Sonderwert '9999' eingegeben werden. Dieser Termin steuert den
               Zeitpunkt der Zuteilung und der Belegschreibung. Außerdem kann er die Konditionsermittlung
               und die Berechnung des Kreditlimits beeinflussen.'''),
'BALFDS': dict(name='stapelsatznummer', format='S11.0',
               doc='''Nummer des Satzes aus der Originalstapeldatei, die zur Erstellung des Positionssatzes
               führte. Umlagerung aus Neuer Disposition: Nummer des verursachenden Stapelsatzes
               aus SSt Einkauf'''),
'BAANNR': dict(name='Angebot', format='P7.0',
               doc='''angebotsnummer, die vom System automatisch vergeben wird oder auch manuell
               vorgegeben werden kann. Der Nummernbereich wird im Parameter 'Nummernkreise' festgelegt.
               Für Aufträge aus dem Einkauf (BKF20A 4-8 = '*XEA*') steht hier die verursachende
               Bestellnummer'''),
'BAANPO': dict(name='angebotsposition', format='P5.0',
               doc='''Für Aufträge aus dem Einkauf (BKVGAR = '*XEA') steht hier die verursachende
               Bestellposition. Umlagerung aus Neuer Disposition: Position des verursachenden Stapelsatzes
               aus SSt Einkauf'''),
'BAEAN': dict(name='ean', format='P13.0',
               doc='''Falls die EAN-Nummer angegeben ist, wird die Artikelnummer nur dann berücksichtigt,
               falls über die EAN-Nummer die Artikelnummer nicht zugeordnet werden konnte. Prüfung erfolgt
               gegen die Artikelzusatzdatei XAZ00.'''),
'BAKZSO': dict(name='Sonderartikel', format='S1.0',
               doc='''Festlegung, ob es sich um einen Sonderartikel handelt. Gültige Werte sind:
               *ZERO Artikel Normalartikel. Im Artikelstamm muss ein Stammsatz vorhanden sein.
               1 Sonderartikel Im Artikel darf unter der angegebenen Nummer kein Stammsatz vorhanden sein.
               Für Sonderartikel ist keine Bestandsführung möglich.'''),
'BAARTG': dict(name='Artikelhauptgruppe', format='A6',
               doc='''Für eine zusammenfassende Bearbeitung können ähnliche Artikel zu Artikelgruppen
               und -hauptgruppen zusammengefasst werden. Die Artikelgruppe muss im Artikelgruppenstamm
               angelegt und einer Hauptgruppe zugeordnet sein. Die Prüfung erfolgt gegen die
               Artikelstammdatei XAR00. Die Vorbesetzung erfolgt ebenfalls lt. Artikelstamm. Eine Eingabe
               ist nur zulässig bei Sonderartikeln, bei Nicht- Sonderartikeln darf das Feld nicht gefüllt
               sein. Ist bei Sonder- artikeln nur die Artikelgruppe angegeben, dann wird die Haupt- gruppe
               lt. Artikelgruppendatei eingesetzt. Dies ist ein Pflichtfeld bei Sonderartikeln.'''),
'BAARTH': dict(name='Artikelhauptgruppe', format='A6',
               doc='''Für eine zusammenfassende Bearbeitung können ähnliche Artikel zu Artikelgruppen
               und -hauptgruppen zusammengefasst werden. Die Artikelgruppe muss im Artikelgruppenstamm
               angelegt und einer Hauptgruppe zugeordnet sein. Die Prüfung erfolgt gegen die
               Artikelstammdatei XAR00. Die Vorbesetzung erfolgt ebenfalls lt. Artikelstamm. Eine Eingabe
               ist nur zulässig bei Sonderartikeln, bei Nicht-Sonderartikeln darf das Feld nicht gefüllt
               sein. Ist bei Sonderartikeln nur die Artikelgruppe angegeben, dann wird die Hauptgruppe
               lt. Artikelgruppendatei eingesetzt.'''),
'BALGNR': dict(name='Lager', format='S4.0',
               doc='''Prüfung: Lagerstammdatei LLG00, Bestandsführungsdatei XLF00 Angabe ist wahlweise.
               Vorbesetzung: Lager lt. Auftragskopf bzw. Artikelstamm'''),
'BAMESL': dict(name='Mengeneinheit', format='S2.0',
               doc='''Gültige Eingaben müssen in der Validierungsdatei unter dem zugehörigen Prüftyp
               hinterlegt sein. Nur zulässig bei Sonderartikeln, bei Nicht-Sonderartikeln darf das Feld
               nicht gefüllt sein. Eine Vorbesetzung erfolgt mit der Bestandsführungseinheit lt.
               Artikelstamm.'''),
'BAKZBE': dict(name='Ohne Bestandsführung', format='S1.0',
               doc='''Festlegung, ob der Artikel im Rahmen der Lagerhaltung bestands- mäßig geführt wird.
               Gültige Werte sind: *ZERO mit Bestandsführung 1 ohne Bestandsführung'''),
#'BAGANZ': dict(name='Bestellte Gebinde', format='P5.0',
#               doc='''* Im Standard nicht/noch nicht unterstützt *'''),
#'BAVOLM': dict(name='Inhalt VE pro Gebinde', format='P15.5',
#               doc='''* Im Standard nicht/noch nicht unterstützt *'''),
#'BAGEWI': dict(name='Gewicht', format='P09.3',
#               doc='''* Im Standard nicht/noch nicht unterstützt *'''),
'BAPREV': dict(name='verkaufspreis', format='P15.2',
               doc='''Falls angegeben, Prüfung gegen Preisdatei. Liegt der angegebene Preis um xx,x %
               unter dem der Preisdatei, wird entsprechend der Festlegung im Parameter
               'Steuerung Stapelschnittstelle' ein W- oder ein F-Fehler ausgewiesen. Der
               Unterschreitungsprozentsatz ist ebenfalls im Parametersatz 'Steuerung Stapelschnittstelle'
               anzugeben. Bei der Prüfung der Preisabweichung bleiben eventuell angegebene
               Rabatte berücksichtigt.'''),
'BAKZPR': dict(name='Preisdimension Verkauf', format='S1.0',
               doc='''Festlegung, für welche Menge der eingegebene Verkaufspreis gilt. Bei der
               Auftragserfassung wird die Eingabe mit der VerkaufsPreisdimension lt. Artikelstamm
               vorbesetzt.'''),
'BAPEBN': dict(name='Zugriff Preisdatei', format='S2.0',
               doc='''Zulässige Ebenen. Gültige Werte sind:
               *ZERO keine Definition
               01 Artikel
               02 Preisgruppe
               03 Artikelgruppe
               04 Artikelhauptgruppe
               06 Kunde/Artikel
               07 Kunde/Preisgruppe
               08 Kunde/Artikelgruppe
               09 Kunde/Artikelhauptgruppe
               10 Angebot: Kunde/Artikel
               11 Preisliste/Artikel
               12 Preisliste/Preisgruppe
               13 Preisliste/Artikelgruppe
               14 Preisliste/Artikelhauptgruppe
               16 Kundennummer
               17 Preisliste
               18 Naturalrabatt: Kunde/Artikel
               19 Naturalrabatt: Kunde/Preisgruppe
               20 Naturalrabatt: Kunde/Artikelgruppe
               21 Naturalrabatt: Kunde/Artikelhauptgruppe
               23 Naturalrabatt: Preisliste/Artikel
               24 Naturalrabatt: Preisliste/Preisgruppe
               25 Naturalrabatt: Preisliste/Artikelgruppe
               26 Naturalrabatt: Preisliste/ArtikelHGrp'''),
'BAMEPR': dict(name='Mengeneinheit Preis', format='S2.0',
               doc='''Gültige Eingaben müssen in der Validierungsdatei unter dem zugehörigen Prüftyp
               hinterlegt sein. Von der Mengeneinheit der Bestandsführung abweichende Mengeneinheit
               für den Verkaufspreis. Sämtliche für diesen Artikel  angelegten Verkaufspreise beziehen
               sich nicht auf die Mengeneinheit der Bestandsführung, sondern auf die hier angegebene
               Mengeneinheit (falls angegeben),'''),
'BAX4TR': dict(name='Transaktionscode', format='A4',
               doc='Die gültigen Transaktionscodes sind in den Auftragsparametern (Satzart N) hinterlegt.'),
'BAMNGL': dict(name='Verfügbare Menge', format='P11.3'''),
'BAAUPS': dict(name='Bezogene Position', format='P5.0',
               doc='''Wird ein Set bei der Erfassung aufgelöst, d.h. je Komponente eine eigene Position
               angelegt, so sollen für den Druck diese Positionen als zusammengehöriger Block erkennbar
               sein. Beim Setartikel stimmen Position und bezogene Position überein.'''),
'BADTLN': dict(name='Neuer Liefertermin', format='P7.0',
               doc='''Bei Positionen mit Rückstandsverwaltung wird nach der ersten Teillieferung der
               'Liefertermin' durch den 'Liefertermin neu' ersetzt. Für unbestimmte Termine oder
               Rahmenkonditionen kann der     Sonderwert '999999' eingegeben werden.'''),
'BADTKD': dict(name='Kundenwunschtermin', format='P7.0',
               doc='''Termin, zu dem der Kunde die Ware erhalten soll. Für unbestimmte Termine oder
               Rahmenkonditionen kann der  Sonderwert '999999' eingegeben werden. Dieser Termin steuert
               den Zeitpunkt der Zuteilung und beeinflusst die Konditionsermittlung.'''),
'BADFSL': dict(name='Dateifuehrungsschluessel', format='A10',
               doc='''Dieses Feld wird von dem Programm, das eine Änderung an Daten dieses Satzes
               vornimmt, während des Änderungsvorgangs mit der Bildschirmidentifikation des ändernden
               Bildschirms belegt.'''),
'BASTAT': dict(name='Satzstatus', format='A1',
               doc='''Gültige Werte sind: *BLANK Satz ist aktiv   X Satz steht zum Löschen an (auf diesen
               Satz kann in den Anwendungsprogrammen nicht mehr zugegriffen werden)'''),
# BAFAPR  Umrechnungsfaktor Preis in MEh Pr->MEh  P   09.Apr  128 132 XREF. XREFF01. XXFAPR
#               Dient zur Umrechnung von der Mengeneinheit/Preis in die Mengeneinheit der Bestandsführung,
#               abhängig von der Umrechnungsart: - Umrechnungsart 0 = Faktor Preis pro Mengeneinheit
#               BstdFhrg: = VkPreis * Faktor - Umrechnungsart 1 = Divisor Preis pro Mengeneinheit
#               BstdFhrg: = VkPreis / Divisor Der Verkaufspreis kann zusätzlich durch den Faktor
#               Preisdimension beeinflusst werden. Beispiel: Preisdimension: "PER 10" Faktor
#               (Preisdimension): 0,1 Mengeneinheit/Preis: Dutzend Umrechnungsart: 1 (d.h. Divisor)
#               Faktor (Einheit/Preis): 12 Verkaufspreis: 144,- Preis pro
#               Mengeneinheit/Preis: = 144,- * 0,1 --> 14,40 Preis pro Mengeneinheit
#               Bestandsführung: = 14,40 / 12 --> 1,20 Wert der Position -----> Wert: = Menge * 1,20'''),
# BAPREL  Listenpreis Brutto ListenPr P   15.2  133 140 XREF. XREFF01. XXPRE
#               Wird bei der Stapelprüfung eingesetzt.'''),
# BAMWAR  Steuerart des Artikels StA Art  S   1.0 141 141 XREF. XREFF01. XXMWAR
#         Zuordnung des Steuersatzes für den Artikel. * Im Standard nicht/noch nicht unterstützt *'''),
# BARBP1  Positionsrabatt PosRb   P   15.2  142 149 XREF. XREFF01. XARBBT
#               Wahlweise Angabe. Falls der Rabattprozentsatz/Betrag (1 bis 4)   angegeben ist, werden
#               die Rabatte lt. Preisdatei nicht berücksichtigt.
#               Eine Vorbesetzung erfolgt lt. Preisdatei'''),
# BARBP2  Positionsrabatt 2 PosRb 2   P   15.2  150 157 ABA00. ABA00F01. BARBP1
#               Wahlweise Angabe. Falls der Rabattprozentsatz/Betrag (1 bis 4)   angegeben ist, werden
#               die Rabatte lt. Preisdatei nicht berücksichtigt.
#               Eine Vorbesetzung erfolgt lt. Preisdatei'''),
# BARBP3  Positionsrabatt 3 PosRb 3   P   15.2  158 165 ABA00. ABA00F01. BARBP1
#               Wahlweise Angabe. Falls der Rabattprozentsatz/Betrag (1 bis 4)   angegeben ist, werden
#               die Rabatte lt. Preisdatei nicht berücksichtigt.
#               Eine Vorbesetzung erfolgt lt. Preisdatei'''),
# BARBP4  Positionsrabatt 4 PosRb 4   P   15.2  166 173 ABA00. ABA00F01. BARBP1
#               Wahlweise Angabe. Falls der Rabattprozentsatz/Betrag (1 bis 4)   angegeben ist, werden
#               die Rabatte lt. Preisdatei nicht berücksichtigt.
#               Eine Vorbesetzung erfolgt lt. Preisdatei'''),
# BAKZR1  Berechnung Rabatt 1 Brchn Rb    S   1.0 174 174 AAP00. AAP00F01. APKZR1
#              Festlegung der Berechnung Gültige Werte sind: *ZERO Rabatt in Prozent 1 Rabatt als Betrag
#              2 Rabatt als Betrag pro Menge abhängig von Preisdimension (z.B. Rabatt per 100)
#              3 Rabatt als Betrag pro Gebindeanzahl nur erlaubt, falls mit Gebinde gearbeitet wird.'''),
# BAKZR2  Berechnung Rabatt 2 Brchn Rb    S   1.0 175 175 AAP00. AAP00F01. APKZR2
#              Festlegung der Berechnung Gültige Werte sind: *ZERO Rabatt in Prozent 1 Rabatt als Betrag
#              2 Rabatt als Betrag pro Menge abhängig von Preisdimension (z.B. Rabatt per 100)
#              3 Rabatt als Betrag pro Gebindeanzahl nur erlaubt, falls mit Gebinde gearbeitet wird.'''),
# BAKZR3  Berechnung Rabatt 3 Brchn Rb    S   1.0 176 176 AAP00. AAP00F01. APKZR3
#              Festlegung der Berechnung Gültige Werte sind: *ZERO Rabatt in Prozent 1 Rabatt als Betrag
#              2 Rabatt als Betrag pro Menge abhängig von Preisdimension (z.B. Rabatt per 100)
#              3 Rabatt als Betrag pro Gebindeanzahl nur erlaubt, falls mit Gebinde gearbeitet wird.'''),
# BAKZR4  Berechnung Rabatt 4 Brchn Rb    S   1.0 177 177 AAP00. AAP00F01. APKZR4
#              Festlegung der Berechnung Gültige Werte sind: *ZERO Rabatt in Prozent 1 Rabatt als Betrag
#              2 Rabatt als Betrag pro Menge abhängig von Preisdimension (z.B. Rabatt per 100)
#              3 Rabatt als Betrag pro Gebindeanzahl nur erlaubt, falls mit Gebinde gearbeitet wird.'''),
# BAX3R1  Textschlüssel zu 'Rabatt 1' TxtSl Rb 1  A   3   178 180 XREF.   XREFF01. XAX3RB
#               In der Validierungsdatei kann zu diesem Rabattschlüssel eine Rabattgruppe hinterlegt
#               werden. Außerdem kann dort für Positionsrabatte angegeben werden, ob es sich um einen
#             Rabatt handelt, der den Positionswert vermindert, oder um einen Zuschlag, der ihn erhöht.'''),
# BAX3R2  Textschlüssel zu 'Rabatt 2' TxtSl Rb 2  A   3   181 183 ABA00. ABA00F01. BAX3R1
#               In der Validierungsdatei kann zu diesem Rabattschlüssel eine     Rabattgruppe hinterlegt
#               werden. Außerdem kann dort für   Positionsrabatte angegeben werden, ob es sich um einen
#           Rabatt handelt, der den Positionswert vermindert, oder um einen Zuschlag, der ihn erhöht.'''),
# BAX3R3  Textschlüssel zu 'Rabatt 3' TxtSl Rb 3  A   3   184 186 ABA00. ABA00F01. BAX3R1
#               In der Validierungsdatei kann zu diesem Rabattschlüssel eine Rabattgruppe hinterlegt
#               werden. Außerdem kann dort für Positionsrabatte angegeben werden, ob es sich um einen
#           Rabatt handelt, der den Positionswert vermindert, oder um einen Zuschlag, der ihn erhöht.'''),
# BAX3R4  Textschlüssel zu 'Rabatt 4' TxtSl Rb 4  A   3   187 189 ABA00. ABA00F01. BAX3R1
#               In der Validierungsdatei kann zu diesem Rabattschlüssel eine Rabattgruppe hinterlegt werden.
#         Außerdem kann dort für Positionsrabatte angegeben werden, ob es sich um einen Rabatt handelt,
#               der den Positionswert vermindert, oder um einen Zuschlag, der ihn erhöht.'''),
# BAKZRB  Vom Auftragsrabatt ausgenommen AufRb    S   1.0 190 190 XREF. XREFF01. XXKZ
#               Information über die Gültigkeit des Auftragsrabattes Gültige Werte sind:
#               *ZERO Auftragsrabatt ist gültig 1 vom Auftragsrabatt ausgenommen'''),
# BAKZRU  Rabatt nicht ausweisen Rb verstec   S   1.0 191 191 XREF. XREFF01. XXKZ
#               Festlegung, ob Rabatte ausgewiesen werden. Gültige Werte sind:
#               *ZERO Rabatte werden ausgewiesen 1 Rabatte n. ausweisen, vom Vk abziehen '''),
# BAKZRX  Rabattbasis RbBasis S   1.0 192 192 XREF. XREFF01. XXKZ
#               Festlegung, auf was sich der Rabatt bezieht Gültige Werte sind:
#               *ZERO Positionswert Netto1 Positionswert Brutto'''),
# BANART  Artikel für Naturalrabatt Art NatRb A   20  193 212 XREF. XREFF01. XXARTN
#               Zur Festlegung des Artikels ist die Artikelnummer anzugeben.
#               * Im Standard nicht/noch nicht unterstützt *'''),
# BANEAN  Artikel für Naturalrabatt EAN NatRb P   13.0    213 219 XREF. XREFF01. XXEAN
#               * Im Standard nicht/noch nicht unterstützt *'''),
# BANMNG  Naturalrabatt Menge NatRbMng    P   11.Mär  220 225 XREF. XREFF01. XXMNG),
# BAPREE  Einstandspreis EstdPr   P   15.2  226 233 XREF. XREFF01. XXPRE
#               * Im Standard nicht/noch nicht unterstützt *').
# BAPRKZ  Preisdimension Lager PrDim Lg   S   1.0 234 234 AAP00. AAP00F01. APPRKZ
#               Über die Preisdimension wird festgelegt, auf wieviele Einheiten der Artikelmengeneinheit
#               sich der angegebene Preis bezieht. Die Preisdimension kann nur bei Sonderartikeln bzw.
#               bei Artikeln mit Kennzeichen 'Einstandspreis änderbar' eingegeben werden.').
# BAWSLE  Währung Einkaufspreis Whr EkPr  A   3   235 237 XREF. XREFF01. XXWSL
#               * Im Standard nicht/noch nicht unterstützt *').
# BAKURS  Wechselkurs Einkauf Kurs EK P   09.6  238 242 XREF. XREFF01. XXKURS).
# BAKUFA  Kursfaktor Einkaufspreis KursF Ek   P   1.0 243 243 XREF. XREFF01. XXKUFA
#               Festlegung, auf wieviele Fremdwährungseinheiten sich der Wechselkurs bezieht:
#           Gültige Werte sind: *ZERO Kurs pro Fremdwährungseinheit 1 Kurs pro 10 Fremdwährungseinheiten
#               2 Kurs pro 100 Fremdwährungseinheiten 3 Kurs pro 1000 Fremdwährungseinheiten
# BAKTO   Konto Konto A   8   244 251 AAP00. AAP00F01. APKTO
# BAKTOK  Kostenkonto KostenKto   A   8   252 259 AAP00. AAP00F01. APKTOK Bei kostenlosen Lieferungen für
#               Kostenzuordnung
# BAKST   Kostenstelle    A   10  260 269 AAP00. AAP00F01. APKST Bei kostenlosen Lieferungen für
#               Kostenzuordnung
# BAKTR   Kostenträger WaWi KTr WaWi  A   20  270 289 AAP00. AAP00F01. APKTR Bei kostenlosen Lieferungen
#               für Kostenzuordnung
# BAZUOR  Zuordnung für dezentrale Anwendungen Zuo dezAnw A   10  290 299 XREF. XREFF01. XAZUOR Reserviert,
#               wenn SoftM Außendienst eingesetzt.
# BAPROB  Objekt/Aktion Obj/Aktion    A   10  304 313 XREF. XREFF01. XAPROB Prüfttyp für Validierung:
#               OBJ Zusätzliches Zugriffskriterium bei der Konditionsermittlung in der Auftragsbearbeitung.
#           * Objekte: Über die Angabe eines Objekts können zeitraumbezogene Konditionen definiert werden.
#               Sie werden dann berücksichtigt, wenn:
#               a) bei der Auftragserfassung das entsprechende Objekt angegeben wird;
#               b) in der Auftragsart die automatische Objektfindung ausgewählt wurde (PAAOBJ = W oder O),
#               und für den Stichtag ein gültiges Objekt vorhanden ist. Falls für denselben Zeitraum und
#               dieselben Zuordnungsbegriffe eine aobjektbezogene Kondition gültig ist, dann ersetzt diese
#               die entsprechende nicht-objektbezogene Kondition. Die azulässigen Objekte sind in der
#               Validierungsdatei hinterlegt. Dort kann auch eine Zeitspanne für die Gültigkeit des Objekts
#               hinterlegt werden. Die Kondition darf diese Zeitspanne nicht über- oder unterschreiten.
#               * Projekte: Projektbezogene Preise werden (im Gegensatz zum Objekt) grundsätzlich nur
#               berücksichtigt, wenn in der Auftragserfassung das entsprechende Projekt angegeben wird.
#               Die zulässigen Projekte sind in der Projektstammdatei hinterlegt. Die SoftM Kostenrechnung
#               muss eingesetzt sein. * Im Standard nicht/noch unterstützt
#* BAPRVP Provision in Prozent Prov (%) P 05.2 314 316 XREF. XREFF01. XXPROV
'BADTER': dict(name='erfassungsdatum', format='P7.0', doc='Datum der Erfassung/Erstellung des Datensatzes.'),
# BAVGAR Vorgangsart VrggA A 4 321 324 ABK00. ABK00F01. BKVGAR Es gibt besondere Vorgangsarten
# (#RET, *AAK, *XEA, *ITS, *CSA), die sich vom Standardparameter nicht nur durch die Parameterisierung
# unterscheiden. Für alle anderen Vorgangsarten (*NO*BLANK) kann über einen eigenen Parametersatz eine
# unterschiedliche Verarbeitung in SSt Auftrag erreicht werden, z.B. Angebot erzeugen. Gültige Werte sind:
# *BLANK Standardparameter #RET Auftrag aus Retourenverwaltung *AAK Änderung eines existierenden Auftrags
# *XEA Auftrag aus Bestellung (XEA00) *ITS Auftrag aus Intershop *CSA Auftrag aus B2B /CSA
# *OFF Auftrag aus Offline-Warenkorb *TST Übernahme Altaufträge in neue Vorgänge
# *NO*BLANK eigener Standardparameter
# BAFNRB  Herkunftsfirma HkFirma  A   2   325 326 AAP00. AAP00F01. APRES1 Werden die Positionen über die
# SSt Einkauf/Auftrag (XEA00) erzeugt, so ist hier die Firma des Ursprungsbelegs (= Bestellfirma) vermerkt.
# Gültige Werte sind: *FIRM gültige Firma
# BAKZUP  Geplanter Update Gpln Upd   A   1   327 327 XREF. XREFF01. XXA1 Gültige Werte sind: *BLANK neue
# Position erzeugen U vorhandene Position aktualisieren D vorhandene Position löschen
# BAUAPR  Umrechnungsart UmrgA    A   1   328 328 XREF. XREFF01. XAUAPR Die artikelbezogenen Preise können
# in einer von der Bestandsführungseinheit abweichenden Mengeneinheit hinterlegt werden. Die Umrechnung
# zwischen diesen Mengeneinheiten erfolgt über einen Umrechnungsfaktor. Je nach Umrechnungsart dient dieser
# zur Multiplikation oder Division. Gültige Werte sind: *ZERO Faktor 1 Divisor
# BAMEGB  Mengeneinheit der Umverpackung MEh UVpk S   2.0 329 330 AAP00. AAP00F01. APMEGB Gültige Eingaben
# müssen in der Validierungsdatei unter dem zugehörigen Prüftyp hinterlegt sein. Je nach Parameterisierung
# können die Positionen über die Mengeneinheit der Umverpackung erfasst werden. Diese Mengeneinheit dient
# nur als Eingabehilfe und wird informationshalber im Datensatz vermerkt.
# BAKZZT  Verfügbarkeitsprüfung Prf Vfgb  A   1   331 331 XREF. XREFF01. XXA1 Gültige Werte sind: *BLANK
# ohne Bestandsprüfung ¥ Verfügbarkeit geprüft @ bereits zugeteilt
# BAFNRX  Firma Bestandsführung FaBstdFhrg    A   2   332 333 AAP00. AAP00F01. APFNRX Gültige Werte sind:
# *FIRM gültige Firma
# BASNPF  Satznummer Konditionsermittlung StzNrKondE  P   9.0 343 347 XREF. XREFF01. XASNPF Die im Lauf der
# Konditionsermittlung gefundenen Konditionen werden in einer eigenen Datei gespeichert. Zugriff erfolgt über
# die Satznummer der Konditionsermittlung.
# BAKZ01  Reservefeld RsvFld  S   1.0 356 356 XREF. XREFF01. XXKZ STB: Zuteilungspriorität IAA00 Gültige
# Werte sind: *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKZ02  Preisstufe Preisstufe   S   1.0 357 357 XREF. XREFF01. XXKZ Gültige Werte sind: *ZERO Funktion
# nicht eingesetzt 1 Funktion eingesetzt
# BAKZ03 Steuerung Übernahme Strg Übn S 1.0 358 358 XREF. XREFF01. XXKZ  Bearbeitungszustand vor letzter
# Übernahme, benötigt für den Wiederanlauf AAU29 Gültige Werte sind: *ZERO Funktion nicht eingesetzt
# 1 Funktion eingesetzt
# BAPRVK Provisionskennzeichen ProvKz A 2 359 360 XREF. XREFF01. XXPRVK ANZFELDART: UPTYP10 Über die
# Zuordnung Vertreter / Provisionskennzeichen wird der Provisionssatz ermittelt. Wird bei der Prüfung
#  eingesetzt Gültige Werte sind: *BLANK keine Provision P1 Provisionskennzeichen 1 P2 Provisionskennzeichen
#  2 P3 Provisionskennzeichen 3
# BAKZBA Bearbeitungskennzeichen BearbKz S 1.0 361 361 XREF. XREFF01. XXKZ Festlegung der Bearbeitung
# Gültige Werte sind:
# 1 geprüft, Warnungen aufgetreten
# 2 geprüft, Fehler aufgetreten
# 3 geprüft, kein Fehler, übernehmen
# 4 wurde übernommen
# *ZERO nicht geprüft
# BADTAE Änderungsdatum ÄndDatum P 7.0 362 365 XREF. XREFF01. XXDTAE Datum der letzten Änderung des
# Datensatzes. Datum der letzten Änderung/Korrektur
# BADTPR Datum letzte Prüfung Dt ltz Prf P 7.0 366 369 ABK00. ABK00F01. BKDTPR
# BAKE01 Artikelgruppe eingesetzt mit ArtGrp S 1.0 370 370 XREF. XREFF01. XXKZ Artikelgruppe wird eingesetzt
# aus ... Gültige Werte sind: *ZERO aus Übergabe 1 per Automatik eingesetzt
# BAKE02 Artikelhauptgruppe eingesetzt mitArtHGrp S 1.0 371 371 XREF. XREFF01. XXKZ Artikelhauptgruppe wird
# eingesetzt aus ... Gültige Werte sind: *ZERO aus Übergabe 1 per Automatik eingesetzt
# BAKE03  Lager eingesetzt Lg egszt   S   1.0 372 372 XREF. XREFF01. XXKZ Lager wird eingesetzt aus ...
# Gültige Werte sind: *ZERO aus Übergabe 1 per Automatik eingesetzt
# BAKE04  Mengeneinheit eingesetzt MEh egszt  S   1.0 373 373 XREF. XREFF01. XXKZ Mengeneinheit wird
# eingesetzt aus ... Gültige Werte sind: *ZERO aus Übergabe 1 per Automatik eingesetzt
# BAKE05  Liefertermin eingesetzt LfTrm egsz  S   1.0 374 374 XREF. XREFF01. XXKZ Liefertermin wird
# eingesetzt aus ... Gültige Werte sind: *ZERO aus Übergabe 1 per Automatik eingesetzt
# BAKE06 Preis eingesetzt Pr egszt S 1.0 375 375 XREF. XREFF01. XXKZ Festlegung, ob/wie der Preis
# eingesetzt/ermittelt wird Gültige Werte sind: 1 Preis bei Stapelprüfung eingesetzt 2 Preis bei
# Dialogprüfung auf 0 Der Preis wurde bei der Dialogkorrektur auf 0 geändert. 3 Preis wird nicht
# überschrieben Der angegebene Preis 0 soll bei der Stapelprüfung nicht überschrieben werden.
# *ZERO Preis lt. Artikelstamm bzw. Preisdatei Bei Preis = 0 soll bei der Stapelprüfung der aktuelle Preis
# lt. Artikelstamm bzw. Preisdatei eingesetzt werden.
# BAKE07  Rabatt eingesetzt Rb egszt  S   1.0 376 376 XREF. XREFF01. XXKZ Festlegung, ob/wie der Rabatt
# eingesetzt/ermittelt wird. Gültige Werte sind: 1 Preisergänzung bei Stapelprüfung
# 2 Rabatt manuell auf 0 bei Dialog 3 keine Rabattergänzung bei Dialog
# *ZERO Rabatt lt. Artikelstamm bzw. Preisdatei
# BAKE08  Naturalrabattangaben eingesetzt NatRb egsz  S   1.0 377 377 XREF. XREFF01. XXKZ Angaben zu
# Naturalrabatt werden eingesetzt aus ... Gültige Werte sind: *ZERO aus Übergabe 1 per Automatik eingesetzt
# BAKE09  Kennzeichen Kz J/N  S   1.0 378 378 XREF. XREFF01. XXKZ Gültige Werte sind:
# *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKE10  Kennzeichen Kz J/N  S   1.0 379 379 XREF. XREFF01. XXKZ Gültige Werte sind:
# *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKE11  Kennzeichen Kz J/N  S   1.0 380 380 XREF. XREFF01. XXKZ Gültige Werte sind:
# *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKE12  Kennzeichen Kz J/N  S   1.0 381 381 XREF. XREFF01. XXKZ Gültige Werte sind:
# *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKE13  Kennzeichen Kz J/N  S   1.0 382 382 XREF. XREFF01. XXKZ Gültige Werte sind:
# *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKE14  Kennzeichen Kz J/N  S   1.0 383 383 XREF. XREFF01. XXKZ Gültige Werte sind:
# *ZERO Funktion nicht eingesetzt 1 Funktion eingesetzt
# BAKE15  Steuerung Sortimentsrabatt/-preis StrgSrtmRb    S   1.0 384 384 AAP00. AAP00F01. APKZET Festlegung,
# ob Position für die Ermittlung von Sortimentsrabatten oder -preisen berücksichtigt wird, und ob für sie
# Sortimentsrabatte oder -preise gewährt werden dürfen. Gültige Werte sind: *ZERO Menge berücksichtigen,
# Rabatt gilt 1 Menge berücksichtigen, kein Rabatt 2 Menge nicht berücksichtigen, kein Rabatt
# BAKZS1  Rabatt 1 ist Sortimentsrabatt Rb1 SrtmRb    S   1.0 385 385 XREF. XREFF01. XXKZ Festlegung, ob der
# entsprechende Rabatt (1 bis 4) ein Sorti- mentsrabatt ist. Angabe wird per Programm bei der Übernahme
# eingesetzt. Gültige Werte sind: *ZERO Rabatt ist kein Sortimentsrabatt 1 Rabatt ist Sortimentsrabatt
# BAKZS2  Rabatt 2 ist Sortimentsrabatt Rb2 SrtmRb    S   1.0 386 386 ABA00. ABA00F01. BAKZS1 Festlegung,
# ob der entsprechende Rabatt (1 bis 4) ein Sorti- mentsrabatt ist.Angabe wird per Programm bei der Übernahme
# eingesetzt. Gültige Werte sind: *ZERO Rabatt ist kein Sortimentsrabatt 1 Rabatt ist Sortimentsrabatt
# BAKZS3  Rabatt 3 ist Sortimentsrabatt Rb3 SrtmRb    S   1.0 387 387 ABA00. ABA00F01. BAKZS1 Festlegung,
# ob der entsprechende Rabatt (1 bis 4) ein Sorti- mentsrabatt ist.Angabe wird per Programm bei der Übernahme
# eingesetzt. Gültige Werte sind: *ZERO Rabatt ist kein Sortimentsrabatt 1 Rabatt ist Sortimentsrabatt
# BAKZS4  Rabatt 4 ist Sortimentsrabatt Rb4 SrtmRb    S   1.0 388 388 ABA00. ABA00F01. BAKZS1 Festlegung,
# ob der entsprechende Rabatt (1 bis 4) ein Sortimentsrabatt ist. Angabe wird per Programm bei der Übernahme
# eingesetzt. Gültige Werte sind: *ZERO Rabatt ist kein Sortimentsrabatt  1 Rabatt ist Sortimentsrabatt
# BAAUFN  Auftrag Auftrag P   7.0 389 392 XREF. XREFF01. XXAUFN Nummer des Auftrags  Die Angabe wird per
# Programm bei der Übernahme eingesetzt.  Der Nummernbereich wird im Parameter Nummernkreise festgelegt.
# BAAUPO  Auftragsposition AufPos P   5.0 393 395 XREF. XREFF01. XXAUPO Die Angabe wird per Programm bei
# der Übernahme eingesetzt.
# BAOWRT  Positionswert PosWrt    P   15.2  396 403 XREF. XREFF01. XAAWRT Angabe wird per Programm bei der
# Übernahme eingesetzt.
# BAIK01  Feld 1A Feld 1A A   1   404 404 XREF. XREFF01. XXA1
# BAIK02  Feld 1A Feld 1A A   1   405 405 XREF. XREFF01. XXA1
# BAIK03  Feld 1A Feld 1A A   1   406 406 XREF. XREFF01. XXA1
# BAIK04  Feld 1A Feld 1A A   1   407 407 XREF. XREFF01. XXA1
# BAIK05  Feld 1A Feld 1A A   1   408 408 XREF. XREFF01. XXA1
# BAIK06  Reservefeld RsvFld  A   3   409 411 XREF. XREFF01. XXA3
# BAIK07  Reservefeld RsvFld  A   3   412 414 XREF. XREFF01. XXA3
# BAIK08  Reservefeld RsvFld  A   3   415 417 XREF. XREFF01. XXA3
# BAIK09  Reservefeld RsvFld  A   3   418 420 XREF. XREFF01. XXA3
# BAIK10  Reservefeld RsvFld  A   3   421 423 XREF. XREFF01. XXA3
# BAIK11  Reservefeld RsvFld  A   10  424 433 XREF. XREFF01. XXA10
# BAIK12  Reservefeld RsvFld  A   20  434 453 XREF. XREFF01. XXA20
}

ABT00 = {
'BTFNR': dict(name='firma', format='A2', required=True, key=True, default='01',
               doc='''Gültige Werte sind: *FIRM gültige Firma'''),
'BTVGNR': dict(name='vorgang', format='S9.0', key=True),
'BTVGPO': dict(name='vorgangsposition', format='S5.0', key=True),
'BTTART': dict(name='textart', format='S1.0', key=True,
               doc='''Gültige Werte sind:
                      1 Benutzer aus B2B Shop
                      2 Abweichende Artikelbezeichnung Pos: ATTX60 Stelle 1-60: abweichende
                        Artikelbezeichnung Kopf: individuell verwendet
                      7 Auftragstexte vor Position
                      8 Auftragsanfangstexte (Position = 0)
                      8 Auftragstexte nach Position
                      9 Auftragsendetexte'''),
'BTLFNR': dict(name='textnummer', format='P4.0', key=True, default=1),
'BTLFDS': dict(name='Stapelsatznummer', format='S11.0', doc='Lfd. Nummer Stapelsatz'),
'BTTX60': dict(name='text', format='A60'),
'BTKZAB': dict(name='auf_auftragsbestaetigung', format='S1.0',
               doc='''Gültige Werte sind:
                      1 drucken
                      *ZERO nicht drucken'''),
'BTKZLF': dict(name='auf_lieferschein', format='S1.0',
               doc='''*ZERO nicht drucken
                      1 auf LfSn/KB drucken
                      2 auf KB drucken
                      3 auf LfSn drucken'''),
'BTKZRG': dict(name='auf_rechnung', format='S1.0',
               doc='''*ZERO nicht drucken
                      1 drucken
                      2 ist gedruckt'''),
'BTKZ04': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken
                      1 drucken'''),
'BTKZ05': dict(name='Textdruck auf Bestellformular', format='S1.0',
               doc='''AAT00.ATKZ05 Bei Streckengeschäften wird dieser Text in die Bestellung kopiert.
                      Gültige Werte sind:
                      *ZERO keinen Text auf Bestellung drucken
                      1 Text auf Bestellung drucken'''),
'BTKZ06': dict(name='Textdruck auf Warenzugangsbeleg', format='S1.0',
               doc='''AAT00.ATKZ06 Bei Streckengeschäften wird dieser Text in die Bestellung kopiert.
                      Gültige Werte sind:
                      *ZERO keinen Text auf WZ-Beleg drucken
                      1 Text auf WZ-Beleg drucken'''),
'BTKZ07': dict(name='Druckkennzeichen', format='S1.0',
               doc='''AAT00.ATKZ07 Das Kennzeichen hat zwei Bedeutungen, je nachdem aus welchem Einkaufspaket
                   (Rechnungsprüfung/Bestellwartung) der Texteditor aufgerufen wird.
                   Bei Streckengeschäften wird dieser Text in die Bestellung kopiert.
                   Gültige Werte sind:
                   *ZERO RgPrf: Kein Textdruck auf Rg
                   1 RgPrf: Text wird auf Rg gedruckt
                   *ZERO Bstl: Kein interner Text
                   1 Bstl: Interner Text für WZ und Rg
                   2 Bstl: Interner Text für WZ
                   3 Bstl: Interner Text für Rg'''),
'BTKZ08': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ09': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ10': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ11': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ12': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ13': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ14': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZ15': dict(name='Druckkennzeichen', format='S1.0',
               doc='''Über Druckkennzeichen wird gesteuert, welche Texte auf welchen Formularen gedruckt
                      werden sollen. Gültige Werte sind:
                      *ZERO nicht drucken, 1 drucken'''),
'BTKZBA': dict(name='Bearbeitungskennzeichen', format='S1.0',
               doc='''Kennzeichen Bearbeitungsstand:
                   Gültige Werte sind:
                   1 geprüft, Warnungen aufgetreten
                   2 geprüft, Fehler aufgetreten
                   3 geprüft, kann übernommen werden
                   4 wurde übernommen
                   *ZERO nicht geprüft'''),
}

# To: m.dornseif@hudora.de
# Subject: Füllen_Lieferadresse_in_Auftragseingangsschnittstelle
# MIME-Version: 1.0
# From: Dietmar Hering <dietmar.hering@softm.com>
# Date: Thu, 18 Sep 2008 12:36:43 +0200
#
# Sehr geehrter Herr Dornseif,
#
# abweichende Lieferadressen für Aufträge die über die Eingangsschnittstelle
# verarbeitet werden, können in die Datei ABV00 geschrieben werden.
# Verknüft wird die Anschrift mit der Vorgangsnummer gemäß ABK00.
# Zur Unterstützung lege ich dieser Mail noch die Feldbeschreibung der ABV00 bei.
#
ABV00 = {
'BVVGNR': dict(name='vorgang', format='S9.0', key=True, default='',
               doc=''''''),
#'BVLFDS': dict(name='stapelsatznummer', format='S11.0', default='',
#               doc='''Lfd. Nummer Stapelsatz Nummer des Satzes aus der Originalstapeldatei, die zur
#                      Erstellung des Kopfsatzes führte. Ist in userem System in der Regel NULL.'''),
'BVAART': dict(name='adressart', format='S1.0', default='',
               doc='''Gültige Werte sind: *ZERO Rechnungsadresse, 1 Lieferadresse'''),
'BVNAME': dict(name='name1', format='A40', default='',
               doc=''''''),
'BVNAM2': dict(name='name2', format='A40', default='',
               doc=''''''),
'BVNAM3': dict(name='name3', format='A40', default='',
               doc=''''''),
'BVNAM4': dict(name='avisieren', format='A40', default='',
               doc=''''''),
'BVSTR': dict(name='strasse', format='A40', default='',
               doc=''''''),
'BVPLZ': dict(name='plz', format='A15', default='',
               doc=''''''),
'BVORT': dict(name='ort', format='A40', default='',
               doc=''''''),
'BVLKZ': dict(name='laenderkennzeichen', format='A3', default='',
               doc=''''''),
'BVKZBA': dict(name='bearbeitungskennzeichen', format='S1.0', default='',
               doc='''Gültige Werte sind: *ZERO nicht geprüft
                        1 geprüft, Warnungen aufgetreten
                        2 geprüft, Fehler aufgetreten
                        3 geprüft, kann übernommen werden
                        4 wurde übernommen'''),
#'BVORTT': dict(name='Ortszusatz', format='A40', default='',
#               doc='''Dieses Feld wird gemäß Postbestimmungen in der deutschen Form der Adressaufbereitung
#                   nicht mehr verwendet, sondern nur in der englischen Fassung für die Ortsbezeichnung.'''),
'BVKZAD': dict(name='Adressaufbereitung', format='S1.0', default=1,
               doc='''Der Datenbestand kann gleichzeitig Adressen enthalten, die nach neuem Muster und nach
                      altem Muster aufbereitet werden. Gültige Werte sind: *ZERO alte Adressaufbereitung
                      1 neue Adressaufbereitung'''),
}
