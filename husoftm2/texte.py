#!/usr/bin/env python
# encoding: utf-8
"""
texte.py - Zugrif auf Auftragstexte (AAT00) in SoftM.

Für einzelne Aufträge sollte `auftragstextdaten()` verwendet werden, `txt_auslesen()` ist für Bulk-Abfragen
gedacht.

Created by Maximillian Dornseif on 2010-12-11.
Copyright (c) 2010, 2011 HUDORA. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import remove_prefix
import warnings


# Auslesen von Auftrags und Leiferscheintexten aus SoftM. Welche Texte werden
# wo und wie angezeigt? Texte, die "Andruck auf Auftragsbestätigung",
# "Andruck auf Lieferschein/Kommibeleg" und "Andruck auf Rechnung" als
# Kennzeichen gesetzt haben, werden **auf allen drei Belegarten** angedruckt.
# Leere Zeilen, oder Zeilen, die nur aus den Trennzeichen `=`, `*`, `_` oder
# `-` bestehen, werden nicht angedruckt.
# Gelegentlich tauchen texte mit dem Wert `andruck_ab` auf `2` auf. Queelle ist unbekannt.
# Diese Texte werden zur Zeit nirgends angedruckt.
# Zeilen, die mit `#:` beginnen dienen der automatischen Verarbeitung und werden nicht angedruckt.
#
# Dinge, die *nicht* in die Auftragstexte, sondern in die Kudnenstammdaten gehören sind:
# * Unsere Lieferantennummer beim Kunden. (Auch "Lieferanten-Nr./ILN", "MGB Lief.-Nr. 465542",
#   "Lieferantennr.", "Lieferanten-Nummer") Die in den Stammdaten gepflegte Nummer wird angedruckt.
# * "Soweit nicht anders angegeben, gilt das Lieferscheindatum als Liefer-/Leistungsdatum." - das
#   Leistungsdatum ist inzwischen auf (fast) allen Rechungen angedruckt.
# * "Anlieferungen avisieren", "Bitte mit Hebeb\xfchne anliefern" kann und soll unter
#   http://hulogi.appspot.com/ gepflegt werden.
# * "Steuer-Nr.:1031...", "Steuer Nummer 5124...", "Steuer Nr.: 151...", "Steuernr. 129...".
#   Diese Daten können unter http://hdmashup.appspot.com/stammdaten/kunde/ gepflegt werden.
#
# Im Allgemeinen geht SoftM ehr wirr mit den textn um, und was wo landet ist nie ganz klar. Das wir Texte,
# die nirgens angeruckt werden sollen und mit `#:` beginnen zum Datentransport nutzen ist eine
# interne Erweiterung.

def texte_trennen(texte):
    """Trennt Texte in eine Liste aus Texten und ein Dict aus spezial Bezeichnern ('#:guid:1234')"""

    rettexte = []
    retdict = {}
    for text in texte:
        if text.startswith('#:'):
            key = str(text.split(':')[1])
            value = ':'.join(text.split(':')[2:])
            if key not in ['guid']:
                raise RuntimeError("Unbekannte Auftragszusatzdaten: %s:%s" % key, value())
            else:
                retdict[key] = value
        else:
            rettexte.append(text)
    return ', '.join([x for x in rettexte if x.strip()]), retdict


def txt_auslesen(auftragsnrs, postexte=None, kopftexte=None, kopfdaten=None, posdaten=None):
    """Gibt Positions und Kopftexte und Zusatzdaten für eine Liste von Auftragsnummern zurück.

    (# Positionstexte
     {'SO1163161': {1: [u'Karton:          Palette:'],
                    2: [u'Karton:          Palette:']},
     # Kopftexte
     {'SO1163161': [u'Menge an VE angepasst! 14765 VE 2, da ungerade Bestand,',
                    u'einmalig abweichend!']}
     # Positionsdaten
     {'SO1163161': {1: {'guid': u'47WTWJUTDTT7FHKUHZP6FU23UI-0'},
                    2: {'guid': u'47WTWJUTDTT7FHKUHZP6FU23UI-1'},
                    3: {'guid': u'47WTWJUTDTT7FHKUHZP6FU23UI-2'}}},
     # Kopfdaten
     {'SO1163161': {'guid': u'047WTWJUTDTT7FHKUHZP6FU23UI'}}
    )
    """

    # Die Clients können dicts mit vorbelegten Daten mitgeben. Das ist vor allem da nützlich, wo
    # Jobs relativ viele Auftruage in Batches abarbeiten.
    postexte = postexte or {}
    posdaten = posdaten or {}
    kopftexte = kopftexte or {}
    kopfdaten = kopfdaten or {}
    # Wir arbeiten auf einer Kopie der Auftragsnummern, da wir von der Liste batches abschneiden.
    allauftrnr = auftragsnrs[:]
    while allauftrnr:
        # In 50er Schritten Texte lesen, 'SO'-Kürzel entfernen.
        # Wir erwarten auf jeden Fall String-Parameter.
        batch = [remove_prefix(x, 'SO') for x in allauftrnr[:50]]
        allauftrnr = allauftrnr[50:]
        # Texte aus SoftM einlesen
        condition = 'ATAUFN IN (%s)' % ','.join((str(x) for x in batch))
        for row in query(['AAT00'], ordering=['ATTART', 'ATLFNR'], condition=condition, ua='husoftm2.texte'):
            # Jeden der eingelesenen Texte nach Textart klassifizieren.
            row['textart'] = int(row['textart'])
            auftragsnr = "SO%s" % remove_prefix(row['auftragsnr'], 'SO')
            # Textzeilen die leer sind oder nur Trennzeichen enthalten, ignorieren wir.
            if not row['text'].strip('=*_- '):
                continue

            # Wir behandeln hier nur Texte, die auf Auftragsbestätigungen, Lieferscheinen oder Rechnungen
            # auftauchen sollen. Allerdings drucken wir diese dann auch auf beiden Belegarten auf - keine
            # weiteren Unterscheidungen.
            if row['andruck_re'] or row['andruck_ls'] or row['andruck_ab']:
                if row['andruck_re'] > 1 or row['andruck_ls'] > 3:
                    raise NotImplementedError(row)

                # Wir haben gelegentlich Texte mit `andruck_ab == 2` die offensichtlich nicht als
                # Kundenbelege sollen. Der Wert 2 in diesem Feld ist gänzlich undokumentiert,
                # wir ignorieren bis auf weiteres einfach diese Zeilen.
                if row['andruck_ab'] > 1:
                    continue

                # Texte wo andruck_ls=2 steht soll man laut SoftM "nur auf KB drucken".
                # Die Inhalte sind manchmal grenzwertig ... an dieser stelle kann man die aussortieren,
                # machen wir aber zur zeit nicht.
                if row['andruck_ls'] == 1 and (not row['andruck_re']) and (not row['andruck_ab']):
                    pass

                # Die Statistische Warennummer wird als Positionstext mit der Nummer 5 transportiert
                # In Produktivdaten haben wir die aber bisher noch nicht gesehen.
                if row['textart'] == 5:
                    postexte.setdefault(auftragsnr, {}
                           ).setdefault(row['auftragsposition'], []
                           ).append("Statistische Warennummer: %s" % row['text'].strip())
                # Die Textarten 2, 7 und 8 sind verschiedenen Positionstexte:
                # * 2 Abweichende Artikelbezeichnung
                # * 7 Auftragstexte vor Position
                # * 8 Auftragstexte nach Position
                # Wir fassen alle drei Textarten in einem einzigen Feld zusammen
                elif row['auftragsposition'] > 0 and row['textart'] in (2, 7, 8):
                    postexte.setdefault(auftragsnr, {}
                           ).setdefault(row['auftragsposition'], []
                           ).append(row['text'].strip())
                # Textart 7 bei Position 0 ist eine Faxnummer. Warum auch immer. Wir ignorieren das.
                # Das Feld ist **sehr oft** gefüllt.
                elif row['auftragsposition'] == 0 and row['textart'] == 7:
                    pass
                # Bei Position 0 sind Textart 8 und 9 Fuß- und Kopftexte für den gesammten Auftrag.
                # Wir fassen die in einem einzigen Feld zusammen.
                elif row['auftragsposition'] == 0 and row['textart'] in (8, 9):
                    kopftexte.setdefault(auftragsnr, []).append(row['text'])
                else:
                    # Andere Textarten sind uns bisher nicht untergekommen.
                    print row
                    raise NotImplementedError
            # Wenn der Text eigentlich nirgends angedruckt werden soll, dann ist es entweder ein Warntext
            # bei der Auftragserfassung, oder ein Verschlüsseltes Datenfeld.
            else:
                if int(row['auftragsposition'] == 0):
                    # Erfassungstexte sind Texte, die bei der Auftragserfassung angeziegt werden, aber auf
                    # keinem Beleg erscheinen (kein druckkennzeichen) - die ignorieren wir hier.
                    # Die gesonderten Datenfelder, die mit #:VARNAME: beginnen, verwenden wir aber weiter
                    _erfassungstexte, daten = texte_trennen([row['text']])
                    if daten:
                        kopfdaten.setdefault(auftragsnr, {}).update(daten)
                else:  # row['auftragsposition'] > 0:
                    erfassungstexte, daten = texte_trennen([row['text']])
                    if daten:
                        posdaten.setdefault(auftragsnr, {}
                                            ).setdefault(row['auftragsposition'], {}
                                            ).update(daten)
    return postexte, kopftexte, posdaten, kopfdaten


def texte_auslesen(auftragsnrs, postexte=None, kopftexte=None):
    """Kompatibilitätslayer zu txt_auslesen()."""
    warnings.warn("use txt_auslesen() instead of texte_auslesen()",
                  DeprecationWarning, stacklevel=2)
    postexte, kopftexte, _posdaten, _kopfdaten = txt_auslesen(auftragsnrs, postexte, kopftexte)
    return postexte, kopftexte


def auftragstextdaten(auftragsnr):
    """Auftrags- und Positionstexte und -daten für einen Auftrag zurückliefern."""
    auftragsnr = "SO%s" % remove_prefix(auftragsnr, 'SO')
    postexte, kopftexte, posdaten, kopfdaten = txt_auslesen([auftragsnr])
    return (postexte.get(auftragsnr, {}),
            kopftexte.get(auftragsnr, []),
            posdaten.get(auftragsnr, {}),
            kopfdaten.get(auftragsnr, {}))


def _test():
    """Einfache Selbsttests."""
    import pprint
    # Ein Auftrag mit Daten, die eigentlich intern bleiben sollten:
    pprint.pprint(auftragstextdaten('SO1161229'))
    # SO1166946 ist ein Auftrag, der *alle* Arten von Texten gesetzt hat.
    pprint.pprint(auftragstextdaten('SO1166946'))
    # Prüfe, ob auch integer Auftragsnummern verarneitet werden können
    assert auftragstextdaten('SO1166946') == auftragstextdaten(1166946)
    assert auftragstextdaten('SO1166946') == auftragstextdaten('1166946')
    # Auftrag mit Kundenartikelnummer
    pprint.pprint(auftragstextdaten('SO1160677'))
    # Auftrag mit Abschlägen
    pprint.pprint(texte_auslesen(['SO1163272']))
    # Mehrere Aufträge gleichzeitig testen. Die Beispielaufträge sind jeweils Aufträge mit recht
    # umfangreichen Daten
    pprint.pprint(txt_auslesen(['SO1163025', 'SO1163161', 'SO1163272']))
    # Ein Auftrag ohne jede Texte
    pprint.pprint(txt_auslesen(['SO1163180']))
    # Droppshipping-Test
    pprint.pprint(txt_auslesen(['SO1161418']))
    # Massentest:
    #for i in range(10000):
    #    pprint.pprint(txt_auslesen(['SO%s' % (1160000 + i)]))
    # Kopfdaten: SO1170482
    # Kopftexte: SO1114412 SO1161417 SO1169563 SO1169567 SO1170199 SO1168384 SO1170387 SO1170395
    #            SO1165116 SO1170604 SO1170308 SO1170500 SO1170410
    # Posdaten: SO1170526 SO1170482 SO1170396
    # Postexte: SO1118711 SO1160988 SO1170432 SO1161304 SO1170604 SO1170606


if __name__ == '__main__':
    _test()
