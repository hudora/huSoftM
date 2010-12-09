#!/usr/bin/env python
# encoding: utf-8
"""
husoftm/rechnungen.py - zugriff auf rechnungen in SoftM

Created by Maximillian Dornseif on 2009-06-04.
Copyright (c) 2009, 2010 HUDORA. All rights reserved.
"""


from husoftm.connection2 import get_connection
from husoftm.tools import sql_quote, sql_escape, pad, date2softm


def kundenauftragsnr_to_rechnungsnr(kundenauftragsnr):
    """Liefert eine Liste mit Rechnungsnummern zurück, die zu einer Kundenauftragsnummer gehören.

    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix RG zurückgegeben."""

    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKNRKD = %s" % (sql_quote(kundenauftragsnr)))
    return [("RG%s" % r[0]) for r in rows]


def auftragsnr_to_rechnungsnr(auftragsnr):
    """Liefert eine Liste mit Rechnungsnummern zurück, die zu einer Auftragsnummer gehören.

    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix RG zurückgegeben."""

    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition="FKAUFN = %s" % (sql_quote(auftragsnr)))
    return [("RG%s" % r[0]) for r in rows]


def rechnungen_for_kunde(kundennr, mindate=None):
    """Liefert eine Liste mit Rechnungsnummern zurück.

    Dies sind allerdings nur Rechnungen, die aus der Warenwirtschaft faktiuriert wurden. 'Rechnungen'
    (eigentlich mauell erstellte offenene Forderungen) aus der Buchhaltung sind hier nicht berücksichtigt.
    Die Nummern werden gemäss https://cybernetics.hudora.biz/intern/trac/wiki/NummernKreise
    mit dem Prefix RG zurückgegeben.
    """
    conditions = ["FKKDNR=%s" % sql_quote(pad('FKKDNR', kundennr))]
    if mindate:
        conditions.append("FKDTER >= %s AND FKRGNR <> 0" % date2softm(mindate))
    rows = get_connection().query(['AFK00'], fields=['FKRGNR'],
                   condition=" AND ".join(conditions))
    return ["RG%s" % row[0] for row in rows if str(row[0]) != '0']


def get_rechnung(rechnungsnr):
    """Liefert ein Tupel aus Rechnungskopf und den Positionen"""

    if str(rechnungsnr).startswith('RG'):
        rechnungsnr = str(rechnungsnr)[2:]
    kopf = get_connection().query(['AFK00'],
                   condition="FKRGNR = %s" % sql_escape(rechnungsnr))
    if len(kopf) < 1:
        raise RuntimeError('inkonsistente Kopfdaten in AFK00: %r' % kopf)
    if len(kopf) > 1:
        print 'warning: inkonsistente Kopfdaten in AFK00: FKRGNR = %s' % rechnungsnr
        print kopf
    kopf = kopf[0]
    # TODO: der code schient nciht zu klappen, wenn gar keine Auftragstexte vorhanden sind.
    postmp = get_connection().query(['AFU00', 'AAT00'],
        condition="FURGNR=%s AND FUAUFN=ATAUFN AND FUAUPO=ATAUPO AND ATTART=8" % sql_escape(rechnungsnr))

    # wenn eine Rechnungsposition mehr als einen Rechnungstext hat, ist sie jetzt mehrfach in positionen
    # dedupen und spezial felder auseinanderklamuesern
    positionen = {}
    texte = {}
    for line in postmp:
        positionen.setdefault(line['auftragsposition'], {}).update(line)
        text = line['text'].strip()
        if not text.startswith('#:'):
            texte.setdefault(line['auftragsposition'], []).append(line['text'])
        if text.startswith('#:guid:'):
            positionen[line['auftragsposition']]['guid'] = text[7:]
    for posnr, textlines in texte.items():
        positionen[posnr]['text'] = ' '.join(textlines)

    return kopf, positionen.values()

def softm_to_invoice(rechnungsnr):
    """Erzeugt daten nach dem very simple invoice Protocol"""
    from pprint import pprint

    if str(rechnungsnr).startswith('RG'):
        rechnungsnr = str(rechnungsnr)[2:]
    rg, orderlines = get_rechnung('RG833645')
    hint = {}
    for attr in 'skontobetrag'.split():
        hint[attr] = rg[attr]
    out = {'hint': hint}
    for attr in '''kundenauftragsnr auftragsnr versandkosten rechnung_steuranteil rechnungsnr
                   zu_zahlen'''.split():
        out[attr] = rg[attr]

    out['leistungsdatum'] = rg['versand_date']
    out['kundennr'] = rg['kundennr_rechnungsempfaenger']
    out['erfasst_von'] = rg['sachbearbeiternr']
    out['abschlag_prozent'] = rg['auftragsrabatt1p'] + rg['auftragsrabatt2p']
    out['auftragsrabatt'] = rg['auftragsrabatt']
    out['rechungsdatum'] = rg['druck_date']
    rabatttext = ' und '.join([x for x in [rg['rabatttext1'].strip(), rg['rabatttext2'].strip()] if x])
    rabatttext = ""
    if rabatttext:
        rabatttext = "%s: %f" % (rabatttext, out['abschlag_prozent'])
    elif out['abschlag_prozent']:
        rabatttext = u"Ab/Zuschläge: %f" % (out['abschlag_prozent'])

    out['infotext_kunde'] = '\n'.join([rabatttext])

    out['orderlines'] = []
    for ol in get_connection().query(['AFU00'], condition="FURGNR=%s" % sql_escape(rechnungsnr)):
        pprint(ol)
        outol = {}
        for attr in '''menge  artnr abschlag rechungsbetrag warenwert'''.split(): # zu_zahlen
            outol[attr] = ol[attr]
        out['orderlines'].append(outol)

    pprint(out)

 #'Kennzeichen Streckengesch{f': 0,
 #'Kz.Rabatt versteckt': 0,
 #'Kz.Rabatte Brutto': 0,
 #'Kz: kein Auftragsrabatt = 1': 0,
 #'Kz:Pos in Rech.nicht andr.=': 0,
 #'Zuschlagssatz': 0,


    #'kundennr_warenempfaenger': '14849',
    #'skontierfaehiger_betrag': Decimal('-1496.84'),
#* **rechnungsbetrag** - Rechnungsbetrag ohne Steuer als String mit zwei Nachkommastellen. Entspricht `warenwert` - `Abschlag`.
#* *steuer_prozent* - mehrwertsteuer Prozentsatz (19, pro Rechnung kann es nur einen Mehrwertsteuersatz geben) *

# skontobetrag

#* *hint/...* Felder, die keinen normativen Charakter haben
#* *hint/abschlag* - Rabatte etc.
#* *zahlungstage* - Zahlungsziel ab Leistungsdatum in Tagen.
#* *hint/zahlungsdatum* - Zahlungsziel ab Leistungsdatum in Tagen.
#* *skonto_prozent* - Prozentsatz Skonto. Darf nur vorhanden sein, wenn `skontobetrag` vorhanden ist.
#* *skontotage* - Tage ab Lieferung innerhalb derer Skonto gezogen werden kann
#* *hint/skontodatum* - Datum bis zu dem Skonto gewährt wird. Darf nur vorhanden sein, wenn `skontotage`
#* *hint/skontobetrag* - Wie viel Euro macht das Skonto aus.
#* *zu_zahlen_bei_skonto* - Zu zahlender Betrag, wenn Skonto gezogen werden kann. (incl. Steuer)
#* *hint/rechnungsbetrag_bei_skonto* - Rechnungsbetrag mit Skonto angewendet.
#* *hint/rechnung_steueranteil_bei_skonto* - Steueranteil wenn Skonto gezogen wird.
#* *hint/steuernr_kunde* - Steuernummer und/oder UStId Kunde
#* *hint/steuernr_lieferant* - Steuernummer und/oder UStId Lieferant
#* *lieferadresse/...* Felder des [AddressProtocol][2] (Zum Teil Pflichtfelder).
#* *lieferadresse/kundennr* Interne Kundennummer. Kann das [AddressProtocol][2] erweitern.
#* *rechnungsadresse/...* Felder des [AddressProtocol][2] (Zum Teil Pflichtfelder). Es handelt sich um dne technischen Rechungsempfänger, der nicht vertragsbeteiligter ist.
#* *rechnungsadresse/kundennr* Interne Kundennummer. Kann das [AddressProtocol][2] erweitern.
#* *absenderadresse/...* Aussteller der Rechunung als Felder des [AddressProtocol][2] (Zum Teil Pflichtfelder).
#* **orderlines** Sammlung von Rechnungspositionen
#
    line = dict(
            guid=p.guid,
            menge=int(p.menge),
            artnr=p.artnr,
            #kundenartnr=f3.artnr_kunde,
            #name=f3.artikelbezeichnung.strip(),
            infotext_kunde=p.text
            #einzelpreis=int(abs(f3.verkaufspreis)*100),
            #warenwert=int(p.wert_netto*100),
            #zu_zahlen=int(abs(f3.wert_brutto)*100),
            #abschlag=int(f4.positionsrabatt_gesamt*100)
        )

    if f3.ean and int(f3.ean):
        line['ean']=f3.ean


def main():
    #print softm_to_invoice('5176070')
    print softm_to_invoice('5175671')
    #print softm_to_invoice('RG833645')

if __name__ == '__main__':
    main()
