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

def rechung_to_invoice(rechnungsnr):
    """Erzeugt daten nach dem very simple invoice Protocol"""


        line = dict(
            guid=p.guid,
            menge=int(p.menge),
            artnr=p.artnr,
            #kundenartnr=f3.artnr_kunde,
            #name=f3.artikelbezeichnung.strip(),
            infotext_kunde=p.text
            #einzelpreis=int(abs(f3.verkaufspreis)*100),
            warenwert=int(p.wert_netto)*100),
            #zu_zahlen=int(abs(f3.wert_brutto)*100),
            #abschlag=int(f4.positionsrabatt_gesamt*100)
        )

        if f3.ean and int(f3.ean):
            line['ean']=f3.ean


def main():
    print get_rechnung('5176070')
    print get_rechnung('5175671')
    
if __name__ == '__main__':
    main()