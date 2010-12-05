#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007, 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.tools import sql_escape, sql_quote, land2iso
from husoftm2.backend import query


def _lieferscheine(additional_conditions=None, limit=None, header_only=False):
    conditions = ["LKLFSN <> 0",
                  "LKSTAT <> 'X'",
                  #"LKKDNR = KDKDNR",
                  #"LKAUFS = AKAUFN"
                  ]
    # if mindate and maxdate:
    #     conditions.append("AKDTER BETWEEN %s AND %s" % (date2softm(mindate), date2softm(maxdate)))
    # elif mindate:
    #     conditions.append("AKDTER > %s" % date2softm(mindate))
    # elif maxdate:
    #     conditions.append("AKDTER < %s" % date2softm(maxdate))
    if additional_conditions:
        conditions.extend(additional_conditions)
    condition = " AND ".join(conditions)
    # Lieferscheinkopf JOIN Kundenadresse JOIN Auftragskopf um die Anzahl der Queries zu minimieren
    koepfe = {}
    #for kopf in query(['ALK00', 'AAK00', 'XKD00'], ordering=['LKSANK DESC'], condition=condition, limit=limit):
    for kopf in query(['ALK00'], ordering=['LKSANK DESC'], condition=condition, limit=limit):
        d = dict(auftragsnr="SO%s" % kopf['auftragsnr'],
                 erfassung=kopf['erfassung'],
                 aenderung=kopf['aenderung'],
                 kundennr="SC%s" % kopf['rechnungsempfaenger'],
                 lieferadresse=dict(kundennr="SC%s" % kopf['warenempfaenger']),
                 lager="LG%03d" % int(kopf['lager']),
                 kommibelegnr="KA%s" % kopf['kommibelegnr'],
                 kommibeleg_datum=kopf['kommibeleg_date'],
                 lieferscheinnr="SL%s" % kopf['lieferscheinnr'],
                 lieferschein_datum=kopf['lieferschein'],
                 )
        koepfe['satznr'] = d
    if header_only:
        return koepfe.values()

    return koepfe.values()


def lieferscheine_auftrag(auftragsnr, header_only=False):
    """Gibt eine Liste mit Lieferscheindicts für einen Auftrag zurück"""
    auftragsnr = str(int(auftragsnr.strip('SO')))  # clean up, avoid attacks
    return _lieferscheine(["LKAUFS = %s" % sql_quote(auftragsnr)], header_only=header_only)


# LEGACY
# def get_lieferscheinnrs_for_lager(lager):
#     "Liefert eine Liste mit allen nicht voll ausgelieferten Lieferscheinnummern für ein Lager."""
#     rows = query("ALK00", fields=["LKLFSN"],
#                  condition="LKLGNR=%r AND LKLFSN>0 AND LKKZVA=0 AND LKSTAT <> 'X'" % lager)
#     return sorted(set((int(row[0]) for row in rows)))
#
#
# def kommibelege_for_auftrag(auftragsnr):
#     """Return all Kommibeleg objects for a given auftragsnr"""
#
#     conditions = ["LKAUFS = %s" % sql_quote(auftragsnr), "LKLFSN = 0"]
#     condition = " AND ".join(conditions)
#     rows = get_connection().query(["ALK00"], condition=condition)
#     return [Kommibeleg(row['kommibelegbelegnr']) for row in rows]


# def kbpos2artnr(komminr, posnr):
#     """Gibt die Artikelnummer zu einer bestimmten Position eines Kommissionierbelegs zurück."""
#     rows = get_connection().query('ALN00', fields=['LNARTN'],
#                condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
#     return rows[0][0]


# def kbpos2artnr_lager(komminr, posnr):
#     """Gibt die Artikelnummer und das Abgangs-Lager zu einer bestimmten Position eines Kommissionierbelegs zurück."""
#     rows = get_connection().query('ALN00', fields=['LNARTN', 'LNLGNR'],
#                condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
#     return rows[0]['artnr'], rows[0]['lager']
#
#
# def kbpos2artnr_zugangslager(komminr, posnr):
#     """Gibt die Artikelnummer und das ZUGANGS-Lager zu einer bestimmten Position eines Kommissionierbelegs zurück."""
#     # Read auftragsnr_kunde
#     rows = get_connection().query('ALN00', fields=['LNARTN', 'LNAUFN'],
#                condition="LNKBNR=%d AND LNBELP=%d" % (int(komminr), int(posnr)))
#     artnr, auftragsnr = rows[0]['artnr'], rows[0]['auftragsnr']
#     rows = get_connection().query('AAK00', condition="AKAUFN='%d'" % int(auftragsnr))
#     return artnr, rows[0]["zugangslager"]


class Lieferschein(object):
    """Repräsentiert einen SoftM Lieferschein. Folgt dem Lieferung Protokol.

    >>> Lieferschein(4034544)
    <Lieferschein object>
    """

    condition = "LKLFSN = %d"

    def __init__(self, lsnr=None):
        self._read_from_softm(int(lsnr))

    def _read_from_softm(self, lsnr):
        """Basierend auf der ALK00 wird ein Datensatz aus allen verwandten Tabellen extrahiert."""

        # Lieferscheinkopf JOIN Kundenadresse JOIN Auftragskopf um die Anzahl der Queries zu minimieren
        conditions = [self.condition % lsnr, "LKKDNR = KDKDNR", "LKAUFS = AKAUFN"]
        rows = get_connection().query(['ALK00', 'AAK00', 'XKD00'], condition=" AND ".join(conditions))
        if len(rows) != 1:
            raise RuntimeError("Probleme bei der Auswahl des Lieferscheins - kein Datensatz mit %s" %
                                (self.condition % lsnr))
        set_attributes(rows[0], self)

        self.anlieferdatum = self.liefer_date
        self.anlieferdatum_min = self.anlieferdatum_max = self.anlieferdatum

        if self.kundenwunsch_date:
            self.anlieferdatum_max = self.kundenwunsch_date

        self.fixtermin = bool(self.fixtermin)

        self.pos_key = self.satznr
        if self.bezogener_kopf:
            self.pos_key = self.bezogener_kopf

        # Property für delayed execution
        self._positionen = None
        self._infotext_kunde = None

        self.lieferadresse = Adresse()
        self._get_abweichendelieferadresse()

    @property
    def positionen(self):
        """Liste der Lieferscheinpositionen"""
        # TODO: JOIN mit Texten
        if self._positionen is None:

            self._positionen = []
            rows = get_connection().query('ALN00', condition="LNSANK = %d" % int(self.pos_key))
            for row in rows:
                position = Lieferscheinposition()
                set_attributes(row, position)
                #position.anfangstext, position.endetext = _get_pos_texte(position.auftrags_position, self.auftragsnr)
                self._positionen.append(position)

        return self._positionen

    @property
    def infotext_kunde(self):
        """Texte zu einem Lieferschein"""
        if self._infotext_kunde is None:
            self.anfangstext, self.endetext = _get_pos_texte(auftragsposnr=0, auftragsnr=self.auftragsnr)
            self._infotext_kunde = '\n'.join([self.anfangstext, self.endetext]).strip()
        return self._infotext_kunde

    def _get_abweichendelieferadresse(self):
        """Abweichende Lieferadresse wenn vorhanden extrahieren."""
        # Wenn eine gesonderte Lieferadresse angegeben ist, Adresse damit überschreiben
        rows = get_connection().query('XAD00', condition="ADAART=1 AND ADRGNR='%d' " % int(self.auftragsnr))
        if len(rows) > 1:
            raise RuntimeError("Probleme bei der Auswahl der Lieferadresse")
        elif len(rows) == 1:
            row = rows[0]
            # bsp fuer auftragsnr 655501:
            # row = {'adressaufbereitung': 0,
            #  'laenderkennzeichen': u'D',
            #  'name1': u'NETTO Supermarkt GmbH & Co. OHG',
            #  'name2': u'',
            #  'name3': u'',
            #  'name4': u'',
            #  'ort': u'Wustermark',
            #  'plz': u'14641',
            #  'strasse': u'Magdeburger Str. 2'}
            set_attributes(row, self)

    def __unicode__(self):
        return u"SL%d, %d Positionen, %r" % (self.lieferscheinnr, len(self.positionen), self.liefer_date)


class Kommibeleg(Lieferschein):
    """Bildet einen Komissionierbeleg ab (der datentechnisch in SoftM ein Lieferschein ist)."""
    condition = "LKKBNR = %d AND LKSANB = 0"


def _get_pos_texte(auftragsposnr, auftragsnr):
    """Positionsanfangs- und Endetexte als string zurückgeben."""
    anfangstext = []
    endetext = []
    rows = get_connection().query('AAT00', ordering='ATLFNR',
                                  condition="ATAUPO=%d AND ATKZLF=1 AND ATAUFN='%d'"
                                  % (auftragsposnr, int(auftragsnr)))
    for row in rows:
        text = row['text']
        textnr = row['nr']
        textart = int(row['textart'])
        if textart in [7, 8]:
            anfangstext.append((textnr, text))
        elif textart in [9]:
            endetext.append((textnr, text))
    # sortieren, damit mehrzeilige Texte zusammen hängen
    anfangstext.sort()
    endetext.sort()
    anfangstext = '\n'.join([entry[1] for entry in anfangstext])
    endetext = '\n'.join([entry[1] for entry in endetext])
    return anfangstext, endetext


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    header = False
    pprint(lieferscheine_auftrag('SO1163764', header_only=header))
    #(get_auftrag_by_guid('Online_20101202', header_only=header))
    #(get_auftrag('SO1163764', header_only=header))
    #(get_auftrag('Online_20101202', header_only=header))
    #(auftraege_kunde('SO66669', limit=20, header_only=header))
    # Kommibeleg(3023551)
    # (vars(Lieferschein(4034544)))
    # kbpos2artnr(3023551, 1)


if __name__ == '__main__':
    from timeit import Timer
    #import cProfile, pstats
    #prof = cProfile.Profile()
    #prof = prof.runctx("_selftest()", globals(), locals())
    #stats = pstats.Stats(prof)
    #stats.sort_stats("time")  # Or cumulative
    #stats.print_stats(80)  # 80 = how many to print
    t = Timer("_selftest()", "from __main__ import _selftest")
    print t.timeit(1)
