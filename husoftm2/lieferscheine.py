#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007, 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.tools import sql_escape, sql_quote, land2iso
from husoftm2.backend import query
import husoftm2.sachbearbeiter


def _lieferscheine(additional_conditions=None, limit=None, header_only=False):
    cachingtime = 60*60*12
    conditions = ["LKLFSN <> 0",
                  "LKSTAT <> 'X'",
                  "LKKDNR = KDKDNR",
                  # "LKAUFS = AKAUFN",
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
    auftragsnr2satznr = {}
    satznr2auftragsnr = {}

    # Lieferscheinkopf JOIN Kundenadresse um die Anzahl der Queries zu minimieren
    for kopf in query(['ALK00', 'XKD00'], ordering=['LKSANK DESC'], condition=condition, limit=limit,
                      cachingtime=cachingtime):
        d = dict(positionen=[],
                 auftragsnr="SO%s" % kopf['auftragsnr'],
                 # auftragsnr_kunde=kopf['auftragsnr_kunde'], aus ALK00
                 erfassung=kopf['ALK_erfassung'],
                 aenderung=kopf['ALK_aenderung'],
                 kundennr="SC%s" % kopf['rechnungsempfaenger'],
                 lieferadresse=dict(kundennr="SC%s" % kopf['warenempfaenger']),
                 lager="LG%03d" % int(kopf['lager']),
                 kommiauftragnr="KA%s" % kopf['kommibelegnr'],
                 kommiauftrag_datum=kopf['kommibeleg_date'],
                 lieferscheinnr="SL%s" % kopf['lieferscheinnr'],
                 datum=kopf.get('lieferschein'),
                 name1=kopf.get('name1', ''),
                 name2=kopf.get('name2', ''),
                 name3=kopf.get('name3', ''),
                 strasse=kopf.get('strasse', ''),
                 land=husoftm2.tools.land2iso(kopf['laenderkennzeichen']),
                 plz=kopf.get('plz', ''),
                 ort=kopf.get('ort', ''),
                 tel=kopf.get('tel', ''),
                 fax=kopf.get('fax', ''),
                 )
                 # 'art': u'',
                 # 'teillieferung_erlaubt': 1,
                 # 'voll_ausgeliefert': 1,
                 # 'anliefer_date': datetime.date(2010, 12, 2)}
        pos_key = str(kopf['satznr'])
        if kopf.get('bezogener_kopf'):
            pos_key = str(kopf['bezogener_kopf'])
        auftragsnr2satznr[kopf['auftragsnr']] = pos_key
        satznr2auftragsnr[pos_key] = kopf['auftragsnr']
        koepfe[pos_key] = d

    if header_only:
        return koepfe.values()

    postexte = {}
    kopftexte = {}       # nach Auftragsnummer
    lieferaddressen = {}  # nach Auftragsnummer
    satznr = koepfe.keys()
    while satznr:
        # In 50er Schritten Auftragspositionen & Texte lesen und den 50 Aufträgen zuordnen
        batch = satznr[:50]
        satznr = satznr[50:]

        # Lieferadressen
        for row in query(['XAD00'], cachingtime=cachingtime,
                         condition="ADAART=1 AND ADRGNR IN (%s)" % ','.join([str(satznr2auftragsnr[x]) for x in batch])):
            d = dict(name1=row.get('name1', ''),
                     name2=row.get('name2', ''),
                     name3=row.get('name3', ''),
                     strasse=row.get('strasse', ''),
                     land=husoftm2.tools.land2iso(row['laenderkennzeichen']),
                     plz=row.get('plz', ''),
                     ort=row.get('ort', ''),
                     tel=row.get('tel', ''),
                     fax=row.get('fax', ''),
                     mobil=row.get('mobil', ''),
                     mail=row.get('mail', ''),
                     )
            lieferaddressen[row['nr']] = d

        # Texte
        for row in query(['AAT00'], ordering=['ATTART', 'ATLFNR'], cachingtime=cachingtime, 
                         condition="ATAUFN IN (%s)" % ','.join([str(x) for x in batch])):
            row['textart'] = int(row['textart'])
            if row['textart'] == 5:
                postexte.setdefault(row['auftragsnr'], {}
                       ).setdefault(row['auftragsposition'], []
                       ).append("Statistische Warennummer: %s" % row['text'].strip())
            elif row['auftragsposition'] > 0 and row['textart'] in (2, 7, 8):
                postexte.setdefault(row['auftragsnr'], {}
                       ).setdefault(row['auftragsposition'], []
                       ).append(row['text'].strip())
            elif row['auftragsposition'] == 0 and row['textart'] == 7:
                # faxnr
                pass
            elif row['auftragsposition'] == 0 and row['textart'] in (8, 9):
                kopftexte.setdefault(row['auftragsnr'], []
                       ).append(row['text'].strip())

        # Positionen
        for row in query(['ALN00'], condition="LNSTAT<>'X' AND LNSANK IN (%s)" % ','.join([str(x) for x in batch]),
                         cachingtime=cachingtime):
            d = dict(artnr=row['artnr'],
                     menge=int(row['menge']),
                     kommipos_guid="KA%s-%s" % (row['kommibelegnr'], row['kommibeleg_position']),
                     #'menge_fakturierung': Decimal('24.0'),
                     #'menge_offen': Decimal('24.0'),
                     auftragpos_guid="SO%s-%s" % (row['auftragsnr'], row['auftrags_position']),
                     # 'lieferscheinstorno': u'',
                     # 'rueckstand_erlaubt': 1,
                     # 'menge_komissionierbeleg': Decimal('24.0'),
                     # 'setartikel': 0,
                     # 'rechnungsstatus': 3,
                     # 'voll_ausgeliefert': 0,
                     # 'storno_date': None,
                     # 'gutschrift': 1,
                     )
            texte = postexte.get(row['auftragsnr'], {}).get(row['auftrags_position'], [])
            # TODO: filter GUIDs and stuff
            if texte:
                d['infotext_kunde'] = '\n'.join(texte)
            koepfe[str(row['satznr_kopf'])]['positionen'].append(d)
            koepfe[str(row['satznr_kopf'])]['sachbearbeiter'] \
                = husoftm2.sachbearbeiter.resolve(row['sachbearbeiter_bearbeitung'])

        # Kopftexte & Lieferadressen zuordnen
        for auftragsnr, texte in kopftexte.items():
            # TODO: filter GUIDs and stuff
            pos_key = auftragsnr2satznr[auftragsnr]
            koepfe[pos_key]['infotext_kunde'] = '\n'.join(texte)

        for auftragsnr, lieferaddresse in lieferaddressen.items():
            pos_key = auftragsnr2satznr[auftragsnr]
            koepfe[pos_key]['lieferadresse'] = lieferaddresse

    return koepfe.values()


def get_changed_after(date, limit=None, header_only=False):
    """Liefert die Lieferscheinnummern zurück, die nach <date> geändert wurden."""
    date = int(date.strftime('1%y%m%d'))
    conditions = ["LKLFSN <> 0",
                  "LKSTAT <> 'X'",
                  "(LKDTER>%d OR LKDTAE>=%d)" % (date, date),
                  ]
    condition = " AND ".join(conditions)
    ret = []
    for kopf in query(['ALK00'], ordering=['LKSANK DESC'], fields=['LKLFSN'],
                      condition=condition, limit=limit):
        ret.append("SL%s" % kopf[0])
    return ret


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
    # Für Kommibelege: condition = "LKKBNR = %d AND LKSANB = 0"

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


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    import datetime
    header = False
    #print len(get_changed_after(datetime.date(2010, 12, 1)))
    #(get_auftrag_by_guid('Online_20101202', header_only=header))
    pprint(lieferscheine_auftrag('SO1163764', header_only=header))
    #(get_auftrag('Online_20101202', header_only=header))
    #(auftraege_kunde('SC66669', limit=20, header_only=header))
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
