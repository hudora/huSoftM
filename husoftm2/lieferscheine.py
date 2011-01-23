#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007, 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import sql_quote
from husoftm2.texte import texte_trennen, texte_auslesen
import husoftm2.sachbearbeiter


def get_ls_kb_data(conditions, additional_conditions=None, limit=None, header_only=False, is_lieferschein=True):
    """Lieferscheindaten oder Kommsissionierbelegdaten entsprechend dem Lieferungsprotokoll.

    Wenn is_lieferschein = False, dann werden Kommiauftragsdaten zurückgebeben (Kommimengen)

    """

    cachingtime = 60 * 60 * 12

    if additional_conditions:
        conditions.extend(additional_conditions)
    condition = " AND ".join(conditions)
    koepfe = {}
    auftragsnr2satznr = {}
    satznr2auftragsnr = {}

    # Lieferscheinkopf JOIN Kundenadresse um die Anzahl der Queries zu minimieren
    # JOIN Lieferadresse geht nicht, weil wir "ADAART=1" mit DB2/400 nicht klappt
    for kopf in query(['ALK00'], ordering=['LKSANK DESC'], condition=condition, limit=limit,
                      joins=[('XKD00', 'LKKDNR', 'KDKDNR'), ('AAK00', 'LKAUFS', 'AKAUFN')],
                      cachingtime=cachingtime, ua='husoftm2.lieferscheine'):
        d = dict(positionen=[],
                 auftragsnr="SO%s" % kopf['auftragsnr'],
                 auftragsnr_kunde=kopf['auftragsnr_kunde'],
                 erfassung=kopf['ALK_erfassung'],
                 aenderung=kopf.get('ALK_aenderung'),
                 anliefer_date=kopf['anliefer_date'],
                 kundennr="SC%s" % kopf['rechnungsempfaenger'],
                 lieferadresse=dict(kundennr="SC%s" % kopf['warenempfaenger']),
                 lieferdatum=kopf['anliefer_date'],  # XXX: Remove!
                 anlieferdatum=kopf['anliefer_date'],
                 lager="LG%03d" % int(kopf['lager']),
                 kommiauftragnr="KB%s" % kopf['kommibelegnr'],
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
                 art=kopf.get('art', ''),
                 )
                 # 'art': u'',
                 # 'teillieferung_erlaubt': 1,
                 # 'voll_ausgeliefert': 1,
                 # 'anliefer_date': datetime.date(2010, 12, 2)}

        pos_key = str(kopf['satznr'])
        if kopf.get('bezogener_kopf'):
            pos_key = str(kopf['bezogener_kopf'])
        auftragsnr2satznr[kopf['auftragsnr']] = pos_key
        satznr2auftragsnr[pos_key] = str(kopf['auftragsnr'])
        koepfe[pos_key] = d

    if header_only:
        return koepfe.values()

    satznr = koepfe.keys()
    allauftrnr = koepfe.keys()
    # Texte einlesen
    postexte, kopftexte = texte_auslesen([satznr2auftragsnr[x] for x in allauftrnr])
    while satznr:
        # In 50er Schritten Auftragspositionen & Texte lesen und den 50 Aufträgen zuordnen
        batch = satznr[:50]
        satznr = satznr[50:]

        # Abweichende Lieferadressen
        condition = "ADAART=1 AND ADRGNR IN (%s) AND ADRGNR=AKAUFN" % ','.join([satznr2auftragsnr[str(x)] for x in batch])
        for row in query(['XAD00', 'AAK00'], cachingtime=cachingtime, ua='husoftm2.lieferscheine', condition=condition):
            aktsatznr = auftragsnr2satznr[row['nr']]
            koepfe[aktsatznr]['lieferadresse'].update(dict(name1=row['name1'],
                                                      name2=row['name2'],
                                                      name3=row['name3'],
                                                      strasse=row['strasse'],
                                                      land=husoftm2.tools.land2iso(row['laenderkennzeichen']),
                                                      plz=row['plz'],
                                                      ort=row['ort']))
            versandadressnr = row['versandadressnr']
            warenempfaenger = koepfe[aktsatznr]['lieferadresse']['kundennr']
            if versandadressnr:
                warenempfaenger = "%s.%03d" % (warenempfaenger, versandadressnr)
            koepfe[aktsatznr]['lieferadresse']['warenempfaenger'] = warenempfaenger

        # Positionen & Positionstexte zuordnen
        for row in query(['ALN00'], condition="LNSTAT<>'X' AND LNSANK IN (%s)" % ','.join([str(x) for x in batch]),
                         cachingtime=cachingtime, ua='husoftm2.lieferscheine'):
            if is_lieferschein == True:
                lsmenge = int(row['menge'])
            else:
                lsmenge = int(row['menge_komissionierbeleg'])
            d = dict(artnr=row['artnr'],
                     guid='%s-%03d-%03d' % (row['kommibelegnr'], row['auftrags_position'], row['kommibeleg_position']),
                     menge=lsmenge)
            texte = postexte.get(row['auftragsnr'], {}).get(row['auftrags_position'], [])
            texte, attrs = texte_trennen(texte)
            d['infotext_kunde'] = texte
            if 'guid' in attrs:
                d['auftragpos_guid'] = attrs['guid']

            koepfe[str(row['satznr_kopf'])]['positionen'].append(d)
            koepfe[str(row['satznr_kopf'])]['sachbearbeiter'] \
                = husoftm2.sachbearbeiter.resolve(row['sachbearbeiter_bearbeitung'])

        # Kopftexte zuordnen
        for auftragsnr, texte in kopftexte.items():
            texte, attrs = texte_trennen(texte)
            pos_key = auftragsnr2satznr[auftragsnr]
            koepfe[pos_key]['infotext_kunde'] = texte
            if 'guid' in attrs:
                koepfe[pos_key]['auftrag_guid'] = attrs['guid']

    return koepfe.values()


def _lieferscheine(additional_conditions=None, limit=None, header_only=False):
    conditions = ["LKLFSN<>0",
                  "LKSTAT<>'X'",
                  ]
    return get_ls_kb_data(conditions, additional_conditions, limit, header_only)


def get_changed_after(date, limit=None):
    """Liefert die Lieferscheinnummern zurück, die nach <date> geändert wurden."""
    date = int(date.strftime('1%y%m%d'))
    conditions = ["LKLFSN<>0",
                  "LKSTAT<>'X'",
                  "(LKDTER>%d OR LKDTAE>=%d)" % (date, date),
                  ]
    condition = " AND ".join(conditions)
    ret = []
    for kopf in query(['ALK00'], ordering=['LKSANK DESC'], fields=['LKLFSN'],
                      condition=condition, limit=limit, ua='husoftm2.lieferscheine'):
        ret.append("SL%s" % kopf[0])
    return ret


def lieferscheine_auftrag(auftragsnr, header_only=False):
    """Gibt eine Liste mit Lieferscheindicts für einen Auftrag zurück"""
    auftragsnr = str(int(auftragsnr.strip('SO')))  # clean up, avoid attacks
    return _lieferscheine(["LKAUFS = %s" % sql_quote(auftragsnr)], header_only=header_only)


def get_lieferschein(lieferscheinnr, header_only=False):
    """Gibt ein Lieferscheindict für eine Lieferscheinnummer zurück"""
    lieferscheinnr = str(int(lieferscheinnr.strip('SL')))  # clean up, avoid attacks
    lscheine = _lieferscheine(["LKLFSN = %s" % sql_quote(lieferscheinnr)], limit=1, header_only=header_only)
    if lscheine:
        return lscheine[0]
    return {}


def _timedelta_to_hours(td):
    hours = td.days * 24
    hours += int(td.seconds / 3600)
    return hours


def get_lagerabgang(day):
    """Liefert im Grunde einen ALN00 Auszug für einen Tag - dient statistischen Zwecken."""
    conditions = ["(LKLFSN<>0 OR LNLFSN<>0)",  # durch Umparametrisierung ist mal das und mal das leer ...
                  "AKLGN2='0'",
                  "LNSTAT<>'X'",
                  "LKSTAT<>'X'",
                  "LNDTLF=%s" % (sql_quote(day.strftime('1%y%m%d')))]

    # SoftM Freuden! Das Feld LKSANKB kann ausser zwischen Oktober 2005 und November 2007
    # für den join genommen werden, ansonsten kann man LKSANK nehmen.

    rows = query(['ALN00'], condition=" AND ".join(conditions),
            fields=['LNAUFN', 'LNAUPO', 'LNARTN', 'LNKZKO', 'LNKDRG', 'LNKDNR', 'LNLFSN', 'LNMNGL', 'LNDTLF',
                    'LNDTVS', 'LNMNGF', 'LNDTER', 'LNLWA2', 'LKKDRG', 'LKKDNR', 'LKLFSN', 'LKDTLF', 'LKDTKB',
                    'LKAUFS', 'LKDTLT', 'AKAUFN', 'AKAUFA', 'AKDTLT', 'AKDTER', 'LNBELP', 'LNDTLT'],
          joins=[('ALK00', 'LNSANK', 'LKSANK'),
                 ('AAK00', 'LNAUFN', 'AKAUFN')])
    ret = []
    for row in rows:
        data = dict(auftragsnr="SO%s" % row['auftragsnr'],
                    lieferscheinnr="SL%s" % (int(row['lieferscheinnr']) or int(row['ALN_lieferscheinnr'])),
                    menge=int(row['menge_fakturierung']),
                    auftragsart=row['art'],
                    artnr=row['artnr'],
                    warenempfaenger="SC%s" % row['warenempfaenger'],
                    kundennr="SC%s" % row['rechnungsempfaenger'],
                    setartikel=(int(row['setartikel']) == 1),
                    wert=int(row['wert'] * 100),
                    auftrag_positionsnr=row['auftrags_position'],
                    positionsnr=row['kommibeleg_position'],
                    vorlauf_h=None, durchlauf_h=None, termintreue_h=None,
                    datum=row['lieferschein_date'],
                   )
        anliefer_date = row['ALN_anliefer_date'] or row['anliefer_date']
        versand_date = row['versand_date'] or row['lieferschein_date']
        if anliefer_date and row['AAK_erfassung_date']:
            row['vorlauf_h'] = _timedelta_to_hours(anliefer_date - row['AAK_erfassung_date']),
        if versand_date and anliefer_date:
            row['termintreue_h'] = _timedelta_to_hours(versand_date - anliefer_date)
        if versand_date and row['AAK_erfassung_date']:
            row['durchlauf_h'] = _timedelta_to_hours(versand_date - row['AAK_erfassung_date'])
        ret.append(data)
    return ret


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    import datetime
    header = False
    print len(get_lagerabgang(datetime.date(2010, 12, 21)))
    pprint(lieferscheine_auftrag('SO1163764', header_only=header))
    print get_changed_after(datetime.date(2010, 12, 1))
    pprint(get_lieferschein('SL4173969'))
    pprint(get_lieferschein('SL4176141'))


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
