#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007, 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.backend import query
from husoftm2.tools import sql_escape, sql_quote, land2iso
from husoftm2.texte import texte_trennen, texte_auslesen
import husoftm2.sachbearbeiter


def _lieferscheine(additional_conditions=None, limit=None, header_only=False):
    cachingtime = 60 * 60 * 12
    conditions = ["LKLFSN<>0",
                  "LKSTAT<>'X'",
                  ]
    if additional_conditions:
        conditions.extend(additional_conditions)
    condition = " AND ".join(conditions)
    koepfe = {}
    auftragsnr2satznr = {}
    satznr2auftragsnr = {}

    # Lieferscheinkopf JOIN Kundenadresse um die Anzahl der Queries zu minimieren
    # JOIN Lieferadresse geht nicht, weil wir "ADAART=1" mit DB2/400 nicht klappt
    for kopf in query(['ALK00'], ordering=['LKSANK DESC'], condition=condition, limit=limit,
                      joins=[('XKD00', 'LKKDNR', 'KDKDNR')],
                      cachingtime=cachingtime, ua='husoftm2.lieferscheine'):
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
        for row in query(['XAD00'], cachingtime=cachingtime, ua='husoftm2.lieferscheine',
                         condition="ADAART=1 AND ADRGNR IN (%s)" % ','.join([str(x) for x in batch])):
            koepfe[str(row['nr'])]['lieferadresse'] = dict(name1=kopf['name1'],
                                                           name2=kopf['name2'],
                                                           name3=kopf['name3'],
                                                           strasse=kopf['strasse'],
                                                           land=husoftm2.tools.land2iso(kopf['laenderkennzeichen']),
                                                           plz=kopf['plz'],
                                                           ort=kopf['ort'],
                                                           )

        # Positionen & Positionstexte zuordnen
        for row in query(['ALN00'], condition="LNSTAT<>'X' AND LNSANK IN (%s)" % ','.join([str(x) for x in batch]),
                         cachingtime=cachingtime, ua='husoftm2.lieferscheine'):
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


def get_changed_after(date, limit=None, header_only=False):
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


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    import datetime
    header = False
    #(get_auftrag_by_guid('Online_20101202', header_only=header))
    #pprint(lieferscheine_auftrag('SO1163764', header_only=header))
    #print get_changed_after(datetime.date(2010, 12, 1))
    #pprint(get_lieferschein('SL4173969'))
    pprint(get_lieferschein('SL4176141'))
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
