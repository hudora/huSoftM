#!/usr/bin/env python
# encoding: utf-8
"""
lieferscheine.py - Zugriff auf Lieferscheine. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-03-17.
Copyright (c) 2007, 2010, 2011 HUDORA GmbH. All rights reserved.
"""

import datetime
import husoftm2.backend
import husoftm2.sachbearbeiter
import logging
from husoftm2.backend import query, x_en
from husoftm2.tools import sql_quote, remove_prefix, pad
from husoftm2.texte import txt_auslesen


def get_ls_kb_data(conditions, additional_conditions=None, limit=None, header_only=False,
                   is_lieferschein=True):
    """Lieferscheindaten oder Kommsissionierbelegdaten entsprechend dem Lieferungsprotokoll.

    Wenn is_lieferschein = False, dann werden Kommiauftragsdaten zurückgebeben (Kommimengen)
    """

    cachingtime = 30
    if additional_conditions:
        conditions.extend(additional_conditions)
    condition = " AND ".join(conditions)
    koepfe = {}
    auftragsnr2satznr = {}
    satznr2auftragsnr = {}

    # Lieferscheinkopf JOIN Kundenadresse um die Anzahl der Queries zu minimieren
    # JOIN Lieferadresse geht nicht, weil hier "ADAART=1" mit DB2/400 nicht klappt
    for row in query(['ALK00'], ordering=['LKSANK DESC'], condition=condition, limit=limit,
                      joins=[('XKD00', 'LKKDNR', 'KDKDNR'), ('AAK00', 'LKAUFS', 'AKAUFN')],
                      cachingtime=cachingtime, ua='husoftm2.lieferscheine'):
        kopf = dict(positionen=[],
                    auftragsnr="SO%s" % row['auftragsnr'],
                    auftragsnr_kunde=row['auftragsnr_kunde'],
                    erfassung=row['ALK_erfassung'],
                    aenderung=row.get('ALK_aenderung'),
                    anliefer_date=row['anliefer_date'],
                    kundennr="SC%s" % row['rechnungsempfaenger'],
                    lieferadresse=dict(kundennr="SC%s" % row['warenempfaenger']),
                    anlieferdatum=row['anliefer_date'],
                    lager="LG%03d" % int(row['lager']),
                    kommiauftragnr="KA%s" % row['kommibelegnr'],
                    kommiauftrag_datum=row['kommibeleg_date'],
                    lieferscheinnr="SL%s" % row['lieferscheinnr'],
                    name1=row.get('name1', ''),
                    name2=row.get('name2', ''),
                    name3=row.get('name3', ''),
                    strasse=row.get('strasse', ''),
                    land=husoftm2.tools.land2iso(row['laenderkennzeichen']),
                    plz=row.get('plz', ''),
                    ort=row.get('ort', ''),
                    tel=row.get('tel', ''),
                    fax=row.get('fax', ''),
                    art=row.get('art', ''),
                    softm_created_at=row.get('ALK_lieferschein'),
                    )

        # Basis dieses Codes:
        # [LH#721] LS mit 0-mengen vermeiden
        # Wir sehen im Produktiv-Betrieb immer wieder Lieferscheine mit der Menge "0"
        # erzeugt werden. Wir vermuten hier eine race Condition, bei der die
        # ALK00 schon mit der Lieferscheinnummer geupdated ist, die ALN00 aber noch
        # nicht mit der Lieferscheinmenge.
        # Eine weitere Vermutung ist, dass wenn in der ALN00 die Menge noch cniht eingetragen
        # ist, dort auch noch die Lieferscheinnummer fehlt. Das versuchen wir hier abzufangen.
        # Lieber ein Crash, als ein Lieferschein mit (unbegründerter) 0-menge.
        if row['ALK_dfsl']:
            raise husoftm2.backend.TransientError("Dateiführungsschlüssel in ALK00: %r" % kopf)
        if is_lieferschein == True:
            if not row['ALK_lieferschein_date']:
                # Wenn kein Lieferscheindatum (ALK_lieferschein_date) gesetzt ist,
                # handelt es sich um Lieferscheine, die erst vor kurzer Zeit erzeugt wurden.
                # SoftM benötigt noch Zeit, um das Lieferscheindatum zu setzen.
                raise husoftm2.backend.TransientError("Noch kein Lieferscheindatum in ALK00: %r" % kopf)
            kopf['datum'] = row['ALK_lieferschein'] or row['ALK_lieferschein_date']
        else:
            kopf['datum'] = row.get('kommibeleg', row['kommibeleg_date'])

        pos_key = remove_prefix((row['satznr']), 'SO')
        if row.get('bezogener_kopf'):
            pos_key = remove_prefix(row['bezogener_kopf'], 'SO')
        auftragsnr2satznr[remove_prefix(row['auftragsnr'], 'SO')] = pos_key
        satznr2auftragsnr[pos_key] = remove_prefix(row['auftragsnr'], 'SO')
        koepfe[pos_key] = kopf

    if header_only:
        return koepfe.values()

    satznr = koepfe.keys()
    allauftrnr = koepfe.keys()
    mengen_mit_mehr_als_null = False
    # Alle texte einlesen
    postexte, kopftexte, posdaten, kopfdaten = txt_auslesen([satznr2auftragsnr[x] for x in allauftrnr])
    while satznr:
        # In 50er Schritten Auftragspositionen & Texte lesen und den 50 Aufträgen zuordnen
        batch = satznr[:50]
        satznr = satznr[50:]

        # Abweichende Lieferadressen
        conditions = ["ADAART=1",
                     "ADRGNR IN (%s)" % ','.join([str(satznr2auftragsnr[x]) for x in batch]),
                     "ADRGNR=AKAUFN"]
        for row in query(['XAD00', 'AAK00'], cachingtime=cachingtime, ua='husoftm2.lieferscheine',
                         condition=" AND ".join(conditions)):
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

        ### Positionen & Positionstexte zuordnen
        for row in query(['ALN00'],
                         condition="LNSTAT<>'X' AND LNSANK IN (%s)" % ','.join([str(x) for x in batch]),
                         cachingtime=cachingtime, ua='husoftm2.lieferscheine'):
            if is_lieferschein == True:
                lsmenge = int(row['menge'])

                # [LH#721] Wenn trotz allem (s.o.) noch 0 Mengen auftauchen, dann markieren
                # Der Code kann entfernt werden, wenn wir die Probleme aus "[LH #721] 0-mengen"
                # komplett verstehen.
                if lsmenge:
                    mengen_mit_mehr_als_null = True

                if row['ALN_dfsl'] and len(koepfe) == 1:
                    # Siehe weiter oben zum Thema [LH#721] LS mit 0-mengen vermeiden
                    # Wir brechen aber nur ab, wenn nur ein einziger Lieferschein angefragt wird.
                    # Beim Abruf mehrerer Lieferscheine gleichzeitig, könnten wir nicht
                    # sinnvoll in einer Exception kommunizieren, welche Lieferscehine weiter verarbeitet
                    # werden können und welche nicht.
                    raise husoftm2.backend.TransientError("Dateiführungsschlüssel in ALK00: %r" % row)

                # Es ist noch unklar, ob wir das folgende Datum als Indikator für Probleme in [LH#721]
                # nutzen können. Wir loggen erstmal.
                if not row['ALN_lieferschein_date'] and len(koepfe) == 1:
                    logging.critical("Lieferscheinen ohne ALN_lieferschein_date datum", row)

            else:
                lsmenge = int(row['menge_komissionierbeleg'])
            pos = dict(artnr=row['artnr'],
                       guid='%s-%03d-%03d' % (row['kommibelegnr'], row['auftrags_position'],
                                              row['kommibeleg_position']),
                       menge=lsmenge,
                       _posnr_auftrag=row['auftrags_position'],
                       _posnr_kommi=row['kommibeleg_position'])
            texte = postexte.get(remove_prefix(row['auftragsnr'], 'SO'),
                                               {}).get(row['auftrags_position'], [])
            if isinstance(texte, list):
                texte = '\n'.join(texte)
            pos['infotext_kunde'] = texte.strip()
            if 'guid' in posdaten.get(remove_prefix(row['auftragsnr'], 'SO'), {}):
                pos['auftragpos_guid'] = posdaten.get(remove_prefix(row['auftragsnr'], 'SO'), {}).get('guid')
            else:
                pos['auftragpos_guid'] = "%s-%03d" % (row['auftragsnr'], row['auftrags_position'])

            lieferung = koepfe[remove_prefix(row['satznr_kopf'], 'SO')]
            lieferung['positionen'].append(pos)
            # *Sachbearbeiter* ist der, der den Vorgang tatsächlich bearbeitet hat. *Betreuer* ist
            # die (oder der), die für den Kunden zusändig ist.
            lieferung['sachbearbeiter'] = husoftm2.sachbearbeiter.resolve(row['sachbearbeiter_bearbeitung'])

        # Kopftexte zuordnen
        for auftragsnr, texte in kopftexte.items():
            pos_key = auftragsnr2satznr[remove_prefix(auftragsnr, 'SO')]
            if isinstance(texte, list):
                texte = '\n'.join(texte)
            koepfe[pos_key]['infotext_kunde'] = texte.strip()
        for auftragsnr, werte in kopfdaten.items():
            if 'guid' in werte:
                pos_key = auftragsnr2satznr[remove_prefix(auftragsnr, 'SO')]
                koepfe[pos_key]['guid_auftrag'] = werte['guid']

        for aktsatznr in koepfe.keys():
            # Entfernt Konstrukte wie das:
            #     "kundennr": "SC19971",
            #      "lieferadresse": {
            #       "kundennr": "SC19971"
            #      }
            if len(koepfe[aktsatznr]['lieferadresse']) == 1:
                if koepfe[aktsatznr]['lieferadresse']['kundennr'] == koepfe[aktsatznr]['kundennr']:
                    del(koepfe[aktsatznr]['lieferadresse'])

    # [LH#721] Wir haben immer wieder Probleme damit, das SoftM Lieferscheine erst "halb" erzeugt hat.
    # Dann sind Datum im Kopf oder Mengen im Lieferschein noch nicht gesetzt. Datum nicht gesetzt
    # fangen wir weiter oben ab. Hier gehen wir mit den 0-mengen um.
    # Das schwierige, dass es ja durchaus legitim sein kann, wenn einzelne Lieferscheinpositionen
    # die Menge 0 haben. Das kommt selten vor, aber es kommt vor.
    # Die Heuristik, die wir nun verwenden ist, dass wir in Fällen, in denen alle Positionen eine
    # 0-menge haben, nur übernehmen wenn sie seit mindestens 6 h in SoftM angelegt sind - in der Hoffnung,
    # dass SoftM hoffentlich jeden Lieferschein fertig bearbeitet hat.
    # Dieses Vorgehen ist natürlich nicht frei von Problemen: Zum einen funktioniert das ganze nur sinnvoll,
    # wenn `get_ls_kb_data()` nur für einen einzelnen Lieferschein - und nicht für eine Liste von
    # Lieferscheinen - aufgerufen wird. Zum Anderen ist die Ahl 6 Stunden natürlich völlig willkürlich
    # gewählt.
    # Wenn wir bessere Wege finden, noch nicht komplett importierte Lieferscheine zu finden, dann
    # könnte dieser Code wieder beseitigt werden.
    if not mengen_mit_mehr_als_null and is_lieferschein == True:
        for kopf in koepfe.values():
            if datetime.datetime.now() - kopf['datum'] < datetime.timedelta(hours=6):
                logging.critical("Lieferscheine haben nur 0-Mengen %r", auftragsnr2satznr.keys())
                logging.critical("Conditions: %r", condition)
                raise husoftm2.backend.TransientError

    return koepfe.values()


def _lieferscheine(additional_conditions=None, limit=None, header_only=False):
    """Hilfsfunktion um Leiferscheine mit beliebigen Bedingungen einzulesen."""
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


# Wir arbeiten im Zusammenhang mit Lieferscheinen mit dem Kundenspezifischen Feld LKKZ02.
# Dies ist standartmässig mit 0 vorbelegt. Wir setzen den Wert nach Verarbeitung auf 1.
def get_new(limit=401):
    """Liefert unverarbeitete Lieferscheine zurück."""
    # Das klappt sinnvoll, wegen folgendem Indexes:
    # CREATE INDEX QGPL.ALKIDX07  ON SMKDIFP.ALK00 ( LKSTAT ASC , LKKZ02 ASC , LKLFSN ASC );
    conditions = ["LKSTAT=' '",
                  "LKKZ02=0",
                  "LKLFSN<>0",
                  ]
    ret = []
    # Wichtig ist es, jedes Caching zu unterbinden, denn möglicherweise arbeiten wir mit get_new()
    # in einer engen Schleife, da würde Caching alles durcheinanderwerfen.
    # Dadurch dsa wir absteigend nach LKDTLF sortieren, senken wir das Risiko, noch nicht komplett
    # von SoftM bearbeitete Datensätze zu erhelten - bei denen ist das Datum und die Menge in der Regel
    # noch nicht gesetzt.
    for kopf in query(['ALK00'], ordering=['LKDTLF DESC, LKZTLF DESC'], fields=['LKLFSN'],
                      condition=' AND '.join(conditions), limit=limit, ua='husoftm2.lieferscheine',
                      cachingtime=0):
        ret.append("SL%s" % kopf[0])
    return ret


def mark_processed(lieferscheinnr):
    """Markiert einen Lieferschein, so dass er von get_new() nicht mehr zurücuk gegeben wird."""
    conditions = ["LKLFSN=%s" % sql_quote(remove_prefix(lieferscheinnr, 'SL')),
                  "LKSTAT<>'X'",
                  # "LKKZ02=0",  # Doppelt markieren ist OK
                  ]
    return x_en('ALK00', condition=' AND '.join(conditions), ua='husoftm2.lieferscheine')


def lieferscheine_auftrag(auftragsnr, header_only=False):
    """Gibt eine Liste mit Lieferscheindicts für einen Auftrag zurück"""
    auftragsnr = remove_prefix(auftragsnr, 'SO')
    return _lieferscheine(["LKAUFS = %s" % sql_quote(auftragsnr)], header_only=header_only)


def lieferschein_for_kommiauftrag(komminr, header_only=False):
    """Gibt den zu dem Kommiauftrag passenden Lieferschein zurück

    Falls der Lieferschein (noch) nicht existiert, wird None zurückgegeben
    """
    komminr = remove_prefix(komminr, 'KA')
    lieferscheine = _lieferscheine(["LKKBNR = %s" % sql_quote(komminr)], limit=1, header_only=header_only)
    if lieferscheine:
        return lieferscheine[0]


def lieferscheine_kunde(kundennr, limit=50, header_only=False):
    """Gibt eine Liste mit Lieferscheindicts für einen Kunden zurück"""
    kundennr = remove_prefix(kundennr, 'SC')
    return _lieferscheine(["LKKDNR = %s" % pad('LKKDNR', kundennr)], limit=limit, header_only=header_only)


def get_lieferschein(lieferscheinnr, header_only=False):
    """Gibt ein Lieferscheindict für eine Lieferscheinnummer zurück"""
    lieferscheinnr = remove_prefix(lieferscheinnr, 'SL')
    lscheine = _lieferscheine(["LKLFSN = %s" % sql_quote(lieferscheinnr)], limit=2, header_only=header_only)
    if lscheine:
        if len(lscheine) > 1:
            raise RuntimeError('Suche nach %s hat mehr als einen Lieferschein ergeben: %r'
                               % (lieferscheinnr, lscheine))
        lschein = lscheine[0]
        if not lschein.get('datum'):
            raise RuntimeError('LS %s hat kein Datum: %r' % (lieferscheinnr, lschein))
        return lschein
    return {}


def _timedelta_to_hours(tdelta):
    """Verwandelt ein timedeltaobjekt in Stungen (Integer)"""
    hours = tdelta.days * 24
    hours += int(tdelta.seconds / 3600)
    return hours


def get_lagerabgang(day):
    """Liefert im Grunde einen ALN00 Auszug für einen Tag - dient statistischen Zwecken."""

    # Der Abruf basiert auf Komissionierbelegen, nicht auf Lieferscheinen! - Beide befinden sich in der
    # ALN00 und sind an (LKLFSN<>0 OR LNLFSN<>0) zu unterscheiden.
    # Wir gehen aber vom Rückmeldedatum des Lieferscheins aus, nicht vom ERstellungsdatum des Lieferscheins.

    sqlday = husoftm2.tools.date2softm(day)
    conditions = ["(LKLFSN<>0 OR LNLFSN<>0)",  # durch Umparametrisierung ist mal das und mal das leer ...
                  "AKLGN2='0'",
                  "LNSTAT<>'X'",
                  "LKSTAT<>'X'",
                  "LNDTVS=%s" % sqlday]

    # SoftM Freuden! Das Feld LKSANKB kann ausser zwischen Oktober 2005 und November 2007
    # für den join genommen werden, ansonsten kann man LKSANK nehmen.
    rows = query(['ALN00'], condition=" AND ".join(conditions),
            fields=['LNAUFN', 'LNAUPO', 'LNARTN', 'LNKZKO', 'LNKDRG', 'LNKDNR', 'LNLFSN', 'LNMNGL', 'LNDTLF',
                    'LNDTVS', 'LNMNGF', 'LNDTER', 'LNLWA2', 'LNBELP', 'LNDTLT', 'LNLGNR',
                    'LKKDRG', 'LKKDNR', 'LKLFSN', 'LKDTKB', 'LKAUFS', 'LKDTLT', 'LKDTLF', 'LKZTLF',
                    'AKAUFN', 'AKAUFA', 'AKDTLT', 'AKDTER',
                    ],
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
                    lager=row['lager'],
                   )
        data['datum'] = row.get('versand_date', None)
        if not data.get('datum'):
            raise RuntimeError("Konnte kein Datum ermitteln %r", row)

        anliefer_date = row['ALN_anliefer_date'] or row['anliefer_date']
        versand_date = row['versand_date'] or row.get('ALK_lieferschein', row.get('AAK_erfassung_date'))
        if anliefer_date and row['AAK_erfassung_date']:
            row['vorlauf_h'] = _timedelta_to_hours(anliefer_date - row['AAK_erfassung_date']),
        if versand_date and anliefer_date:
            row['termintreue_h'] = _timedelta_to_hours(versand_date - anliefer_date)
        if versand_date and row['AAK_erfassung_date']:
            row['durchlauf_h'] = _timedelta_to_hours(versand_date - row['AAK_erfassung_date'])
        ret.append(data)
    return ret


def get_lieferschein_statistics():
    """ gibt ein tuple (anzahl_nicht_uebernommene_lieferscheine, uebernommene_lieferscheine) zurueck

    Tatsächlich geben wir nicht die Zahl der übernommenen Lieferscheine zurück - das ist zu langsam.
    """

    # Das ganze klappt nur sinnvoll, wegen folgendem Indexes:
    # CREATE INDEX QGPL.ALKIDX07  ON SMKDIFP.ALK00 (LKSTAT ASC, LKKZ02 ASC , LKLFSN ASC );
    # SELECT COUNT(*) FROM ALK00 WHERE LKLFSN<>0 AND LKSTAT= ' ' AND LKKZ02 = 0
    # Grouping, wie heri ursprünglich implementiert ist ein Preformance Killer
    rows = query(fields=['COUNT(*)'],
                 tables=['ALK00'],
                 condition="LKLFSN<>0 AND LKSTAT=' ' AND LKKZ02=0",
                 cachingtime=50)

    if not rows:
        return (0, 0)  # alles übernommen
    nicht_uebernommen = rows[0][0]
    return (nicht_uebernommen, 0)


def _selftest():
    """Test basic functionality"""
    # Viele Texte: SL300300
    # Kundenartikelnummer: SL4179659
    # Kopfdaten: SO1170482
    # Kopftexte: SO1114412 SO1161417 SO1169563 SO1169567 SO1170199 SO1168384 SO1170387 SO1170395
    #            SO1165116 SO1170604 SO1170308 SO1170500 SO1170410
    # Posdaten: SO1170526 SO1170482 SO1170396
    # Postexte: SO1118711 SO1160988 SO1170432 SO1161304 SO1170604 SO1170606
    from pprint import pprint
    import datetime
    header = False
    print len(get_lagerabgang(datetime.date(2010, 12, 21)))
    pprint(lieferscheine_auftrag(1163764, header_only=header))
    pprint(lieferscheine_auftrag('1163764', header_only=header))
    pprint(lieferscheine_auftrag('SO1163764', header_only=header))
    # print get_changed_after(datetime.date.today() - datetime.timedelta(days=2))
    pprint(get_lieferschein('SL4173969'))
    lschein = get_lieferschein('SL4176141')
    pprint(lschein)
    assert lschein['datum']

    # Bei dem lieferschein trat ein int/str dict key mixup auf. Sollte in 7777df6 gefixed sein.
    pprint(get_lieferschein('300300'))

    print get_new()
    # [LH #687] Lieferscheine sollten kein Lieferadressfeld haben, wenn es keine gesonderte
    # Lieferadresse gibt.
    # https://hudora.lighthouseapp.com/projects/42977/tickets/687
    lschein = get_lieferschein('SL4181680')
    pprint(lschein)
    assert 'lieferadresse' not in lschein


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
