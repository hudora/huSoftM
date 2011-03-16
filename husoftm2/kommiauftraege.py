#!/usr/bin/env python
# encoding: utf-8
"""
kommissionierauftraege.py - Kommiaufträge und Rückmeldungen
(Früher "Kommibelege" genannt.)

Technisch hängt das in SoftM an den Lieferscheinen.

Created by Christian Klein on 2011-01-03.
Copyright (c) 2011 HUDORA. All rights reserved.
"""

from husoftm2.lieferscheine import get_ls_kb_data
from husoftm2.tools import remove_prefix, sql_quote
from husoftm2.backend import query, x_en, raw_SQL
import datetime
import warnings


def get_kommiauftrag(komminr, header_only=False):
    """Gibt einen Kommiauftrag zurück"""

    prefix = 'KA'
    if komminr.startswith('KB'):
        warnings.warn("get_kommiauftrag('%s') ist nicht mit 'KA' Nummer aufgerufen worden." % komminr,
                      DeprecationWarning)
        prefix = 'KB'
    komminr = remove_prefix(komminr, prefix)

    # In der Tabelle ALK00 stehen Kommissionierbelege und Lieferscheine.
    # Die Kommissionierbelege haben '0' als Lieferscheinnr.
    # Zusätzlich werden die (logisch) gelöschten Lieferscheine rausgefiltert.
    conditions = ["LKLFSN = 0", "LKKBNR = %s" % sql_quote(komminr), "LKSTAT<>'X'"]
    belege = get_ls_kb_data(conditions, header_only=header_only, is_lieferschein=False)

    if belege:
        beleg = belege[0]
        # Falls es bereits einen Lieferschein gibt, die Lieferscheinnr in das dict schreiben.
        # Ansonsten die Eintrag 'lieferscheinnr' entfernen (wäre sonst SL0)
        rows = query(['ALK00'], condition="LKLFSN <> 0 AND LKKBNR = %s" % sql_quote(komminr),
                     ua='husoftm2.kommiauftraege')
        if rows:
            beleg['lieferscheinnr'] = rows[0]['lieferscheinnr']
        else:
            beleg.pop('lieferscheinnr', None)
        return beleg
    return {}


def get_kommibeleg(komminr, header_only=False):
    warnings.warn("get_kommibeleg() is obsolete, use get_kommiauftrag() instead", DeprecationWarning)
    return get_kommiauftrag(komminr)


def get_rueckmeldedaten(komminr):
    """Liefert Informationen aus der SoftM Rückmeldeschnittstelle zurück."""
    komminr = remove_prefix(komminr, 'KA')
    rows = query(['ISR00'], condition="IRKBNR = %s" % sql_quote(komminr), ua='husoftm2.kommiauftraege',
                 cachingtime=0)
    ret = {}
    for row in rows:
        ret[str(row['kommibelegposition'])] = row
    return ret


def get_rueckmeldestatus():
    """Liefert die Zahl der nicht zurück gemeldeten Aufträge zurück."""
    rows = query(['ISR00'], fields=['COUNT(*)', 'IRSTAT'], condition="IRSTAT<>'X'",
                 ua='husoftm2.kommiauftraege', grouping=['IRSTAT'], cachingtime=0)
    # [{'status': u'A', 'COUNT(*)': 67}, {'status': u'', 'COUNT(*)': 73}]
    ret = {}
    for row in rows:
        ret[row['status']] = row['COUNT(*)']
    return ret


def get_new(limit=401):
    """Liefert unverarbeitete Kommiaufträge zurück."""

    ret = []
    # Wichtig ist es, jedes Caching zu unterbinden, denn möglicherweise arbeiten wir mit get_new()
    # in einer engen Schleife, da würde Caching alles durcheinanderwerfen.
    # Dadurch dsa wir absteigend nach LKDTLF sortieren, senken wir das Risiko, noch nicht komplett
    # von SoftM bearbeitete Datensätze zu erhelten - bei denen ist das Datum und die Menge in der Regel
    # noch nicht gesetzt.
    for kopf in query(['ISA00'], fields=['IAKBNR'],
                      condition="IASTAT<>'X'", limit=limit, ua='husoftm2.kommiauftraege',
                      cachingtime=0):
        ret.append("KA%s" % kopf[0])
    return ret


def mark_processed(kommiauftragnr):
    """Markiert einen Kommiauftrag, so dass er von get_new() nicht mehr zurücuk gegeben wird."""
    conditions = ["IAKBNR=%s" % sql_quote(remove_prefix(kommiauftragnr, 'KA'))]
    return x_en('ISA00', condition=' AND '.join(conditions), ua='husoftm2.kommiauftraege')


def zurueckmelden(auftragsnr, komminr, positionen):
    """Rückmeldung zu SoftM über die Rückmeldeschnittstelle (ISR00).

    Es wird immer ein kompletter Kommiauftrag zurückgemeldet. Dh. jede Position, die im Kommiauftrag enthalten
    war, muss auch in den Rückmeldedaten vorhanden sein. Die Mengen in diesen Positionen dürfen voneinander
    abweichen. Rückmeldungen von bereits in dieser Schnittstelle stehenden Kommiaufträgen werden abgelehnt.
    """

    komminr = remove_prefix(komminr, 'KA')
    auftragsnr = remove_prefix(auftragsnr, 'SO')
    if get_rueckmeldedaten(komminr):
        msg = u'%s: Kommiauftrag schon in ISR00' % komminr, 'wms2ic_rueckmeldung'
        raise RuntimeError(msg)

    lock_key = datetime.datetime.now().strftime("%m%d%H%M%S")
    for pos in positionen:
        pos_sql = dict(IRFNR='01',
                       IRKBNR=int(komminr),
                       IRKPOS=int(pos['posnr']),
                       IRAUFN=int(auftragsnr),
                       IRAUPO=int(pos['posnr_auftrag']),
                       IRDFSL=lock_key,
                       IRMENG=int(pos['menge']))
        sqlstr = 'INSERT INTO ISR00 (%s) VALUES (%s)' % (','.join(pos_sql.keys()),
                                                         ','.join([repr(x) for x in pos_sql.values()]))
        raw_SQL(sqlstr, ua='husoftm2.kommiauftrag.zurueckmelden')

    # set all records to be unlocked
    sqlstr = "UPDATE ISR00 SET IRDFSL='' WHERE IRKBNR='%s' AND IRDFSL='%s'" % (int(komminr),
                                                                               lock_key)
    raw_SQL(sqlstr, ua='husoftm2.kommiauftrag.zurueckmelden')
