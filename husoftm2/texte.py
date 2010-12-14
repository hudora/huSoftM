#!/usr/bin/env python
# encoding: utf-8
"""
texte.py - Access AAT00 in SoftM

Created by Maximillian Dornseif on 2010-12-11.
Copyright (c) 2010 HUDORA. All rights reserved.
"""

from husoftm2.backend import query


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
    return '\n'.join(rettexte), retdict


def texte_auslesen(auftragsnrs, postexte=None, kopftexte=None):
    """Gibt Positions und Kopftexte für eine Liste von Auftragsnummern zurück."""

    postexte = postexte or {}
    kopftexte = kopftexte or {}
    allauftrnr = auftragsnrs[:]
    while allauftrnr:
        # In 50er Schritten Texte lesen
        batch = allauftrnr[:50]
        allauftrnr = allauftrnr[50:]
        # Texte einlesen
        for row in query(['AAT00'], ordering=['ATTART', 'ATLFNR'],
                         condition="ATAUFN IN (%s)" % ','.join((str(x) for x in batch)),
                         ua='husoftm2.texte'):
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
                kopftexte.setdefault(row['auftragsnr'], []).append(row['text'].strip())
    return postexte, kopftexte
