#!/usr/bin/env python
# encoding: utf-8
"""
auftraege.py

Created by Christian Klein on 2010-03-15.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm.connection2 import get_connection


def auftraege(additional_conditions=None):
    """
    Alle Aufträge
    """
    
    conditions = ["AKSTAT<>'X'"]
    
    # You should REALLY know what you are doing!
    if additional_conditions:
        conditions.extend(additional_conditions)
    
    condition = " AND ".join(conditions)
    rows = get_connection().query('AAK00', ordering=['AKAUFN DESC', 'AKDTLT'], condition=condition)
    return rows


def get_auftrag(auftragsnr):
    
    kopf = get_connection().query('AAK00',
        condition="AKSTAT<>'X' AND AKAUFN=%s" % sql_escape(bestellnr))
    if len(kopf) != 1:
        raise RuntimeError('inkonsistente Kopfdaten in AKSTAT')
    kopf = kursfaktorkorrektur(kopf)[0]
    
    positionen = get_connection().query(['AAP00'], ordering=['APAUFN DESC', 'APDTLT'],
        condition="APSTAT<>'X' AND APAUFN=%s" % sql_escape(auftragsnr))
    return kopf, positionen

    