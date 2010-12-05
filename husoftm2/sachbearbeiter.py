#!/usr/bin/env python
# encoding: utf-8
"""
sachbearbeiter.py - Teil von huSoftM.

Created by Christian Klein on 2010-03-25.
Copyright (c) 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.backend import query


_sachbearbeiter = {}


def resolve(sachbearbeiternr):
    """Returns the name for a sachbearbeiternr"""
    
    global _sachbearbeiter
    if not _sachbearbeiter:
        for row in query('XSB00', fields=['SBSBNR', 'SBNAME'], cachingtime=60*60*24*10):
            _sachbearbeiter[str(row['id'])] = row['name']
    return _sachbearbeiter[str(sachbearbeiternr)]
