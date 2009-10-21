#!/usr/bin/env python
# encoding: utf-8
"""
misc.py various bits and pices to be acessed in SoftM.

Created by Maximillian Dornseif on 2008-11-25.
Copyright (c) 2008 HUDORA. All rights reserved.
"""

import warnings
from husoftm.connection2 import get_connection


def bestellungen():
    """Liefert eine liste mit allen bestellten aber nicht stornierten Wareneingängen.
    
    >>> bestellungen()
    [{'artnr': u'64114',
      'bestellmenge': 300,
      'bestellnr': 43042,
      'created_by': 62,
      'dateifuehrungsschluessel': u'',
      'geliefert_kz': 0,
      'gelieferte_menge': 0,
      'lager': 16,
      'liefer_date': datetime.date(2009, 12, 24),
      'lieferant': u'90088',
      'menge_offen': 300,
      'position': 11,
      'status': u'',
      'streckengeschaeft': 0,
      'termin1_date': datetime.date(2009, 12, 24),
      'termin2_date': datetime.date(2009, 12, 24),
      'updated_by': 62,
      'wunsch2_date': datetime.date(2009, 12, 24),
      'wunsch_date': None,
      'zugang_date': None},
      ...]
    
    """
    
    warnings.warn("misc.bestellungen() is deprecated use module bestellungen instead",
                  DeprecationWarning, stacklevel=2) 
                          # detailierte Informationen gibts in EWZ00
    rows = get_connection().query('EBP00', ordering=['BPBSTN DESC', 'BPDTLT'], condition="BPSTAT<>'X'")
    # AND BPKZAK=0 to get only the open ones
    return rows
