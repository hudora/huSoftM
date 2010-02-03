#!/usr/bin/env python
# encoding: utf-8
"""
lagerschnittstelle.py - acessing SoftM tables ISA00, ISR00.

Die LAgerschnittstelle dient überwiegend der Entgegennahme von Kamissionierbelegen und dem
anschließenden Zurückmelden sowie dem Verbuchen von Warenzugängen.

Created by Lars Ronge, Maximillian Dornseif on 2007-12-07.
Copyright (c) 2007 HUDORA. All rights reserved.
"""


import datetime
import sqlite3
import unittest
import husoftm.fields
from husoftm.softmtables import SoftMreadOnlyTable, SoftMtable, SqliteConnector_mixin, AS400Connector_mixin


class ISA00_abstract(SoftMtable):
    """Zugriff auf die # MyPL Schnittstelle - Komissionierbeleg / Abstrakte Basisklasse.
    
    ISA00_test() und ISA00() sind die tatsächlichen Implementerungen."""
    
    def __init__(self):
        super(ISA00_abstract, self).__init__()
        self.name_dateifuehrungsschluessel = 'IADFSL'
        self.name_status = 'IASTAT'
        self.tablename = 'ISA00'
        self.name_schluessel = 'IAKBNR'
        self.fieldmappings = {
            'IAFNR': 'firma',
            'IALGNR': 'lagernr',
            'IAKBNR': 'kommibelegnr',
            'IAAUFN': 'auftragsnr',
            'IADATE': 'anforderungs_date',
            'IATIME': 'anforderung_time',
            'IADFSL': 'dateifuehrungsschluessel',
            'IASTAT': 'status',
            'IASANR': 'satznr',
        }
    

class ISA00_test(ISA00_abstract, SqliteConnector_mixin):
    """Zugriff auf die # MyPL Schnittstelle - Komissionierbeleg / Testdaten."""
    pass
    

class ISA00(ISA00_abstract, AS400Connector_mixin):
    """Zugriff auf die # MyPL Schnittstelle - Komissionierbeleg / Lifesystem."""
    pass
    

class ALN00(SoftMreadOnlyTable):
    """Zugriff auf die SoftM Lieferscheindatei."""
    
    def __init__(self):
        super(ALN00, self).__init__()
        self.name_dateifuehrungsschluessel = 'LNDFSL'
        self.name_status = 'LNSTAT'
        self.tablename = 'ALN00'
        self.name_schluessel = 'LNSANP'
        self.fieldmappings = { # too complex 
                              }
    

class ISK00(SoftMtable, AS400Connector_mixin):
    """Bildet Zugänge aus Umlagerungen ab."""
    
    def __init__(self):
        super(ISK00, self).__init__()
        self.name_dateifuehrungsschluessel = 'IKDFSL'
        self.name_status = 'IKSTAT'
        self.tablename = 'ISK00'
        self.name_schluessel = 'IKSANR'
        self.fieldmappings = {
            'IKFNR': 'firma',
            'IKKBNR': 'komminr',
            'IKKPOS': 'kommiposition',
            'IKAUFN': 'auftragsnr',
            'IKAUPO': 'auftragsposition',
            'IKRMNG': 'rueckmeldemenge',
            'IKDATE': 'rueckmeldung_date',
            'IKTIME': 'rueckmeldung_time',
            'IKDFSL': 'dateifuehrungsschluessel',
            'IKSTAT': 'status',
            'IKSANR': 'satznr',
        }
    

class ISZ00(SoftMtable, AS400Connector_mixin):
    """Bildet Warenzugänge (bspw. Containerlieferungen) ab."""
    
    def __init__(self):
        super(ISZ00, self).__init__()
        self.tablename = 'ISZ00'
        self.name_dateifuehrungsschluessel = 'IZDFSL'
        self.name_status = 'IZSTAT'
        self.name_schluessel = 'IZSANR'
        self.fieldmappings = husoftm.fields.MAPPINGDIR[self.tablename]


def artnr_for_kommibleg_position(self, kommibelegnr, position):
    """Wandelt eine Artikelnummer in eine Kommibelegpositionsnummer.
    
    Führt zu Problemen, wenn ein Artikelmehrfach auf einem Komissioneirbeleg erscheint."""
    
    aln00 = ALN00()
    ret = aln00.select("LNKBNR='%d' AND LNBELP='%d'" % (int(kommibelegnr), int(position)),
                       fields="LNARTN as artnr")
    return ret[0][0]
    

### Tests

class Isa00tests(unittest.TestCase):
    """Testcaseses for the ISA00 class."""
    
    def setUp(self):
        """Set up data and environment for testing."""
        init_testtables()
        self.table = ISA00_test()
    
    def test_basic_select(self):
        """Test ISA00().select()."""
        rows = self.table.select("IAFNR='01' AND IALGNR='100' AND IADFSL='' AND IASTAT=''",
                                 fields="IAKBNR, IAAUFN, IASANR")
        self.assertEqual(self.table.sqlhistory,
                         ["SELECT IAKBNR, IAAUFN, IASANR FROM ISA00 WHERE IAFNR='01' AND IALGNR='100'"
                          " AND IADFSL='' AND IASTAT=''"])
        self.assertEqual(len(rows), 1)
    
    def test_locking(self):
        """Test ISA00().lock() and friends."""
        # lock a row
        lock_key = self.table.lock('931795')
        # check how many unlocked rows exist
        rows = self.table.select("IADFSL='' AND IASTAT=''", fields="IAKBNR, IAAUFN, IASANR")
        self.assertEqual(len(rows), 0)
        # unlock row
        self.table.unlock(lock_key)
        # check how many unlocked rows exist
        rows = self.table.select("IADFSL='' AND IASTAT=''", fields="IAKBNR, IAAUFN, IASANR")
        self.assertEqual(len(rows), 1)
    
    def test_deleting(self):
        """Test ISA00().delete()."""
        # lock a row
        lock_key = self.table.lock('931795')
        self.table.delete(lock_key)
        self.table.unlock(lock_key)
        # check how many unlocked & undeleted rows exist
        rows = self.table.select("IADFSL='' AND IASTAT=''", fields="IAKBNR, IAAUFN, IASANR")
        self.assertEqual(len(rows), 0)
    
    def test_available_rows(self):
        """Test ISA00().available_rows()."""
        rows = self.table.available_rows()
        self.assertEqual(len(rows), 1)
        rows = self.table.available_rows(fields="IAKBNR")
        self.assertEqual(len(rows), 1)
        self.assertEqual(len(rows[0]), 1)
        rows = self.table.available_rows(fields="IAKBNR, IAAUFN")
        self.assertEqual(len(rows[0]), 2)
    
    def test_removing_locks(self):
        """Test ISA00().clean_stale_locks()."""
        
        # generate stale lock keys
        yesterday = datetime.datetime.now() - datetime.timedelta(hours=6)
        stale_key = yesterday.strftime("%m%d%H%M%S")
        self.table.update("IADFSL='%s'" % stale_key, "IADFSL<>''")
        
        # test cleaning them
        rows = self.table.select("IADFSL='' AND IASTAT=''", fields="IAKBNR, IAAUFN, IASANR")
        self.assertEqual(len(rows), 1)
        self.table.clean_stale_locks()
        rows = self.table.select("IADFSL='' AND IASTAT=''", fields="IAKBNR, IAAUFN, IASANR")
        self.assertEqual(len(rows), 2)
        
    
def _fill_table(cursor, name, data):
    """Create and fill a Table for testing purposes."""
    head = data.strip().split('\n')[0]
    fields = [x.strip('# ') for x in head.strip('|').split('|')]
    try:
        cursor.execute('DROP TABLE %s' % name)
    except sqlite3.OperationalError:
        pass
    cursor.execute('CREATE TABLE %s (%s)' % (name, ','.join(fields)))
    for line in data.split('\n'):
        line = line.strip()
        if line and not line.startswith('#'):
            data = [x.strip() for x in line.strip('|').split('|')]
            placeholder = ','.join(['?']*len(data))
            cursor.execute('INSERT INTO %s VALUES (%s)' % (name, placeholder), tuple(data))
    

def init_testtables():
    """Initialize a testing table using sqlite."""
    
    conn = sqlite3.connect('./as400-sqlite-test.db')
    cursor = conn.cursor()

    testdata1 = """
    # IAFNR| IALGNR| IAKBNR   | IAAUFN   | IADATE   | IATIME  | IADFSL    | IASTAT| IASANR     |
    | 01   | 100   | 930396   | 648529   | 1071128  | 204756  |           | X     | 1          |
    | 01   | 100   | 931787   | 650143   | 1071207  | 73252   | 1206134437|       | 1316       |
    | 01   | 100   | 931795   | 650173   | 1071207  | 114601  |           |       | 1317       |
    """
    _fill_table(cursor, 'ISA00', testdata1)
    
    testdata2 = """
    # IKFNR| IKKBNR   | IKKPOS | IKAUFN   | IKAUPO | IKRMNG      | IKDATE   | IKTIME  | IKDFSL    | IKSTAT| IKSANR     |
    | 01   | 930403   | 1      | 649169   | 2      | 2.000       | 1071129  | 111713  |           | X     | 1          |
    | 01   | 930403   | 2      | 649169   | 23     | 5.000       | 1071129  | 111713  |           | X     | 2          |
    | 01   | 930403   | 3      | 649169   | 25     | 5.000       | 1071129  | 111713  |           | X     | 3          |
    | 01   | 930403   | 4      | 649169   | 4      | 0           | 1071129  | 111713  |           | X     | 4          |
    | 01   | 930403   | 5      | 649169   | 18     | 1.000       | 1071129  | 111713  |           | X     | 5          |
    | 01   | 930403   | 6      | 649169   | 22     | 1.000       | 1071129  | 111713  |           | X     | 6          |
    | 01   | 930403   | 7      | 649169   | 26     | 1.000       | 1071129  | 111713  |           | X     | 7          |
    | 01   | 931067   | 1      | 649686   | 5      | 16.000      | 1071206  | 114731  |           |       | 230        |
    | 01   | 931067   | 2      | 649686   | 12     | 5.000       | 1071206  | 114731  |           |       | 231        |
    | 01   | 931067   | 3      | 649686   | 10     | 70.000      | 1071206  | 114731  |           |       | 232        |
    | 01   | 931067   | 4      | 649686   | 11     | 14.000      | 1071206  | 114731  |           |       | 233        |
    | 01   | 931067   | 5      | 649686   | 1      | 18.000      | 1071206  | 114732  |           |       | 234        |
    | 01   | 931067   | 6      | 649686   | 2      | 14.000      | 1071206  | 114732  |           |       | 235        |
    | 01   | 931067   | 7      | 649686   | 3      | 7.000       | 1071206  | 114732  |           |       | 236        |
    | 01   | 931067   | 8      | 649686   | 4      | 14.000      | 1071206  | 114732  |           |       | 237        |
    | 01   | 931067   | 9      | 649686   | 6      | 86.000      | 1071206  | 114732  |           |       | 238        |
    | 01   | 931067   | 10     | 649686   | 7      | 86.000      | 1071206  | 114732  |           |       | 239        |
    | 01   | 931067   | 11     | 649686   | 8      | 52.000      | 1071206  | 114732  |           |       | 240        |
    | 01   | 931067   | 12     | 649686   | 9      | 38.000      | 1071206  | 114732  |           |       | 241        |
    """
    _fill_table(cursor, 'ISK00', testdata2)
    
    testdata3 = """
    # LNSANK     | LNSANP     | LNFNR| LNABT | LNSBNR  | LNKANR   | LNAUFN   | LNAUPO | LNARTN              | LNARTG| LNARTH| LNKZSO| LNKZKO| LNBSTN   | LNBSTP | LNVKE | LNLGNR| LNLGPL| LNLGRP| LNKZBE| LNLWA2           | LNMNGO       | LNGANO | LNDTLT   | LNJWLT | LNKZRK| LNKZZL| LNKZZU| LNKDRG  | LNKDNR  | LNLFSN   | LNMNGL       | LNGANL | LNDTLF   | LNZTLF  | LNKZVL| LNKZLD| LNKZL2| LNKZLF| LNKZFA| LNKZFF| LNDTST   | LNKZRE| LNKBNR   | LNMNGK       | LNGANK | LNDTKB   | LNZTKB  | LNKZKB| LNRGST| LNPKNR         | LNDTVS   | LNSTOR| LNBELP | LNFGNR     | LNFNFA| LNGEWI     | LNCOLL | LNLSTO| LNMNGF       | LNGANF | LNMESN   | LNKZV2| LNPSTA| LNKZVS| LNPROG  | LNKZUB| LNKINF| LNAUPS | LNKZ01| LNKZ02| LNKZ03| LNRSA1| LNRSA2| LNRSA3| LNRSA4| LNRSA5| LNRSA6    | LNRSA7    | LNRSN1| LNRSN2| LNRSN3| LNRSN4| LNRSN5| LNIK01| LNIK02| LNIK03| LNIK04| LNIK05| LNIK06| LNIK07| LNIK08| LNIK09| LNIK10| LNIK11    | LNIK12              | LNDTER   | LNZTER  | LNDTAE   | LNZTAE  | LNDFSL    | LNSTAT|
    | 683196     | 963079     | 01   | 1     | 30      | 649686   | 649686   | 1      | 65536               | 4001  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 8091.00          | 180.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 180.000      | 180    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 180.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 5      | 165338     |       | 0          | 0      |       | 180.000      | 180    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963080     | 01   | 1     | 30      | 649686   | 649686   | 2      | 10000               | 4001  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 4893.00          | 140.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 140.000      | 140    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 140.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 6      | 165338     |       | 0          | 0      |       | 140.000      | 140    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963081     | 01   | 1     | 30      | 649686   | 649686   | 3      | 65536               | 4001  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 2446.50          | 70.000       | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 70.000       | 70     | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 70.000       | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 7      | 165338     |       | 0          | 0      |       | 70.000       | 70     | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963082     | 01   | 1     | 30      | 649686   | 649686   | 4      | 10000               | 4001  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 5250.00          | 140.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 140.000      | 140    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 140.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 8      | 165338     |       | 0          | 0      |       | 140.000      | 140    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963083     | 01   | 1     | 30      | 649686   | 649686   | 5      | 65536               | 4003  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 5752.00          | 160.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 160.000      | 160    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 160.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 1      | 165338     |       | 0          | 0      |       | 160.000      | 160    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963084     | 01   | 1     | 30      | 649686   | 649686   | 6      | 10000               | 5351  | 5350  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 432.00           | 864.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 864.000      | 864    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 864.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 9      | 165338     |       | 0          | 0      |       | 864.000      | 864    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963085     | 01   | 1     | 30      | 649686   | 649686   | 7      | 65536               | 5351  | 5350  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 1296.00          | 864.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 864.000      | 864    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 864.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 10     | 165338     |       | 0          | 0      |       | 864.000      | 864    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963086     | 01   | 1     | 30      | 649686   | 649686   | 8      | 10000               | 5351  | 5350  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 1557.60          | 528.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 528.000      | 528    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 528.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 11     | 165338     |       | 0          | 0      |       | 528.000      | 528    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963087     | 01   | 1     | 30      | 649686   | 649686   | 9      | 65536               | 5901  | 5900  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 1516.80          | 384.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 384.000      | 384    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 384.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 12     | 165338     |       | 0          | 0      |       | 384.000      | 384    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963088     | 01   | 1     | 30      | 649686   | 649686   | 10     | 10000               | 4001  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 23065.00         | 700.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 700.000      | 700    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 700.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 3      | 165338     |       | 0          | 0      |       | 700.000      | 700    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963089     | 01   | 1     | 30      | 649686   | 649686   | 11     | 65536               | 4001  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 4893.00          | 140.000      | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 140.000      | 140    | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 140.000      | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 4      | 165338     |       | 0          | 0      |       | 140.000      | 140    | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |
    | 683196     | 963090     | 01   | 1     | 30      | 649686   | 649686   | 12     | 10000               | 4003  | 4000  | 0     | 0     | 0        | 0      |       | 42    |       | 42    | 0     | 2103.30          | 54.000       | 0      | 1071203  | 10749  | 0     | 0     | 0     |    17200|    17200| 440019   | 54.000       | 54     | 1071206  | 114733  | 0     | 1     | 1     | 2     | 0     | 0     | 0        | 0     | 931067   | 54.000       | 0      | 1071203  | 161311  | 2     | 3     |                | 1071206  | 1     | 2      | 165338     |       | 0          | 0      |       | 54.000       | 54     | 0        | 0     |       |       | AF3539  |       | 15    | 0      | 0     | 0     | 0     |       |       |       |       |       |           |           | 0     | 0     | 0     | 0     | 0     |       |       |       |       |       |       |       |       |       |       |           |                     | 1071203  | 0       | 0        | 0       |           |       |    
    """
    _fill_table(cursor, 'ALN00', testdata3)

    cursor.execute("VACUUM") # forces commit to disk
    cursor.close()
    conn.close()

if __name__ == '__main__':
    unittest.main()
