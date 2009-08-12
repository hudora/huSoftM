#!/usr/bin/env python
# encoding: utf-8
"""
softmtables.py - acessing SoftM tables abstract classes.

This abstract class comes without unittests. Child classes are expected to include extensive testcases.

Created by Lars Ronge, Maximillian Dornseif on 2007-12-07.
Copyright (c) 2007 HUDORA. All rights reserved.
"""


import datetime
import logging
import sqlite3
from husoftm.tools import sql_quote
from husoftm.connection import get_connection


class SoftMError(RuntimeError):
    """Represents an error within the SoftM connection."""
    pass


class SqliteConnector_mixin(object):
    """This Mixin connects a SoftM Table Object to a local SQLite db -- ideal for testing."""
    
    def sql_select(self, sqlstr):
        """Execute the actual SQL query (SELECT)."""
        conn = sqlite3.connect('./as400-sqlite-test.db')
        cur = conn.cursor()
        cur.execute(sqlstr)
        return cur.fetchall()
    
    def sql_insert(self, sqlstr):
        """Execute the actual SQL query (INSERT)."""
        conn = sqlite3.connect('./as400-sqlite-test.db')
        cur = conn.cursor()
        cur.execute(sqlstr)
        ret = cur.rowcount
        cur.execute("VACUUM") # forces commit to disk
        return ret
        
    def sql_update(self, sqlstr):
        """Execute the actual SQL query (UPDATE)."""
        conn = sqlite3.connect('./as400-sqlite-test.db')
        cur = conn.cursor()
        cur.execute(sqlstr)
        ret = cur.rowcount
        cur.execute("VACUUM") # forces commit to disk
        return ret
        
    
class AS400Connector_mixin(object):
    """This Mixin connects a SoftM Table Object ti the real AS/400 database."""
    
    def sql_select(self, sqlstr):
        """Execute the actual SQL query (SELECT)."""
        return get_connection()._raw_sql(sqlstr)
    
    def sql_update(self, sqlstr):
        """Execute the actual SQL query (UPDATE)."""
        get_connection().update_raw(sqlstr)
        return 1
    
    def sql_insert(self, sqlstr):
        """Execute the actual SQL query (INSERT)."""
        get_connection().insert_raw(sqlstr)
        return 1
    

class SoftMreadOnlyTable(object):
    """This is a generic engine to access SoftM interface tables.
    
    This means data exchange tables which are accessed purely in an row oriented manner and use
    deploy 'Dateiführungsschlüssel" fields and logical deletions usint the 'XXSTAT' column.
    
    The object supports locking rows and automatic removal of stale locks after three hours - so you must
    finish your work within 60 minutes.
    
    All SoftM tables need a mixin class to implement the actual database access. See SqliteConnector_mixin and
    AS400Connector_mixin for examples.
    """
    
    def __init__(self):
        """Method should be overwritten by subclasses."""
        self.name_dateifuehrungsschluessel = 'XXDFSL'
        self.name_status = 'XXSTAT'
        self.name_schluessel = 'XXxxNR'
        self.tablename = 'XX'
        self.sqlhistory = []
    
    @property
    def tabledict(self):
        """Returns a dict with names of various fields for this table - mostly used for SQL generation."""
        return dict(table=self.tablename, 
                    dffeld=self.name_dateifuehrungsschluessel,
                    statusfeld=self.name_status,
                    schluesselfeld=self.name_schluessel)
    
    def generate_select_sql(self, condition, fields):
        """Generate the SQL-string for executing a select command."""
        return "SELECT %s FROM %s WHERE %s" % (fields, self.tablename, condition)
    
    def select(self, condition='INVALID', fields='*'):
        """Call to do a select on the table."""
        sql = self.generate_select_sql(condition, fields)
        self.sqlhistory.append(sql)
        return self.sql_select(sql)
    
    def available_rows(self, fields='*', conditions='1=1'):
        """Get non-locked, non-deleted rows.
        
        You can give a list of fields you want to get returned.
        You can also give a list of additional conditiones as SQL."""
        return self.select(("%(dffeld)s='' AND %(statusfeld)s='' AND " % self.tabledict) + conditions,
                           fields=fields)
    

class SoftMtable(SoftMreadOnlyTable):
    """This is a generic engine to access SoftM interface tables.
    
    This means data exchange tables which are accessed purely in an row oriented manner and use
    deploy 'Dateiführungsschlüssel" fields and logical deletions usint the 'XXSTAT' column.
    
    The object supports locking rows and automatic removal of stale locks after three hours - so you must
    finish your work within 60 minutes."""
    
    def generate_update_sql(self, fieldupdate, condition):
        """Generate the SQL-string for executing a update command."""
        return "UPDATE %s SET %s WHERE %s" % (self.tablename, fieldupdate, condition)
    
    def generate_insert_sql(self, fields, values):
        """Generate the SQL-string for executing a insert command."""
        return "INSERT INTO %s (%s) VALUES(%s)" % (self.tablename, fields,
                                                   ','.join([sql_quote(x) for x in values]))
    
    def update(self, fieldupdate='abc', condition='INVALID'):
        """Call to do a update on the table."""
        sql = self.generate_update_sql(fieldupdate, condition)
        self.sqlhistory.append(sql)
        return self.sql_update(sql)
    
    def insert(self, fields, values):
        """Call to do an insert into the table."""
        sql = self.generate_insert_sql(fields, values)
        self.sqlhistory.append(sql)
        return self.sql_insert(sql)
    
    def lock(self, schluessel):
        """Locks a row in the Table.
        
        Expects a key, (see self.name_schluesselfeld) returns a lock_handle to be used 
        with other functions or None if the row couldn't be locked."""
        
        lock_key = datetime.datetime.now().strftime("%m%d%H%M%S")
        sqldict = self.tabledict
        sqldict.update({'lock_key': lock_key, 'schluessel': schluessel})
        
        # setze Dateifuehrungsschluessel wenn Status (XXSTAT) leer und dateifuehrungsschluessel (XXDFSL) leer
        if self.update("%(dffeld)s='%(lock_key)s'" % sqldict,
                       "%(dffeld)s='' AND %(schluesselfeld)s='%(schluessel)s' AND %(statusfeld)s=''"
                       % sqldict) != 1:
            raise RuntimeError("SQL UPDATE failed.")
        
        # pruefe ob Dateifuehrungsschluessel erfolgreich gesetzt
        actual_set = self.select("%(dffeld)s='%(lock_key)s'"
                                 " AND %(schluesselfeld)s='%(schluessel)s'" % sqldict)
        if not actual_set:
            logging.warning("Dateifuehrungsschluesselkonflikt fuer "
                            "%(schluessel)s bei %(table)s.%(dffeld)s='%(lock_key)s'" % sqldict)
            return None
        return (lock_key, schluessel)
    
    def clean_stale_locks(self):
        """Removes all locks older than three hours.
        
        This removes all locks (Dateifuehrungsschlüssel) older than three hours."""
        # why three hours? because even when timezones are fucked up (UTC vs CEST)
        # there should be still one hour left
        yesterday = datetime.datetime.now() - datetime.timedelta(hours=3)
        stale_key = yesterday.strftime("%m%d%H%M%S")
        sqldict = self.tabledict
        sqldict.update({'lock_key': stale_key})
        # find locked, non deleted rows
        stale_rows = self.select("%(dffeld)s<'%(lock_key)s' AND %(dffeld)s<>''" % sqldict,
                                 "%(dffeld)s, %(schluesselfeld)s" % sqldict)
        for row in stale_rows:
            # remova all locks on stale rows
            sqldict.update({'schluessel': row[1], 'lock_key': row[0]})
            logging.info("Removing stale Lock in %(table)s.%(schluesselfeld)s='%(schluessel)s'" % sqldict)
            if self.update("%(dffeld)s=''" % sqldict,
                        "%(dffeld)s='%(lock_key)s' AND %(schluesselfeld)s='%(schluessel)s'" % sqldict) != 1:
                raise RuntimeError("SQL UPDATE failed.")
    
    def delete(self, lock_handle):
        """Marks a record in table as deleted (XXSTAT='X')."""
        assert isinstance(lock_handle, tuple)
        lock_key, schluessel = lock_handle
        sqldict = self.tabledict
        sqldict.update({'lock_key': lock_key, 'schluessel': schluessel})
        
        if self.update("%(statusfeld)s='X'" % sqldict,
                       "%(dffeld)s='%(lock_key)s' AND %(schluesselfeld)s='%(schluessel)s'" % sqldict) != 1:
            raise RuntimeError("SQL UPDATE failed.")
    
    def unlock(self, lock_handle):
        """Unlocks a row in table.
        
        Expects the lock_handle returned by lock."""
        assert isinstance(lock_handle, tuple)
        lock_key, schluessel = lock_handle
        sqldict = self.tabledict
        sqldict.update({'lock_key': lock_key, 'schluessel': schluessel})
        if self.update("%(dffeld)s=''" % sqldict,
                       "%(dffeld)s='%(lock_key)s' AND %(schluesselfeld)s='%(schluessel)s'" % sqldict) != 1:
            raise RuntimeError("SQL UPDATE failed.")
    
# this module has no own unittests but mypl/softmtables extensively tests funktionality in this module.
