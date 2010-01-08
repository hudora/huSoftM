#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""husoftm is a toolkit for accessing a AS/400 running SoftM Suite.

This module enables connections to a server which in turn enables ODBC connections to the AS/400.
"""

__revision__ = "$Revision$"


import datetime
import logging
import time
import warnings
import Pyro
import Pyro.core
from types import StringType
from husoftm.fields import MAPPINGDIR, DATETIMEDIR
from husoftm.tools import softm2date
import husoftm.mock_as400


Pyro.core.initClient(banner=False)
LOG = logging.getLogger('huSoftM.sql')
LOG.setLevel(logging.WARN)


def _combine_date_and_time(mappings, fields, i, row, rowdict):
    """If there is also a time field in addition to a date field combine them."""
    basename = '_'.join(mappings[fields[i]].split('_')[:-1])
    timefield = DATETIMEDIR[fields[i]]
    timepos = fields.index(timefield)
    if (timepos and row[timepos] and
        not str(row[timepos]).startswith('99')): # Zeit = 999999: Unbestimmt
        try:
            if len(str(int(row[i]))) == 7:
                rowdict[basename] = datetime.datetime(*(
                    time.strptime(str(int(row[i])), '1%y%m%d')[:3]
                    + time.strptime(str(int(row[timepos])), '%H%M%S')[3:6]))
            else:
                raise ValueError
        except ValueError:
            print int(row[i]), int(row[timepos])
            raise


class PyRoMoftSconnection(object):
    """Represents an connection which can execute SWL on the iSeries-AS/400."""

    def __init__(self):
        # finds object automatically if you're running the Name Server.
        self.__server = Pyro.core.getProxyForURI("PYRONAME://pyro-ns.local.hudora.biz/mofts_connector1")
        warnings.warn("hudoftm.connection is deprecated use hudoftm.connection2 instead",
                      DeprecationWarning, stacklevel=2) 

    def _fix_field(self, data):
        """Fix field types returned by DB2/400."""

        if isinstance(data, str): # fix strings
            return data.strip().decode('latin-1').encode('utf-8')
        elif isinstance(data, float):
            if data == int(data): # fix floats:
                return int(data)
            else:
                return float(data)
        return data

    def _rows2dict(self, fields, mappings, rows):
        """Convert the list of rows we get from the server to a dict of columnames."""

        ret = []
        for row in rows:
            rowdict = {}
            for i in range(len(fields)):
                data = self._fix_field(row[i])
                if fields[i] in mappings:
                    if mappings[fields[i]].endswith('_date'):
                        if not row[i]:
                            rowdict[mappings[fields[i]]] = None
                        else:
                            rowdict[mappings[fields[i]]] = softm2date(row[i])
                            # check if there is also a time field
                            if fields[i] in DATETIMEDIR:
                                _combine_date_and_time(mappings, fields, i, row, rowdict)
                    else:
                        rowdict[mappings[fields[i]]] = data
                else:
                    rowdict[fields[i]] = data
            ret.append(rowdict)
        return ret

    def _get_tablename(self, name):
        """Generates the Name of a Table on the AS/400."""

        return "SMKDIFP.%s" % name

    def _raw_sql(self, querystr):
        """Executes an arbitary SQL - not meant for public use."""

        rows = self._execute_query(querystr, {}, [])
        return [[self._fix_field(f) for f in r] for r in rows]

    def _execute_query(self, querystr, querymappings, fields):
        try:
            rows = self.__server.select(querystr)
        except Exception, msg:
            LOG.error('PyRO remote exception:' + (''.join(Pyro.util.getPyroTraceback(msg))))
            raise

        if querymappings:
            rows = self._rows2dict(fields, querymappings, rows)
        else:
            rows = [[y for y in x] for x in rows]
        return rows

    def query(self, tables=None, condition=None, fields=[], querymappings=None, grouping=[], ordering=[],
              nomapping=False):
        """Execute a SELECT on the AS/400 turning the results in a list of dicts.

        In fields you can give a list of fields you are interested in. If fields is left empty the engine
        generates a list of field on it own by consulting the field mapping database in from fields.MAPPINGDIR.
        """

        if isinstance(tables, StringType):
            tables = [tables]
        if isinstance(fields, StringType):
            fields = [fields]
        if isinstance(grouping, StringType):
            grouping = [grouping]
        if isinstance(ordering, StringType):
            ordering = [ordering]
        if not querymappings and len(fields) != 1 and not nomapping:
            querymappings = dict()
            for table in tables:
                querymappings.update(MAPPINGDIR.get(table, {}))
        if not fields:
            fields = querymappings.keys()
        if not fields:
            raise RuntimeError("can't deduce field names")

        querystr = "SELECT %s FROM %s" % (','.join(fields),
                                          ','.join([self._get_tablename(x) for x in tables]))
        if condition:
            querystr += ' WHERE %s' % condition
        if grouping:
            querystr += ' GROUP BY %s' % (','.join(grouping), )
        if ordering:
            querystr += ' ORDER BY %s' % (','.join(ordering), )

        return self._execute_query(querystr, querymappings, fields)

    def delete(self, table, condition):
        """Delete rows from table where condition is met."""

        querystr = "DELETE FROM %s WHERE %s" % (table, condition)
        LOG.debug(querystr)
        try:
            rows = self.__server.select(querystr)
        except Exception, msg:
            LOG.error('PyRO remote exception:' + (''.join(Pyro.util.getPyroTraceback(msg))))
            raise
        return rows

    def insert_raw(self, sqlstr):
        """Insert rows into a table by directly executing SQL"""

        LOG.debug(sqlstr)
        try:
            rows = self.__server.insert(sqlstr, token='Aes.o=j7eS(')
        except Exception, msg:
            LOG.error('PyRO remote exception:' + (''.join(Pyro.util.getPyroTraceback(msg))))
            raise
        return rows

    def update_raw(self, sqlstr):
        """Insert rows into a table by directly executing SQL"""

        LOG.debug(sqlstr)
        try:
            rows = self.__server.update2(sqlstr, token="E~iy3*eej^")
        except Exception, msg:
            LOG.error('PyRO remote exception:' + (''.join(Pyro.util.getPyroTraceback(msg))))
            raise
        return rows

    def update_adtastapel(self, vorgangsnummer):
        self.__server.update_adtastapel(vorgangsnummer, token='Ae.so=7e,S(')


class PyRoMoftSconnectionToTestDB(PyRoMoftSconnection):
    """Represents an connection which can execute SWL on the Testdatabase (SMKDIFT) on the iSeries-AS/400."""

    def _get_tablename(self, name):
        """Generates the Name of a Table on the AS/400-Test-Database."""
        raise RuntimeError("stale test code")

    def update_test(self, query):
        """Update the Databasetable on the AS/400-Test-Database with the given query."""
        self.__server.update_test(query)

    def insert_test(self, query):
        """Inserts the given query into the Test-Database on the i5"""
        self.__server.insert_test(query)


class TestMoftSconnection(PyRoMoftSconnection):
    """Simulation fo SoftM connection. Data is taken from mock_as400.py.

    Use it by assigning husoftm.MoftSconnection = husoftm.TestMoftSconnection"""

    class MockServer(object):
        """Simulated server side for testing purposes."""

        def select(self, query, parameters=None):
            """Execute a SELECT on the AS/400 turning the results in a list of dicts. (Mock)"""

            ret = husoftm.mock_as400.sql(query, parameters)
            return ret

    def __init__(self):
        # finds object automatically if you're running the Name Server.
        self.__server = TestMoftSconnection.MockServer()


class TrainingMoftSconnection(PyRoMoftSconnection):
    """Simulation fo SoftM connection. Data is taken from mock_as400.py.

    If data is not found, the "real SoftM connection is used to get the data
    and print it. Mainly used for generating test data to put into mock_as400"
    Use it by assigning husoftm.MoftSconnection = husoftm.TrainingMoftSconnection"""

    class MockServer(object):
        """Simulated server side for testing purposes."""

        def select(self, query, parameters=None):
            """Execute a SELECT on the AS/400 turning the results in a list of dicts. (Mock)"""

            ret = husoftm.mock_as400.sql(query, parameters)
            if not ret:
                print (query, parameters)
                self.__server = Pyro.core.getProxyForURI("PYRONAME://192.168.0.22/mofts_connector4")
                ret = self.__server.sql(query, parameters)
                print repr(ret)
            return ret

    def __init__(self):
        # finds object automatically if you're running the Name Server.
        PyRoMoftSconnection.__init__(self)
        self.__server = TestMoftSconnection.MockServer()


TestMoftSconnection = TrainingMoftSconnection
MoftSconnection = PyRoMoftSconnection
MoftSconnectionToTestDB = PyRoMoftSconnectionToTestDB


def get_connection():
    """Get a PyRoMoftSconnection Object. Meant to one day introduce connection pooling."""
    
    # return MoftSconnectionToTestDB()
    return MoftSconnection()
