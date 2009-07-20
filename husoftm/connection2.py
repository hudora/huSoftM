#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""husoftm is a toolkit for accessing a AS/400 running SoftM Suite.

This module enables connections to a server which in turn enables ODBC connections to the AS/400.
"""

__revision__ = "$Revision$"


import datetime
import logging
import time
import httplib
import urllib
import simplejson as json
from decimal import Decimal
from types import ListType, TupleType, StringType
from husoftm.fields import MAPPINGDIR, DATETIMEDIR, DECIMALIZE2
from husoftm.tools import softm2date


LOG = logging.getLogger('huSoftM.sql')
LOG.setLevel(logging.WARN)


class TimeoutException(IOError):
    pass
    

def as400_2_int(num):
    """Converts u'4.000' to 4 et. al."""
    return int(str(num).split('.')[0])
    

def _combine_date_and_time(mappings, fields, i, row, rowdict):
    """If there is also a time field in addition to a date field combine them."""
    basename = '_'.join(mappings[fields[i]].split('_')[:-1])
    timefield = DATETIMEDIR[fields[i]]
    try:
        timepos = fields.index(timefield)
    except ValueError:
        return
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
    

class MoftSconnection(object):
    """Represents an connection which can execute SQL on the iSeries-AS/400."""
    
    def _fix_field(self, data):
        """Fix field types returned by DB2/400."""
        
        if type(data) == type('abc'): # fix strings
            return data.strip().decode('latin-1').encode('utf-8')
        elif type(data) == type(0.0):
            if data == int(data): # fix floats:
                return int(data)
            else:
                return float(data)
        else:
            return data
    
    def _rows2dict(self, fields, mappings, rows):
        """Convert the list of rows we get from the server to a dict of columnames."""
        
        ret = []
        for row in rows:
            rowdict = {}
            for i in range(len(fields)):
                # fields[i] = feldname
                if fields[i] in DECIMALIZE2:
                    data = Decimal(str(row[i])).quantize(Decimal(10) ** -2)
                else:
                    data = self._fix_field(row[i])
                if fields[i] in mappings:
                    # special mapping of date time fields
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
    
    def query(self, tables=None, condition=None, fields=[], querymappings=None, grouping=[], ordering=[],
              nomapping=False):
        """Execute a SELECT on the AS/400 turning the results in a list of dicts.
        
        In fields you can give a list of fields you are interested in. If fields is left empty the engine
        generates a list of field on it own by consulting the field mapping database in from fields.MAPPINGDIR.
        """
        
        if type(tables) == StringType:
            tables = [tables]
        if type(fields) == StringType:
            fields = [fields]
        if type(grouping) == StringType:
            grouping = [grouping]
        if type(ordering) == StringType:
            ordering = [ordering]
        if not querymappings and len(fields) != 1 and not nomapping:
            querymappings = dict()
            for table in tables:
                querymappings.update(MAPPINGDIR.get(table, {}))
        if not fields:
            fields = querymappings.keys()
        if not fields:
            raise RuntimeError("can't deduce field names, check fields.py")
        
        querystr = "SELECT %s FROM %s" % (','.join(fields),
                                          ','.join([self._get_tablename(x) for x in tables]))
        if condition:
            querystr += ' WHERE %s' % condition
        if grouping:
            querystr += ' GROUP BY %s' % (','.join(grouping), )
        if ordering:
            querystr += ' ORDER BY %s' % (','.join(ordering), )
        
        return self._execute_query(querystr, querymappings, fields)

    def _select(self, query):
        param = urllib.quote(query)
        # TODO: use "real" dns entry (odbc_bridge.local.hudora.biz?)
        conn = httplib.HTTPConnection("balancer.local.hudora.biz:8000")
        # conn.set_debuglevel(5)
        conn.request("GET", "/select?query=" + param)
        response = conn.getresponse()
        if response.status != 200:
            errorinfo = response.read()
            if errorinfo.startswith('Internal Error: {\'EXIT\',\n                    {timeout'):
                raise TimeoutException(errorinfo)
            raise RuntimeError("Server Error: %r" % errorinfo)
        return json.loads(response.read())
    
    def _execute_query(self, querystr, querymappings, fields):
        start = time.time()
        LOG.debug(querystr)
        rows = self._select(querystr)
        
        querydelta = time.time() - start
        start = time.time()
        if querymappings:
            rows = self._rows2dict(fields, querymappings, rows)
        else:
            rows = [[y for y in x] for x in rows]
        mapdelta = time.time()-start
        LOG.info("%.3fs/%.3fs, %d rows: %s" % (querydelta, mapdelta, len(rows), querystr))
        return rows
    
    def delete(self, table, condition):
        raise NotImplementedError
        
    def insert_raw(self, sqlstr):
        raise NotImplementedError
    
    def update_raw(self, sqlstr):
        raise NotImplementedError
    
    def update_adtastapel(self, vorgangsnummer):
        # this is needed bei stapelschnittstelle.py
        raise NotImplementedError
    

def get_connection():
    """Get a PyRoMoftSconnection Object. Meant to one day introduce connection pooling."""
    
    return MoftSconnection()
