#!/usr/bin/env python
# encoding: utf-8

"""husoftm is a toolkit for accessing a AS/400 running SoftM Suite.

This module enables connections to a server which in turn enables ODBC connections to the AS/400.
"""

__revision__ = "$Revision$"


import datetime
import doctest
import logging
import time
import httplib
import urllib
import simplejson as json
import sys
from decimal import Decimal
from husoftm.fields import MAPPINGDIR, DATETIMEDIR, DECIMALIZE2
from husoftm.tools import softm2date


LOG = logging.getLogger('huSoftM.sql')
LOG.setLevel(logging.WARN)


class TimeoutException(IOError):
    pass
    

def as400_2_int(num):
    """Converts u'4.000' to 4 et. al."""
    return int(str(num).split('.')[0])
    

def _rows2dict(fields, mappings, rows):
    """Convert the list of rows we get from the server to a dict of columnames and fix types."""
    
    ret = []
    for row in rows:
        rowdict = {}
        for i in range(len(fields)):
            _rows2dict_field_helper(fields, i, row, mappings, rowdict)
        ret.append(rowdict)
    return ret
    

def _rows2dict_field_helper(fields, i, row, mappings, rowdict):
    """Called per column in a row to convert it to a dict and fix field types.
    
    fields.DECIMALIZE2 and fields.DATETIMEDIR are used to determine special field handling.
    Also fields ending in '_date' are converted to datetime objects.
    
    >>> _rows2dict_field_helper(['LFARTN'], 0, ['12345'], {'LFARTN': 'artnr'}, {})
    {'artnr': '12345'}
    >>> _rows2dict_field_helper(['TST'], 0, ['1030821'], {'TST': 'test_date'}, {})
    {'test_date': datetime.date(2003, 8, 21)}
    >>> _rows2dict_field_helper(['LFARTN'], 0, [12345], {}, {'stuff': 'ruff'})
    {'LFARTN': 12345, 'stuff': 'ruff'}
    >>> _rows2dict_field_helper(['PNPRB'], 0, [12345], {}, {})
    {'PNPRB': Decimal('12345.00')}
    """
    
    feldname = fields[i]
    data = _fix_field(row[i], feldname)
    
    # fixup based on verbose field names / dict keys
    if feldname in mappings:
        # key ist der "schöne" Feldname
        finalkey = mappings[feldname]
    else:
        # key ist das AS400 Feldname
        finalkey = feldname
        
    if finalkey.endswith('_date'):
        # special mapping for date time fields
        if not data:
            rowdict[finalkey] = None
        else:
            rowdict[finalkey] = softm2date(data)
            # check if there is also a time field
            if feldname in DATETIMEDIR:
                rowdict.update(_combine_date_and_time(mappings, feldname, data, fields, row))
    else:
        # key ist der "schöne" feldname
        rowdict[finalkey] = data
    
    return rowdict
    

def _fix_field(data, feldname):
    """Fix field types returned by DB2/400 based on AS/400 fieldnames."""
    
    if feldname in DECIMALIZE2:
        # convert to 2 digits significant Decimal()
        return Decimal(str(data)).quantize(Decimal(10) ** -2)
    elif isinstance(data, unicode): # fix strings
        # due to various levels of braindamage in various programs we get unicode objects with latin-1
        # strings in them. So we first force unocode() -> str() and then decode to unicse
        try:
            data = data.decode('latin-1')
        except UnicodeEncodeError:
            rawdata = repr(data).strip('u')
            data = eval(rawdata) # this allows the odbc_bridge to 0wn us
            data = data.decode('latin-1')
        return data.strip()
    else:
        return data
    

def _combine_date_and_time(mappings, fieldname, data, fields, row):
    """If there is also a time field in addition to a date field combine them."""
    
    basename = '_'.join(mappings[fieldname].split('_')[:-1])
    timefield = DATETIMEDIR[fieldname]
    try:
        timepos = fields.index(timefield)
    except ValueError:
        return {}
    if (timepos and row[timepos] and
        not str(row[timepos]).startswith('9999')): # Zeit = 9999: Unbestimmt
        if len(str(int(data))) == 7:
            return {'basename': datetime.datetime(*(
                time.strptime(str(int(data)), '1%y%m%d')[:3]
                + time.strptime(str(int(row[timepos])), '%H%M%S')[3:6]))}
        else:
            raise ValueError
    return {}
    

class MoftSconnection(object):
    """Represents an connection which can execute SQL on the iSeries-AS/400."""
    
    def _get_tablename(self, name):
        """Generates the Name of a Table on the AS/400."""
        
        return "SMKDIFP.%s" % name
    
    def _raw_sql(self, querystr):
        """Executes an arbitary SQL - not meant for public use."""
        
        rows = self._execute_query(querystr, {}, [])
        return [[_fix_field(f) for f in r] for r in rows]
    
    def query(self, tables=None, condition=None, fields=None, querymappings=None,
              grouping=None, ordering=[], limit=None):
        r"""Execute a SELECT on the AS/400 turning the results in a list of dicts.
        
        In fields you can give a list of fields you are interested in. If fields is left empty the engine
        generates a list of field on it own by consulting the field mapping database in from 
        fields.MAPPINGDIR.
        
        >>> MoftSconnection().query('ALK00', condition="LKLFSN=4034544") #doctest: +ELLIPSIS
        [{'lager': 100, 'versandart': '013', ...}]
        
        To suppress mapping provide querymappings={} and fields=[].
        >>> MoftSconnection().query(tables=['XPN00'], condition="PNSANR=2255")
        [{'satznummer': 2255, 'preis': Decimal('16.10')}]
        >>> MoftSconnection().query(tables=['XPN00'], condition="PNSANR=2255",
        ... fields=['PNSANR', 'PNPRB'], querymappings={})
        [(2255, Decimal('16.10'))]
        
        To get only certain fields give a list of fieldnames in fields=[...].
        >>> MoftSconnection().query(tables=['XPN00'], condition="PNSANR=2255", fields=['PNPRB'])
        [(Decimal('16.10'),)]
        
        Joins are straightforward if used with condition="<expression>":
        >>> MoftSconnection().query(['XPN00', 'XPR00'], condition="PNSANR=PRSANR and PNSANR=2255",
        ... fields=['PRDTVO', 'PNPRB'])
        [{'preis': Decimal('16.10'), 'gueltig_ab_date': datetime.date(2004, 12, 16)}]
        
        Aggregate functions can be created by using the "grouping" keyword:
        >>> sorted(MoftSconnection().query('XLF00', fields=['LFARTN', 'SUM(LFMGLP)'], grouping=['LFARTN'],
        ... condition="LFLGNR=3"))
        [('65166/01', '0'), ('65198', '0'), ('76095', '0'), ('76102', '0'), ('ED76095', '0')]
        
        If desired "querymappings" can be used to return a list of dicts:
        >>> sorted(MoftSconnection().query('XLF00', fields=['LFARTN', 'SUM(LFMGLP)'], grouping=['LFARTN'],
        ... condition="LFLGNR=3", querymappings={'LFARTN': 'artnr',
        ... 'SUM(LFMGLP)': 'menge'})) #doctest: +ELLIPSIS
        [{'menge': '0', 'artnr': '65166/01'}, {'menge': '0', 'artnr': '65198'}, ...]
        
        We also should be - to a certain degree - be Unicode aware:
        >>> MoftSconnection().query(u'XKD00', u"KDKDNR LIKE '%18287'")[0]['ort'].encode('utf8')
        'G\xc3\xbcnzburg'
        """
        
        # fixup sloppy parameter passing
        if isinstance(tables, basestring):
            tables = [tables]
        if isinstance(grouping, basestring):
            grouping = [grouping]
        if not grouping:
            grouping = []
        if isinstance(ordering, basestring):
            ordering = [ordering]
        if isinstance(fields, basestring):
            fields = [fields]
        
        if not ordering:
            ordering = []
        if not fields:
            fields = []
        
        if querymappings == {} and not fields:
            raise RuntimeError("Please give fieldnames.")
        
        if querymappings is None and len(fields) != 1:
            querymappings = {}
            for table in tables:
                querymappings.update(MAPPINGDIR.get(table, {}))
        
        if not fields: # decuce fieldnames from querymappings
            fields = querymappings.keys()
        if not fields: # still nothing found
            raise RuntimeError("can't deduce field names, check fields.py")
        
        querystr = ["SELECT %s FROM %s" % (','.join(fields),
                                          ','.join([self._get_tablename(x) for x in tables]))]
        if condition:
            querystr.append('WHERE %s' % condition)
        if grouping:
            querystr.append('GROUP BY %s' % (','.join(grouping), ))
        if ordering:
            querystr.append('ORDER BY %s' % (','.join(ordering), ))
        if limit:
            querystr.append('FETCH FIRST %d ROWS ONLY' % limit)
        
        return self._execute_query(' '.join(querystr), querymappings, fields)
    
    def _select(self, query):
        param = urllib.quote(query)
        conn = httplib.HTTPConnection("odbcbridge.local.hudora.biz:8000")
        # conn.set_debuglevel(5)
        conn.request("GET", "/select?query=" + param)
        response = conn.getresponse()
        if response.status != 200:
            errorinfo = response.read()
            # TODO: this looks extremely fragile. Must have be drunk while coding this.
            # needs a better implementation
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
            rows = _rows2dict(fields, querymappings, rows)
        else:
            rows = [tuple([_fix_field(data, name) for data, name in zip(row, fields)]) for row in rows]
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
    """Get a MoftSconnection Object. Meant to one day introduce connection pooling."""
    
    return MoftSconnection()

if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    sys.exit(failure_count)
