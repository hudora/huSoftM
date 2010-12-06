#!/usr/bin/env python
# encoding: utf-8

"""husoftm/backend.py

husoftm is a toolkit for accessing a AS/400 running SoftM Suite.

This module enables connections to a SoftMexpress server which in
turn enables ODBC connections to the AS/400.

branched of connection3.py, Maximillian Dornseif on 2010-11-30.
Copyright (c) 2006, 2008, 2009, 2010 HUDORA. All rights reserved.
"""


from decimal import Decimal
from husoftm2.fields import MAPPINGDIR, DATETIMEDIR, DECIMALIZE2
from husoftm2.tools import softm2date
import datetime
import doctest
import hashlib
import hmac
import huTools.http
import huTools.hujson as hujson
import logging
import os
import sys
import time
import urllib


try:
    from django.conf import settings
    # trigger the lazy importer
    getattr(settings, 'DUMMY', None)
except (ImportError, EnvironmentError):
    settings = object()

try:
    import config
except:
    config = object()

try:
    import cs.keychain as keychain
except:
    keychain = object()


class DummyCache(object):
    def __init__(self, *args, **kwargs):
        pass

    def add(self, *args, **kwargs):
        pass

    def get(self, key, default=None):
        return default


try:
    from google.appengine.api import memcache
except ImportError:
    memcache = DummyCache()


def _find_credentials(credentials=None):
    if not credentials:
        credentials = getattr(settings, 'SOFTMEXPRESS_CREDENTIALS', None)
    if not credentials:
        credentials = getattr(config, 'SOFTMEXPRESS_CREDENTIALS', None)
    if not credentials:
        credentials = getattr(keychain, 'SOFTMEXPRESS_CREDENTIALS', None)
    if not credentials:
        credentials = os.environ.get('SOFTMEXPRESS_CREDENTIALS', None)
    if not credentials:
        raise RuntimeError('set SOFTMEXPRESS_CREDENTIALS')
    return credentials


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

    fields.DECIMALIZE2 and g.DATETIMEDIR are used to determine special field handling.
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
    elif isinstance(data, unicode):  # fix strings
        # due to various levels of braindamage in various programs we get unicode objects with latin-1
        # strings in them. So we first force unicode() -> str() and then decode to unicode
        try:
            data = data.decode('latin-1')
        except UnicodeEncodeError:
            rawdata = repr(data).strip('u')
            data = eval(rawdata, {}, {})  # this allows the odbc_bridge to 0wn us
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
        not str(row[timepos]).startswith('9999')):  # Zeit = 9999: Unbestimmt
        if len(str(int(data))) == 7:
            return {basename: datetime.datetime(*(
                time.strptime(str(int(data)), '1%y%m%d')[:3]
                + time.strptime(str(int(row[timepos])), '%H%M%S')[3:6]))}
        else:
            raise ValueError
    return {}


def _get_tablename(name):
    """Generates the Name of a Table on the AS/400."""
    return "SMKDIFP.%s" % name


def query(tables=None, condition=None, fields=None, querymappings=None,
          grouping=None, ordering=None, limit=None, ua='', cachingtime=300):
    r"""Execute a SELECT on the AS/400 turning the results in a list of dicts.

    In fields you can give a list of fields you are interested in. If fields is left empty the engine
    generates a list of field on it own by consulting the field mapping database in from
    fields.MAPPINGDIR.

    >>> query('ALK00', condition="LKLFSN=4034544") #doctest: +ELLIPSIS
    [{'lager': 100, 'versandart': '013', ...}]

    To suppress mapping provide querymappings={} and fields=[].
    >>> query(tables=['XPN00'], condition="PNSANR=2255")
    [{'satznummer': 2255, 'preis': Decimal('16.10')}]
    >>> query(tables=['XPN00'], condition="PNSANR=2255",
    ... fields=['PNSANR', 'PNPRB'], querymappings={})
    [(2255, Decimal('16.10'))]

    To get only certain fields give a list of fieldnames in fields=[...].
    >>> query(tables=['XPN00'], condition="PNSANR=2255", fields=['PNPRB'])
    [(Decimal('16.10'),)]

    Joins are straightforward if used with condition="<expression>":
    >>> query(['XPN00', 'XPR00'], condition="PNSANR=PRSANR and PNSANR=2255",
    ... fields=['PRDTVO', 'PNPRB'])
    [{'preis': Decimal('16.10'), 'gueltig_ab_date': datetime.date(2004, 12, 16)}]

    Aggregate functions can be created by using the "grouping" keyword:
    >>> sorted(query('XLF00', fields=['LFARTN', 'SUM(LFMGLP)'], grouping=['LFARTN'],
    ... condition="LFLGNR=3"))
    [('65166/01', '0'), ('65198', '0'), ('76095', '0'), ('76102', '0'), ('ED76095', '0')]

    If desired "querymappings" can be used to return a list of dicts:
    >>> sorted(query('XLF00', fields=['LFARTN', 'SUM(LFMGLP)'], grouping=['LFARTN'],
    ... condition="LFLGNR=3", querymappings={'LFARTN': 'artnr',
    ... 'SUM(LFMGLP)': 'menge'})) #doctest: +ELLIPSIS
    [{'menge': '0', 'artnr': '65166/01'}, {'menge': '0', 'artnr': '65198'}, ...]

    We also should be - to a certain degree - be Unicode aware:
    >>> query(u'XKD00', u"KDKDNR LIKE '%18287'")[0]['ort'].encode('utf8')
    'G\xc3\xbcnzburg'

    Results are cached for 300 seconds unless you set something else via the cachingtime parameter.
    """

    # fixup sloppy parameter passing
    if isinstance(tables, basestring):
        tables = [tables]
    if isinstance(grouping, basestring):
        grouping = [grouping]
    if isinstance(ordering, basestring):
        ordering = [ordering]
    if isinstance(fields, basestring):
        fields = [fields]

    if not grouping:
        grouping = []
    if not ordering:
        ordering = []
    if not fields:
        fields = []
    tablenames = [_get_tablename(x) for x in tables]

    if querymappings == {} and not fields:
        raise RuntimeError("Please give fieldnames.")
    if querymappings is None and len(fields) != 1:
        querymappings = {}
        for table in tables:
            querymappings.update(MAPPINGDIR.get(table, {}))

    if not fields:  # decuce fieldnames from querymappings
        fields = querymappings.keys()
    if not fields:  # still nothing found
        raise RuntimeError("can't deduce field names, check fields.py")

    args = dict(fields=fields,
                tablenames=tablenames,
                tag=ua)
    if condition:
        args['condition'] = condition
    if grouping:
        args['grouping'] = grouping
    if ordering:
        args['ordering'] = ordering
    if limit:
        args['limit'] = limit

    rows = memcache.get('husoftm_query_%r_%r' % (querymappings, args))
    if rows:
        return rows

    #logging.debug("Starting SQL query: %s", args)
    start = time.time()
    args_encoded = urllib.urlencode({'q': hujson.dumps(args)})
    url = "/sql?" + args_encoded
    digest = hmac.new(_find_credentials(), url, hashlib.sha1).hexdigest()
    (status, headers, content) = huTools.http.fetch('http://api.hudora.biz:8082' + url,
                                                    method='GET',
                                                    headers={'X-sig': digest},
                                                    ua='%s/husoftm2.backend' % ua)
    if status != 200:
        # TODO: this looks extremely fragile. Must have be drunk while coding this.
        # needs a better implementation
        if content.startswith('Internal Error: {\'EXIT\',\n                    {timeout'):
            raise TimeoutException(content)
        raise RuntimeError("Server Error: %r" % content)
    rows = hujson.loads(content)

    if querymappings:
        rows = _rows2dict(fields, querymappings, rows)
    else:
        rows = [tuple([_fix_field(data, name) for data, name in zip(row, fields)]) for row in rows]

    logging.debug("Done SQL query in %.3fs: %s", time.time() - start, args)
    memcache.add(key='husoftm_query_%r_%r' % (querymappings, args),
                 value=rows, time=cachingtime)
    return rows


if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    sys.exit(failure_count)
