#!/usr/bin/env python
# encoding: utf-8

"""husoftm/backend.py

husoftm is a toolkit for accessing a AS/400 running SoftM Suite.

This module enables connections to a SoftMexpress server which in
turn enables ODBC connections to the AS/400.

branched of connection3.py, Maximillian Dornseif on 2010-11-30.
Copyright (c) 2006, 2008-2011 HUDORA. All rights reserved.
"""


from decimal import Decimal
from husoftm2.fields import MAPPINGDIR, DATETIMEDIR, DECIMALIZE2, DECIMALIZE
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

config = object()
try:
    import config
except:
    pass

keychain = object()
try:
    import cs.keychain as keychain
except:
    pass


SOFTMEXPRESSHOST = 'api.hudora.biz:8082'


class SoftMError(Exception):
    """This is the base for all exceptions in this package."""
    pass


class TransientError(SoftMError):
    """There was a transient error while accessing the Data. Please try again later."""
    pass


class TimeoutException(SoftMError, IOError):
    """The the HTTP transport or SQL backend reports a timeout."""
    pass


class SQLError(SoftMError):
    """Exception for failure of raw_SQL """


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

    husoftm2.fields.DECIMALIZE2 and husoftm2.fields.DATETIMEDIR are used to determine special field handling.
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
    elif feldname in DECIMALIZE:
        return Decimal(str(data))
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
    """Combine date and time if there is a time field belonging to the date field."""

    # Basename is the resulting field name for the combined datetime value
    basename = '_'.join(mappings[fieldname].split('_')[:-1])

    # Find the position of the according time field in the result row
    # Return an empty dict if the field is not found
    timefield = DATETIMEDIR[fieldname]
    try:
        timepos = fields.index(timefield)
    except ValueError:
        return {}

    # The timefield is supposed to be of type int
    if not isinstance(row[timepos], int):
        return {}

    # Try to combine date and time if both values seem to be sane
    timevalue = '%06d' % int(row[timepos])
    if not timevalue.startswith('999999'):  # time = 999999 means undefined
        if len(str(int(data))) == 7:
            date = datetime.datetime.strptime("%sT%s" % (int(data), timevalue), '1%y%m%dT%H%M%S')
            return {basename: date}
        else:
            raise ValueError
    return {}


def _get_tablename(name):
    """Generates the Name of a Table on the AS/400."""
    return "SMKDIFP.%s" % name


def execute(url, args, ua='', bust_cache=False):
    """Execute SQL comand - SQL generation is done by the Server"""

    url, headers = execute_prepare_headers(url, args, bust_cache)
    status, headers, content = huTools.http.fetch(url,
                                                  method='GET',
                                                  headers=headers,
                                                  ua='%s/husoftm2.backend' % ua,
                                                  timeout=50)
    return execute_process_results(status, content, headers)


def execute_prepare_headers(url, args, bust_cache):
    args_encoded = urllib.urlencode({'q': hujson.dumps(args)})
    url = ("/%s?" % url) + args_encoded
    headers = {}
    # See http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4 for the reasoning here
    if bust_cache:
        logging.debug("busting cache")
        headers.update({'Cache-Control': "no-cache"})
        url += '&ts=%s' % time.time()
    # sign request
    digest = hmac.new(_find_credentials(), url, hashlib.sha1).hexdigest()
    softmexpresshost = os.environ.get('SOFTMEXPRESSHOST', SOFTMEXPRESSHOST)
    headers.update({'X-sig': digest})
    url = 'http://' + softmexpresshost + url
    return url, headers


def execute_process_results(status, content, headers):
    if status != 200:
        # TODO: this looks extremely fragile. Must have be drunk while coding this.
        # needs a better implementation
        if content.startswith('Internal Error: {\'EXIT\',\n                    {timeout'):
            raise TimeoutException(content)
        logging.error(headers)
        logging.error(content)
        raise RuntimeError("Server Error: %r" % content)
    return content


def raw_SQL(command, ua=''):
    """Reines SQL ausführen - darf nur von 4-reviewtem Code benutzt werden."""

    args_encoded = urllib.urlencode({'query': command})
    url = "/raw?" + args_encoded
    headers = {'Cache-Control': "no-cache"}
    url += '&ts=%s' % time.time()
    # sign request
    digest = hmac.new(_find_credentials(), url, hashlib.sha1).hexdigest()
    softmexpresshost = os.environ.get('SOFTMEXPRESSHOST', SOFTMEXPRESSHOST)
    headers = {'X-sig': digest}
    url = 'http://' + softmexpresshost + url
    method = 'POST'
    if command.upper().startswith('SELECT'):
        method = 'GET'
    # Wir nutzen POST mit einem Query-String in der URL. Das ist dirty
    # See http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.9.4 for the reasoning here

    method = 'GET'
    if command.startswith('UPDATE'):
        method = 'POST'
    if command.startswith('INSERT'):
        method = 'POST'
    status, headers, content = huTools.http.fetch(url,
                                                  method=method,
                                                  headers=headers,
                                                  content={'query': command, 'ua': ua},
                                                  ua='%s/husoftm2.backend' % ua,
                                                  timeout=3000)
    if status != 200:
        logging.error("%s %s" % (method, url))
        raise SQLError(status, content)

    # Not all replies are JSON encoded
    try:
        return hujson.loads(content)
    except:
        return content
    return content


def query(tables=None, condition=None, fields=None, querymappings=None,
          joins=None,
          grouping=None, ordering=None, limit=None, ua='', cachingtime=300):
    r"""Execute a SELECT on the AS/400 turning the results in a list of dicts.

    In fields you can give a list of fields you are interested in. If fields is left empty the engine
    generates a list of field on it own by consulting the field mapping database in from
    fields.MAPPINGDIR.

    >>> query('ALK00', condition="LKLFSN=4034544") #doctest: +ELLIPSIS
    [{'lager': 100, ...}]

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
    [(u'65166/01', u'0'), (u'65198', u'0'), (u'76095', u'0'), (u'76102', u'0'), (u'ED76095', u'0')]

    If desired "querymappings" can be used to return a list of dicts:
    >>> sorted(query('XLF00', fields=['LFARTN', 'SUM(LFMGLP)'], grouping=['LFARTN'],
    ... condition="LFLGNR=3", querymappings={'LFARTN': 'artnr',
    ... 'SUM(LFMGLP)': 'menge'})) #doctest: +ELLIPSIS
    [{'menge': u'0', 'artnr': u'65166/01'}, {'menge': u'0', 'artnr': u'65198'}, ...]

    You can use 'joins' to define LEFT OUTER JOINs. E.g.:
    >>> rows = query(['XKD00'],
    ...              condition="KDKDNR='%8d'" % int(66669),
    ...              joins=[('XXC00', 'KDKDNR', 'XCADNR'),
    ...                     ('XKS00', 'KDKDNR', 'KSKDNR'),
    ...                     ('AKZ00', 'KDKDNR', 'KZKDNR')])

    Will result in "SELECT * FROM XKD00 LEFT OUTER JOIN XXC00 ON KDKDNR=XCADNR LEFT OUTER
    JOIN XKS00 ON KDKDNR=KSKDNR LEFT OUTER JOIN AKZ00 ON KDKDNR=KZKDNR WHERE KDKDNR='   10001'".

    We also should be - to a certain degree - be Unicode aware:
    >>> query(u'XKD00', u"KDKDNR LIKE '%18287'")[0]['ort'].encode('utf8')
    'G\xc3\xbcnzburg'

    Results are cached for 300 seconds unless you set something else via the cachingtime parameter.
    """

    args, bust_cache, querymappings, fields = query_prepare_parameters(tables, grouping, ordering,
        fields, joins, querymappings, ua, condition, limit, cachingtime)

    start = time.time()
    result = execute('sql', args, ua=ua, bust_cache=bust_cache)
    rows = hujson.loads(result)
    delta = time.time() - start
    if delta > 5:
        logging.warning("Slow (%.3fs) SQL query in  %s", delta, args)
    return query_process_results(querymappings, fields, rows, args, cachingtime)


def query_async(tables=None, condition=None, fields=None, querymappings=None,
          joins=None,
          grouping=None, ordering=None, limit=None, ua='', cachingtime=300,
          returnhandler=lambda x: x):

    def internelreturnhandler(status, rheaders, rcontent):
        result = execute_process_results(status, rcontent, rheaders)
        rows = hujson.loads(result)
        processed = query_process_results(querymappings, fields, rows, args, cachingtime)
        return returnhandler(processed)

    args, bust_cache, querymappings, fields = query_prepare_parameters(tables, grouping, ordering,
        fields, joins, querymappings, ua, condition, limit, cachingtime)

    # async implementation of `execute()`
    url, headers = execute_prepare_headers('sql', args, bust_cache)
    return huTools.http.fetch_async(url, method='GET',
                                    headers=headers,
                                    ua='%s/husoftm2.backend (async)' % ua,
                                    timeout=26, returnhandler=internelreturnhandler)


def query_prepare_parameters(tables, grouping, ordering, fields, joins, querymappings, ua, condition, limit, cachingtime):
    # fixup sloppy parameter passing
    if isinstance(tables, basestring):
        tables = [tables]
    if isinstance(grouping, basestring):
        grouping = [grouping]
    if isinstance(ordering, basestring):
        ordering = [ordering]
    if isinstance(fields, basestring):
        fields = [fields]

    if not joins:
        joins = []
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
        jointables = [table for table, foo, bar in joins]
        for table in tables + jointables:
            # dubletten = set(querymappings.values()) & set(MAPPINGDIR.get(table, {}).values())
            # if dubletten:
            #     logging.warning('field name clash: %s' % list(dubletten))
            querymappings.update(MAPPINGDIR.get(table, {}))

    if not fields:  # decuce fieldnames from querymappings
        fields = querymappings.keys()
    if not fields:  # still nothing found
        raise RuntimeError("can't deduce field names, check fields.py")

    args = dict(fields=fields, tablenames=tablenames, tag=ua)
    if condition:
        args['condition'] = condition
    if grouping:
        args['grouping'] = grouping
    if ordering:
        args['ordering'] = ordering
    if limit:
        args['limit'] = limit
    if joins:
        # ensure a list of 3-tuples
        joins = [(_get_tablename(a), b, c) for (a, b, c) in joins]
        args['joins'] = joins

    bust_cache = True
    if cachingtime > 0:
        bust_cache = False
    return args, bust_cache, querymappings, fields


def query_process_results(querymappings, fields, rows, args, cachingtime):
    if querymappings:
        rows = _rows2dict(fields, querymappings, rows)
    else:
        rows = [tuple([_fix_field(data, name) for data, name in zip(row, fields)]) for row in rows]
    return rows


def x_en(tablename, condition, ua=''):
    """Setze Status in Tabelle auf 'X'

    Kann auch andere Tabellenzustände setzen, wenn SoftMexpress dies vorsieht.

    >>> x_en('ISR00', 'IRKBNR=930429 AND IRAUPO=13')
    '1'
    """

    args = dict(tablename=tablename,
                condition=condition,
                tag=ua)

    result = execute('x_en', args, ua=ua)
    if result != '1\n':
        if result.strip() == '0':
            logging.info("No columns updated: %s %s %r", tablename, condition, result)
        else:
            raise RuntimeError("UPDATE Problem: %s %s %r" % (tablename, condition, result))
    return result.rstrip('\n')


if __name__ == '__main__':
    failure_count, test_count = doctest.testmod()
    sys.exit(failure_count)
