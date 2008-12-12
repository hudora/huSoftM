#!/usr/bin/env python
# encoding: utf-8
"""
test odbc bridge - this expects the bridge running on localhost:8000 and expects a table HDTST0 like this:

 Flg  Feld                Datei              Art                   L{nge  Skala
      THECHAR             HDTST0             CHARACTER                10       
      THEVARCHAR          HDTST0             VARCHAR                  10       
      THEINT              HDTST0             INTEGER                   9       
      THEFLOAT            HDTST0             FLOAT                    16    15 
      THEDECIMAL          HDTST0             DECIMAL                  12     3 

Created by Maximillian Dornseif on 2008-12-11.
Copyright (c) 2008 HUDORA. All rights reserved.
"""

import getopt
import httplib
# httplib.HTTPConnection.debuglevel = 1
import simplejson as json
import sys
import time
import urllib
import urllib2
import urlparse

def get(url, sql):
    urlp = urlparse.urlparse(url)
    param = urllib.quote(sql)
    conn = httplib.HTTPConnection(urlp.netloc)
    conn.request("GET", ("%s?query=" % urlp.path) + param)
    response = conn.getresponse()
    if response.status != 200:
        errorinfo = response.read()
        raise RuntimeError("Server Error: %r %r" % (errorinfo, sql))
    return json.loads(response.read())

def post(url, sql):
    urlp = urlparse.urlparse(url)
    data = urllib.urlencode([('query', sql)])
    conn = httplib.HTTPConnection(urlp.netloc)
    conn.request("POST", ("%s" % urlp.path), data, {'Content-Type': 'application/x-www-form-urlencoded'})
    response = conn.getresponse()
    if response.status != 200:
        errorinfo = response.read()
        raise RuntimeError("Server Error: %r %r" % (errorinfo, sql))
    return json.loads(response.read())


def test():
    start = time.time()
    # does it work at all?
    query = ("SELECT LFARTN, SUM(LFMGLP) FROM XLF00 WHERE LFLGNR=100"
            " AND LFMGLP<>0  AND LFSTAT<>'X' GROUP BY (LFARTN)")
    get('http://localhost:8000/select', query)
    
    # INSERT
    ts = str(int(time.time()))
    query = ("INSERT INTO HDTST0 (THECHAR, THEVARCHAR, THEINT, THEFLOAT, THEDECIMAL)"
             " VALUES ('%s', '%s', %s, %f, 12345678)") % (ts, ts, ts, time.time())
    assert post('http://localhost:8000/insert', query) == 1
    
    # UPDATE
    query = ("UPDATE HDTST0 SET THECHAR='XXX' WHERE THEINT = %s") % ts
    assert post('http://localhost:8000/update', query) == 1
    
    # SELECT
    query = ("SELECT * FROM HDTST0 WHERE THECHAR='XXX' AND THEINT=%s" % ts)
    reply = get('http://localhost:8000/select', query)
    assert len(reply) == 1
    
    # check if broken SQL raises an exception
    query = ("UPDATE HDTST0_br0kn SET THECHAR='XXX' WHERE THEINT = %s") % ts
    try:
        post('http://localhost:8000/update', query)
    except RuntimeError:
        pass # everything is fine
    else:
        assert True == False # No exception thrown

    # check if broken SQL raises an exception
    query = ("SELECT * FROM HDTST0_br0kn WHERE THECHAR='XXX' AND THEINT=%s") % ts
    try:
        post('http://localhost:8000/select', query)
    except RuntimeError:
        pass # everything is fine
    else:
        assert True == False # No exception thrown
    
    # try if SELECT still works
    query = ("SELECT * FROM HDTST0 WHERE THECHAR='XXX' AND THEINT=%s" % ts)
    reply = get('http://localhost:8000/select', query)
    assert len(reply) == 1
    
    # INSERT with data to long
    query = ("INSERT INTO HDTST0 (THECHAR, THEVARCHAR, THEINT, THEFLOAT, THEDECIMAL)"
             " VALUES ('12345678901', '12345678901', 12345678901, 12345678901.367598, 12345678901)")
    try:
        post('http://localhost:8000/insert', query)
    except RuntimeError:
        pass # everything is fine
    else:
        assert True == False # No exception thrown
    
    return time.time() - start

def main():
    print "Calltimes: %.3f" % test(),
    time.sleep(1)
    print "%.3f" % test(),
    time.sleep(1)
    print "%.3f" % test()
    
if __name__ == "__main__":
    main()
