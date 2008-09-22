#!/usr/bin/env python
# encoding: utf-8
"""
mock_as400.py - simulation an AS/400 for testing purposes.

Created by Maximillian Dornseif on 2007-05-24.
Copyright (c) 2007 HUDORA GmbH. All rights reserved.
"""

__revision__ = "$Revision$"

responses = {
# Setartikel Bezogenen Abfragen
("SELECT SKLFNR,SKKART,SKMENG FROM SMKDIFP.ASK00 WHERE SKARTN='00049'", None):
    [[1.0, 'A42438', 1.0], [2.0, 'A42439', 1.0], [3.0, 'A42440', 1.0], [4.0, 'A42441', 2.0]],
("SELECT SKLFNR,SKKART,SKMENG FROM SMKDIFP.ASK00 WHERE SKARTN='00537'", None):
    [[1.0, '42050/A', 1.0], [2.0, '42051/A', 3.0], [3.0, '42052/A', 1.0]],
# Auftragsposition
("SELECT AKSFAK,AKSFAL,APLGNR,APFA4A,APKZPS,APDTZU,AKLKZ,AKSBNR,APKZVA,APMG01,APFWRT,APKZTA,APMEKZ,APPREL,"
  "AKLSCO,APDTKD,APARTN,APPREV,APKZAE,APKZZL,AKNRKD,APOWRT,AKDTLT,APKZMA,AKDTKD,AKKZVA,APMESL,APKZKO,APMNGR,"
  "APJNZU,APMNGG,APMNGF,APMNGB,APMNGL,AKAUFN,APKZRE,AKKBCO,APVGPO,APVGNR,APDTLN,APKZFG,AKDTKW,AKAUFA,APMNG,"
  "AKKZTF,APKZLF,APZTZU,APSBNR,APAUPO,APDTLT FROM SMKDIFP.AAP00,SMKDIFP.AAK00 WHERE AKAUFN=APAUFN AND "
  "APKZVA=0 AND AKKZVA=0 AND APAUFA<>'U' AND APAUFA<>'R' AND AKSTAT<>'X' AND APSTAT<>'X' AND APARTN='10105' "
  "AND APLGNR=100 ORDER BY APDTLT", None):
    [[0.0, 0.0, 100.0, '0', 0.0, 0.0, 'D', 32.0, 0.0, 0.0, 208.5599999999999, 0.0, 0.0, 33.949999999999999,
      0.0, 1070420.0, '10105', 4.960000000000001, 1.0, 1.0, '4500022162', 512.75999999999999, 1070420.0,
      1.0, 1070430.0, 0.0, 1.0, 0.0, 0.0, '', 0.0, 126.0, 137.0, 0.0, 172101.0, 0.0, 0.0, 0.0, 0.0, 1070420.0,
      '', 1070420.0, '', 137.0, 1.0, 1.0, 0.0, 38.0, 1.0, 1070420.0]],
}
querylog = []


def sql(query, params=None):
    """Mock execute a SQL query"""
    querylog.append((query, params))
    # return responses[(query, params)]
    return responses.get((query, params), [])
