#!/usr/bin/env python
# encoding: utf-8
"""
huSoftM/kunden.py - High Level Access Kundeninformationen. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-04-13.
Copyright (c) 2007, 2010 HUDORA GmbH. All rights reserved.
"""

from husoftm2.backend import query
import datetime
import husoftm2.tools
import logging


betreuerdict = {
            'verkauf': 'Verkaufsinnendienst',
            'bbonrath': 'Birgit Bonrath',
            'cgiermann': 'Carsten Giermann',
            'ngerloff': 'Nadiene Gerloff',
            'ajames': 'Andrea James',
            'alangen': 'Anja Langen',
            'cblumberg': 'Claudia Blumberg',
            'cgerlach': 'Christoph Gerlach',
            'dgrossmann': u'Dirk Grossmann',
            'export': u'Export',
            'falin': u'Fuesun Alin',
            'jtiszekker': u'Juergen Tiszekker',
            'jwestpahl': u'Jutta Westphal',
            'kschulze': u'Katrin Schulze',
}


def get_kundennummern():
    """Returns a list of all 'Kundennummern'"""
    rows = query('XKD00', fields=['KDKDNR'])
    return ["SC%s" % int(x[0]) for x in rows]


def get_changed_after(date):
    """Returns a list of all Kundennummern where the underlying Data has changed since <date>."""

    date = int(date.strftime('1%y%m%d'))
    rows1 = query('XKD00', fields=['KDKDNR'], condition="KDDTER>%d OR KDDTAE>=%d" % (date, date))
    rows2 = query('AKZ00', fields=['KZKDNR'], condition="KZDTAE>=%d" % (date))
    return list(set(["SC%s" % int(x[0]) for x in rows1]) | set(["SC%s" % int(x[0]) for x in rows2]))


def get_kunde(kundennr):
    """Get the Kunde object representing Kundennummer <kundennr>.

    <kundennr> must be an Integer in the Range 10000..99999.
    If no data exists for that KdnNr ValueError is raised."""

    kundennr = int(kundennr.strip('SC'))
    rows = query(['XKD00'],
                 condition="KDKDNR='%8d' AND KDSTAT<>'X'" % kundennr,
                 joins=[('XXC00', 'KDKDNR', 'XCADNR'),
                        ('XKS00', 'KDKDNR', 'KSKDNR'),
                        ('AKZ00', 'KDKDNR', 'KZKDNR')])
    # Kreditoren aus XXC00 entfernen - im JOIN geht das nicht
    rows = [x for x in rows if x.get('art') != 'K']
    if len(rows) > 1:
        raise RuntimeError("Mehr als einen Kunden gefunden: %r" % kundennr)
    if not rows:
        raise ValueError("Keine Daten für Kundennummer %r gefunden" % kundennr)
    return _softm_to_dict(rows[0])


def get_kunde_by_iln(iln):
    """Get Kunden Address based on ILN.

    See http://cybernetics.hudora.biz/projects/wiki/AddressProtocol for the structure of returned data.
    <iln> must be an valit GLN/ILN encoded as an String.
    If no data exists for that GLN/ILN ValueError is raised.
    """
    rows = query(['XKS00'], condition="KCE2IL='%s'" % (int(iln), ))
    if rows:
        # stammadresse
        return get_kunde(rows[0]['kundennr'])
    else:
        # abweichende Lieferadresse
        rows = query(['AVA00'], condition="VAILN='%s'" % (int(iln), ))
        if rows:
            rows2 = query(['XXA00'], condition="XASANR='%s'" % (int(rows[0]['satznr']), ))
            if rows2:
                kunde = _softm_to_dict(rows2[0])
                kunde['kundennr'] = kunde['kundennr'] + ('/%03d' % int(rows[0]['versandadresssnr']))
                return kunde
    raise ValueError("Keine Daten für GLN/ILN %r gefunden" % iln)


def get_lieferadressen(kundennr):
    """Sucht zusätzliche Lieferadressen für eine Kundennr raus.

    Gibt eine Liste aller möglichen Lieferadressen in Form von Kunden-Dicts zurück.
    """

    kundennr = kundennr.strip('SC')
    avrows = query(['AVA00'], condition="VAKDNR='%8s' AND VASTAT <>'X'" % int(kundennr))
    kunden = []
    for row in avrows:
        xarows = query(['XXA00'], condition="XASANR='%s'" % int(row['satznr']))
        if xarows:
            if len(xarows) != 1:
                raise RuntimeError("Kunden-Lieferadresse inkonsistent: %s/%s" % (kundennr, row['satznr']))
            kunde = _softm_to_dict(xarows[0])
            kunde['kundennr'] = '%s/%03d' % (kunde['kundennr'], int(row['versandadresssnr']))
            kunden.append(kunde)
    return kunden


def _softm_to_dict(row):
    row = dict((k, v) for (k, v) in row.items() if v is not None)
    ret = dict(kundennr="SC%s" % row.get('kundennr', ''),               # 10003
               name1=row.get('name1', ''),                              # Sport A
               name2=row.get('name2', ''),
               name3=row.get('name3', ''),
               strasse=row.get('strasse', ''),                          # Marktplatz 5
               land=husoftm2.tools.land2iso(row['laenderkennzeichen']),  # D
               plz=row.get('plz', ''),                                  # 42477
               ort=row.get('ort', ''),                                  # Neurade
               tel=row.get('tel', ''),
               fax=row.get('fax', ''),
               mobil=row.get('mobil', ''),
               mail=row.get('mail', ''),
               ustid=row.get('ustid', ''),
               adressdatei_id=row.get('adressdatei_id', ''),
               company=row.get('company', ''),                          # '06'
               # gebiet=row.get('gebiet', ''),                          # ': u'04'
               # distrikt=row.get('distrikt', ''),                      # ': u'16'
               # vertreter=row.get('vertreter', ''),                    # ': u'201'
               # branche=row.get('branche', ''),                        # ': u'13'
               # kundengruppe=row.get('kundengruppe', ''),
               betreuer_handle=row.get('betreuer', ''),                        # ': u'Birgit Bonrath'
               interne_firmennr=row.get('interne_firmennr', ''),        # ': u''
               unsere_lieferantennr=row.get('unsere_lieferantennumemr', ''),
              )
    ret['betreuer'] = betreuerdict.get(ret['betreuer_handle'], '')
    if not ret['betreuer']:
        logging.error('Kunde %s (%s) hat mit %r keinen gueltigen Betreuer' % (ret['name1'],
                                                                              ret['kundennr'],
                                                                              ret['betreuer_handle']))
    if 'verbandsnr' in row and row['verbandsnr']:
        ret['verbandsnr'] = 'SC%s' % row['verbandsnr']
        ret['mitgliednr'] = row.get('mitgliednr', '')
    if 'iln' in row and row['iln']:
        ret['iln'] = unicode(int(row['iln'])).strip()
    if row['erfassung_date']:
        ret['erfassung'] = row['erfassung_date']
    if row['aenderung_date']:
        ret['aenderung'] = row['aenderung_date']
    else:
        ret['aenderung'] = ret['erfassung']
    return ret


def _selftest():
    """Test basic functionality"""
    from pprint import pprint
    get_kundennummern()
    get_kunde('66669')
    print get_kunde('SC66669')
    print get_kunde_by_iln('4306544031019')
    print get_kunde_by_iln('4306544000008')
    print get_changed_after(datetime.date(2010, 11, 1))
    print get_lieferadressen('SC28000')
    pprint(get_kunde('64090'))
    print get_kunde('SC64000')
    print get_kunde('10001')
    print get_kunde('SC67100')


if __name__ == '__main__':
    _selftest()
