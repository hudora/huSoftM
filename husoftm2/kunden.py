#!/usr/bin/env python
# encoding: utf-8
"""
huSoftM/kunden.py - High Level Access Kundeninformationen. Teil von huSoftM.

Created by Maximillian Dornseif on 2007-04-13.
Copyright (c) 2007, 2010, 2011 HUDORA GmbH. All rights reserved.
"""
import datetime
import logging
from decimal import Decimal

import husoftm2.tools
from husoftm2.backend import query, raw_SQL


betreuerdict = {
            'ajames': 'Andrea James',
            'alangen': 'Anja Langen',
            'bbonrath': 'Birgit Bonrath',
            'cblumberg': 'Claudia Blumberg',
            'cgerlach': 'Christoph Gerlach',
            'cgiermann': 'Carsten Giermann',
            'dgrossmann': u'Dirk Grossmann',
            'export': u'Export',
            'falin': u'Füsun Alin',
            'kschulze': u'Katrin Schulze',
            'mfischer': u'Melanie Fischer',
            'ngerloff': 'Nadine Gerloff',
            'verkauf': 'Verkaufsinnendienst',
}


# Die Liste wird von Frau James gepflegt.
# Diese Werte entsprechen dem Stand Maerz 2011
vertreterdict = {
    u'': u'unbekannt',
    u'100': u'Laurenz Kooistra',
    u'101': u'Laurenz Kooistra',
    u'102': u'Laurenz Kooistra',
    u'200': u'Frank Ilmert',
    u'201': u'Frank Ilmert',
    u'202': u'Frank Ilmert',
    u'300': u'Christoph Gerlach',
    u'301': u'Thomas Harnischmacher',
    u'302': u'Tobias Lehnert',
    u'400': u'Andreas Karger',
    u'401': u'Philipp Rauch',
    u'500': u'Jürgen Tiszekker',
    u'501': u'Alexander Schrey',
    u'800': u'Thomas Ludwig',
    u'900': u'Silvertoys',
    u'901': u'Agefe Scandinavia AB',
    u'910': u'Optitrade & Service AG',
    u'999': u'Hauskunde',
}


def get_kundennummern():
    """Returns a list of all 'Kundennummern'"""
    rows = query('XKD00', fields=['KDKDNR'], condition="KDSTAT <> 'X'")
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

    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    rows = query(['XKD00'],
                 condition="KDKDNR=%s AND KDSTAT<>'X'" % husoftm2.tools.pad('KDKDNR', kundennr),
                 joins=[('XKS00', 'KDKDNR', 'KSKDNR'),
                        ('AKZ00', 'KDKDNR', 'KZKDNR')])
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

    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    avrows = query(['AVA00'], condition="VAKDNR='%8s' AND VASTAT <>'X'" % kundennr)
    kunden = []
    for row in avrows:
        xarows = query(['XXA00'], condition="XASANR='%s'" % int(row['satznr']))
        if xarows:
            if len(xarows) != 1:
                raise RuntimeError("Kunden-Lieferadresse inkonsistent: %s/%s" % (kundennr, row['satznr']))
            kunde = _softm_to_dict(xarows[0])
            kunde['kundennr'] = '%s.%03d' % (kunde['kundennr'], int(row['versandadresssnr']))
            kunden.append(kunde)
    return kunden


def get_lieferadresse(warenempfaenger):
    """Lieferadresse für Warenempfänger ermitteln"""

    warenempfaenger = str(warenempfaenger)
    if warenempfaenger.startswith('SC'):
        warenempfaenger = warenempfaenger[2:]
    tmp = warenempfaenger.split('.')
    if len(tmp) == 1:
        return get_kunde(warenempfaenger)

    rows = query(['AVA00'], joins=[('XXA00', 'VASANR', 'XASANR')],
                 condition="VAKDNR='%8s' AND VAVANR=%03d AND VASTAT <>'X'" % (int(tmp[0]), int(tmp[1])))
    if len(rows) == 1:
        return _softm_to_dict(rows[0])
    elif len(rows) > 1:
        raise RuntimeError(u"Kunden-Lieferadresse inkonsistent: %s" % warenempfaenger)


def _softm_to_dict(row):
    """Daten aus SoftM in ein dict umwandeln."""
    row = dict((k, v) for (k, v) in row.items() if v is not None)
    ret = dict(kundennr="SC%s" % row.get('kundennr', ''),               # 10003
               name1=row.get('name1', ''),                              # Sport A
               name2=row.get('name2', ''),
               name3=row.get('name3', ''),
               strasse=row.get('strasse', ''),                          # Marktplatz 5
               land=husoftm2.tools.land2iso(row['laenderkennzeichen']),
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
               distrikt=row.get('distrikt', ''),
               vertreter_handle=row.get('vertreter', ''),
               # branche=row.get('branche', ''),                        # ': u'13'
               # kundengruppe=row.get('kundengruppe', ''),
               betreuer_handle=row.get('betreuer', ''),                 # ': u'Birgit Bonrath'
               interne_firmennr=row.get('interne_firmennr', ''),        # ': u''
               unsere_lieferantennr=row.get('unsere_lieferantennr', ''),
              )
    ret['name'] = ' '.join((ret['name1'], ret['name2'])).strip()
    ret['betreuer'] = betreuerdict.get(ret['betreuer_handle'], '')
    if not ret['betreuer']:
        logging.error('Kunde %s (%s) hat mit "%s" keinen gueltigen Betreuer', ret['name1'],
                                                                              ret['kundennr'],
                                                                              ret['betreuer_handle'])
    ret['vertreter'] = vertreterdict.get(ret['vertreter_handle'], '')
    if not ret['vertreter']:
        logging.error('Kunde %s (%s) hat mit "%s" keinen gueltigen Vertreter', ret['name1'],
                                                                               ret['kundennr'],
                                                                               ret['vertreter_handle'])
    if 'verbandsnr' in row and row['verbandsnr']:
        ret['verbandsnr'] = 'SC%s' % row['verbandsnr']
        ret['mitgliedsnr'] = row.get('mitgliedsnr', '')
    if 'iln' in row and row['iln']:
        ret['iln'] = unicode(int(row['iln'])).strip()
    if row.get('XKD_erfassung_date'):
        ret['erfassung'] = row['XKD_erfassung_date']
        ret['aenderung'] = ret['erfassung']
    if row.get('XKD_aenderung_date'):
        ret['aenderung'] = row['XKD_aenderung_date']
    return ret


def get_konditionen(kundennr):
    """Liefere die Zahlungsbedingungen für einen Kunden

    Der Rückgabewert ist ein Drei-Tupel bestehend aus Skonto-Tage, Skonto und Netto-Tage.
    >>> get_konditionen2('SC66669')
    (8, Decimal('2'), 30)
    """

    # Die Kundenkonditionen sind im Format der Datei XPX00E03 gespeichert,
    # diese stehen kodiert als Base16 in der Tabelle XPX00.
    # Die Anwendung ist 'X' ("sonstige"), Dateiart 'S' (Skonto)
    # und der Schlüssel ist der Skontoschlüssel aus der Tabelle XKS00.
    # Die Datentypen der Schlüssel unterscheiden sich allerdings:
    # PXPKEY ist CHAR(3), PXPKEY ist NUMERIC(3), mit DIGITS() wird die benötigte Konvertierung
    # durchgeführt.
    #
    # Die Zahlen in PXPARM sind in einem obskuren Format gespeichert,
    # das 0xf an die Zahlen hängt. Der Einfachheit halber wird die letzte Stelle einfach abgeschnitten.
    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    conditions = ["PXANW = 'X'",
                  "PXSART = 'S'",
                  "KSKDNR = %s" % husoftm2.tools.pad('KSKDNR', kundennr),
                  'PXPKEY = DIGITS(KSSKSL)']
    rows = query(tables=['XKS00', 'XPX00'], fields=['HEX(PXPARM)'], condition=' AND '.join(conditions),
                 ua='husoftm2.kunden.get_konditionen')
    if rows:
        data = rows[0][0]
        skontotage, faelligkeit, skonto = [int(part) for part in (data[0:3], data[12:15], data[16:19])]
        return skontotage, Decimal(skonto) / 100, faelligkeit


def get_verband(kundennr):
    """Suche Verbandsinformationen zu einem Kunden zusammen"""

    kunde = get_kunde(kundennr)
    if kunde.get('verbandsnr'):
        verband = get_kunde(kunde['verbandsnr'])
        return {'verbandsnr': kunde['verbandsnr'],
                'name1': verband['name1'],
                'name2': verband.get('name2', ''),
                'name3': verband.get('name3', ''),
                'name4': verband.get('name4', ''),
                'mitgliedsnr': kunde.get('mitgliedsnr', '')}


def get_betreuer(kundennr):
    """Liefert einen String, der den Betreuer im Hause für einen bestimmten Kunden identifizert oder ''.

    >>> get_betreuer('SC66669')
    "Nadine Gerloff"
    """
    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    rows = query(['AKZ00'], fields=['KZINFO'],
                 condition="KZKDNR = %s" % husoftm2.tools.pad('KZKDNR', kundennr),
                 limit=1, ua='husoftm2.kunden.get_konditionen')
    if rows:
        return betreuerdict.get(rows[0][0], '')
    return ''


def set_betreuer(kundennr, betreuer):
    """Updatet den Betreuer in SoftM wenn es für den Kunden schon einen Satz in der AKZ00 gibt..

    >>> set_betreuer('SC10001', 'Birgith Bonrath')
    """

    # Eine der wenigen Funktionen, die in SoftM schreiben

    # Das Betreuerkürzel finden. Wurde der volle Betreuername übergeben?
    for key, value in betreuerdict.items():
        if value == betreuer:
            # Betreuername durch der Betreuerkürzel ersetzen
            betreuer = key
            break
    if betreuer not in betreuerdict:
        raise ValueError("Unbekannter Betreuer %s" % betreuer)
    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    sql = "UPDATE AKZ00 SET KZINFO=%s WHERE KZKDNR=%s" % (husoftm2.tools.sql_quote(betreuer),
                                                          husoftm2.tools.pad('KZKDNR', kundennr))
    raw_SQL(sql, ua='husoftm.kunden')


def kredit_limit(kundennr):
    """Gib Kreditlimit für einen Kunden zurück.

    >>> kredit_limit('SC66660')
    Decimal('60000.0')
    """
    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    rows = query('XKS00', fields=['KSLIMI'], condition="KSKDNR=%s" % husoftm2.tools.pad('KSKDNR', kundennr),
                 limit=1, ua='husoftm2.kunden.kredit_limit')
    if rows:
        return rows[0][0]


def offene_posten(kundennr):
    """Ermittle die Summe er offenen Posten für einen Kunden
    >>> offene_posten('SC66660')
    Decimal('1234.56')
    """

    kundennr = husoftm2.tools.remove_prefix(kundennr, 'SC')
    rows = query('BOP00', fields=['OPRGSH', 'SUM(OPOPBT)'],
                 condition='OPPKTO=%s' % husoftm2.tools.pad('OPPKTO', kundennr),
                 grouping='OPRGSH',  # Gruppiert nach Typ: 'S' (Soll) und 'H' (Haben)
                 querymappings={},  # Verhindere ein Mapping, so dass der Rückgabewert ein Tupel ist
                 ua='husoftm2.kunden.offene_posten')
    if rows:
        offene_posten = dict(rows)
        return Decimal(offene_posten.get('S', 0)) - Decimal(offene_posten.get('H', 0))


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
    print get_verband('SC10123')
    print get_konditionen('SC66669')
    print get_betreuer('SC10001')
    print get_betreuer('SC66669')
    set_betreuer('SC10001', 'Nadine Gerloff')

if __name__ == '__main__':
    _selftest()
