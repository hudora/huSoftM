#!/usr/bin/env python
# encoding: utf-8
"""
huSoftM ist eine Sammlung von Funktionen um Zugriff auf Daten der "SoftM Suite" zu geben.
"SoftM Suite" ist ein Produkt der SoftM AG. SoftM Suite läuft nativ auf einer
AS/400|iSeries|i5 allerdings verkauft SoftM auch eine "Windows Version", die auf einem
AS/400 Emulator namens AxWare unter NTff läuft. AxWare stammt von PKS Software GmbH und
ist eine "Legacy Migration" Produkt. Wir selbst haben von AX/ware zurück zur "echten"
AS/400 migriert. huSoftM ist nur mit einer echten AS/400 Umgebung getestet. Als Client
nutzen wir FreeBSD 5.x, 6.x, 7.x mit Python 2.4, 2.5 und 2.6 sowie die Google AppEngine.

Um den Kontakt mit der AS/400 herzustellen nutzen wir einen Stub, der unter Unix und
unter Windows laufen kann und das IBM "iSeries Access" Toolkit nutzt. Dieses bietet
eine ODBC Schnittstelle zur iSeries an. Die entsprechende Komponente heisst
odbc_bridge und ist in Erlang geschrieben. Die Kommunikation zwischen der Python
Applikation und der odbc_bridge läuft mittels HTTP ab.

http://www-03.ibm.com/servers/eserver/iseries/access

Das ganze ist im Ergebnis mit der Easysoft ODBC-ODBC Bridge vergleichbar.

http://www.easysoft.com/products/data_access/odbc_odbc_bridge/index.html

Auf jeden Fall muss der ODBC-Zugriff auf der Maschine, auf der der huSoftM Adapter läuft,
konfiguriert sein.

Darauf auf setzt der SoftMexpress Server auf, der zwischen dem Internet und dem Zugang zur
odbc_brige mapped.

Der Zugriff auf der Clientseite wird über das Modul "backend" abgewickelt.

Mehr unter https://github.com/hudora/huSoftM
"""
