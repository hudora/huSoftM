#!/usr/bin/env python
# encoding: utf-8
"""
huSoftM ist eine Sammlung von Funktionen um Zugriff auf Daten der "SoftM Suite" zu geben. 
"SoftM Suite" ist ein Produkt der SoftM AG. SoftM Suite läuft nativ auf einer 
AS/400|iSeries|i5 allerdings verkauft SoftM auch eine "Windows Version", die auf einem
AS/400 Emulator namens AxWare unter NTff läuft. AxWare stammt von PKS Software GmbH und
ist eine "Legacy Migration" Produkt. Wir selbst haben von AX/ware zurück zur "echten"
AS/400 migriert. pySoftM ist nur mit einer echten AS/400 Umgebung getestet. Als Client
nutzen wir FreeBSD 5.x, 6.x, 7.x mit Python 2.4, 2.5 und 2.6.

Um den Kontakt mit der AS/400 herzustellen nutzen wir einen Stub, der unter Unix und 
unter Windows laufen kann und das IBM "iSeries Access" Toolkit nutzt. Dieses bietet
eine ODBC Schnittstelle zur iSeries an. Die entsprechende Komponente heisst
SoftM_ODBCadapter und findet sich im Verzeichnis 'server'. Die Kommunikation
zwischen der Python Applikation und dem SoftM_ODBCadapter läuft mittels
PyRO - Python Remote Objects ab. Der ODBC Zugriff auf der iSeries Access Toolkit
erfolgt mittels mxODBC. 

http://www-03.ibm.com/servers/eserver/iseries/access
http://pyro.sourceforge.net/

Das ganze ist im Ergebnis mit der Easysoft ODBC-ODBC Bridge vergleichbar.

http://www.easysoft.com/products/data_access/odbc_odbc_bridge/index.html

Auf jeden Fall muss der ODBC-Zugriff auf der Maschine, auf der der PySoftM Adapter läuft,
konfiguriert sein.

Der Zugriff auf der Clientseite wird über das Modul "connection" abgewickelt.
"""

__revision__ = "$Revision$"
