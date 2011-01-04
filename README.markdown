# huSoftM

huSoftM is a toolkit for acessing the "SoftM Suite" ERP system in Python. See the module documentation for a
general introduction into the workings of huSoftM. Major subsystems:

* odbc_bridge Erlang Language http-to-odbc converter beeing the basis for all SoftM access in this library.
* softmexpress Javascript/node.js derver to act as a frontend to odbc_bridge.
* softmtables - read write access to single table rows including locking.
* [kunden](http://github.com/hudora/huSoftM/blob/master/html/kunden.html) - customer information following
  the [AddressProtocol](http://github.com/hudora/huTools/blob/master/doc/standards/address_protocol.markdown).
* [tools](http://github.com/hudora/huSoftM/blob/master/html/tools.html) - nice helper functions
* [connection](http://github.com/hudora/huSoftM/blob/master/html/connection.html) - SQL read acess to tables
  including automatic mapping and conversion.
* [bestaende](http://github.com/hudora/huSoftM/blob/master/html/bestaende.html) - Zugriff auf (Lager-)
  Bestandsdaten.

See [SoftMtabellen](http://cybernetics.hudora.biz/projects/wiki/SoftMtabellen) for a representation of our
knowlege concerning the Tables in SoftM.


# Architecture

Die Architektur ist recht vielschichtig. Im code sind auch immer noch legacy-Versatzstücke f¨ru andere
Kommunikationswege, zB über PyRO-RPC vorhanden. Die Architektur soll es ermöglichen, das man die Abfragen von
connection3.py and softmexpress.js mit einigermassem ruhigen Gewissens über dsa Internet schicken kann, ohne
dsa zur Manipulation der AS400 per SQL nun Tür und Tor geöffnet sind. Allerdings ist keine
Sicherheitsmassnahme unfehlbar. Vor zufälligen Fehlern der Frontend-Entwickler oder drive-by-hacking sollten
die Massnahmen jedoch schützen.

Der Client nutzt die verschiedenen module aus softm, die (im Idealfall zumindest) alle connection3.py zur
Kommunikation einsetzten. connection3.py braucht nur eine HTTP-Verbindung zum softmexpress Server. Der
gesammte Client braucht nur "pure Python" module und ist damit beispielsweise auf der Google Appengine
einsetzbar. Alle Anfragen an softmexpress.js  werden durch eine HMAC-Signatur vor Verfälschungen geschützt.

softmexpress.js ist ein Proxy Server, der für node.js entwickelt wurde. Er nimmt die Anfragen von
connection3.py entgegen, prüft die Signatur und formale PArameter, erzeugt dann aus den übermittelten Daten
eine SQL anfrage. Diese wird per HTTP an die odbc_bridge weitergeletet.

Die odbc_bridge muss auf einem Server laufen, auf dem der gesammte iSeries Access Stack bzw. zumindest der
ODBC Treiber von IBM läuft. Bei uns bereitet das erhebliche Probleme und daher haben wir eine dedizierte
Maschine dazu. odbc_bridge ist in Erlang geschrieben, hat aber trotz der viel gerühmten Robustheit von Erlang
Stabilitätsprobleme mit nicht wohlgeformten SQL-Anfragen. Der ODBC-Treiber scheint sich aufzuhängen und kann
mur durch einen Neustart der Erlang-Runtime wiederbelebt werden.


    (kunden | bestaende | auftraege )      -+
                  |                         | On your Webhost or in the Cloud
                  V                         |
            connection3.py                 -+
                  |
                  V                        -+
           softmexpress.js                  | in you DMZ
                  |                        -+
                  V
            odbc_bridge.erl                -+
           IBM ODBC drivers                 | on a host near the AS/400 or together with softmexpress.js 
                  |                        -+
                  V
                AS/400

# Deployment

Um SoftM Express laufen zu lassen, wird node.js > 0.3 benötigt. 

    node softmexpress.js s3kri1t odbc_bridge.local.example.com 8000 8082

Der Software muss ein Passwort übergeben werden, Hostname und Port, auf dem die ODBC-Bridge läuft und port auf dem softmexpress die anfragen beantworten soll. softmexpress geht nicht in den Hintergrund und sollte deswegen unter einem Tool, wie supervise/svscan gestartet werden.




# Downloads

Get it at [https://github.com/hudora/huSoftM][7]

[7]: https://github.com/hudora/huSoftM

# Changes

See http://github.com/hudora/huSoftM/blob/master/CHANGES

# Copyright

BSD Licensed
Contains node-http-proxy Copyright (c) 2010 Charlie Robbins, Mikeal Rogers, Fedor Indutny, & Marak Squires.
Contains colors.js Copyright (c) 2010 Alexis Sellier (cloudhead) , Marak Squires.
Conatins pool.js Copyright (c) 2010 Mikeal Rogers.