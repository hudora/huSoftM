# huSoftM

huSoftM is a toolkit for acessing the "SoftM Suite" ERP system in Python. See the module documentation for a
general introduction into the workings of huSoftM. Mayor subsystems:

* odbc_bridge Erlang Language http-to-odbc converter beeing the basis for all SoftM access in this library.
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


# Downloads

Get it at [https://github.com/hudora/huSoftM](https://github.com/hudora/huSoftM)


# Changes

* 0.2 - added husoftm.bestaende 
* 0.01 - first public release based on internal packages "pySoftM" and mofts.client.as400 .
