# huSoftM

huSoftM is a toolkit for acessing the "SoftM Suite" ERP system in Python. See the module documentation for a general introduction into the workings of huSoftM. Mayor subsystems:

* odbc_bridge Erlang Language http-to-odbc converter beeing the basis for all SoftM access in this library.
* softmtables - read write access to single table rows including locking.
* [kunden][1] - customer information following the [AddressProtocol][2].
* [tools][3] - nice helper functions
* [connection][4] - SQL read acess to tables including automatic mapping and conversion.
* [bestaende][5] - Zugriff auf (Lager-) Bestandsdaten.

See [SoftMtabellen][6] for a representation of our knowlege concerning the Tables in SoftM.

[1]: http://github.com/hudora/huSoftM/blob/master/html/kunden.html
[2]: http://github.com/hudora/huTools/blob/master/doc/standards/address_protocol.markdown
[3]: http://github.com/hudora/huSoftM/blob/master/html/tools.html
[4]: http://github.com/hudora/huSoftM/blob/master/html/connection.html
[5]: http://github.com/hudora/huSoftM/blob/master/html/bestaende.html
[6]: http://cybernetics.hudora.biz/projects/wiki/SoftMtabellen

# Downloads

Get it at [https://github.com/hudora/huSoftM][7]

[7]: https://github.com/hudora/huSoftM

# Changes

* 0.2 - added husoftm.bestaende 
* 0.01 - first public release based on internal packages "pySoftM" and mofts.client.as400 .
