huSoftM
=======

huSoftM is a toolkit for acessing the "SoftM Suite" ERP system in Python. See the module documentation for a general introduction into the workings of huSoftM. Mayor subsystems:

* odbc_bridge Erlang Language http-to-odbc converter beeing the basis for all SoftM access in this library.
* softmtables - read write access to single table rows including locking.
* kunden - customer information following the AddressProtocol_.
* tools - nice helper functions
* connection - SQL read acess to tables including automatic mapping and conversion.
* bestaende - Zugriff auf (Lager-) Bestandsdaten.

See SoftMtabellen_ for a representation of our knowlege concerning the Tables in SoftM.


.. _AddressProtocol: http://cybernetics.hudora.biz/projects/wiki/AddressProtocol
.. _SoftMtabellen: http://cybernetics.hudora.biz/projects/wiki/SoftMtabellen


Downloads
---------

Get it at https://github.com/hudora/huSoftM


Changes
-------

* 0.2 - added husoftm.bestaende
* 0.01 - first public release based on internal packages "pySoftM" and mofts.client.as400 .
