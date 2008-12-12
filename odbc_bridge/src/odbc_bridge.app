{application, odbc_bridge,
 [{description, "odbc_bridge"},
  {vsn, "0.01"},
  {modules, [
    odbc_bridge,
    odbc_bridge_app,
    odbc_bridge_sup,
    odbc_bridge_web,
    odbc_bridge_deps,
    odbc_bridge_read,
    odbc_bridge_write,
    odbc_bridge_log
  ]},
  {registered, []},
  {mod, {odbc_bridge_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, odbc]}]}.
