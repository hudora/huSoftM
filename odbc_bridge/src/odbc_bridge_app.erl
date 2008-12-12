%% @author Maximillian Dornseif <md@hudora.de>
%% @copyright 2008-12-10 Maximillian Dornseif.

%% @doc Callbacks for the odbc_bridge application.

-module(odbc_bridge_app).
-author('Maximillian Dornseif <md@hudora.de>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for odbc_bridge.
start(_Type, _StartArgs) ->
    odbc_bridge_deps:ensure(),
    odbc_bridge_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for odbc_bridge.
stop(_State) ->
    ok.
