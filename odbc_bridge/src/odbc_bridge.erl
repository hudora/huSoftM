%% @author Maximillian Dornseif <md@hudora.de>
%% @copyright 2008-12-10 Maximillian Dornseif.

%% @doc TEMPLATE.

-module(odbc_bridge).
-author('Maximillian Dornseif <md@hudora.de>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the odbc_bridge server.
start() ->
    odbc_bridge_deps:ensure(),
    ensure_started(odbc),
    ensure_started(crypto),
    application:start(odbc_bridge).

%% @spec stop() -> ok
%% @doc Stop the odbc_bridge server.
stop() ->
    Res = application:stop(odbc_bridge),
    application:stop(odbc),
    application:stop(crypto),
    Res.
