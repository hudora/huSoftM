%% @author Maximillian Dornseif <md@hudora.de>
%% @copyright 2008-12-10 Maximillian Dornseif.

%% @doc Supervisor for the odbc_bridge application.

-module(odbc_bridge_sup).
-author('Maximillian Dornseif <md@hudora.de>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    Ip = case os:getenv("MOCHIWEB_IP") of false -> "0.0.0.0"; Any1 -> Any1 end,   
    Dsn = case os:getenv("ODBC_DSN") of false -> "HD400utf8"; Any2 -> Any2 end,   
    WebConfig = [
         {ip, Ip},
                 {port, 8000},
                 {docroot, odbc_bridge_deps:local_path(["priv", "www"])}],
    Web = {odbc_bridge_web,
           {odbc_bridge_web, start, [WebConfig]},
           permanent, 31000, worker, dynamic},
    OdbcConfig = [{dsn, Dsn}],
    OdbcWrite = {odbc_bridge_write,
                 {odbc_bridge_write, start_link, [OdbcConfig]},
                 permanent, 30000, worker, dynamic},
    OdbcRead = {odbc_bridge_read,
                {odbc_bridge_read, start_link, [OdbcConfig]},
                permanent, 30000, worker, dynamic},
    Log = {odbc_bridge_log,
           {odbc_bridge_log, start_link, []},
           permanent, 5000, worker, dynamic},
                  
    Processes = [OdbcWrite, OdbcRead, Web, Log],
    {ok, {{one_for_one, 10, 10}, Processes}}.
