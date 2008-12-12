%% @author Maximillian Dornseif <md@hudora.de>
%% @copyright 2008-12-10 Maximillian Dornseif.

%% @doc Web server for odbc_bridge.

-module(odbc_bridge_web).
-author('Maximillian Dornseif <md@hudora.de>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

-spec clean_string(_) -> string()|binary().
clean_string(S) when is_list(S) ->
    list_to_binary(string:strip(S));
clean_string(S) ->
    S.

%% handle a select query by calling odbc_bridge_read:select() to do the actual query
%% and then reformat the results to JSON
-spec do_select(atom(),string()) -> any().
do_select(Req, QueryStr) ->
    case string:left(string:to_upper(QueryStr), 7) of
        "SELECT " ->
            case catch odbc_bridge_read:select(QueryStr) of
                {ok, Rows} ->
                    % convert tuples in response to lists
                    RowList = [[clean_string(C) || C <- tuple_to_list(R)] || R <- Rows],
                    Json = mochijson2:encode(RowList),
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}], [Json, "\n"]});
                {error, Message} ->
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                                 io_lib:format("Error~n~w~n~s~n", [Message, QueryStr])});
                Any ->
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                                io_lib:format("Internal Error: ~p~n", [Any])})
            end;
        _->
            Req:respond({500, [{"Content-Type", "text/plain"}], ["Query must start with 'SELECT'\n"]})
    end.

%% handele an update request by calling odbc_bridge_write:update
%% and return the row count
-spec do_update(atom(),string()) -> any().
do_update(Req, QueryStr) ->
    case string:left(string:to_upper(QueryStr), 7) of
        "UPDATE " ->
            case catch odbc_bridge_write:update(QueryStr) of
                {ok, RowCount} ->
                    Json = mochijson2:encode(RowCount),
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}], [Json, "\n"]});
                {error, Message} ->
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                                       io_lib:format("Internal Error: ~p~n", [Message])});
                Any ->
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                                io_lib:format("Internal Error: ~p~n", [Any])})
            end;
        _->
            Req:respond({500, [{"Content-Type", "text/plain"}], ["Query must start with 'UPDATE'\n"]})
    end.

%% handele an insert request by calling odbc_bridge_write:insert
%% and return the row count
-spec do_insert(atom(),string()) -> any().
do_insert(Req, QueryStr) ->
    case string:left(string:to_upper(QueryStr), 7) of
        "INSERT " ->
            case catch odbc_bridge_write:update(QueryStr) of
                {ok, RowCount} ->
                    Json = mochijson2:encode(RowCount),
                    Req:respond({200, [{"Content-Type", "application/json; charset=utf-8"}], [Json, "\n"]});
                {error, Message} ->
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                                       io_lib:format("Internal Error: ~p~n", [Message])});
                Any ->
                    Req:respond({500, [{"Content-Type", "text/plain"}],
                                io_lib:format("Internal Error: ~p~n", [Any])})
            end;
        _->
            Req:respond({500, [{"Content-Type", "text/plain"}], ["Query must start with 'INSERT'\n"]})
    end.


loop(Req, _DocRoot) ->
    "/" ++ Path = Req:get(path),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
                "info" ->
                    % Present Information on successfgull reads/writes
                    % and the time in microseconds it takes to collect that information
                    {Rtime, {Rsuccess, Rerror, Rconnect}} = timer:tc(odbc_bridge_read, info,  []),
                    {Wtime, {Wsuccess, Werror, Wconnect}} = timer:tc(odbc_bridge_write, info, []),
                    Req:respond({200, [{"Content-Type", "text/plain; charset=utf-8"}],
                                io_lib:format("read_latency: ~w~nread_success: ~w~nread_error: ~w~n"
                                              ++ "read_reconnects: ~w~nwrite_latency: ~w~n"
                                              ++"write_success: ~w~nwrite_error:~w~nwrite_reconnects: ~w~n",
                                              [Rtime, Rsuccess, Rerror, Rconnect,
                                               Wtime, Wsuccess, Werror, Wconnect])});                    
                "select" ->
                    case proplists:get_value("query", Req:parse_qs()) of
                        undefined ->
                            Req:respond({500, [{"Content-Type", " text/plain; charset=utf-8"}],
                                        "/select needs a 'query' parameter\n"});
                        QueryStr ->
                            do_select(Req, QueryStr)
                    end;
                "update" ->
                        Req:respond({405, [{"Content-Type", " text/plain; charset=utf-8"}],
                                    "/update requires POST method\n"});
                "insert" ->
                        Req:respond({405, [{"Content-Type", " text/plain; charset=utf-8"}],
                                    "/update requires POST method\n"});
                _ ->
                    Req:respond({404, 
                                 [{"Content-Type", " text/plain; charset=utf-8"}],
                                 "Try /info\n"})
            end;
        'POST' ->
            case Path of
                "update" ->
                    case proplists:get_value("query", Req:parse_post()) of
                        undefined ->
                            Req:respond({500, [{"Content-Type", " text/plain; charset=utf-8"}],
                                        "/update needs a 'query' parameter\n"});
                        QueryStr ->
                            do_update(Req, QueryStr)
                    end;
                "insert" ->
                    case proplists:get_value("query", Req:parse_post()) of
                        undefined ->
                            Req:respond({500, [{"Content-Type", " text/plain; charset=utf-8"}],
                                        "/insert needs a 'query' parameter\n"});
                        QueryStr ->
                            do_insert(Req, QueryStr)
                    end;
                _ ->
                    Req:not_found()
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
