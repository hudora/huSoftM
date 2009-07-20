%%% @author    Maximillian Dornseif <> []
%%% @copyright 2008 Maximillian Dornseif
%%% @doc  This module implements ODBC read (SELECT) access.
%%% @end  
%%%
%%% @TODO: I have seen this process getting into an unresponsive state due to odbc errors. Perhaps we should crash on errors.
%%%
%%% @since 2008-12-10 by Maximillian Dornseif
-module(odbc_bridge_read).
-author('Maximillian Dornseif <md@hudora.de>').

-behaviour(gen_server).

%% API
-export([start_link/1, select/1, info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {odbcref, dsn, successcount=0, errorcount=0, reconnects=0}).

%% @doc Starts the server
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc SQL SELECT command
-spec select(QueryStr::string()) -> {'ok', Rows::integer()} | {'error', Reason::any()}.
select(QueryStr) when is_list(QueryStr) ->
     gen_server:call(?MODULE, {select, QueryStr, 2}, 30000).

%% @doc Return Server Informtion
-spec info() -> {SuccessCount::integer(), ErrorCount::integer(), Reconnects::integer()}.
info() ->
     gen_server:call(?MODULE, {info}).

%% gen_server callbacks

%% @doc This is called when teh server starts up and has to initiate the server state.
init(Options) ->
    Dsn = proplists:get_value(dsn, Options),
    % set up ODBC connection
    {ok, Ref} = odbc:connect("DSN=" ++ Dsn, []),
    {ok, #state{odbcref=Ref, dsn=Dsn}}.

%% This handles the syncrounous requests to the server.
%% basically every function variant implements some of the API functions defined above
handle_call({select, QueryStr, RecoursionCoutner}, From, State) when RecoursionCoutner > 0 ->
    % execute query
    {Time, Result} = timer:tc(odbc, sql_query, [State#state.odbcref, QueryStr]),
    case Result of
        {selected, _RowNames, Rows} ->
            odbc_bridge_log:log(io_lib:format("~5..0w ~4..0w ~s", [Time div 1000, length(Rows), QueryStr])),
            % we got a result, send the result and an updated server state back.
            {reply, {ok, Rows}, State#state{successcount=State#state.successcount+1}};
        {error, connection_closed} ->
            % the query failed because the connection has been closed:
            % reopen conection and retry by recursively call ourself
            % with updated state containing the new connection and updated counters
            % unforunaly 'connection_closed' will also returned with certain malformed queries
            % so we limit recursion
            odbc:disconnect(State#state.odbcref),
            {ok, Ref} = odbc:connect("DSN=" ++ State#state.dsn, []),
            handle_call({select, QueryStr, RecoursionCoutner-1}, From,
                         State#state{odbcref=Ref, reconnects=State#state.reconnects+1});
        {error, Info} ->
            % reopen conection to clean up for the next call
            odbc:disconnect(State#state.odbcref),
            {ok, Ref} = odbc:connect("DSN=" ++ State#state.dsn, []),
            % some other error: return the error message
            {reply, {error, Info}, State#state{odbcref=Ref, errorcount=State#state.errorcount+1}}
    end;
handle_call({select, _QueryStr, _RecoursionCoutner}, _From, State) ->
    % de have tried seceral times and the whole odbc stack seems to need a restart
    % odbc:disconnect(State#state.odbcref),
    % application:stop(odbc),
    {reply, {error, "Crash in ODBC driver"}, State#state{errorcount=State#state.errorcount+1}};


handle_call({info}, _From, State) ->
    {reply, {State#state.successcount, State#state.errorcount, State#state.reconnects}, State}.

%% Handling of async server calls
%% None implemented so far.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handling all non call/cast messages
%% None implemented so far.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    odbc:disconnect(State#state.odbcref),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% This is for hot code updates
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
