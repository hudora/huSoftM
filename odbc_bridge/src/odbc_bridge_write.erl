%%% @author    Maximillian Dornseif <> []
%%% @copyright 2008 Maximillian Dornseif
%%% @doc  This module implements ODBC write (INSERT, UPDATE) access.
%%% @end  
%%% @since 2008-12-11 by Maximillian Dornseif
-module(odbc_bridge_write).
-author('Maximillian Dornseif <md@hudora.de>').

-behaviour(gen_server).

%% API
-export([start_link/1, update/1, insert/1, info/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {dsn, successcount=0, errorcount=0}).

%% @doc Starts the server
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc SQL UPDATE command
-spec update(QueryStr::string()) -> {'ok', RowCount::integer()} | {'error', Reason::any()}.
update(QueryStr) when is_list(QueryStr) ->
     gen_server:call(?MODULE, {update, QueryStr}).

%% @doc SQL INSERT command
-spec insert(QueryStr::string()) -> {'ok', RowCount::integer()} | {'error', Reason::any()}.
insert(QueryStr) when is_list(QueryStr) ->
     gen_server:call(?MODULE, {insert, QueryStr}).

%% @doc Return Server Informtion
-spec info() -> {SuccessCount::integer(), ErrorCount::integer(), Reconnects::integer()}.
info() ->
     gen_server:call(?MODULE, {info}).


%% gen_server callbacks

%% @doc This is called when teh server starts up and has to initiate the server state.
init(Options) ->
    Dsn = proplists:get_value(dsn, Options),
    % set up ODBC connection
    {ok, #state{dsn=Dsn}}.

%% This handles the syncrounous requests to the server.
%% basically every function variant implements some of the API functions defined above
handle_call({update, QueryStr}, _From, State) ->
    % for write applications we open each time a fresh connection
    {ok, Ref} = odbc:connect("DSN=" ++ State#state.dsn, []),
    {Time, Result} = timer:tc(odbc, sql_query, [Ref, QueryStr]),
    odbc:disconnect(Ref),
    case Result of
        {updated, Rowcount} ->
            odbc_bridge_log:log(io_lib:format("~5..0w ~4..0w ~s", [Time div 1000, Rowcount, QueryStr])),
            {reply, {ok, Rowcount}, State#state{successcount=State#state.successcount+1}};
        {error, Info} ->
            {reply, {error, Info}, State#state{errorcount=State#state.errorcount+1}};
        Any ->
            erlang:display(Any),
            {reply, {error, Any}, State#state{errorcount=State#state.errorcount+1}}
    end;

handle_call({info}, _From, State) ->
    {reply, {State#state.successcount, State#state.errorcount, 0}, State}.

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
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% This is for hot code updates
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
