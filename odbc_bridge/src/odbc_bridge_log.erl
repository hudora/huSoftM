%%% this is based on yaws_log.erl
%%% Created : 26 Jan 2002 by Claes Wikstrom <klacke@hyber.org>

-module(odbc_bridge_log).
-author('klacke@hyber.org').
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         handle_event/2]).

-export([log/1]).

%% 1 meg log we wrap
-define(WRAP_LOG_SIZE, 1000000).

-record(state, {
          fd,
          filename,
          dir,
          linecounter = 0,
          log_wrap_size = ?WRAP_LOG_SIZE}).


%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log(Message) ->
    gen_server:cast(?MODULE, {log, calendar:local_time(), Message}).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    FileName = filename:join(["./log", "access.log"]),
    case file:open(FileName, [write, raw, append]) of
        {ok, Fd} ->
            write_message(Fd, calendar:local_time(), "logging started ===================="),
            {ok, #state{fd=Fd, filename=FileName, dir="./log"}};
        _Err ->
            error_logger:format("Cannot open ~p",[FileName]),
            {stop, "Cannot open logfile"}
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(_, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast({log, Time, Message}, State) ->
    write_message(State#state.fd, Time, Message),
    {noreply, rotate(State)}.

handle_event(_Event, State) ->
    {ok, State}.

%% Handling all non call/cast messages
%% None implemented so far.
handle_info(_Info, State) ->
    {noreply, State}.


%% spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% This is for hot code updates
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%% internal functions    

write_message(Fd, Time, Message) ->
    {{Year, Month, Date}, {Hour, Min, Sec}} = Time,
    file:write(Fd, io_lib:format("~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0w.0 ~s~n",
                                 [Year, Month, Date, Hour, Min, Sec, Message])).

% after writing 1000 lies check if we have to rotate the logfile
rotate(State) when State#state.linecounter > 1000 ->
    case file:read_file_info(State#state.filename) of
        {ok, FileInfo} when FileInfo#file_info.size > State#state.log_wrap_size ->
             write_message(State#state.fd, calendar:local_time(), "logfile rotated ===================="),
             file:close(State#state.fd),
             Old = [State#state.filename, ".old"],
             file:delete(Old),
             file:rename(State#state.filename, Old),
             {ok, Fd2} = file:open(State#state.filename, [write, raw]),
             error_logger:info_msg("Rotated log ~p",[State#state.filename]),
             write_message(State#state.fd, calendar:local_time(), "logfile rotated ===================="),
             State#state{fd = Fd2, linecounter=0};
        {error, enoent} ->
             {ok, Fd2} = file:open(State#state.filename, [write, raw, append]),
             error_logger:info_msg("Reopened log ~p",[State#state.filename]),
             write_message(State#state.fd, calendar:local_time(), "logfile reopened ==================="),
             State#state{fd = Fd2, linecounter=0};
        _ ->
            State#state{linecounter=State#state.linecounter + 1}
    end;
rotate(State) -> 
    State#state{linecounter=State#state.linecounter + 1}.
