%% @author Churikov Daniil <ddosia@gmail.com>
%% @private
-module(oakconn).

-behaviour(gen_server).

-include("oakem.hrl").
%% API
-export([start_link/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(context, {conn, pg_name, auto_commit, owner, gc_timer, query_max_time = ?DB_TIMEOUT * 20}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(InitialOpts) ->
    gen_server:start_link(?MODULE, InitialOpts, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    %% To know when the client shutdowns
    %% and do correct termination
    process_flag(trap_exit, true),

    {ok, Conn} = connect(Opts),

    CT = mark_as_new(Opts),

    MT = proplists:get_value(query_max_time, Opts),
    {ok, CT#context{conn = Conn, query_max_time = MT}}.



handle_call({begin_transaction, Client}, {From, _Ref}, CT = #context{auto_commit = off, query_max_time = MT, owner = undefined}) -> 
    case whereis(oakmanager) of 
         From ->
            ok = pg2:leave(oakpool_tpg, self()),
            {ok, TRef} = timer:send_after(MT, close_expired),
            MRef = monitor(process, Client),
            et:trace_me(50, oakconn, oakmanager, transaction_accepted, [{client, Client}, {oakconn, self()}]),
            {reply, ok, CT#context{owner = {Client, MRef}, gc_timer = TRef}};
        _ ->
            et:trace_me(50, oakconn, oakmanager, transaction_declined, [{client, Client}, {oakconn, self()}]),
            {reply, {error, prohibited}, CT}
    end;


handle_call({end_transaction, CommitMode}, {Client, _Ref}, CT = #context{owner = {Client, _MRef}, conn = Conn}) -> 
    Res = odbc:commit(Conn, CommitMode),
    NewCT = mark_as_new(CT),
    et:trace_me(50, oakconn, client, transaction_ended, [{client, Client}, {oakconn, self()}, {commit_mode, CommitMode}]),
    {reply, Res, NewCT};


%% transact query
handle_call({'query', SQLQuery, Params}, {Client, _Ref}, CT = #context{owner = {Client, _MRef}, conn = Conn}) ->
    Res = odbc:param_query(Conn, SQLQuery, Params),
    et:trace_me(50, oakconn, client, query_answer, [{client, Client}, {oakconn, self()}, {res, Res}]),
    {reply, Res, CT};

%% atomic query
handle_call({'query', SQLQuery, Params}, _From, CT = #context{auto_commit = on, conn = Conn}) ->
    Res = odbc:param_query(Conn, SQLQuery, Params),
    et:trace_me(50, oakconn, client, query_answer, [{client, _From}, {oakconn, self()}, {res, Res}]),
    {reply, Res, CT};


handle_call(Request, From, CT) ->
    et:trace_me(99, oakconn, oakconn, unknown_call, [{info, Request}, {from, From}]),
    {reply, {error, unexpected_request}, CT}.



handle_cast(_Request, CT) ->
    {noreply, CT}.



handle_info(close_expired, CT) ->
    et:trace_me(50, oakconn, transaction_expired, [{oakconn, self()}]),
    NewCT = mark_as_new(CT),
    {noreply, NewCT};


handle_info({'DOWN', MRef, process, Client, _Reason}, CT = #context{owner = {Client, MRef}}) ->
    NewCT = mark_as_new(CT),
    {noreply, NewCT};


handle_info(stop, CT) ->
    {stop, shutdown, CT};
handle_info({'EXIT', _, _}, CT) ->
    {stop, shutdown, CT};


handle_info(Info, CT) ->
    et:trace_me(99, oakconn, oakconn, unknown_msg, [{info, Info}]),
    {noreply, CT}.


code_change(_OldVsn, CT, _Extra) ->
      {ok, CT}.


terminate(_Reason, #context{conn = Conn}) ->
    disconnect(Conn).

%%%===================================================================
%%% Internal functions
%%%===================================================================

connect(Opts) ->
    CS = proplists:get_value(connection_string, Opts),
    AutoCommit = proplists:get_value(auto_commit, Opts),

    odbc:connect(CS, [
        {auto_commit, AutoCommit},
        {timeout, ?DB_TIMEOUT},
        {scrollable_cursor, off},
        {binary_strings, on}
    ]).


disconnect(Conn) ->
    odbc:disconnect(Conn).



mark_as_new(Opts) when is_list(Opts) ->
    AutoCommit = proplists:get_value(auto_commit, Opts),
    mark_as_new(#context{auto_commit = AutoCommit});

mark_as_new(CT = #context{gc_timer = TRef, owner = Client, auto_commit = AutoCommit}) ->
    timer:cancel(TRef),
    case Client of
        undefined ->
            true;
        {_Pid, MRef} ->
            demonitor(MRef)
    end,
    PGName = case AutoCommit of
        on ->
            oakpool_apg;
        off ->
            et:trace_me(50, oakconn, oakmanager, transaction_ended, [{oakconn, self()}]),
            gen_server:cast(oakmanager, transaction_ended),
            oakpool_tpg
    end,
    ok = pg2:join(PGName, self()),
    CT#context{pg_name = PGName, owner = undefined, gc_timer = undefined}.
