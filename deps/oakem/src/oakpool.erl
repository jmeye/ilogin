%% @author Churikov Daniil <ddosia@gmail.com>
%% @doc Organizes connection pool to DB and provides functions to work with it.
-module(oakpool).

-behaviour(gen_server).

-include("oakem.hrl").

%% API
-export([
    start_link/0,
    begin_transaction/0,
    commit/1, rollback/1,
    a_atomic_query/2, a_atomic_query/3,
    atomic_query/1, atomic_query/2,
    a_transact_query/3, a_transact_query/4,
    transact_query/2, transact_query/3
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-record(context, {wait_queue = queue:new(), gc_timer}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
%% @doc Starts gen_server and links caller to this process.
start_link() ->
    gen_server:start_link({local, oakmanager}, ?MODULE, [], []).


-spec begin_transaction() -> pid() | {error, timeout}.
%% @doc Asks `oakmanager' process for new transaction.
%%      When no transaction connections are available at call moment it will wait while:
%%      `oakmanager' sends transaction pid back or `gen_server' timeout occured(fails with `{error, timeout}').
begin_transaction() ->
    et:trace_me(50, client, oakmanager, begin_transaction, [{client, self()}]),
    gen_server:call(oakmanager, begin_transaction, ?DB_TIMEOUT * 2).


-spec commit(Connection :: pid()) -> ok | {error, _}.
%% @doc Commits transaction.
commit(Conn)->
    end_transaction(Conn, commit).


-spec rollback(Connection :: pid()) -> ok | {error, _}.
%% @doc Rollbacks transaction.
rollback(Conn)->
    end_transaction(Conn, rollback).


-spec end_transaction(Connection :: pid(), Mode :: commit | rollback) -> ok | {error, _}.
%% @doc Ends transaction with `Mode' result.
end_transaction(Conn, CommitMode) when is_pid(Conn) ->
    et:trace_me(50, client, oakconn, end_transaction, [{client, self()}, {commit_mode, CommitMode}, {oakconn, Conn}]),
    gen_server:call(Conn, {end_transaction, CommitMode}, ?DB_TIMEOUT).


-spec a_atomic_query(
    SQLQuery :: string(),
    Callback :: fun()
) -> odbc:result_tuple().
%% @doc Async atomic_query/1.
a_atomic_query(SQLQuery, Callback) ->
    a_atomic_query(SQLQuery, [], Callback).

-spec a_atomic_query(
    SQLQuery :: string(),
    Params :: [{odbc:odbc_data_type(), [odbc:value()]}],
    Callback :: fun()
) -> odbc:result_tuple().
%% @doc Async atomic_query/2.
a_atomic_query(SQLQuery, Params, Callback) ->
    Conn = get_free_conn(oakpool_apg),
    a_transact_query(Conn, SQLQuery, Params, Callback).

-spec atomic_query(SQLQuery :: string()) -> odbc:result_tuple().
%% @doc As atomic_query/2 but `Params' is ommited.
atomic_query(SQLQuery) ->
    atomic_query(SQLQuery, []).

-spec atomic_query(
    SQLQuery :: string(),
    Params :: [{odbc:odbc_data_type(), [odbc:value()]}]
) -> odbc:result_tuple().
%% @doc Does atomic query to DB without transaction.
atomic_query(SQLQuery, Params) ->
    Conn = get_free_conn(oakpool_apg),
    transact_query(Conn, SQLQuery, Params).


-spec a_transact_query(Connection :: pid(), SQLQuery :: string(), Callback :: fun()) -> odbc:result_tuple().
%% @doc Async transact_query/2.
a_transact_query(Conn, SQLQuery, Callback) ->
    a_transact_query(Conn, SQLQuery, [], Callback).

-spec a_transact_query(
    Connection :: pid(),
    SQLQuery :: string(),
    Params :: [{odbc:odbc_data_type(), [odbc:value()]}],
    Callback :: fun()
) -> odbc:result_tuple().
%% @doc Async transact_query/3.
a_transact_query(Conn, SQLQuery, Params, Callback) ->
    spawn(
        fun () ->
            Res = transact_query(Conn, SQLQuery, Params),
            Callback(Res)
        end
    ), ok.

-spec transact_query(Connection :: pid(), SQLQuery :: string()) -> odbc:result_tuple().
%% @doc As transact_query/3 but `Params' is ommited.
transact_query(Conn, SQLQuery) ->
    transact_query(Conn, SQLQuery, []).

-spec transact_query(
    Connection :: pid(),
    SQLQuery :: string(),
    Params :: [{odbc:odbc_data_type(), [odbc:value()]}]
) -> odbc:result_tuple().
%% @doc Does query to DB via transact pool.
transact_query(Conn, SQLQuery, []) ->
    % something wrong with odbc:param_query Params when it is empty `[]'
    transact_query(Conn, SQLQuery, [{sql_integer, [-1]}]);
transact_query(Conn, SQLQuery, Params) ->
    GuardedParams = [guard_param(P) || P <- Params],
    et:trace_me(50, client, oakconn, 'query', [{client, self()}, {'query', {'query', SQLQuery, Params}}, {oakconn, Conn}]),
    gen_server:call(Conn, {'query', SQLQuery, GuardedParams}, ?DB_TIMEOUT).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    process_flag(trap_exit, true),
    TRef = case ?DB_TIMEOUT of
        infinity ->
            undefined;
        _ ->
            {ok, Timer} = timer:send_interval(round(?DB_TIMEOUT / 4), clean_timeouted),
            Timer
    end,
    {ok, #context{gc_timer = TRef}}.

%% @private
handle_call(begin_transaction, From, CT = #context{wait_queue = WQ}) ->
    NewWQ = queue:in({From, now_secs()}, WQ),
    NewCT = serve_transaction(CT#context{wait_queue = NewWQ}),
    {noreply, NewCT}.


%% @private
handle_cast(transaction_ended, CT) ->
    NewCT = serve_transaction(CT),
    {noreply, NewCT};

handle_cast(Request, CT) ->
    et:trace_me(99, oakmanager, oakmanager, unknown_cast, [{info, Request}]),
    {noreply, CT}.


%% @private
handle_info(clean_timeouted, CT = #context{wait_queue = WQ}) ->
    et:trace_me(75, oakmanager, clean_timeout, [{oakconn, self()}]),
    NewWQ = queue:filter(
        fun({_, QTime}) ->
            (now_secs() - QTime) < (?DB_TIMEOUT / 1000) 
        end,
        WQ
    ),
    {noreply, CT#context{wait_queue = NewWQ}};

handle_info(stop, CT) ->
    {stop, shutdown, CT};
handle_info({'EXIT', _, _}, CT) ->
    {stop, shutdown, CT};
handle_info(Info, CT) ->
    et:trace_me(99, oakmanager, oakmanager, unknown_msg, [{info, Info}]),
    {noreply, CT}.


%% @private
code_change(_OldVsn, CT, _Extra) ->
      {ok, CT}.


%% @private
terminate(_Reason, #context{gc_timer = TRef}) ->
    case TRef of
        undefined ->
            ok;
        _ ->
            timer:cancel(TRef),
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_free_conn(PGName) ->
    pg2:get_closest_pid(PGName).


now_secs() ->
    {MS, S, _MiS} = erlang:now(),
    MS * 1000000 + S.

serve_transaction(CT = #context{wait_queue = WQ}) ->
    case get_free_conn(oakpool_tpg) of
        Pid when is_pid(Pid) ->
            case queue:out(WQ) of
                {{value, {Client = {CPid, _Ref}, _Time}}, NewWQ} ->
                    et:trace_me(50, oakmanager, oakconn, begin_transaction, [{client, CPid}, {oakconn, Pid}]),
                    gen_server:call(Pid, {begin_transaction, CPid}),
                    et:trace_me(50, oakmanager, client, transaction_accepted, [{client, CPid}, {oakconn, Pid}]),
                    gen_server:reply(Client, Pid),
                    CT#context{wait_queue = NewWQ};
                {empty, WQ} ->
                    CT
            end;
        {error, {no_process, _}} ->
            CT
    end.

%% @doc Check type of input Value and fails if it is not propper value for such type.
guard_param(P = {_, [null]}) ->
    P;
guard_param(P = {sql_bit, [Value]}) when is_boolean(Value) ->
    P;
guard_param(P = {sql_integer, [Value]}) when is_integer(Value) ->
    P;
guard_param(P = {{sql_varchar, _}, [Value]}) when is_binary(Value) ->
    P;
guard_param({T = {sql_varchar, _}, [Value]}) when is_list(Value) ->
    {T, [list_to_binary(Value)]};
guard_param({T = {sql_varchar, _}, [Value]}) when is_integer(Value); is_float(Value) ->
    guard_param({T, [io_lib:write(Value)]});
guard_param(_) ->
    throw (bad_sql_datatype).
