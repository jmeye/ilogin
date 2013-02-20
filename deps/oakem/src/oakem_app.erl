%% @author Churikov Daniil <ddosia@gmail.com>
%% @doc Main app module. Contains all start/stop related functions and main supervisor.

-module(oakem_app).
-behaviour(supervisor).
-behaviour(application).

-include("oakem.hrl").

%% Application API
-export([start/0, stop/0, start/2, stop/1]).

%% Supervisor API
-export([start_link/0, init/1]).

%% ===================================================================
%% API
%% ===================================================================
%% @doc Starts standalone application with all deps.
start() ->
    ensure_started(odbc),
    application:start(oakem).

%% @doc Stops application and all its deps. Ensures that all deps were stopped
%%      after main app stops.
stop() ->
    Res = application:stop(oakem),
    application:stop(odbc),
    Res.


%% @doc Starts application.
start(_Type, _StartArgs) ->
    ensure_started(odbc),
    ?MODULE:start_link().

%% @doc Stops application.
stop(_State) ->
    ok.

%% ===================================================================
%% internal
%% ===================================================================

%% @private
start_link() ->
    {ok, ESpecs} = application:get_env(oakem, entities),
    ok = oakentity:compile_entites(ESpecs),
    supervisor:start_link({local, oakpool_sup}, ?MODULE, []).


%% @private
init([]) ->
    pg2:start(),
    % atomic connections group
    ok = pg2:create(oakpool_apg),
    % transact connections group
    ok = pg2:create(oakpool_tpg),

    MSpec = {
        oakmanager,
        {oakpool, start_link, []},
        permanent, 5000, worker, [oakpool]
    },

    {ok, Pools} = application:get_env(oakem, pools),

    PSpecs = lists:map(
        fun({PoolName, PoolConfig}) ->
            Opts = [{pool_name, PoolName} | PoolConfig],
            {
                PoolName,
                {supervisor, start_link, [{local, PoolName}, ?MODULE, [{init_pool_sup, Opts}]]},
                permanent, infinity, supervisor, [?MODULE]
            }
        end, Pools
    ),
    {ok, { {one_for_one, 5, 10}, [MSpec | PSpecs]} };



%% @private
%% pool`s main supervisor callback
init([{init_pool_sup, Opts}]) ->
    PS = proplists:get_value(size, Opts),
    ConnSpecs = [
        {ID, {oakconn, start_link, [Opts]}, permanent, 5000, worker, [oakconn]} || ID <- lists:seq(1, PS)
    ],
    {ok, { {one_for_one, 5, 10}, ConnSpecs} }.



%% internal
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

