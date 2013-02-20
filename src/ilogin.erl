%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc ilogin.

-module(ilogin).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the ilogin server.
start() ->
    ilogin_deps:ensure(),
    ensure_started(crypto),
    %ensure_started(oakem),%连接SQLSERVER数据库
    application:start(ilogin).


%% @spec stop() -> ok
%% @doc Stop the ilogin server.
stop() ->
    application:stop(ilogin).
