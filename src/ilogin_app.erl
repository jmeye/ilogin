%% @author Mochi Media <dev@mochimedia.com>
%% @copyright ilogin Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the ilogin application.

-module(ilogin_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ilogin.
start(_Type, _StartArgs) ->
    ilogin_deps:ensure(),
    ilogin_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ilogin.
stop(_State) ->
    ok.
