-module(ilogin_shortcuts).
%%-compile(export_all). debug
-export([render_ok/3, render_ok/4, get_cookie_value/3]).

render_ok(Req, TemplateModule, Params) ->
    render_ok(Req, [], TemplateModule, Params).

render_ok(Req, Headers, TemplateModule, Params) ->
    {ok, Output} = TemplateModule:render(Params),
    Req:ok({"text/html", Headers, Output}).

get_cookie_value(Req, Key, Default) ->
    case Req:get_cookie_value(Key) of
        undefined -> Default;
        Value -> Value
    end.