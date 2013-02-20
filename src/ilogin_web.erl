%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for ilogin.

-module(ilogin_web).
-author("Mochi Media <dev@mochimedia.com>").

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

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    try
        case dispatch(Req, ilogin_views:urls()) of
            none -> 
                % No request handler found
                case filelib:is_file(filename:join([DocRoot, Path])) of
                    true -> 
                        % If there's a static file, serve it
                        Req:serve_file(Path, DocRoot);
                    false ->
                        % Otherwise the page is not found
                        Req:not_found()
                end;
            Response -> 
                Response
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.
    
loop(Req, DocRoot, Parames) ->
    "/" ++ Path = Req:get(path),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case Path of
                    "index" ->
                        Type1 = ok,
                        Message = <<"你好，欢迎来到mochiweb的世界！">>,
                        Req:ok({"text/javascript", mochijson2:encode({
                                struct, [
                                    {Type1, Message}
                                ]
                            })
                        });
                    "index.php" ->
                        QueryStringData = Req:parse_qs(),
                        Username = proplists:get_value("username", QueryStringData, "杨毅"),
                        {ok, HTMLOutput} = index_dtl:render([{username, Username}]),
                        Req:respond({200, [{"Content-Type", "text/html"}],
                                HTMLOutput});
                    _ ->
                        Req:serve_file(Path, DocRoot)
                end;
            'POST' ->
                case Path of
                    _ ->
                        Req:not_found()
                end;
            _ ->
                Req:respond({501, [], []})
        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

% Iterate recursively on our list of {Regexp, Function} tuples
dispatch(_, []) -> none;
dispatch(Req, [{Regexp, Function}|T]) -> 
    "/" ++ Path = Req:get(path),
    Method = Req:get(method),
    Match = re:run(Path, Regexp, [global, {capture, all_but_first, list}]),
    case Match of
        {match,[MatchList]} -> 
            % We found a regexp that matches the current URL path
            case length(MatchList) of
                0 -> 
                    % We didn't capture any URL parameters
                    ilogin_views:Function(Method, Req);
                Length when Length > 0 -> 
                    % We pass URL parameters we captured to the function
                    Args = lists:append([[Method, Req], MatchList]),
                    apply(ilogin_views, Function, Args)
            end;
        _ -> 
            dispatch(Req, T)
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
