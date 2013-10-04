-module(erlevator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(C_ACCEPTORS,  100).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Routes    = routes(),
    Dispatch  = cowboy_router:compile(Routes),
    Port      = port(),
    TransOpts = [{port, Port}],
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    HttpStart = cowboy:start_http(http, ?C_ACCEPTORS, TransOpts, ProtoOpts),
    io:format("cowboy:start_http (on port ~p): ~p~n", [Port, HttpStart]),
    {ok, _}   = HttpStart,
    erlevator_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================
routes() ->
    [
     {'_', [
            {"/nextCommand", erlevator_handler, [next_command]},
            %% Events
            {"/call", erlevator_handler, [call]},
            {"/go",   erlevator_handler, [go]},
            {"/userHasEntered",   erlevator_handler, [user_entered]},
            {"/userHasExited",   erlevator_handler, [user_exited]},
            {"/reset", erlevator_handler, [reset]}
           ]}
    ].

%%
%% Retrieve the PORT either from an os environment variable
%% e.g. in Heroku environment, or from the application conf.
%%
port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.