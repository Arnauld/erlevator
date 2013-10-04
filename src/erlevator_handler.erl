-module(erlevator_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, Opts) ->
    % Opts is defined "as is" for the State value
    % in the handle/2 method
    {ok, Req, Opts}.


handle(Req, [Command]) ->
  {Method, Req1} = cowboy_req:method(Req),
  io:format("Method: ~p // ~p ~n", [Method, Command]),
  {ok, Req0} = case [Method, Command] of
                  [<<"GET">>, next_command] ->
                      cowboy_req:reply(200, [], <<"UP">>, Req1);
                  [<<"GET">>, _] ->
                      cowboy_req:reply(200, Req1);
                  _ ->
                      %% Method not allowed.
                      cowboy_req:reply(405, Req1)
                end,
  {ok, Req0, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.