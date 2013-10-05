-module(erlevator_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% ===================================================================
%% Handler callbacks
%% ===================================================================

init(_Transport, Req, Opts) ->
    % Opts is defined "as is" for the State value
    % in the handle/2 method
    {ok, Req, Opts}.

handle(Req, []) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  % io:format("Method: ~p // ~p  ~n", [Method, Path]),
  {ok, Req0} = case [Method, Path] of
                  [<<"GET">>, <<"/nextCommand">>] ->
                      NewCmd = erlevator_ia:next_command(),
                      Body = command_to_body(NewCmd),
                      cowboy_req:reply(200, [], Body, Req2);

                  [<<"GET">>, <<"/reset">>] ->
                      {Cause, Req3} = cowboy_req:qs_val(<<"cause">>, Req2),
                      erlevator_ia:event(reset, [Cause]),
                      cowboy_req:reply(200, Req3);

                  [<<"GET">>, <<"/call">>] ->
                      {AtFloor,   Req3} = cowboy_req:qs_val(<<"atFloor">>, Req2),
                      {Direction, Req4} = cowboy_req:qs_val(<<"to">>, Req3),
                      erlevator_ia:event(call, [AtFloor, Direction]),
                      cowboy_req:reply(200, Req4);

                  [<<"GET">>, <<"/go">>] ->
                      {Destination, Req3} = cowboy_req:qs_val(<<"floorToGo">>, Req2),
                      erlevator_ia:event(go, [Destination]),
                      cowboy_req:reply(200, Req3);

                  [<<"GET">>, <<"/userHasEntered">>] ->
                      erlevator_ia:event(user_entered, []),
                      cowboy_req:reply(200, Req2);

                  [<<"GET">>, <<"/userHasExited">>] ->
                      erlevator_ia:event(user_exited, []),
                      cowboy_req:reply(200, Req2);

                  _ ->
                      %% Method not allowed.
                      cowboy_req:reply(405, Req2)
                end,
  {ok, Req0, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

command_to_body(up)      -> <<"UP">>;
command_to_body(down)    -> <<"DOWN">>;
command_to_body(opened)  -> <<"OPEN">>;
command_to_body(closed)  -> <<"CLOSE">>;
command_to_body(nothing) -> <<"NOTHING">>.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


command_to_body_test() ->
  ?assertEqual(<<"UP">>, command_to_body(up)),
  ?assertEqual(<<"NOTHING">>, command_to_body(nothing)).

-endif.

