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
  {ok, Req0} = handle0(Method, Path, Req2),
  io:format("Elevator: ~p ~n", [erlevator_ia:state()]),
  {ok, Req0, undefined}.


terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Handle functions
%% ===================================================================

%%
%% nextCommand
%%
handle0(<<"GET">>, <<"/nextCommand">>, Req2) ->
  NewCmd = erlevator_ia:next_command(),
  Body = command_to_body(NewCmd),
  cowboy_req:reply(200, [], Body, Req2);

%%
%% reset event
%%
handle0(<<"GET">>, <<"/reset">>, Req2) ->
  {Cause, Req3} = cowboy_req:qs_val(<<"cause">>, Req2),
  erlevator_ia:event(reset, [Cause]),
  cowboy_req:reply(200, Req3);

%%
%% call event
%%
handle0(<<"GET">>, <<"/call">>, Req2) ->
  {AtFloor,   Req3} = cowboy_req:qs_val(<<"atFloor">>, Req2),
  {Direction, Req4} = cowboy_req:qs_val(<<"to">>, Req3),
  try
    Int = binary_to_integer(AtFloor),
    Dir = case Direction of
            <<"UP">> -> up;
            _ -> down
          end,
    erlevator_ia:event(call, [Int, Dir]),
    cowboy_req:reply(200, Req4)
  catch
    Ex:Term ->
      io:format("Ouch 'call/': ~p:~p ~n", [Ex, Term]),
      cowboy_req:reply(400, Req4)
  end;

%%
%% go event
%%
handle0(<<"GET">>, <<"/go">>, Req2) ->
  {Destination, Req3} = cowboy_req:qs_val(<<"floorToGo">>, Req2),
  try
    Int = binary_to_integer(Destination),
    erlevator_ia:event(go, [Int]),
    cowboy_req:reply(200, Req3)
  catch
    Ex:Term ->
      io:format("Ouch 'go/': ~p:~p ~n", [Ex, Term]),
      cowboy_req:reply(400, Req3)
  end;

%%
%% user entered event
%%
handle0(<<"GET">>, <<"/userHasEntered">>, Req2) ->
  erlevator_ia:event(user_entered, []),
  cowboy_req:reply(200, Req2);

%%
%% user exited event
%%
handle0(<<"GET">>, <<"/userHasExited">>, Req2) ->
  erlevator_ia:event(user_exited, []),
  cowboy_req:reply(200, Req2);

%%
%% All Other GET cases
%%
handle0(<<"GET">>, _, Req2) ->
  cowboy_req:reply(404, Req2);

%%
%% Any other Method cases
%%
handle0(_, _, Req2) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req2).

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

