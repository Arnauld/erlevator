-module(erlevator_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%% ===================================================================
%% Handler callbacks
%% ===================================================================

%%
%% @doc init/3 handler callback
%%
init(_Transport, Req, Opts) ->
    % Opts is defined "as is" for the State value
    % in the handle/2 method
    {ok, Req, Opts}.

%%
%% @doc handle/2 handler callback
%%
handle(Req, []) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path,   Req2} = cowboy_req:path(Req1),
  % io:format("Method: ~p // ~p  ~n", [Method, Path]),
  {ok, Req0} = handle0(Method, Path, Req2),
  io:format("Elevator: ~p ~n", [erlevator_ia:state()]),
  {ok, Req0, undefined}.


%%
%% @doc terminate/3 handler callback
%%
terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Handle functions
%% ===================================================================

%%
%% @doc nextCommand
%% @private
%%
handle0(<<"GET">>, <<"/nextCommand">>, Req2) ->
  NewCmd = erlevator_ia:next_command(),
  Body = command_to_body(NewCmd),
  cowboy_req:reply(200, [], Body, Req2);

%%
%% @doc reset event
%% @private
%%
handle0(<<"GET">>, <<"/reset">>, Req2) ->
  {Cause,        Req3} = cowboy_req:qs_val(<<"cause">>, Req2),
  {LowerFloor ,  Req4} = cowboy_req:qs_val(<<"lowerFloor">>, Req3),
  {HigherFloor , Req5} = cowboy_req:qs_val(<<"higherFloor">>, Req4),
  erlevator_ia:event(reset, [Cause, LowerFloor, HigherFloor]),
  cowboy_req:reply(200, Req5);

%%
%% @doc call event
%% @private
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
%% @doc go event
%% @private
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
%% @doc user entered event
%% @private
%%
handle0(<<"GET">>, <<"/userHasEntered">>, Req2) ->
  erlevator_ia:event(user_entered, []),
  cowboy_req:reply(200, Req2);

%%
%% @doc user exited event
%% @private
%%
handle0(<<"GET">>, <<"/userHasExited">>, Req2) ->
  erlevator_ia:event(user_exited, []),
  cowboy_req:reply(200, Req2);

%%
%% @doc All Other GET cases returns a 404 HTTP response
%% @private
%%
handle0(<<"GET">>, _, Req2) ->
  cowboy_req:reply(404, Req2);

%%
%% @doc Any other Method cases a 405 HTTP response
%% @private
%%
handle0(_, _, Req2) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req2).

%% ===================================================================
%% Internal functions
%% ===================================================================

%%
%% @doc convert an atom to the expected string.
%%     e.g. 'up' to '<<"UP">>'
%% @private
%%
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
  ?assertEqual(<<"DOWN">>, command_to_body(down)),
  ?assertEqual(<<"OPEN">>, command_to_body(opened)),
  ?assertEqual(<<"CLOSE">>, command_to_body(closed)),
  ?assertEqual(<<"NOTHING">>, command_to_body(nothing)).

-endif.

