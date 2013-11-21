-module(erlevator_handler).

% Shared record definiation
-include("erlevator.hrl").

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
  % io:format("Elevator: ~p ~n", [erlevator_ia:state()]),
  {ok, Req0, undefined}.


%%
%% @doc terminate/3 handler callback
%%
terminate(_Reason, _Req, _State) ->
    ok.

%% ===================================================================
%% Handle functions
%% ===================================================================

join_events(Array) ->
  array:foldl(fun(Index, #floor_event{what = What, idle = Idle}, Acc) ->
                Loc = io_lib:format("{\"i\":~w, \"what\":\"~w\", \"idle\":~w}",
                                    [Index, What, Idle]),
                if  Index > 0 -> io_lib:format("~s, ~s", [Loc, Acc]);
                    true      -> Loc
                end
              end,
              undefined,
              Array).

%%
%% @doc nextCommand
%% @private
%%
handle0(<<"GET">>, <<"/status">>, Req2) ->
  #state{floor        = Floor,
         floor_max    = FloorMax,
         floor_min    = FloorMin,
         direction    = Direction,
         state        = State,
         state_to_use = StateToUse,
         algo         = Algo,
         floor_events = Events} = erlevator_ia:state(),
  Body  = io_lib:format("{\"floor\": ~w, \"floor_min\":~w, \"floor_max\":~w, "
                        "\"direction\": \"~w\", "
                        "\"state\": \"~w\", "
                        "\"state-to_use\": \"~w\", "
                        "\"algo\" :\"~w\", \"events\":[~s]}", [Floor, FloorMin, FloorMax,
                                                      Direction,
                                                      State, StateToUse,
                                                      Algo, join_events(Events)]),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>},
                         {<<"content-encoding">>, <<"utf-8">>}], Body, Req2);


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
  {LowerFloorS,  Req4} = cowboy_req:qs_val(<<"lowerFloor">>, Req3, 0),
  {HigherFloorS, Req5} = cowboy_req:qs_val(<<"higherFloor">>, Req4, 5),
  {CapacityS,    Req6} = cowboy_req:qs_val(<<"cabinSize">>, Req5, 100),

  try
    LowerFloor  = binary_to_integer(LowerFloorS),
    HigherFloor = binary_to_integer(HigherFloorS),
    Capacity    = binary_to_integer(CapacityS),
    erlevator_ia:event(reset, [Cause, LowerFloor, HigherFloor, Capacity]),
    cowboy_req:reply(200, Req6)
  catch
    Ex:Term ->
      io:format("Ouch 'reset/': ~p:~p ~n", [Ex, Term]),
      cowboy_req:reply(400, Req6)
  end;




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

