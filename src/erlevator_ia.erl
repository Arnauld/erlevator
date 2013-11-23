-module(erlevator_ia).

% Shared record definiation
-include("erlevator.hrl").

-export([start/1, start/2, loop/1, stop/0]).
-export([next_command/0]).
-export([event/2]).
-export([format_state/1, state/0, debug/0]).


%% ===================================================================
%% Exposed API
%% ===================================================================

%%
%% @doc start an elevator with <code>NbFloor</code> floors and based
%%      on the <code>optimized</code> algorithm.
%% @see erlevator:start/2
%%

-spec(start(integer()) -> pid()).

start(NbFloor) ->
  start(NbFloor, optimized).

%%
%% @doc start an elevator with <code>NbFloor</code> floors and based
%%      on the <code>Algo</code> algorithm. Once started the
%%      corresponding <code>pid</code> is registered with the
%%      <code>erlevator_ia</code> key.
%%

-spec(start(integer(), (optimized|omnibus)) -> pid()).

start(NbFloor, Algo) ->
  Elevator = new_elevator(0, NbFloor, 100, Algo),
  Pid = spawn(erlevator_ia, loop, [Elevator]),
  io:format("erlevator_ia#start: Pid: ~p~n", [Pid]),
  register(erlevator_ia, Pid).

%%
%% @doc stop the registered elevator.
%%

stop() ->
  case whereis(erlevator_ia) of
    undefined ->
      io:format("erlevator_ia#stop: no Pid bound~n");

    Pid ->
      Pid ! stop
  end.

%%
%% @doc stop the registered elevator.
%%
debug() ->
  whereis(erlevator_ia) ! debug.


%%
%% @doc notify the registered elevator.
%%
event(EventType, Details) ->
  Pid = whereis(erlevator_ia),
  io:format("erlevator_ia#event: (~p, ~p) // Pid: ~p~n", [EventType, Details, Pid]),
  Pid ! { event, EventType, Details }.

%%
%% @doc compute and returns the next command from the registered elevator.
%%
next_command() ->
  whereis(erlevator_ia) ! { next_command, self() },
  receive
    {Command} ->
      % io:format("erlevator_ia#next_command: ~p~n", [Command]),
      Command
  end.

%%
%% @doc query the current state of the registered elevator.
%%
state() ->
  whereis(erlevator_ia) ! { state, self() },
  receive
    {Elevator} ->
      Elevator
  end.

%%
%% @doc Process loop that maintains the Elevator's State.
%% @private
%%
loop(Elevator) ->
  receive
    {event, reset, [Cause, LowerFloor, HigherFloor, Capacity]} ->
      io:format("erlevator_ia#loop(reset):{} min: ~p, max:~p, reason: ~p ~n",
                [LowerFloor, HigherFloor, Cause]),
      NewElevator = new_elevator(LowerFloor,
                                 HigherFloor,
                                 Capacity,
                                 Elevator#state.algo),
      loop(NewElevator);

    {event, EventType, Details} ->
      % io:format("erlevator_ia#loop(event): ~p, ~p, state: ~p~n", [EventType, Details, Elevator]),
      NewElevator = event(Elevator, EventType, Details),
      loop(NewElevator);

    {next_command, From} ->
      NewElevator = next_command(Elevator),
      % io:format("erlevator_ia#loop(next_command): ~p~n", [NewElevator]),
      State = case NewElevator#state.state_to_use of
                undefined ->
                  NewElevator#state.state;
                Other ->
                  Other
              end,
      From ! { State },
      NewElevator1 = tick(NewElevator),
      loop(NewElevator1);

    {state, From} ->
      From ! { Elevator },
      loop(Elevator);

    %%
    {algo, Algo} ->
      NewElevator = Elevator#state{algo = Algo},
      loop(NewElevator);

    debug ->
      Debug = Elevator#state.debug,
      NewElevator = Elevator#state{debug = not Debug},
      loop(NewElevator);

    stop ->
      true
  end.

%%
%%
%% @private
%%
join_events(Array) ->
  array:foldl(fun(Index, #floor_event{what     = What,
                                      idle     = Idle,
                                      nb_users = NbUsers,
                                      nb_users_last_tick = NbUsersL}, Acc) ->
                Loc = io_lib:format("    {"
                                    "\"i\":~w, "
                                    "\"what\":\"~w\", "
                                    "\"idle\":~w, "
                                    "\"nb_users\":~w, "
                                    "\"nb_users_last_tick\":~w, "
                                    "}~n",
                                    [Index, What, Idle, NbUsers, NbUsersL]),
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
format_state(#state{floor        = Floor,
                    floor_max    = FloorMax,
                    floor_min    = FloorMin,
                    capacity     = Capacity,
                    nb_users     = NbUsers,
                    direction    = Direction,
                    state        = State,
                    state_to_use = StateToUse,
                    nb_ticks_opened = NbTicksOpened,
                    algo         = Algo,
                    floor_events = Events}) ->
  io_lib:format("{\"floor\": ~w, ~n"
                "\"floor_min\":~w, ~n"
                "\"floor_max\":~w, ~n"
                "\"direction\": \"~w\", ~n"
                "\"nb_users\": ~w, ~n"
                "\"capacity\": ~w, ~n"
                "\"state\": \"~w\", ~n"
                "\"state_to_use\": \"~w\", ~n"
                "\"nb_ticks_opened\": ~w, ~n"
                "\"algo\" :\"~w\", ~n"
                "\"events\":[~s]}",
                [Floor, FloorMin, FloorMax,
                 Direction,
                 NbUsers, Capacity,
                 State, StateToUse,
                 NbTicksOpened,
                 Algo, join_events(Events)]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%%
%% @doc create a new Elevator. It is used to store the current state of
%% an Elevator, but does not start any process.
%%
%% @private
%% @see erlevator:start/1
%% @see erlevator:start/2
%%
%%
new_elevator(FloorMin, FloorMax, Capacity, Algo) ->
  NbFloor = abs(FloorMax - FloorMin) + 1,
  #state{floor     = FloorMin,
         floor_min = FloorMin,
         floor_max = FloorMax,
         capacity  = Capacity,
         nb_users  = 0,
         direction = +1,
         state     = closed,
         state_to_use = undefined,
         algo      = Algo,
         floor_events = array:new([{size,    NbFloor},
                                   {fixed,   true},
                                   {default, new_event()}]),
         debug     = false}.

%%
%% @doc create a new event to hold a floor state.
%% @private
%%
new_event() -> #floor_event{idle = 0,
                            what = undefined,
                            nb_users = 0,
                            nb_users_last_tick = 0}.

%%
%% @doc Event handle
%% @private
%%
event(Elevator, call, [AtFloor, Direction]) ->
  FloorMin = Elevator#state.floor_min,
  FloorEvents = Elevator#state.floor_events,
  FloorEvent  = array:get(AtFloor - FloorMin, FloorEvents),
  case FloorEvent#floor_event.what of
    stop -> % whatever the direction one already stops there :)
      Elevator;

    Direction -> % direction already recorded
      Elevator;

    undefined -> %
      NewEvent    = FloorEvent#floor_event{what=Direction},
      NewEvents   = array:set(AtFloor - FloorMin, NewEvent, FloorEvents),
      Elevator#state{floor_events = NewEvents};

    _ -> % the other direction is already tracked
      NewEvent    = FloorEvent#floor_event{what=stop},
      NewEvents   = array:set(AtFloor - FloorMin, NewEvent, FloorEvents),
      Elevator#state{floor_events = NewEvents}
  end;

event(Elevator, go, [Destination]) ->
  FloorMin = Elevator#state.floor_min,
  FloorEvents = Elevator#state.floor_events,
  FloorEvent  = array:get(Destination - FloorMin, FloorEvents),
  case FloorEvent#floor_event.what of
    stop -> % whatever the direction one already stops there :)
      Elevator;

    _ -> % stop there :)
      NewEvent    = FloorEvent#floor_event{what=stop},
      NewEvents   = array:set(Destination - FloorMin, NewEvent, FloorEvents),
      Elevator#state{floor_events = NewEvents}
  end;

event(Elevator, user_entered, []) ->
  nb_users_changed(Elevator, +1);

event(Elevator, user_exited, []) ->
  nb_users_changed(Elevator, -1).

%%
%%
%%
nb_users_changed(Elevator = #state{floor     = Floor,
                                   floor_min = FloorMin,
                                   floor_events = FloorEvents},
                 Amount) ->
  % update floor data
  Index = Floor - FloorMin,
  Event = array:get(Index, FloorEvents),
  Updated  = Event#floor_event.nb_users + Amount,
  NewEvent = Event#floor_event{nb_users=Updated},
  NewEvents = array:set(Index, NewEvent, FloorEvents),

  % update elevator data
  Elevator#state{nb_users = Elevator#state.nb_users + Amount,
                 floor_events = NewEvents}.

%%
%%
%%
tick(Elevator = #state{floor     = Floor,
                       floor_min = FloorMin,
                       floor_events = FloorEvents}) ->
  % update floor data
  Index    = Floor - FloorMin,
  Event    = array:get(Index, FloorEvents),
  NbUsers  = Event#floor_event.nb_users,
  NewEvent = Event#floor_event{nb_users_last_tick=NbUsers},

  io:format("elevator_ia:tick ~p~n", [NewEvent]),
  NewEvents = array:set(Index, NewEvent, FloorEvents),

  % update elevator data
  Elevator#state{floor_events = NewEvents}.


%%
%%
%%
has_tick_changed(Elevator = #state{floor     = Floor,
                       floor_min = FloorMin,
                       floor_events = FloorEvents}) ->
  Index    = Floor - FloorMin,
  Event    = array:get(Index, FloorEvents),
  NbUsers  = Event#floor_event.nb_users,
  NbUsersPrev = Event#floor_event.nb_users_last_tick,
  io:format("has_tick_changed: '~p' <> '~p' ~n", [NbUsers, NbUsersPrev]),
  not(NbUsers == NbUsersPrev).

%%
%% Reset the Event for the specified Floor
%%
reset_event(Floor, FloorMin, FloorEvents) ->
  Event = new_event(),
  array:set(Floor - FloorMin, Event, FloorEvents).

reset_event_on_open(Floor, FloorMin, FloorEvents) ->
  Event  = new_event(),
  EventN = Event#floor_event{nb_users=-1},
  array:set(Floor - FloorMin, EventN, FloorEvents).


%%
%%
%%
increment_idle(Floor, FloorMin, FloorEvents) ->
  Index    = Floor - FloorMin,
  Event = array:get(Index, FloorEvents),
  Idle  = Event#floor_event.idle,
  array:set(Index, Event#floor_event{idle = Idle + 1}, FloorEvents).

%%
%%
%%
state_for_direction(Dir) ->
  if
    Dir > 0 -> up;
    true -> down
  end.


%%
%%
%%
-record(result, {pass_through, destination}).

%%
%%
%%
result() -> #result{pass_through = undefined,
                    destination = undefined}.

%%
%%
%%
is_result_empty(#result{pass_through = PassThrough,
                        destination  = Destination}) ->
  if
    (PassThrough == undefined) and (Destination == undefined) ->
      true;

    true -> % aka else
      false
  end.

%% ===================================================================
%% next_command/1 when Omnibus
%% ===================================================================

next_command(Elevator = #state{floor = Floor,
                               floor_min = FloorMin,
                               state = Prev,
                               algo  = Algo,
                               floor_events = FloorEvents}) when Algo == omnibus ->
  case Prev of
    opened ->
      FloorEvent = array:get(Floor - FloorMin, FloorEvents),
      Idle = FloorEvent#floor_event.idle,
      TickChanged = has_tick_changed(Elevator),
      if
        TickChanged or ((Floor == 0) and (Idle < 3)) ->
           Elevator#state{state = opened,
                          state_to_use = nothing,
                          floor_events = increment_idle(Floor, FloorMin, FloorEvents)};

        true -> % aka else
           Elevator#state{state = closed,
                          state_to_use = undefined,
                          floor_events = reset_event(Floor, FloorMin, FloorEvents)}
      end;

    closed ->
      move(Elevator);

    nothing ->
      Elevator;

    up ->
      Elevator#state{state = opened};

    down ->
      Elevator#state{state = opened}
  end;

%% ===================================================================
%% next_command/1 when Optimized
%% ===================================================================

next_command(Elevator = #state{floor     = Floor,
                               floor_max = Max,
                               floor_min = Min,
                               state     = Prev,
                               direction = Dir,
                               algo      = Algo,
                               floor_events = FloorEvents}) when Algo == optimized ->
  if
    (Prev == opened) ->
      FloorEvent = array:get(Floor - Min, FloorEvents),
      Idle = FloorEvent#floor_event.idle,
      TickChanged = has_tick_changed(Elevator),
      NbTicks = Elevator#state.nb_ticks_opened,

      if
        TickChanged or (NbTicks == 0) or ((Floor == 0) and (Idle < 3)) ->
          Elevator#state{state = opened,
                         state_to_use = nothing,
                         nb_ticks_opened = NbTicks + 1,
                         floor_events = increment_idle(Floor, Min, FloorEvents)};

        true -> % aka else
           Elevator#state{state = closed,
                          state_to_use = undefined,
                          floor_events = reset_event(Floor, Min, FloorEvents)}
      end;

    (Prev == closed) or (Prev == up) or (Prev == down) ->

      Result = next_floor(Floor, Min, Max, Dir, FloorEvents, result()),
      ShouldOpen = should_open_door(Elevator, Result),

      % io:format("... [floor:~p] ~p, should open: ~p ~n", [Floor, Result, ShouldOpen]),
      if
         ShouldOpen ->
           io:format("elevator_ia:next_command: Opening doors..~n"),
           Elevator0 = Elevator#state{state = opened,
                                      state_to_use = undefined,
                                      nb_ticks_opened = 0,
                                      floor_events = reset_event_on_open(Floor, Min, FloorEvents)},
           io:format("elevator_ia:next_command: => ~s ..~n", [format_state(Elevator0)]),
           Elevator0;

         true -> % aka else
           case is_result_empty(Result) of
             true ->
               % check in the other dir
               Result1 = next_floor(Floor, Min, Max, -Dir, FloorEvents, result()),
               case is_result_empty(Result1) of
                 true ->
                   Elevator#state{state_to_use = nothing};

                 false ->
                   AdjustedFloor = Floor - Dir,
                   Elevator#state{state = state_for_direction(-Dir),
                                  direction = -Dir,
                                  state_to_use = undefined,
                                  floor = AdjustedFloor}
               end;

             false ->
               AdjustedFloor = Floor + Dir,
               Elevator#state{state = state_for_direction(Dir),
                              state_to_use = undefined,
                              floor = AdjustedFloor}

           end
      end
  end.

%%
%%
%%
move(Elevator = #state{floor = Floor,
                       floor_min = FloorMin,
                       floor_max = FloorMax,
                       direction = Dir,
                       algo      = Algo,
                       debug     = Debug}) when Algo == omnibus ->
  NextFloor = Floor + Dir,
  NewState  = if
                (NextFloor > FloorMax) or (NextFloor < FloorMin) ->
                  NewDir = -Dir,
                  AdjustedFloor = Floor + NewDir,
                  NewElevator = state_for_direction(NewDir),
                  Elevator#state{floor=AdjustedFloor, direction=NewDir, state=NewElevator};

                true -> % aka else
                  NewElevator = state_for_direction(Dir),
                  Elevator#state{floor=NextFloor, state=NewElevator}
              end,
  case Debug of
    true ->
      io:format("move Floor: ~p, Dir:~p -> NewState: ~p ~n", [Floor, Dir, NewState]);
    _ ->
       ok
  end,
  NewState.

%% ===================================================================
%% Optimized
%% ===================================================================

should_open_door(#state{floor     = Floor,
                        floor_min = FloorMin,
                        capacity  = Capacity,
                        nb_users  = NbUsers,
                        direction = Dir,
                        floor_events = Events,
                        debug     = Debug},
                 #result{destination  = Destination,
                         pass_through = PassThrough}) ->


  Event = array:get(Floor - FloorMin, Events),
  What   = Event#floor_event.what,
  StateForDir = state_for_direction(Dir),
  Res = if
          (What == stop) ->
            true;

          (NbUsers >= Capacity) ->
            false; % already full

          (Destination == undefined) and (PassThrough == Floor) ->
            true;

          (What == StateForDir) ->
                true;
          true -> % aka else
            false
        end,
  %%
  case Debug of
    true ->
      io:format("should_open_door Floor: ~p, Dir:~p, Dst: ~p, Passthrough: ~p ==> ~p ~n", [Floor, Dir, Destination, PassThrough, Res]);
    _ ->
       ok
  end,
  Res.



next_floor(Floor, Min, Max, Dir, Events, Result) ->
  if
    (Floor > Max) or (Floor < Min) ->
      Result;

    true -> % aka else
      Event = array:get(Floor - Min, Events),
      What  = Event#floor_event.what,
      StateForDir = state_for_direction(Dir),
      case What of
        stop ->
          Result#result{destination = Floor};

        StateForDir ->
          Result#result{destination = Floor};

        undefined ->
          next_floor(Floor + Dir, Min, Max, Dir, Events, Result);

        _ ->
          next_floor(Floor + Dir, Min, Max, Dir, Events, Result#result{pass_through = Floor})

      end
  end.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_elevator_test() ->
	IA0 = new_elevator(0, 5, 100, beuark),
  IA1 = IA0#state{floor_events = undefined}, %
	Expected = #state{floor=0,
                    floor_min = 0,
                    floor_max = 5,
                    nb_users  = 0,
                    capacity  = 100,
                    direction = +1,
                    state = closed,
                    algo  = beuark,
                    floor_events = undefined,
                    debug = false},
	?assertEqual(Expected, IA1),
	ok.

omnibus_should_change_direction_when_hitting_roof_test() ->
  IA  = new_elevator(0, 5, 100, omnibus),
  IA1 = #state{floor=Floor1} = move(IA), % 1st floor?
  ?assertEqual(1, Floor1),
  IA2 = #state{floor=Floor2} = move(IA1), % 2st floor?
  ?assertEqual(2, Floor2),
  IA3 = #state{floor=Floor3} = move(IA2), % 3st floor?
  ?assertEqual(3, Floor3),
  IA4 = #state{floor=Floor4} = move(IA3), % 4st floor?
  ?assertEqual(4, Floor4),
  IA5 = #state{floor=Floor5} = move(IA4), % 5st floor?
  ?assertEqual(5, Floor5),
  #state{floor=Floor6} = move(IA5), % 4st floor?
  ?assertEqual(4, Floor6),
  ok.

-endif.
