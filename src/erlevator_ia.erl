-module(erlevator_ia).

% Shared record definiation
-include("erlevator.hrl").

-export([start/1, start/2, loop/1, state/0]).
-export([next_command/0]).
-export([event/2]).


%% ===================================================================
%% Exposed API
%% ===================================================================

start(NbFloor) ->
  start(NbFloor, optimized).

start(NbFloor, Algo) ->
  Elevator = new_elevator(NbFloor, Algo),
  Pid = spawn(erlevator_ia, loop, [Elevator]),
  register(erlevator_ia, Pid).

stop() ->
  whereis(erlevator_ia) ! stop.

event(EventType, Details) ->
  io:format("erlevator_ia#event: (~p, ~p)~n", [EventType, Details]),
  whereis(erlevator_ia) ! { event, EventType, Details }.

next_command() ->
  whereis(erlevator_ia) ! { next_command, self() },
  receive
    {Command} ->
      % io:format("erlevator_ia#next_command: ~p~n", [Command]),
      Command
  end.

state() ->
  whereis(erlevator_ia) ! { state, self() },
  receive
    {Elevator} ->
      Elevator
  end.

%%
%% Process loop that maintains the Elevator's State
%%
loop(Elevator) ->
  receive
    {event, reset, [Cause]} ->
      % io:format("erlevator_ia#loop(reset):{} ~p ~n", [Cause]),
      NewElevator = new_elevator(Elevator#state.floor_max,
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
      loop(NewElevator);

    {state, From} ->
      From ! { Elevator },
      loop(Elevator);

    %%
    {algo, Algo} ->
      NewElevator = Elevator#state{algo = Algo},
      loop(NewElevator);

    stop ->
      true
  end.



%% ===================================================================
%% Internal functions
%% ===================================================================

%%
%%
%%
new_elevator(NbFloor, Algo) ->
  #state{floor     = 0,
         floor_min = 0,
         floor_max = NbFloor,
         direction = +1,
         state     = closed,
         state_to_use = undefined,
         algo      = Algo,
         floor_events = array:new([{size,    NbFloor + 1},
                                   {fixed,   true},
                                   {default, new_event()}])}.

%%
%%
%%
new_event() -> #floor_event{idle = 0,
                            what = undefined}.

%%
%% Event handle
%%
event(Elevator, call, [AtFloor, Direction]) ->
  FloorEvents = Elevator#state.floor_events,
  FloorEvent  = array:get(AtFloor, FloorEvents),
  case FloorEvent#floor_event.what of
    stop -> % whatever the direction one already stops there :)
      Elevator;

    Direction -> % direction already recorded
      Elevator;

    undefined -> %
      NewEvent    = FloorEvent#floor_event{what=Direction},
      NewEvents   = array:set(AtFloor, NewEvent, FloorEvents),
      Elevator#state{floor_events = NewEvents};

    _ -> % the other direction is already tracked
      NewEvent    = FloorEvent#floor_event{what=stop},
      NewEvents   = array:set(AtFloor, NewEvent, FloorEvents),
      Elevator#state{floor_events = NewEvents}
  end;

event(Elevator, go, [Destination]) ->
  FloorEvents = Elevator#state.floor_events,
  FloorEvent  = array:get(Destination, FloorEvents),
  case FloorEvent#floor_event.what of
    stop -> % whatever the direction one already stops there :)
      Elevator;

    _ -> % stop there :)
      NewEvent    = FloorEvent#floor_event{what=stop},
      NewEvents   = array:set(Destination, NewEvent, FloorEvents),
      Elevator#state{floor_events = NewEvents}
  end;

event(Elevator, user_entered, []) ->
  Elevator;

event(Elevator, user_exited, []) ->
  Elevator.

%%
%% Reset the Event for the specified Floor
%%
reset_event(Floor, FloorEvents) ->
    array:set(Floor, new_event(), FloorEvents).

%%
%%
%%
increment_idle(Floor, FloorEvents) ->
    Event = array:get(Floor, FloorEvents),
    Idle  = Event#floor_event.idle,
    array:set(Floor, Event#floor_event{idle = Idle + 1}, FloorEvents).

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

result() -> #result{pass_through = undefined,
                    destination = undefined}.

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
                               state = Prev,
                               algo  = Algo,
                               floor_events = FloorEvents}) when Algo == omnibus ->
  case Prev of
    opened ->
      FloorEvent = array:get(Floor, FloorEvents),
      Idle = FloorEvent#floor_event.idle,
      if
        (Floor == 0) and (Idle < 3) ->
           Elevator#state{state = opened,
                          state_to_use = nothing,
                          floor_events = increment_idle(Floor, FloorEvents)};

        true -> % aka else
           Elevator#state{state = closed,
                          state_to_use = undefined,
                          floor_events = reset_event(Floor, FloorEvents)}
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
      FloorEvent = array:get(Floor, FloorEvents),
      Idle = FloorEvent#floor_event.idle,
      if
        (Floor == 0) and (Idle < 3) ->
           Elevator#state{state = opened,
                          state_to_use = nothing,
                          floor_events = increment_idle(Floor, FloorEvents)};

        true -> % aka else
           Elevator#state{state = closed,
                          state_to_use = undefined,
                          floor_events = reset_event(Floor, FloorEvents)}
      end;

    (Prev == closed) or (Prev == up) or (Prev == down) ->

      Result = next_floor(Floor, Min, Max, Dir, FloorEvents, result()),
      ShouldOpen = should_open_door(Floor, Dir, FloorEvents, Result),

      % io:format("... [floor:~p] ~p, should open: ~p ~n", [Floor, Result, ShouldOpen]),
      if
         ShouldOpen ->
            Elevator#state{state = opened,
                           state_to_use = undefined,
                           floor_events = reset_event(Floor, FloorEvents)};

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
                       algo      = Algo}) when Algo == omnibus ->
  NextFloor = Floor + Dir,
  if
    (NextFloor > FloorMax) or (NextFloor < FloorMin) ->
      NewDir = -Dir,
      AdjustedFloor = Floor + NewDir,
      NewElevator = state_for_direction(NewDir),
      Elevator#state{floor=AdjustedFloor, direction=NewDir, state=NewElevator};

    true -> % aka else
      NewElevator = state_for_direction(Dir),
      Elevator#state{floor=NextFloor, state=NewElevator}
  end.

%% ===================================================================
%% Optimized
%% ===================================================================

should_open_door(Floor, Dir, Events, #result{destination  = Destination,
                                             pass_through = PassThrough}) ->
  if
    (Destination == undefined) and (PassThrough == Floor) ->
      true;

    true -> % aka else

      Event = array:get(Floor, Events),
      What  = Event#floor_event.what,
      StateForDir = state_for_direction(Dir),
      if
        (What == stop) or (What == StateForDir) ->
          true;

        true -> % aka else
          false
      end
 end.


next_floor(Floor, Min, Max, Dir, Events, Result) ->
  if
    (Floor > Max) or (Floor < Min) ->
      Result;

    true -> % aka else
      Event = array:get(Floor, Events),
      What  = Event#floor_event.what,
      StateForDir = state_for_direction(Dir),
      case What of
        stop ->
          Result#result{destination = Floor};

        StateForDir ->
          Result#result{destination = Floor};

        undefined ->
          next_floor(Floor + Dir, Min, Max, Dir, Events, Result);

        Other ->
          next_floor(Floor + Dir, Min, Max, Dir, Events, Result#result{pass_through = Floor})

      end
  end.

%% ===================================================================
%% Dump Events
%% ===================================================================
dump_events(#state{floor_max = Max,
                   floor_events = Events}) ->
  io:format("FloorEvents: ["),
  dump_events0(0, Max, Events).

dump_events0(Floor, Max, Events) when (Floor > Max) -> io:format("]~n");
dump_events0(Floor, Max, Events) ->
  #floor_event{what = What} = array:get(Floor, Events),
  io:format("[~p] ~p, ", [Floor, What]),
  dump_events0(Floor + 1, Max, Events).


%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_elevator_test() ->
	IA0 = new_elevator(5, beuark),
  IA1 = IA0#state{floor_events = undefined}, %
	Expected = #state{floor=0,
                    floor_min = 0,
                    floor_max = 5,
                    direction = +1,
                    state = closed,
                    algo  = beuark,
                    floor_events = undefined},
	?assertEqual(Expected, IA1),
	ok.

omnibus_should_change_direction_when_hitting_roof_test() ->
  IA  = new_elevator(5, omnibus),
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
  IA6 = #state{floor=Floor6} = move(IA5), % 4st floor?
  ?assertEqual(4, Floor6),
  ok.

omnibus_full_test() ->
  start(5, omnibus),
  try
    ?assertEqual(up,      next_command()),
    ?assertEqual(opened,  next_command()), % 1st floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()),
    ?assertEqual(opened,  next_command()), % 2nd floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()),
    ?assertEqual(opened,  next_command()), % 3rd floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()),
    ?assertEqual(opened,  next_command()), % 4th floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()),
    ?assertEqual(opened,  next_command()), % 5th floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()),
    ?assertEqual(opened,  next_command()), % 4th floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()),
    ?assertEqual(opened,  next_command()), % 3rd floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()),
    ?assertEqual(opened,  next_command()), % 2nd floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()),
    ?assertEqual(opened,  next_command()), % 1st floor
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()),
    ?assertEqual(opened,  next_command()), % 0st floor
    ?assertEqual(nothing, next_command()), % idle
    ?assertEqual(nothing, next_command()), % idle
    ?assertEqual(nothing, next_command()), % idle
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()),
    ok
  after
    stop()
  end.

optimized_should_open_door_when_called_at_the_same_floor_test() ->
  try
    start(5, optimized),
    event(call, [0, up]),
    ?assertEqual(opened,  next_command()),
    ok
  after
    stop()
  end.

optimized_should_move_to_the_called_floor_test() ->
  try
    start(5, optimized),
    event(call, [2, down]),
    ?assertEqual(up,     next_command()), % 1st
    ?assertEqual(up,     next_command()), % 2nd
    ?assertEqual(opened, next_command()),
    ok
  after
    stop()
  end.

optimized_should_move_to_the_called_floor_but_stop_when_called_on_its_way_test() ->
  try
    start(5, optimized),
    event(call, [2, down]),
    ?assertEqual(up,     next_command()), % 1st
    event(call, [1, up]),
    ?assertEqual(opened, next_command()),
    ?assertEqual(closed, next_command()),
    ?assertEqual(up,     next_command()), % 2nd
    ?assertEqual(opened, next_command()),
    ok
  after
    stop()
  end.

optimized_should_move_to_the_called_floor_but_not_stop_when_called_on_the_opposite_way_test() ->
  try
    start(5, optimized),
    event(call, [2, down]),
    ?assertEqual(up,      next_command()), % 1st
    event(call, [1, down]),
    ?assertEqual(up,      next_command()), % 2nd
    ?assertEqual(opened,  next_command()),
    event(go, [0]),
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()), % 1st
    ?assertEqual(opened,  next_command()),
    ?assertEqual(closed,  next_command()),
    ?assertEqual(down,    next_command()), % ground
    ?assertEqual(opened,  next_command()),
    ?assertEqual(nothing, next_command()), % idle 3 times
    ok
  after
    stop()
  end.

-endif.
