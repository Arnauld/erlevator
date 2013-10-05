-module(erlevator_ia).

% Shared record definiation
-include("erlevator.hrl").

-export([start/1, start/2, loop/1]).
-export([next_command/0]).
-export([event/2]).

%% ===================================================================
%% Exposed API
%% ===================================================================

start(NbFloor) ->
  start(NbFloor, omnibus).

start(NbFloor, Algo) ->
  Elevator = new_elevator(NbFloor, Algo),
  Pid = spawn(erlevator_ia, loop, [Elevator]),
  register(erlevator_ia, Pid).

stop() ->
  whereis(erlevator_ia) ! stop.

event(EventType, Details) ->
  whereis(erlevator_ia) ! { event, EventType, Details }.

next_command() ->
  whereis(erlevator_ia) ! { next_command, self() },
  receive
    {Command} ->
      % io:format("erlevator_ia#next_command: ~p~n", [Command]),
      Command
  end.


loop(Elevator) ->
  receive
    {event, reset, [Cause]} ->
      % io:format("erlevator_ia#loop(reset):{} ~p ~n", [Cause]),
      NewElevator = new_elevator(Elevator#state.floor_max,
                                 Elevator#state.algo),
      loop(NewElevator);

    {event, EventType, Details} ->
      % io:format("erlevator_ia#loop(event): ~p, ~p, state: ~p~n", [EventType, Details, Elevator]),
      loop(Elevator);

    {next_command, From} ->
      NewElevator = next_command(Elevator),
      % io:format("erlevator_ia#loop(next_command): ~p~n", [NewElevator]),
      From ! { NewElevator#state.state },
      loop(NewElevator);

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
%
%%
new_elevator(NbFloor, Algo) ->
  #state{floor = 0,
         floor_min = 0,
         floor_max = NbFloor,
         direction = +1,
         state = closed,
         algo  = Algo}.

%%
%
%%
next_command(Elevator = #state{state = Prev}) ->
  NewElevator = case Prev of
               opened ->
                 Elevator#state{state = closed};
               closed ->
                 move(Elevator);
               nothing ->
                 Elevator;
               up ->
                 Elevator#state{state = opened};
               down ->
                 Elevator#state{state = opened}
             end,
  % io:format("erlevator_ia#next_command/1 : ~p -> ~p ~n", [Prev, NewElevator]),
  NewElevator.



%%
%
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

%%
%%
%%
state_for_direction(Dir) ->
  if
    Dir > 0 -> up;
    true -> down
  end.

%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
	IA = new_elevator(5, beuark),
	Expected = #state{floor=0,
                    floor_min = 0,
                    floor_max = 5,
                    direction = +1,
                    state = closed,
                    algo  = beuark},
	?assertEqual(Expected, IA),
	ok.

elevator_should_change_direction_when_hitting_roof_test() ->
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
  ?assertEqual(4, Floor6).

elevator_should_be_an_omnibus_test() ->
  start(5),
  ?assertEqual(up,     next_command()),
  ?assertEqual(opened, next_command()), % 1st floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(up,     next_command()),
  ?assertEqual(opened, next_command()), % 2nd floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(up,     next_command()),
  ?assertEqual(opened, next_command()), % 3rd floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(up,     next_command()),
  ?assertEqual(opened, next_command()), % 4th floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(up,     next_command()),
  ?assertEqual(opened, next_command()), % 5th floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(down,   next_command()),
  ?assertEqual(opened, next_command()), % 4th floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(down,   next_command()),
  ?assertEqual(opened, next_command()), % 3rd floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(down,   next_command()),
  ?assertEqual(opened, next_command()), % 2nd floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(down,   next_command()),
  ?assertEqual(opened, next_command()), % 1st floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(down,   next_command()),
  ?assertEqual(opened, next_command()), % 0st floor
  ?assertEqual(closed, next_command()),
  ?assertEqual(up,     next_command()).

-endif.
