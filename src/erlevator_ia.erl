-module(erlevator_ia).

% Shared record definiation
-include("erlevator.hrl").

%%
%
%%
new(NbFloor) -> #state{floor = 0,
                       floor_min = 0,
                       floor_max = NbFloor,
                       direction = +1,
                       state = nothing}.

%%
%
%%
next_command(State = #state{state = Prev}) ->
  case Prev of
    opened ->
      State#state{state = closed};
    closed ->
      move(State);
    nothing ->
      State;
    _ ->
      State#state{state = open}
  end.



%%
%
%%
move(State = #state{floor = Floor,
                    floor_min = FloorMin,
                    floor_max = FloorMax,
                    direction = Dir}) ->
  NextFloor = Floor + Dir,
  if
    (NextFloor > FloorMax) or (NextFloor < FloorMin) ->
      NewDir = -Dir,
      AdjustedFloor = Floor + NewDir,
      NewState = if
                   NewDir > 0 -> up;
                   true -> down
                 end,
      State#state{floor=AdjustedFloor, direction=NewDir, state=NewState};
    true -> % aka else
      State#state{floor=NextFloor}
  end.

%%
%
% Tests
%
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
	IA = new(5),
	Expected = #state{
                    floor=0,
                    floor_min = 0,
                    floor_max = 5,
                    direction = +1,
                    state = nothing},
	?assertEqual(Expected, IA),
	ok.

elevator_should_change_direction_when_hitting_roof_test() ->
  IA  = new(5),
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

-endif.
