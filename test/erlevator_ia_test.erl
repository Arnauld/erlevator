-module(erlevator_ia_test).

% Shared record definiation
-include("../src/erlevator.hrl").
-include_lib("eunit/include/eunit.hrl").

start(NbFloor, Algo) ->
  erlevator_ia:start(NbFloor, Algo).

stop() ->
  erlevator_ia:stop().

debug() ->
  erlevator_ia:debug().

next_command() ->
  erlevator_ia:next_command().

event(EventType, Details) ->
  erlevator_ia:event(EventType, Details).

state() ->
  erlevator_ia:state().

format_state(Elevator) ->
  erlevator_ia:format_state(Elevator).


%% ===================================================================
%% Tests
%% ===================================================================

-ifdef(IGNORE).

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


optimized_should_move_to_the_called_floor_but_not_stop_when_called_on_the_opposite_way_2_test() ->
  try
    start(5, optimized),
    debug(),
    event(call, [5, down]),
    ?assertEqual(up,      next_command()), % 1st
    ?assertEqual(up,      next_command()), % 2nd
    ?assertEqual(up,      next_command()), % 3rd
    ?assertEqual(up,      next_command()), % 4th
    ?assertEqual(up,      next_command()), % 5th
    ?assertEqual(opened,  next_command()),
    ?assertEqual(closed,  next_command()),
    ?assertEqual(nothing, next_command()), % idle
    event(call, [3, up]),
    event(call, [2, up]),
    ?assertEqual(down,    next_command()), % 4th
    ?assertEqual(down,    next_command()), % 3rd
    ?assertEqual(down,    next_command()), % 2nd
    ?assertEqual(opened,  next_command()),
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()), % 3th
    ?assertEqual(opened,  next_command()),
    ok
  after
    stop()
  end.

reset_test() ->
  try
    start(5, optimized),
    debug(),
    Min = -13,
    Max =  27,
    Cap = 45,
    event(reset, [<<"Ooops">>, Min, Max, Cap]),
    #state{floor     = Floor,
           floor_min = FloorMin,
           floor_max = FloorMax,
           capacity  = Capacity} = state(),
    ?assertEqual(Min, Floor),
    ?assertEqual(Min, FloorMin),
    ?assertEqual(Max, FloorMax),
    ?assertEqual(Cap, Capacity),
    ok
  after
    stop()
  end.

negative_floors_test() ->
  try
    Min = -13,
    Max = 27,
    Cap = 45,
    start(5, optimized),
    event(reset, [<<"Ooops">>, Min, Max, Cap]),
    debug(),
    event(call, [-7, down]),
    ?assertEqual(up,      next_command()), % -12
    ?assertEqual(up,      next_command()), % -11
    ?assertEqual(up,      next_command()), % -10
    ?assertEqual(up,      next_command()), % -9
    ?assertEqual(up,      next_command()), % -8
    ?assertEqual(up,      next_command()), % -7
    ?assertEqual(opened,  next_command()),
    ?assertEqual(closed,  next_command()),
    ?assertEqual(nothing, next_command()), % idle
    event(go, [-5]),
    ?assertEqual(up,      next_command()), % -6
    ?assertEqual(up,      next_command()), % -5
    ?assertEqual(opened,  next_command()),
    ?assertEqual(closed,  next_command()),
    ok
  after
    stop()
  end.

doors_stay_opened_if_user_keep_entering_or_exiting_test() ->
  try
    Min = -5,
    Max = 5,
    Cap = 45,
    start(5, optimized),
    event(reset, [<<"Ooops">>, Min, Max, Cap]),
    debug(),

    event(call, [-4, up]),
    event(call, [-2, up]),
    event(call, [+2, up]),
    ?assertEqual(up,      next_command()), % -4
    ?assertEqual(opened,  next_command()),
    event(user_entered, []),
    ?assertEqual(nothing, next_command()), % keep doors opened
    ?assertEqual(closed,  next_command()), % no newcomers since last tick
    ?assertEqual(up,      next_command()), % -3
    ?assertEqual(up,      next_command()), % -2
    ?assertEqual(opened,  next_command()),
    event(user_entered, []),
    event(user_entered, []),
    ?assertEqual(nothing, next_command()), % keep doors opened
    event(user_entered, []),
    ?assertEqual(nothing, next_command()), % keep doors opened
    event(user_exited, []),
    ?assertEqual(nothing, next_command()), % keep doors opened
    ?assertEqual(closed,  next_command()), % no newcomers since last tick
    ?assertEqual(up,      next_command()), % -1
    ok
  after
    stop()
  end.


negative_floors__2_test() ->
  try
    Min = -13,
    Max = 27,
    Cap = 45,
    start(5, optimized),
    event(reset, [<<"Ooops">>, Min, Max, Cap]),
    debug(),

    event(call, [-5, up]),
    event(call, [-11, up]),
    event(call, [-10, down]),
    ?assertEqual(up,      next_command()), % -12
    event(call, [27, down]),
    ?assertEqual(up,      next_command()), % -11
    ?assertEqual(opened,  next_command()),
    event(user_entered, []),
    ?assertEqual(nothing, next_command()), % user entered, let's stay there
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()), % -10
    event(call, [16, down]),
    ?assertEqual(up,      next_command()), % -9
    ?assertEqual(up,      next_command()), % -8
    ?assertEqual(up,      next_command()), % -7
    event(call, [19, down]),
    ?assertEqual(up,      next_command()), % -6
    ?assertEqual(up,      next_command()), % -5

    ST0 = state(),
    ?assertEqual(-5,      ST0#state.floor),
    event(call, [-4, up]),
    ?assertEqual(opened,  next_command()), %
    event(call, [11, up]),
    event(call, [-5, up]),
    event(call, [17, down]),
    ?assertEqual(closed,  next_command()),
    ?assertEqual(up,      next_command()), % -4
    event(call, [-9, down]),
    event(call, [-12, up]),

    ST1 = state(),
    ?assertEqual(-4,      ST1#state.floor),
    ?assertEqual(opened,  next_command()), %
    ?assertEqual(closed,  next_command()), %
    event(call, [-13, up]),
    event(call, [-4, up]),
    event(call, [7, up]),
    event(call, [18, down]),
    event(call, [17, down]),
    event(call, [-5, up]),

    ?assertEqual(opened,  next_command()), % call: -4, up

    event(call, [1, up]),
    next_command(),
    event(call, [-7, down]),
    event(call, [-10, up]),
    next_command(),
    event(user_entered, []),
    event(go, [-9]),
    event(call, [-6, up]),
    next_command(),
    event(call, [26, down]),
    event(call, [17, down]),
    next_command(),
    event(call, [9, up]),
    event(call, [26, down]),
    next_command(),
    event(call, [23, down]),
    next_command(),
    next_command(),
    event(call, [11, down]),
    event(user_entered, []),
    event(go, [16]),
    event(call, [8, down]),
    next_command(),
    next_command(),
    event(call, [-9, down]),
    next_command(),
    next_command(),
    event(call, [-8, up]),
    event(call, [5, down]),
    next_command(),
    next_command(),
    event(call, [25, down]),
    next_command(),
    next_command(),
    event(call, [14, up]),
    event(call, [13, down]),
    next_command(),
    event(go, [17]),
    event(user_entered, []),
    event(go, [21]),
    event(call, [10, down]),
    event(call, [10, down]),
    io:format("state: ~s ~n", [format_state(state())]),
    ?assertEqual(nothing,  next_command()),
    ?assertEqual(closed,  next_command()),
    ok
  after
    stop()
  end.

-endif.

negative_floors__3_test() ->
  try
    Min = -2,
    Max =  4,
    Cap =  5,
    start(5, optimized),
    event(reset, [<<"Ooops">>, Min, Max, Cap]),
    debug(),

    event(call, [ 1, up]),
    ?assertEqual(up,      next_command()), % -1
    ?assertEqual(up,      next_command()), %  0
    ?assertEqual(up,      next_command()), %  1
    ?assertEqual(opened,  next_command()),
    io:format("~~ state: ~s ~n", [format_state(state())]),
    io:format("~nDoors open and should keep them open~n...~n"),
    Cmd = next_command(),
    io:format("~~ state: ~s ~n", [format_state(state())]),
    ?assertEqual(nothing, Cmd), %< no user entered yet, let's stay there
    ?assertEqual(closed,  next_command()),
    ?assertEqual(nothing, next_command()),
    event(go, [2]),
    ?assertEqual(up,      next_command()), %  2
    ?assertEqual(opened,  next_command()),
    ?assertEqual(nothing, next_command()),
    event(call, [ 1, up]),
    ?assertEqual(closed, next_command()),
    ?assertEqual(down,   next_command()),
    ok
  after
    stop()
  end.

























