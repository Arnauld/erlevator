-type(status() :: up | down | open | close).
-type(extended_status() :: status() | nothing).

-record(state, {floor         :: integer(),
                floor_max     :: integer(),
                floor_min     :: integer(),
                direction     :: integer(),
                state         :: status(),
                state_to_use  :: extended_status(),
                algo          :: term(),
                floor_events,
                debug         :: boolean()}).

-record(floor_event, {what,
                      idle    :: (undefined | integer())}).
