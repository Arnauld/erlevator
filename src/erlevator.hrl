-type(status() :: up | down | open | close).
-type(extended_status() :: status() | nothing).

-record(state, {floor         :: integer(),
                floor_max     :: integer(),
                floor_min     :: integer(),
                capacity      :: integer(),
                nb_users      :: integer(),
                direction     :: integer(),
                state         :: status(),
                state_to_use  :: extended_status(),
                nb_ticks_opened :: integer(),
                algo          :: term(),
                floor_events,
                debug         :: boolean(),
                ouch}).

-record(floor_event, {what,
                      idle    :: (undefined | integer()),
                      nb_users :: integer(),
                      nb_users_last_tick :: integer()}).
