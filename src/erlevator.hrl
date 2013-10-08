-record(state, {floor,
                floor_max,
                floor_min,
                direction,
                state,
                state_to_use,
                algo,
                floor_events}).

-record(floor_event, {what, idle}).
