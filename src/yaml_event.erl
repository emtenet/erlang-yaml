%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_event).

-export([ start/1
        , next/1
        ]).

-export_type([ event/0
             , state/0
             , next/0
             ]).

-include("yaml_event.hrl").

%=======================================================================

-type event() ::
    {stream, yaml:coord()} |
    {end_of_stream, yaml:coord()}.

-opaque state() :: #event{}.

-type next() ::
    {event, event(), state()} |
    {error, term(), state()} |
    end_of_events.

%=======================================================================

-spec start(binary()) -> state().

start(Source) when is_binary(Source) ->
    S = #scan
      { b = Source
      , r = 0
      , c = 0
      },
    #event
        { scan = S
        , next = fun end_of_events/1
        }.

%=======================================================================

-spec next(state()) -> next().

next(E = #event{next = Next}) when is_function(Next, 1) ->
    Next(E).

%=======================================================================

end_of_events(#event{}) ->
    end_of_events.

