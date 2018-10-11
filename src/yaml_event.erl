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
    {start_of_stream, yaml:coord()} |
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
      , r = 1
      , c = 1
      },
    #event
        { scan = S
        , next = fun start_of_stream/1
        }.

%=======================================================================

-spec next(state()) -> next().

next(E = #event{next = Next}) when is_function(Next, 1) ->
    Next(E).

%=======================================================================

end_of_events(#event{}) ->
    end_of_events.

%=======================================================================

start_of_stream(E = #event{scan = S}) ->
    Event = {start_of_stream, ?COORD(S)},
    Next = fun end_of_stream/1,
    {event, Event, E#event{next = Next}}.

%=======================================================================

end_of_stream(E = #event{scan = S}) ->
    Event = {end_of_stream, ?COORD(S)},
    Next = fun end_of_events/1,
    {event, Event, E#event{next = Next}}.

