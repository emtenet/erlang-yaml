%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_event).

-export([ start/1
        , next/1
        ]).

-export_type([ event/0
             , state/0
             , emit/0
             , next/0
             ]).

-export([ stream_should_end/1
        ]).

-export([ coord/1
        , grapheme/1
        , indent/1
        , indent_after_indicator/2
        , indent_plus/2
        , indent_next/2
        , is_indented/2
        , scan/1
        , scan_to/2
        , emit/3
        , error/3
        , top/1
        , push/4
        , pop/1
        ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-export([ mock/1 ]).
-endif.

%=======================================================================

-record(event,
        { scan :: yaml_scan:state()
        , next :: next()
        , indent :: integer()
        , stack :: [{atom(), integer(), yaml:coord()}]
        }).

-type event() ::
    {start_of_stream, yaml:coord()} |
    {end_of_stream, yaml:coord()} |
    {start_of_document, yaml:coord()} |
    {end_of_document, yaml:coord()} |
    {start_of_sequence, yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag()} |
    {end_of_sequence, yaml:coord()} |
    {start_of_mapping, yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag()} |
    {end_of_mapping, yaml:coord()} |
    {plain, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {single, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {double, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {empty, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()}.

-opaque state() :: #event{}.

-type emit() ::
    {event, event(), state()} |
    {error, term(), state()} |
    end_of_events.

-type next() :: fun((state()) -> emit()).

%=======================================================================

-spec start(binary()) -> state().

start(Source) when is_binary(Source) ->
    #event
        { scan = yaml_scan:start(Source)
        , next = fun start_of_stream/1
        , indent = -1
        , stack = [{stream, -1, {1, 1}}]
        }.

%=======================================================================

-ifdef(TEST).

-spec mock(yaml_scan:state()) -> state().

mock(Scan) ->
    #event
        { scan = Scan
        , next = fun end_of_events/1
        , indent = -1
        , stack = [{stream, -1, {1, 1}}]
        }.

-endif.

%=======================================================================

-spec next(state()) -> emit().

next(E = #event{next = Next}) when is_function(Next, 1) ->
    Next(E).

%=======================================================================

end_of_events(#event{}) ->
    end_of_events.

%=======================================================================

start_of_stream(E = #event{scan = S}) ->
    Event = {start_of_stream, yaml_scan:coord(S)},
    Next = fun yaml_document:document_may_start/1,
    emit(Event, E, Next).

%=======================================================================

-spec stream_should_end(state()) -> emit().

stream_should_end(E = #event{scan = S}) ->
    end_of_stream = yaml_scan:grapheme(S),
    Event = {end_of_stream, yaml_scan:coord(S)},
    Next = fun end_of_events/1,
    emit(Event, E, Next).

%=======================================================================

-spec coord(state()) -> yaml:coord().

coord(#event{scan = Scan}) ->
    yaml_scan:coord(Scan).

%=======================================================================

-spec grapheme(state()) -> yaml_scan:grapheme().

grapheme(#event{scan = Scan}) ->
    yaml_scan:grapheme(Scan).

%=======================================================================

-spec indent(state()) -> integer().

indent(#event{indent = N}) ->
    N.

%=======================================================================

-spec indent_after_indicator(state(), integer()) ->
    {atom(), not_indented, integer()} |
    {more_indented, integer()} |
    {less_indented, integer()}.

indent_after_indicator(E = #event{}, N) ->
    case E#event.stack of
        [{A, I, _} | _] when N =:= I ->
            {A, not_indented, N};

        [{_, I, _} | _] when N > I ->
            {more_indented, N};

        [{_, I, _} | _] when N < I ->
            {less_indented, N}
    end.

%=======================================================================

-spec indent_plus(state(), integer()) -> integer().

indent_plus(#event{indent = N}, M) ->
    % N was already incremented when {content, N, _} was pushed onto stack
    N + M - 1.

%=======================================================================

-spec indent_next(state(), integer()) ->
    pop |
    {atom(), integer()} |
    {atom(), not_indented, integer()} |
    {atom(), less_indented, integer()} |
    {atom(), more_indented, integer()}.

indent_next(E = #event{}, N) ->
    case E#event.stack of
        [{A, I, _}, {_, I, _} | _] when N =:= I ->
            {A, not_indented, I};

        [{A, I, _} | _] when N =:= I ->
            {A, I};

        [_, {_, P, _} | _] when N =< P ->
            pop;

        [{A, I, _} | _] when N < I ->
            {A, less_indented, I};

        [{A, I, _} | _] when N > I ->
            {A, more_indented, I}
    end.

%=======================================================================

-spec is_indented(state(), yaml_scan:state()) -> boolean().

is_indented(#event{indent = I}, Scan) ->
    yaml_scan:is_indented_at_least_by(Scan, I).

%=======================================================================

-spec scan(state()) -> yaml_scan:state().

scan(#event{scan = Scan}) ->
    Scan.

%=======================================================================

-spec scan_to(state(), yaml_scan:state()) -> state().

scan_to(E = #event{}, Scan) ->
    E#event{scan = Scan}.

%=======================================================================

-spec emit(event(), state(), next()) -> emit().

emit(Event, E = #event{}, Next) ->
    {event, Event, E#event{next = Next}}.

%=======================================================================

-spec error(term(), state(), next()) -> emit().

error(Error, E = #event{}, Next) ->
    {error, Error, E#event{next = Next}}.

%=======================================================================

-spec top(state()) -> {atom(), integer(), yaml:coord()}.

top(#event{stack = [Top | _]}) ->
    Top.

%=======================================================================

-spec push(state(), atom(), integer(), yaml:coord()) -> state().

push(E = #event{}, Push, N, At) ->
    Stack = [{Push, N, At} | E#event.stack],
    E#event{indent = N, stack = Stack}.

%=======================================================================

-spec pop(state()) -> state().

pop(E = #event{stack = [_ | Stack = [{_, N, _} | _]]}) ->
    E#event{indent = N, stack = Stack}.

