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
             , stack/0
             ]).

-export([ stream_should_end/1
        ]).

-export([ coord/1
        , coord_row/1
        , grapheme/1
        , indent/1
        , indent_after_indicator/2
        , indent_next/2
        , indent_to_column/2
        , is_indented/1
        , is_indented/2
        , is_indented/3
        , scan/1
        , scan_to/2
        , scan_next/1
        , emit/3
        , error/3
        , top/1
        , top/4
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
        , stack :: [stack()]
        }).

-type event() ::
    {start_of_stream, yaml:coord()} |
    {end_of_stream, yaml:coord()} |
    {start_of_document, yaml:coord()} |
    {end_of_document, yaml:coord()} |
    {yaml_directive, yaml:version(), yaml:coord(), yaml:coord()} |
    {reserved_directive, yaml:coord(), yaml:coord(), binary(), list()} |
    {start_of_sequence, yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag()} |
    {end_of_sequence, yaml:coord()} |
    {start_of_mapping, yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag()} |
    {end_of_mapping, yaml:coord()} |
    {alias, yaml:coord(), yaml:coord(), binary()} |
    {plain, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {single, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {double, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {literal, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {folded, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag(), list()} |
    {empty, yaml:coord(), yaml:coord(), yaml:maybe_anchor(), yaml:maybe_tag()}.

-opaque state() :: #event{}.

-type emit() ::
    {event, event(), state()} |
    {error, term(), state()} |
    end_of_events.

-type next() :: fun((state()) -> emit()).

-type stack() :: {atom(), integer() | flow, yaml:coord()}.

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
    Next = fun yaml_document:stream/1,
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

-spec coord_row(state()) -> yaml:coord().

coord_row(#event{scan = Scan}) ->
    yaml_scan:coord_row(Scan).

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

-spec indent_to_column(state(), pos_integer()) -> pos_integer().

indent_to_column(#event{indent = I}, By) ->
    % A literal's indent is already 1 more than the parent block
    % But the indent (zero based) is one less than column (one based)
    % So they cancel each other out
    I + By.

%=======================================================================

-spec is_indented(state()) -> boolean().

is_indented(#event{indent = I, scan = Scan}) ->
    yaml_scan:is_indented(Scan, I).

%=======================================================================

-spec is_indented(state(), yaml_scan:state()) -> boolean().

is_indented(#event{indent = I}, Scan) ->
    yaml_scan:is_indented(Scan, I).

%=======================================================================

-spec is_indented(state(), yaml_scan:state(), pos_integer()) -> boolean().

is_indented(#event{indent = I}, Scan, By) ->
    % A literal's indent is already 1 more than the parent block
    % So to take that into account
    yaml_scan:is_indented(Scan, I - 1 + By).

%=======================================================================

-spec scan(state()) -> yaml_scan:state().

scan(#event{scan = Scan}) ->
    Scan.

%=======================================================================

-spec scan_to(state(), yaml_scan:state()) -> state().

scan_to(E = #event{}, Scan) ->
    E#event{scan = Scan}.

%=======================================================================

-spec scan_next(state()) -> state().

scan_next(E = #event{scan = Scan}) ->
    E#event{scan = yaml_scan:next(Scan)}.

%=======================================================================

-spec emit(event(), state(), next()) -> emit().

emit(Event, E = #event{}, Next) ->
    {event, Event, E#event{next = Next}}.

%=======================================================================

-spec error(term(), state(), next()) -> emit().

error(Error, E = #event{}, Next) ->
    {error, Error, E#event{next = Next}}.

%=======================================================================

-spec top(state()) -> stack().

top(#event{stack = [Top | _]}) ->
    Top.

%=======================================================================

-spec top(state(), atom(), flow, yaml:coord()) -> state().

top(E = #event{stack = [{_, flow, _} | Pop]}, Push, N = flow, At = {_, _})
        when is_atom(Push) ->
    Stack = [{Push, N, At} | Pop],
    E#event{stack = Stack}.

%=======================================================================

-spec push(state(), atom(), integer() | flow, yaml:coord()) -> state().

push(E = #event{indent = I, stack = [{_, I, _} | _]}, Push, N, At = {_, _})
        when is_atom(Push) andalso
             is_integer(N) andalso
             N >= I ->
    Stack = [{Push, N, At} | E#event.stack],
    E#event{indent = N, stack = Stack};
push(E = #event{}, Push, N = flow, At = {_, _})
        when is_atom(Push) ->
    Stack = [{Push, N, At} | E#event.stack],
    E#event{stack = Stack}.

%=======================================================================

-spec pop(state()) -> state().

pop(E = #event{stack = [_ | Stack = [{_, N, _} | _]]}) ->
    case N of
        flow ->
            E#event{stack = Stack};

        _ ->
            E#event{indent = N, stack = Stack}
    end.

