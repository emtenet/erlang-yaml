%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_flow).

-export([ mapping/2
        , sequence/2
        ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec mapping(yaml_event:state(), yaml:props()) -> yaml_event:emit().

mapping(E, #{ from := From, anchor := Anchor, tag := Tag }) ->
    Event = {start_of_mapping, From, Anchor, Tag},
    Next = fun (EE) -> collection(EE, ${, mapping, From) end,
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

end_of_mapping(E, #{ anchor := no_anchor, tag := no_tag }, At) ->
    end_of_mapping(E, At).

%-----------------------------------------------------------------------

end_of_mapping(E, At) ->
    case yaml_event:top(E) of
        {explicit, flow, From} ->
            Next = fun (EE) -> end_of_mapping(EE, At) end,
            Top = yaml_event:top(E, value, flow, From),
            empty_after_indicator(Top, From, Next);

        {key, flow, _} ->
            end_of_mapping(yaml_event:pop(E), At);

        {value, flow, From} ->
            Next = fun (EE) -> end_of_mapping(EE, At) end,
            empty_after_indicator(yaml_event:pop(E), From, Next);

        {mapping, flow, _} ->
            Event = {end_of_mapping, yaml_event:coord(E)},
            Next = fun (EE) -> end_of_collection(EE) end,
            yaml_event:emit(Event, yaml_event:pop(E), Next);

        T ->
            throw({E, At, T})
    end.

%=======================================================================

-spec sequence(yaml_event:state(), yaml:props()) -> yaml_event:emit().

sequence(E, #{ from := From, anchor := Anchor, tag := Tag }) ->
    Event = {start_of_sequence, From, Anchor, Tag},
    Next = fun (EE) -> collection(EE, $[, sequence, From) end,
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

end_of_sequence(E, #{ anchor := no_anchor, tag := no_tag }, At) ->
    end_of_sequence(E, At).

%-----------------------------------------------------------------------

end_of_sequence(E, At) ->
    case yaml_event:top(E) of
        {sequence, flow, _} ->
            Event = {end_of_sequence, yaml_event:coord(E)},
            Next = fun (EE) -> end_of_collection(EE) end,
            yaml_event:emit(Event, yaml_event:pop(E), Next);

        T ->
            throw({E, At, T})
    end.

%=======================================================================

collection(E, Indicator, Flow, At) ->
    S = yaml_event:scan(E),
    Indicator = yaml_scan:grapheme(S),
    Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
    Pushed = yaml_event:push(Scanned, Flow, flow, At),
    entry(Pushed, Flow).

%-----------------------------------------------------------------------

end_of_collection(E) ->
    case yaml_event:top(E) of
        {pair_key, flow, _} ->
            end_of_content(E);

        {sequence, flow, _} ->
            end_of_content(E);

        {_, N, _} when N =/= flow ->
            yaml_block:flow_did_end(E);

        T ->
            throw({E, T})
    end.

%=======================================================================

-spec props_empty(yaml:coord()) -> yaml:props().

props_empty(From) ->
    #{ from => From, anchor => no_anchor, tag => no_tag }.

%=======================================================================

space_in_flow(E) ->
    case yaml_space:space(E) of
        {{_, in_line, _, _}, E1} ->
            {flow, E1};

        {Space = {_, indent_line, _, _}, E1} ->
            case yaml_event:is_indented(E) of
                true ->
                    {flow, E1};

                false ->
                    {Space, E1}
            end;

        {Space, E1} ->
            {Space, E1}
    end.

%=======================================================================

entry(E, Flow) ->
    case space_in_flow(E) of
        {flow, E1} ->
            entry_detect(E1, Flow);

        {Space, E1} ->
            throw({E1, Space})
    end.

%-----------------------------------------------------------------------

entry_detect(E, mapping) ->
    case yaml_implicit:detect(E, flow) of
        explicit_key ->
            explicit_key(E);

        explicit_value ->
            explicit_missing_key(E);

        implicit_key ->
            implicit_key(E);

        false ->
            implicit_key(E);

        D ->
            throw({E, D})
    end;
entry_detect(E, sequence) ->
    case yaml_implicit:detect(E, flow) of
        explicit_key ->
            explicit_pair(E);

        explicit_value ->
            explicit_pair_missing_key(E);

        implicit_key ->
            implicit_pair(E);

        false ->
            entry_with_empty_props(E, yaml_event:coord(E));

        D ->
            throw({E, D})
    end;
entry_detect(E, _) ->
    entry_with_empty_props(E, yaml_event:coord(E)).

%-----------------------------------------------------------------------

entry_with_empty_props(E, At) ->
    entry_with_props(E, props_empty(At)).

%-----------------------------------------------------------------------

entry_with_props(E, Props) ->
    S = yaml_event:scan(E),
    case yaml_scan:grapheme(S) of
        $& ->
            property_anchor(E, Props);

        $! ->
            property_tag(E, Props);

        $* ->
            alias(E, Props);

        $' ->
            scalar(yaml_single:scalar(E, flow, Props));

        $\" ->
            scalar(yaml_double:scalar(E, flow, Props));

        ${ ->
            mapping(E, Props);

        $} ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            end_of_mapping(Scanned, Props, yaml_scan:coord(S));

        $[ ->
            sequence(E, Props);

        $] ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            end_of_sequence(Scanned, Props, yaml_scan:coord(S));

        $, ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            comma(Scanned, Props, yaml_scan:coord(S));

        G when ?IS_PLAIN_CHECK_INDICATOR(G) ->
            scalar(yaml_plain:scalar(E, flow, Props));

        G when ?IS_INDICATOR(G) ->
            throw({E, Props, G});

        G when ?IS_PRINTABLE(G) ->
            scalar(yaml_plain:scalar(E, flow, Props));

        G ->
            throw({E, Props, G})
    end.

%=======================================================================

property_anchor(E, Props) ->
    {Anchor, Errors, E1} = yaml_anchor:property(E),
    property_anchor(E1, Errors, Anchor, Props).

%-----------------------------------------------------------------------

property_anchor(E, [Error | Errors], Anchor, Props) ->
    Next = fun (EE) -> property_anchor(EE, Errors, Anchor, Props) end,
    yaml_event:error(Error, E, Next);
property_anchor(E, [], Anchor, Props = #{ anchor := no_anchor }) ->
    after_property(E, Props#{ anchor => Anchor });
property_anchor(E, [], Anchor, Props = #{ from := Start }) ->
    {anchor, From, Thru, _} = Anchor,
    Error = {multiple_anchors, From, Thru, {flow, Start, Thru}},
    Next = fun (EE) -> after_property(EE, Props#{ anchor => Anchor }) end,
    yaml_event:error(Error, E, Next).

%-----------------------------------------------------------------------

property_tag(E, Props) ->
    {Tag, Errors, E1} = yaml_tag:property(E),
    property_tag(E1, Errors, Tag, Props).

%-----------------------------------------------------------------------

property_tag(E, [Error | Errors], Tag, Props) ->
    Next = fun (EE) -> property_tag(EE, Errors, Tag, Props) end,
    yaml_event:error(Error, E, Next);
property_tag(E, [], Tag, Props = #{ tag := no_tag }) ->
    after_property(E, Props#{ tag => Tag });
property_tag(E, [], Tag, Props = #{ from := Start }) ->
    {tag, From, Thru, _, _} = Tag,
    Error = {multiple_tags, From, Thru, {flow, Start, Thru}},
    Next = fun (EE) -> after_property(EE, Props#{ tag => Tag }) end,
    yaml_event:error(Error, E, Next).

%-----------------------------------------------------------------------

after_property(E, Props) ->
    {Space, E1} = yaml_space:space(E),
    after_property_space(E1, Props, Space).

%-----------------------------------------------------------------------

after_property_space(E, Props, {End, in_line, _, _}) ->
    case yaml_implicit:detect(E, flow) of
        explicit_value ->
            Next = fun after_content/1,
            empty(E, Props, End, Next);

        false ->
            entry_with_props(E, Props)
    end.

%=======================================================================

empty(E, At = {_, _}, Next) ->
    Event = {empty, At, At, no_anchor, no_tag},
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

empty(E, #{ from := From, anchor := Anchor, tag := Tag }, Thru, Next) ->
    Event = {empty, From, Thru, Anchor, Tag},
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

empty_after_indicator(E, {R, C}, Next) ->
    At = {R, C + 1},
    Event = {empty, At, At, no_anchor, no_tag},
    yaml_event:emit(Event, E, Next).

%=======================================================================

alias(E, #{ anchor := no_anchor, tag := no_tag }) ->
    scalar(yaml_anchor:alias(E)).

%-----------------------------------------------------------------------

scalar({Scalar, Errors, E}) ->
    scalar_emit(E, Errors, Scalar).

%-----------------------------------------------------------------------

scalar_emit(E, [Error | Errors], Scalar) ->
    Next = fun (EE) -> scalar_emit(EE, Errors, Scalar) end,
    yaml_event:error(Error, E, Next);
scalar_emit(E, [], Scalar) ->
    Next = fun end_of_content/1,
    yaml_event:emit(Scalar, E, Next).

%=======================================================================

end_of_content(E) ->
    case yaml_event:top(E) of
        {explicit, flow, At} ->
            space_after_content(yaml_event:top(E, key, flow, At));

        {key, flow, _} ->
            space_after_content(E);

        {pair_explicit, flow, At} ->
            space_after_content(yaml_event:top(E, pair_key, flow, At));

        {pair_key, flow, _} ->
            space_after_content(E);

        {pair_value, flow, _} ->
            At = yaml_event:coord(E),
            Event = {end_of_mapping, At},
            Next = fun space_after_content/1,
            yaml_event:emit(Event, yaml_event:pop(E), Next);

        {sequence, flow, _} ->
            space_after_content(E);

        {value, flow, _} ->
            space_after_content(yaml_event:pop(E))
    end.

%-----------------------------------------------------------------------

space_after_content(E) ->
    case space_in_flow(E) of
        {flow, E1} ->
            after_content(E1);

        {Space, E1} ->
            throw({E1, Space})
    end.

%-----------------------------------------------------------------------

after_content(E) ->
    S = yaml_event:scan(E),
    case yaml_scan:grapheme(S) of
        $: ->
            % no need to check for following non-plain character
            % yaml_plain:scalar() would alrady have checked and a $:
            % following a JSON like key can be immediately adjacent to
            % a plain value
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            colon_after_content(Scanned, yaml_scan:coord(S));

        $, ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            comma_after_content(Scanned, yaml_scan:coord(S));

        $} ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            end_of_mapping(Scanned, yaml_scan:coord(S));

        $] ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            end_of_sequence(Scanned, yaml_scan:coord(S));

        _ ->
            throw(E)
    end.

%=======================================================================

colon_after_content(E, At) ->
    case yaml_event:top(E) of
        {key, flow, _} ->
            entry(yaml_event:top(E, value, flow, At), value);

        {pair_key, flow, _} ->
            entry(yaml_event:top(E, pair_value, flow, At), value);

        {sequence, flow, From} ->
            Next = fun (EE) -> comma_after_content(EE, At) end,
            Error = {bad_implicit_key, At, At, {sequence, From, At}},
            yaml_event:error(Error, E, Next);

        T ->
            throw({E, At, T})
    end.

%=======================================================================

comma(E, Props, Thru) ->
    case yaml_event:top(E) of
        {value, flow, _} ->
            comma_missing_value(E, Props, Thru);

        T ->
            throw({E, Props, Thru, T})
    end.

%-----------------------------------------------------------------------

comma_after_content(E, At) ->
    case yaml_event:top(E) of
        {key, flow, _} ->
            comma_missing_value(E, At);

        {mapping, flow, _} ->
            entry(E, mapping);

        {sequence, flow, _} ->
            entry(E, sequence);

        T ->
            throw({E, At, T})
    end.

%-----------------------------------------------------------------------

comma_missing_value(E, At) ->
    Next = fun (EE) -> entry(EE, mapping) end,
    empty(yaml_event:pop(E), At, Next).

%-----------------------------------------------------------------------

comma_missing_value(E, Props, Thru) ->
    Next = fun (EE) -> entry(EE, mapping) end,
    empty(yaml_event:pop(E), Props, Thru, Next).

%=======================================================================

explicit_key(E) ->
    S = yaml_event:scan(E),
    At = yaml_scan:coord(S),
    $? = yaml_scan:grapheme(S),
    Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
    Pushed = yaml_event:push(Scanned, explicit, flow, At),
    entry(Pushed, explicit).

%=======================================================================

explicit_pair(E) ->
    S = yaml_event:scan(E),
    At = yaml_scan:coord(S),
    $? = yaml_scan:grapheme(S),
    Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
    Pushed = yaml_event:push(Scanned, pair_explicit, flow, At),
    Event = {start_of_mapping, At, no_anchor, no_tag},
    Next = fun (EE) -> entry(EE, explicit) end,
    yaml_event:emit(Event, Pushed, Next).

%=======================================================================

explicit_missing_key(E) ->
    explicit_missing_key(E, value).

%-----------------------------------------------------------------------

explicit_missing_key(E, Flow) ->
    S = yaml_event:scan(E),
    At = yaml_scan:coord(S),
    $: = yaml_scan:grapheme(S),
    Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
    Pushed = yaml_event:push(Scanned, Flow, flow, At),
    Next = fun (EE) -> entry(EE, Flow) end,
    empty(Pushed, At, Next).

%=======================================================================

explicit_pair_missing_key(E) ->
    At = yaml_event:coord(E),
    Event = {start_of_mapping, At, no_anchor, no_tag},
    Next = fun (EE) -> explicit_missing_key(EE, pair_value) end,
    yaml_event:emit(Event, E, Next).

%=======================================================================

implicit_key(E) ->
    At = yaml_event:coord(E),
    Pushed = yaml_event:push(E, key, flow, At),
    entry_with_empty_props(Pushed, At).

%=======================================================================

implicit_pair(E) ->
    At = yaml_event:coord(E),
    Pushed = yaml_event:push(E, pair_key, flow, At),
    Event = {start_of_mapping, At, no_anchor, no_tag},
    Next = fun (EE) -> entry_with_empty_props(EE, At) end,
    yaml_event:emit(Event, Pushed, Next).

