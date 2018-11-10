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

entry(E, Flow) ->
    case yaml_space:space(E) of
        {{in_line, _, _}, E1} ->
            entry_detect(E1, Flow);

        {Space = {indent_line, _, _}, E1} ->
            case yaml_event:is_indented(E) of
                true ->
                    entry_detect(E1, Flow);

                false ->
                    throw({E1, Space})
            end;

        {Space, E1} ->
            throw({E1, Space})
    end.

%-----------------------------------------------------------------------

entry_detect(E, mapping) ->
    case yaml_implicit:detect(E, flow) of
        explicit_key ->
            explicit_key(E);

        explicit_value ->
            explicit_value_missing_key(E);

        implicit_key ->
            implicit_key(E);

        false ->
            entry_with_empty_props(E, yaml_event:coord(E));

        D ->
            throw({E, D})
    end;
entry_detect(E, sequence) ->
    case yaml_implicit:detect(E, flow) of
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
        $' ->
            scalar(yaml_single:scalar(E, flow, Props));

        $\" ->
            scalar(yaml_double:scalar(E, flow, Props));

        $} ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            end_of_mapping(Scanned, Props, yaml_scan:coord(S));

        $] ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            end_of_sequence(Scanned, Props, yaml_scan:coord(S));

        $, ->
            Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
            comma(Scanned, Props, yaml_scan:coord(S));

        G when ?IS_INDICATOR(G) ->
            throw({E, Props, G});

        G when ?IS_PRINTABLE(G) ->
            scalar(yaml_plain:scalar(E, flow, Props));

        G ->
            throw({E, Props, G})
    end.

%=======================================================================

empty(E, At = {_, _}, Next) ->
    Event = {empty, At, At, no_anchor, no_tag},
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

empty(E, #{ from := From, anchor := Anchor, tag := Tag }, Thru, Next) ->
    Event = {empty, From, Thru, Anchor, Tag},
    yaml_event:emit(Event, E, Next).

%=======================================================================

scalar({Scalar, Errors, E}) ->
    Next = fun (EE) -> end_of_scaler(EE, Errors) end,
    yaml_event:emit(Scalar, E, Next).

%-----------------------------------------------------------------------

end_of_scaler(E, [Error | Errors]) ->
    Next = fun (EE) -> end_of_scaler(EE, Errors) end,
    yaml_event:error(Error, E, Next);
end_of_scaler(E, []) ->
    end_of_content(E).

%=======================================================================

end_of_content(E) ->
    case yaml_event:top(E) of
        {explicit, flow, At} ->
            space_after_content(yaml_event:top(E, key, flow, At));

        {key, flow, _} ->
            space_after_content(E);

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
    case yaml_space:space(E) of
        {{in_line, _, _}, E1} ->
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

        T ->
            throw({E, At, T})
    end.

%=======================================================================

comma(E, Props, Thru) ->
    {value, flow, _} = yaml_event:top(E),
    Next = fun (EE) -> entry(EE, mapping) end,
    empty(yaml_event:pop(E), Props, Thru, Next).

%-----------------------------------------------------------------------

comma_after_content(E, At) ->
    case yaml_event:top(E) of
        {mapping, flow, _} ->
            entry(E, mapping);

        {sequence, flow, _} ->
            entry(E, sequence);

        T ->
            throw({E, At, T})
    end.

%=======================================================================

explicit_key(E) ->
    S = yaml_event:scan(E),
    At = yaml_scan:coord(S),
    $? = yaml_scan:grapheme(S),
    Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
    Pushed = yaml_event:push(Scanned, explicit, flow, At),
    entry(Pushed, explicit).

%=======================================================================

explicit_value_missing_key(E) ->
    S = yaml_event:scan(E),
    At = yaml_scan:coord(S),
    $: = yaml_scan:grapheme(S),
    Scanned = yaml_event:scan_to(E, yaml_scan:next(S)),
    Pushed = yaml_event:push(Scanned, value, flow, At),
    Next = fun (EE) -> entry(EE, mapping) end,
    empty(Pushed, At, Next).

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

