%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_block).

-export([ document/2
        , flow_did_end/1
        ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec document(yaml_event:state(), yaml_space:space()) -> yaml_event:emit().

document(E, {in_line, N, _}) ->
    detect_block(E, 3 + N);
document(E, {indent_line, N, _}) ->
    detect_block(E, N).

%=======================================================================

detect_block(E, N) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            explicit_key_first(E, N);

        implicit_key ->
            implicit_key_first(E, N);

        sequence ->
            sequence_first(E, N);

        false ->
            content_block(E)
    end.

%-----------------------------------------------------------------------

detect_compact_block(E, M) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            {_, N, _} = yaml_event:top(E),
            explicit_key_first(E, N + 1 + M);

        implicit_key ->
            {_, N, _} = yaml_event:top(E),
            implicit_key_first(E, N + 1 + M);

        sequence ->
            {_, N, _} = yaml_event:top(E),
            sequence_first(E, N + 1 + M);

        false ->
            content_block(E)
    end.

%=======================================================================

content_block(E) ->
    N = yaml_event:indent(E),
    At = yaml_event:coord(E),
    Pushed = yaml_event:push(E, block, N + 1, At),
    content_start(Pushed, At).

%-----------------------------------------------------------------------

content_start(E, At) ->
    content_continue(E, props_empty(At)).

%-----------------------------------------------------------------------

content_continue(E, Props) ->
    case yaml_event:grapheme(E) of
        $& ->
            property_anchor(E, Props);

        $! ->
            property_tag(E, Props);

        $* ->
            alias(E, Props);

        $' ->
            single(E, Props);

        $\" ->
            double(E, Props);

        $| ->
            literal(E, literal, Props);

        $> ->
            literal(E, folded, Props);

        ${ ->
            yaml_flow:mapping(E, Props);

        $[ ->
            yaml_flow:sequence(E, Props);

        G when ?IS_PRINTABLE(G) ->
            plain(E, Props)
    end.

%=======================================================================

after_content(E) ->
    {Space, E1} = yaml_space:space(E),
    after_content(E1, Space).

%-----------------------------------------------------------------------

after_content(E, {in_line, _, _}) ->
    case yaml_event:top(E) of
        {implicit_key, _, _} ->
            after_implicit_key(E);

        _ ->
            throw({not_implemented, space_trailing})
    end;
after_content(E, Space) ->
    after_block(E, Space).

%=======================================================================

-spec flow_did_end(yaml_event:state()) -> yaml_event:emit().

flow_did_end(E) ->
    after_content(E).

%=======================================================================

alias(E, #{ anchor := no_anchor, tag := no_tag }) ->
    scalar(yaml_anchor:alias(E)).

%-----------------------------------------------------------------------

plain(E, Props) ->
    scalar(yaml_plain:scalar(E, block, Props)).

%-----------------------------------------------------------------------

single(E, Props) ->
    scalar(yaml_single:scalar(E, block, Props)).

%-----------------------------------------------------------------------

double(E, Props) ->
    scalar(yaml_double:scalar(E, block, Props)).

%-----------------------------------------------------------------------

literal(E, Style, Props) ->
    scalar(yaml_literal:block(E, Style, Props)).

%-----------------------------------------------------------------------

scalar({Scalar, Errors, E}) ->
    Next = fun (EE) -> after_scalar(EE, Errors) end,
    yaml_event:emit(Scalar, E, Next).

%-----------------------------------------------------------------------

after_scalar(E, [Error | Errors]) ->
    Next = fun (EE) -> after_scalar(EE, Errors) end,
    yaml_event:error(Error, E, Next);
after_scalar(E, []) ->
    after_content(E).

%=======================================================================

-spec props_empty(yaml:coord()) -> yaml:props().

props_empty(From) ->
    #{ from => From, anchor => no_anchor, tag => no_tag }.

%-----------------------------------------------------------------------

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
    Error = {multiple_anchors, From, Thru, {block, Start, Thru}},
    Next = fun (EE) -> after_property(EE, Props#{ anchor => Anchor }) end,
    yaml_token:error(Error, E, Next).

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
    {tag, From, Thru, _} = Tag,
    Error = {multiple_tags, From, Thru, {block, Start, Thru}},
    Next = fun (EE) -> after_property(EE, Props#{ tag => Tag }) end,
    yaml_token:error(Error, E, Next).

%-----------------------------------------------------------------------

after_property(E, Props) ->
    {Space, E1} = yaml_space:space(E),
    after_property_space(E1, Space, Props).

%-----------------------------------------------------------------------

after_property_space(E, {in_line, _, _}, Props) ->
    content_continue(E, Props);
after_property_space(E, {indent_line, N, _}, Props) ->
    after_property_continue(E, yaml_event:indent_next(E, N), Props).

%-----------------------------------------------------------------------

after_property_continue(E, Continue, Props) ->
    case Continue of
        {block, N} ->
           after_property_block(E, N, Props) ;

        {implicit_key, more_indented, _} ->
            throw({E, Continue, Props})
    end.

%-----------------------------------------------------------------------

after_property_block(E, N, Props) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            explicit_key_first(yaml_event:pop(E), N, Props)
    end.

%=======================================================================

after_block(E, Space) ->
    case yaml_event:top(E) of
        {document, _, _} ->
            yaml_document:block_did_end(E, Space);

        {block, _, _} ->
            after_block(yaml_event:pop(E), Space);

        _ ->
            after_block_space(E, Space)
    end.

%-----------------------------------------------------------------------

after_block_space(E, Space = {end_of, _, _}) ->
    after_block_pop(E, Space);
after_block_space(E, Space = {indent_line, N, _}) ->
    after_block_continue(E, Space, yaml_event:indent_next(E, N)).

%-----------------------------------------------------------------------

after_block_continue(E, Space, Continue) ->
    case Continue of
        pop ->
            after_block_pop(E, Space);

        {explicit_key, N} ->
            after_block_explicit_key(E, N);

        {explicit_value, N} ->
            after_block_mapping(E, N);

        {implicit_value, N} ->
            after_block_mapping(E, N);

        {sequence, N} ->
            after_block_sequence(E, N);

        {sequence, not_indented, N} ->
            after_block_sequence_or_pop(E, Space, N)
    end.

%-----------------------------------------------------------------------

after_block_explicit_key(E, N) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            Next = fun (EE) -> explicit_key_next(EE, N) end,
            explicit_value_empty(E, Next);

        explicit_value ->
            explicit_value_next(E, N)
    end.

%-----------------------------------------------------------------------

after_block_mapping(E, N) ->
    case yaml_implicit:detect(E, block) of
        explicit_value ->
            Next = fun (EE) -> after_block_explicit_key(EE, N) end,
            explicit_key_empty(E, N, Next);

        implicit_key ->
            implicit_key_next(E, N)
    end.

%-----------------------------------------------------------------------

after_block_sequence(E, N) ->
    case yaml_implicit:detect(E, block) of
        sequence ->
            sequence_next(E, N)
    end.

%-----------------------------------------------------------------------

after_block_sequence_or_pop(E, Space, N) ->
    case yaml_implicit:detect(E, block) of
        sequence ->
            sequence_next(E, N);

        _ ->
            after_block_pop(E, Space)
    end.

%-----------------------------------------------------------------------

after_block_pop(E, Space) ->
    case yaml_event:top(E) of
        {explicit_key, _, _} ->
            Next = fun (EE) ->
                after_block_pop_end_of(EE, Space, end_of_mapping)
            end,
            explicit_value_empty(E, Next);

        {explicit_value, _, _} ->
            after_block_pop_end_of(E, Space, end_of_mapping);

        {implicit_value, _, _} ->
            after_block_pop_end_of(E, Space, end_of_mapping);

        {sequence, _, _} ->
            after_block_pop_end_of(E, Space, end_of_sequence)
    end.

%-----------------------------------------------------------------------

after_block_pop_end_of(E, Space, EndOf) ->
    Next = fun (EE) -> after_block(EE, Space) end,
    At = yaml_event:coord_row(E),
    Event = {EndOf, At},
    yaml_event:emit(Event, yaml_event:pop(E), Next).

%=======================================================================

push_indicator(E, Indicator, Block, N) ->
    S = yaml_event:scan(E),
    Indicator = yaml_scan:grapheme(S),
    After = yaml_scan:next(S),
    At = yaml_scan:coord(After),
    {Space, E1} = yaml_space:space(yaml_event:scan_to(E, After)),
    after_indicator(yaml_event:push(E1, Block, N, At), Space).

%-----------------------------------------------------------------------

after_indicator(E, {in_line, M, _}) ->
    detect_compact_block(E, M);
after_indicator(E, Space = {indent_line, N, _}) ->
    after_indicator_indent(E, Space, yaml_event:indent_after_indicator(E, N)).

%-----------------------------------------------------------------------

after_indicator_indent(E, _Space, Indent) ->
    case Indent of
        {more_indented, N} ->
            detect_block(E, N);

        {explicit_value, not_indented, N} ->
            explicit_value_not_indented(E, N);

        {implicit_value, not_indented, N} ->
            implicit_value_not_indented(E, N);

        {sequence, not_indented, N} ->
            sequence_value_not_indented(E, N)
    end.

%=======================================================================

explicit_key_empty(E, N, Next) ->
    At = yaml_event:coord(E),
    Event = {empty, At, At, no_anchor, no_tag},
    Popped = yaml_event:pop(E),
    Pushed = yaml_event:push(Popped, explicit_key, N, At),
    yaml_event:emit(Event, Pushed, Next).

%-----------------------------------------------------------------------

explicit_key_first(E, N) ->
    At = yaml_event:coord(E),
    Event = {start_of_mapping, At, no_anchor, no_tag},
    Next = fun (EE) -> push_indicator(EE, $?, explicit_key, N) end,
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

explicit_key_first(E, N, #{ from := From, tag := Tag, anchor := Anchor }) ->
    Event = {start_of_mapping, From, Anchor, Tag},
    Next = fun (EE) -> push_indicator(EE, $?, explicit_key, N) end,
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

explicit_key_next(E, N) ->
    push_indicator(yaml_event:pop(E), $?, explicit_key, N).

%=======================================================================

explicit_value_empty(E, Next) ->
    case yaml_event:top(E) of
        {explicit_key, _, _} ->
            At = yaml_event:coord_row(E),
            Event = {empty, At, At, no_anchor, no_tag},
            yaml_event:emit(Event, E, Next);

        {explicit_value, _, At} ->
            Event = {empty, At, At, no_anchor, no_tag},
            yaml_event:emit(Event, E, Next)
    end.

%-----------------------------------------------------------------------

explicit_value_next(E, N) ->
    push_indicator(yaml_event:pop(E), $:, explicit_value, N).

%-----------------------------------------------------------------------

explicit_value_not_indented(E, N) ->
    case yaml_implicit:detect(E, block) of
        implicit_key ->
            Next = fun (EE) -> implicit_key_next(EE, N) end,
            explicit_value_empty(E, Next);

        sequence ->
            sequence_first(E, N)
    end.

%=======================================================================

implicit_key_first(E, N) ->
    At = yaml_event:coord(E),
    Event = {start_of_mapping, At, no_anchor, no_tag},
    Next = fun (EE) -> content_start(EE, At) end,
    Pushed = yaml_event:push(E, implicit_key, N, At),
    yaml_event:emit(Event, Pushed, Next).

%-----------------------------------------------------------------------

implicit_key_next(E, N) ->
    At = yaml_event:coord(E),
    Popped = yaml_event:pop(E),
    Pushed = yaml_event:push(Popped, implicit_key, N, At),
    content_start(Pushed, At).

%=======================================================================

after_implicit_key(E) ->
    S = yaml_event:scan(E),
    case yaml_scan:grapheme(S) of
        $: ->
            Z = yaml_scan:next(S),
            {Space, E1} = yaml_space:space(yaml_event:scan_to(E, Z)),
            after_implicit_indicator(E1, Space);

        _ ->
            throw({not_implemented, expecting_implicit_colon})
    end.

%-----------------------------------------------------------------------

after_implicit_indicator(E, Space) ->
    At = yaml_event:coord(E),
    {implicit_key, N, _} = yaml_event:top(E),
    Pushed = yaml_event:push(yaml_event:pop(E), implicit_value, N, At),
    after_implicit_space(Pushed, Space).

%-----------------------------------------------------------------------

after_implicit_space(E, {in_line, _, _}) ->
    case yaml_implicit:detect(E, block) of
        false ->
            content_block(E)
    end;
after_implicit_space(E, Space) ->
    after_indicator(E, Space).

%=======================================================================

implicit_value_not_indented(E, N) ->
    case yaml_implicit:detect(E, block) of
        sequence ->
            sequence_first(E, N)
    end.

%=======================================================================

sequence_first(E, N) ->
    At = yaml_event:coord(E),
    Event = {start_of_sequence, At, no_anchor, no_tag},
    Next = fun (EE) -> push_indicator(EE, $-, sequence, N) end,
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

sequence_next(E, N) ->
    push_indicator(yaml_event:pop(E), $-, sequence, N).

%-----------------------------------------------------------------------

sequence_value_not_indented(E, N) ->
    case yaml_implicit:detect(E, block) of
        sequence ->
            {sequence, _, At} = yaml_event:top(E),
            Event = {empty, At, At, no_anchor, no_tag},
            Next = fun (EE) -> sequence_next(EE, N) end,
            yaml_event:emit(Event, E, Next)
    end.

