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

document(E, Space = {_, in_line, M, _}) ->
    block_start(E, Space, 3 + M);
document(E, Space = {_, indent_line, N, _}) ->
    block_start(E, Space, N).

%=======================================================================

block(E, Space = {_, in_line, M, _}) ->
    {_, N, _} = yaml_event:top(E),
    block_start(E, Space, N + 1 + M);
block(E, Space = {_, indent_line, N, _}) ->
    block_start(E, Space, N).

%-----------------------------------------------------------------------

block_start(E, Space, N) ->
    {E1, Props} = block_start_prepare(E),
    block_detect(E1, Props, Space, N).

%-----------------------------------------------------------------------

block_start_prepare(E) ->
    block_start_prepare_indented(E, 1).

%-----------------------------------------------------------------------

block_start_prepare_not_indented(E) ->
    block_start_prepare_indented(E, 0).

%-----------------------------------------------------------------------

block_start_prepare_indented(E, By) ->
    From = yaml_event:coord(E),
    Indent = yaml_event:indent(E) + By,
    % assume simplest block, update later after detection
    Pushed = yaml_event:push(E, block, Indent, From),
    {Pushed, props_empty(From)}.

%-----------------------------------------------------------------------

block_detect(E, Props, Space, N) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            explicit_key_first(E, Props, N);

        explicit_value ->
            explicit_value_first_missing_key(E, Props, Space, N);

        implicit_key ->
            implicit_key_first(E, Props, N);

        sequence ->
            sequence_first(E, Props, N);

        false ->
            content_continue(E, Props)
    end.

%=======================================================================

content_start(E) ->
    {E1, Props} = block_start_prepare(E),
    content_continue(E1, Props).

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
    {tag, From, Thru, _} = Tag,
    Error = {multiple_tags, From, Thru, {block, Start, Thru}},
    Next = fun (EE) -> after_property(EE, Props#{ tag => Tag }) end,
    yaml_event:error(Error, E, Next).

%-----------------------------------------------------------------------

after_property(E, Props) ->
    {Space, E1} = yaml_space:space(E),
    after_property_space(E1, Props, Space).

%-----------------------------------------------------------------------

after_property_space(E, Props, {_, in_line, _, _}) ->
    content_continue(E, Props);
after_property_space(E, Props, Space = {_, indent_line, N, _}) ->
    Continue = yaml_event:indent_next(E, N),
    after_property_continue(E, Props, Space, Continue);
after_property_space(E, Props, Space = {End, end_of, _, _}) ->
    Next = fun (EE) -> after_flow(EE, Space) end,
    empty(E, Props, End, Next).

%-----------------------------------------------------------------------

after_property_continue(E, Props, Space = {End, _, N, _}, Continue) ->
    case Continue of
        pop ->
            Next = fun (EE) -> after_flow(EE, Space) end,
            empty(E, Props, End, Next);

        {block, _} ->
           after_property_block(E, N, Props) ;

        {block, more_indented, _} ->
            block_detect(E, Props, Space, N);

        _ ->
            throw({E, Props, Space, Continue})
    end.

%-----------------------------------------------------------------------

after_property_block(E, N, Props) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            explicit_key_first(E, Props, N)
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
    after_flow(E).

%=======================================================================

-spec flow_did_end(yaml_event:state()) -> yaml_event:emit().

flow_did_end(E) ->
    after_flow(E).

%=======================================================================

after_flow(E) ->
    {Space, E1} = yaml_space:space(E),
    after_flow(E1, Space).

%-----------------------------------------------------------------------

after_flow(E, Space) ->
    case yaml_event:top(E) of
        {block, _, _} ->
            after_flow(yaml_event:pop(E), Space);

        {Block, _, _} ->
            after_flow(E, Block, Space)
    end.

%-----------------------------------------------------------------------

after_flow(E, implicit_key, {_, in_line, _, _}) ->
    after_implicit_key(E);
after_flow(E, Block, {_, in_line, _, _}) ->
    throw({trailing, Block, E});
after_flow(E, _, Space) ->
    after_block(E, Space).

%=======================================================================

explicit_key_first(E, Props, N) ->
    Next = fun (EE) -> scan_indicator(EE, $?, explicit_key, N) end,
    indicator_first(E, Props, start_of_mapping, Next).

%-----------------------------------------------------------------------

explicit_key_next(E, N) ->
    scan_indicator(E, $?, explicit_key, N).

%=======================================================================

explicit_value_first_missing_key(E, Props, Space, N) ->
    At = yaml_event:coord(E),
    Top = yaml_event:top(E, explicit_value, N, At),
    Next = fun (EE) -> after_block(EE, Space) end,
    indicator_first(Top, Props, start_of_mapping, Next).

%-----------------------------------------------------------------------

explicit_value_next(E, N) ->
    scan_indicator(E, $:, explicit_value, N).

%=======================================================================

implicit_key_first(E, Props, N) ->
    At = yaml_event:coord(E),
    Top = yaml_event:top(E, implicit_key, N, At),
    Next = fun content_start/1,
    indicator_first(Top, Props, start_of_mapping, Next).

%-----------------------------------------------------------------------

implicit_key_next(E, N) ->
    At = yaml_event:coord(E),
    Popped = yaml_event:pop(E),
    Pushed = yaml_event:push(Popped, implicit_key, N, At),
    content_start(Pushed).

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
    Top = yaml_event:top(E, implicit_value, N, At),
    after_implicit_space(Top, Space).

%-----------------------------------------------------------------------

after_implicit_space(E, {_, in_line, _, _}) ->
    case yaml_implicit:detect(E, block) of
        false ->
            content_start(E)
    end;
after_implicit_space(E, Space) ->
    after_indicator(E, Space).

%=======================================================================

sequence_first(E, Props, N) ->
    Next = fun (EE) -> scan_indicator(EE, $-, sequence, N) end,
    indicator_first(E, Props, start_of_sequence, Next).

%-----------------------------------------------------------------------

sequence_next(E, N) ->
    scan_indicator(E, $-, sequence, N).

%=======================================================================

indicator_first(E, Props, Start, Next) ->
    #{ from := From, tag := Tag, anchor := Anchor } = Props,
    Event = {Start, From, Anchor, Tag},
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

scan_indicator(E, Indicator, Block, N) ->
    S = yaml_event:scan(E),
    Indicator = yaml_scan:grapheme(S),
    Scan = yaml_scan:next(S),
    After = yaml_scan:coord(Scan),
    {Space, E1} = yaml_space:space(yaml_event:scan_to(E, Scan)),
    after_indicator(yaml_event:top(E1, Block, N, After), Space).

%-----------------------------------------------------------------------

after_indicator(E, Space = {_, in_line, _, _}) ->
    block(E, Space);
after_indicator(E, Space = {_, indent_line, N, _}) ->
    Indent = yaml_event:indent_after_indicator(E, N),
    after_indicator_indent(E, Space, Indent);
after_indicator(E, Space = {End, end_of, _, _}) ->
    Next = fun (EE) -> after_block(EE, Space) end,
    empty(E, End, Next).

%-----------------------------------------------------------------------

after_indicator_indent(E, Space = {End, _, _, _}, Indent) ->
    case Indent of
        {more_indented, _} ->
            block(E, Space);

        {Block, not_indented, N} ->
            case yaml_implicit:detect(E, block) of
                sequence when Block =/= sequence ->
                    {E1, Props} = block_start_prepare_not_indented(E),
                    sequence_first(E1, Props, N);

                _ ->
                    Next = fun (EE) -> after_block(EE, Space) end,
                    empty(E, End, Next)
            end
    end.

%=======================================================================

after_block(E, Space) ->
    case yaml_event:top(E) of
        {document, _, _} ->
            yaml_document:block_did_end(E, Space);

        _ ->
            after_block_space(E, Space)
    end.

%-----------------------------------------------------------------------

after_block_space(E, Space = {_, end_of, _, _}) ->
    after_block_pop(E, Space);
after_block_space(E, Space = {_, indent_line, N, _}) ->
    after_block_continue(E, Space, yaml_event:indent_next(E, N)).

%-----------------------------------------------------------------------

after_block_continue(E, Space, Continue) ->
    case Continue of
        pop ->
            after_block_pop(E, Space);

        {explicit_key, N} ->
            after_block_explicit_key(E, Space, N);

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

after_block_explicit_key(E, {End, _, _, _}, N) ->
    case yaml_implicit:detect(E, block) of
        explicit_key ->
            Next = fun (EE) -> explicit_key_next(EE, N) end,
            empty(E, End, Next);

        explicit_value ->
            explicit_value_next(E, N)
    end.

%-----------------------------------------------------------------------

after_block_mapping(E, N) ->
    case yaml_implicit:detect(E, block) of
        explicit_value ->
            At = yaml_event:coord(E),
            Next = fun (EE) -> explicit_value_next(EE, N) end,
            empty(E, At, Next);

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

after_block_pop(E, Space = {End, _, _, _}) ->
    case yaml_event:top(E) of
        {explicit_key, _, _} ->
            Next = fun (EE) ->
                after_block_pop_end_of(EE, Space, end_of_mapping)
            end,
            empty(E, End, Next);

        {explicit_value, _, _} ->
            after_block_pop_end_of(E, Space, end_of_mapping);

        {implicit_value, _, _} ->
            after_block_pop_end_of(E, Space, end_of_mapping);

        {sequence, _, _} ->
            after_block_pop_end_of(E, Space, end_of_sequence)
    end.

%-----------------------------------------------------------------------

after_block_pop_end_of(E, Space = {End, _, _, _}, EndOf) ->
    Next = fun (EE) -> after_block(EE, Space) end,
    Event = {EndOf, End},
    yaml_event:emit(Event, yaml_event:pop(E), Next).

