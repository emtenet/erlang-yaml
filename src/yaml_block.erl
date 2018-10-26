%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_block).

-export([ document/2
        ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec document(yaml_event:state(), yaml_space:space()) -> yaml_event:emit().

document(E, Space = {indent_line, _, _}) ->
    % new block like after a '-', '?', or ':' indicator
    after_indicator(E, Space).

%=======================================================================

content_block(E, Space) ->
    N = yaml_event:indent(E),
    At = yaml_event:coord(E),
    Pushed = yaml_event:push(E, block, N + 1, At),
    content_start(Pushed, Space, At).

%-----------------------------------------------------------------------

content_start(E, Space, At) ->
    content_continue(E, Space, props_empty(At)).

%-----------------------------------------------------------------------

content_continue(E, _Space, Props) ->
    case yaml_event:grapheme(E) of
        G when ?IS_PRINTABLE(G) ->
            plain(E, Props)
    end.

%=======================================================================

after_content(E) ->
    {Space, E1} = yaml_space:space(E),
    after_content(E1, Space).

%-----------------------------------------------------------------------

after_content(E, Space) ->
    after_block(E, Space).

%=======================================================================

plain(E, Props) ->
    scalar(yaml_plain:scalar(E, block, Props)).

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

%=======================================================================

after_block(E, Space) ->
    case yaml_event:top(E) of
        {document, _, _} ->
            yaml_document:document_should_end(E, Space);

        {block, _, _} ->
            after_block(yaml_event:pop(E), Space);

        _ ->
            after_block_space(E, Space)
    end.

%-----------------------------------------------------------------------

after_block_space(E, Space = {end_of, _, _}) ->
    after_block_pop(E, Space);
after_block_space(E, Space = {indent_line, N, _}) ->
    case yaml_event:top(E) of
        {_, I, _} when I =:= N ->
            after_block_next(E, Space)
    end.

%-----------------------------------------------------------------------

after_block_next(E, Space) ->
    case yaml_event:top(E) of
        {sequence, _, _} ->
            after_block_sequence(E, Space)
    end.

%-----------------------------------------------------------------------

after_block_sequence(E, _Space) ->
    case yaml_implicit:detect(E, block) of
        sequence ->
            sequence_next(E)
    end.

%-----------------------------------------------------------------------

after_block_pop(E, Space) ->
    case yaml_event:top(E) of
        {sequence, _, _} ->
            Next = fun (EE) -> after_block(EE, Space) end,
            At = yaml_event:coord(E),
            Event = {end_of_sequence, At},
            yaml_event:emit(Event, yaml_event:pop(E), Next)
    end.

%=======================================================================

push_indicator(E, Indicator, Block, N) ->
    S = yaml_event:scan(E),
    Indicator = yaml_scan:grapheme(S),
    After = yaml_scan:next(S),
    At = yaml_scan:coord(After),
    {Space, E1} = yaml_space:space(yaml_event:scan_to(E, After)),
    after_indicator(yaml_event:push(E1, Block, N, At), Space).

%-----------------------------------------------------------------------

swap_indicator(E, Indicator, Block) ->
    N = yaml_event:indent(E),
    push_indicator(yaml_event:pop(E), Indicator, Block, N).

%-----------------------------------------------------------------------

after_indicator(E, Space = {in_line, _, _}) ->
    case yaml_implicit:detect(E, block) of
        false ->
            content_block(E, Space)
    end;
after_indicator(E, Space = {indent_line, N, _}) ->
    after_indicator_indent(E, Space, N).

%-----------------------------------------------------------------------

after_indicator_indent(E, Space, N) ->
    case yaml_implicit:detect(E, block) of
        sequence ->
            sequence_first(E, N);

        false ->
            content_block(E, Space)
    end.

%=======================================================================

sequence_first(E, N) ->
    At = yaml_event:coord(E),
    Event = {start_of_sequence, At, no_anchor, no_tag},
    Next = fun (EE) -> push_indicator(EE, $-, sequence, N) end,
    yaml_event:emit(Event, E, Next).

%-----------------------------------------------------------------------

sequence_next(E) ->
    swap_indicator(E, $-, sequence).

