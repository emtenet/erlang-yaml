%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_document).

-export([ stream/1
        , block_did_end/2
        ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec stream(yaml_event:state()) -> yaml_event:emit().

stream(E0) ->
    case yaml_space:space(E0) of
        {{_, end_of, directives, At}, E1} ->
            directive_start(E1, At);

        {{_, end_of, stream, _}, E1} ->
            yaml_event:stream_should_end(E1);

        {{_, end_of, byte_order_mark, _}, E1} ->
            stream(yaml_event:scan_next(E1));

        {Space = {_, end_of, _, _}, E1} ->
            stream_did_error(E1, Space);

        {Space = {_, indent_line, 0, 0}, E1} ->
            may_start_document(E1, Space);

        {Space, E1} ->
            Next = fun (EE) -> yaml_block:document(EE, Space) end,
            start_of_document(E1, Next)
    end.

%-----------------------------------------------------------------------

stream_did_error(E, {Thru, end_of, Bad, _}) ->
    {stream, -1, From} = yaml_event:top(E),
    Error = {Bad, Thru, Thru, {stream, From, Thru}},
    Next = fun stream/1,
    yaml_event:error(Error, yaml_event:scan_next(E), Next).

%=======================================================================

start_of_document(E, Next) ->
    At = yaml_event:coord(E),
    start_of_document(E, At, Next).

%-----------------------------------------------------------------------

start_of_document(E, At, Next) ->
    E1 = yaml_event:push(E, document, -1, At),
    Event = {start_of_document, At},
    yaml_event:emit(Event, E1, Next).

%-----------------------------------------------------------------------

may_start_document(E, Space) ->
    case yaml_event:grapheme(E) of
        byte_order_mark ->
            stream(yaml_event:scan_next(E));

        $% ->
            Next = fun (EE) -> directive(EE) end,
            start_of_document(E, Next);

        _ ->
            Next = fun (EE) -> yaml_block:document(EE, Space) end,
            start_of_document(E, Next)
    end.

%-----------------------------------------------------------------------

directive_start(E, At) ->
    Next = fun directive_end/1,
    start_of_document(E, At, Next).

%-----------------------------------------------------------------------

directive(E) ->
    {Directive, Errors, E1} = yaml_directive:document(E),
    directive_emit(E1, Errors, Directive).

%-----------------------------------------------------------------------

directive_emit(E, [Error | Errors], Directive) ->
    Next = fun (EE) -> directive_emit(EE, Errors, Directive) end,
    yaml_event:error(Error, E, Next);
directive_emit(E, [], Directive) ->
    Next = fun (EE) -> directive_next(EE) end,
    yaml_event:emit(Directive, E, Next).

%-----------------------------------------------------------------------

directive_next(E) ->
    case yaml_space:space(E) of
        {Space = {_, indent_line, 0, 0}, E1} ->
            directive_next_or_block(E1, Space);

        {{_, end_of, directives, _}, E1} ->
            directive_end(E1)
    end.

%-----------------------------------------------------------------------

directive_next_or_block(E, Space) ->
    case yaml_event:grapheme(E) of
        $% ->
            directive(E);

        _ ->
            yaml_block:document(E, Space)
    end.

%-----------------------------------------------------------------------

directive_end(E) ->
    case yaml_space:space(E) of
        {Space = {_, in_line, _, _}, E1} ->
            yaml_block:document(E1, Space);

        {Space = {_, indent_line, _, _}, E1} ->
            yaml_block:document(E1, Space);

        {{From, end_of, document, _}, E1} ->
            Event = {empty, From, From, no_anchor, no_tag},
            Next = fun document_suffix/1,
            yaml_event:emit(Event, E1, Next)
    end.

%=======================================================================

document_suffix(E) ->
    case yaml_space:space(E) of
        {{End, end_of, directives, At}, E1} ->
            Next = fun (EE) -> directive_start(EE, At) end,
            document_did_end(E1, End, Next);

        {Space = {End, indent_line, 0, 0}, E1} ->
            Next = fun (EE) -> may_start_document(EE, Space) end,
            document_did_end(E1, End, Next);

        {{End, end_of, document, _}, E1} ->
            Next = fun stream/1,
            document_did_end(E1, End, Next);

        {{End, end_of, stream, _}, E1} ->
            Next = fun yaml_event:stream_should_end/1,
            document_did_end(E1, End, Next)
    end.

%=======================================================================

-spec block_did_end(yaml_event:state(), yaml_space:space()) ->
    yaml_event:emit().

block_did_end(E, Space) ->
    case Space of
        {End, end_of, directives, At} ->
            Next = fun (EE) -> directive_start(EE, At) end,
            document_did_end(E, End, Next);

        {_, end_of, document, _} ->
            document_suffix(E);

        {End, end_of, stream, _} ->
            Next = fun yaml_event:stream_should_end/1,
            document_did_end(E, End, Next);

        _ ->
            document_did_error(E, Space)
    end.

%-----------------------------------------------------------------------

document_did_error(E, {Thru, end_of, Bad, _}) ->
    {document, -1, From} = yaml_event:top(E),
    Error = {Bad, Thru, Thru, {document, From, Thru}},
    Next = fun (EE) -> document_did_end(EE, Thru, fun stream/1) end,
    yaml_event:error(Error, yaml_event:scan_next(E), Next).

%-----------------------------------------------------------------------

document_did_end(E, At, Next) ->
    {document, -1, _} = yaml_event:top(E),
    Event = {end_of_document, At},
    yaml_event:emit(Event, yaml_event:pop(E), Next).

