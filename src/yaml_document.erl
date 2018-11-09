%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_document).

-export([ document_may_start/1
        , block_did_end/2
        ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-spec document_may_start(yaml_event:state()) -> yaml_event:emit().

document_may_start(E0) ->
    case yaml_space:space(E0) of
        {Space = {indent_line, 0, 0}, E1} ->
            start_of_document(E1, Space);

        {{end_of, stream, _}, E1} ->
            yaml_event:stream_should_end(E1)
    end.

%=======================================================================

start_of_document(E, Space) ->
    At = yaml_event:coord(E),
    E1 = yaml_event:push(E, document, -1, At),
    Event = {start_of_document, At},
    Next = fun (EE) -> yaml_block:document(EE, Space) end,
    yaml_event:emit(Event, E1, Next).

%=======================================================================

-spec block_did_end(yaml_event:state(), yaml_space:space()) ->
    yaml_event:emit().

block_did_end(E, {end_of, stream, _}) ->
    At = yaml_event:coord(E),
    Next = fun yaml_event:stream_should_end/1,
    document_did_end(E, At, Next).

%-----------------------------------------------------------------------

document_did_end(E, At, Next) ->
    {document, -1, _} = yaml_event:top(E),
    Event = {end_of_document, At},
    yaml_event:emit(Event, yaml_event:pop(E), Next).

