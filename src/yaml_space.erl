%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_space).

-export([ space/1
        ]).

-export_type([ space/0
             , space_line/0
             , space_indent/0
             , space_white/0
             , space_end_of/0
             ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-type space_line() :: in_line | indent_line.

-type space_indent() :: non_neg_integer().

-type space_white() :: non_neg_integer().

-type space_end_of() :: stream | document | directives.

-type space() ::
    {space_line(), space_indent(), space_white()} |
    {end_of, space_end_of(), yaml:coord()} |
    bad_encoding.

%=======================================================================

-spec space(yaml_event:state()) -> {space(), yaml_event:state()}.

space(Event) ->
    Scan = yaml_event:scan(Event),
    case yaml_scan:is_start_of_line(Scan) of
        true ->
            start_of_line(Event, Scan);

        false ->
            start_of_space(Event, Scan, in_line)
    end.

%-----------------------------------------------------------------------

start_of_line(E, S) ->
    case yaml_scan:end_of(S) of
        {What, At, End} ->
            {{end_of, What, At}, yaml_event:scan_to(E, End)};

        false ->
            start_of_space(E, S, indent_line)
    end.

%-----------------------------------------------------------------------

start_of_space(E, S, Line) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            end_of_stream(E, S);

        break ->
            start_of_line(E, yaml_scan:next(S));

        $\s ->
            indent(E, yaml_scan:next(S), Line, 1);

        $\t ->
            whitespace(E, yaml_scan:next(S), Line, 0, 1);

        $# ->
            comment(E, yaml_scan:next(S));

        _ ->
            line(E, S, Line, 0, 0)
    end.

%-----------------------------------------------------------------------

indent(E, S, Line, Indent) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            end_of_stream(E, S);

        break ->
            start_of_line(E, yaml_scan:next(S));

        $\s ->
            indent(E, yaml_scan:next(S), Line, Indent + 1);

        $\t ->
            whitespace(E, yaml_scan:next(S), Line, Indent, 1);

        $# ->
            comment(E, yaml_scan:next(S));

        _ ->
            line(E, S, Line, Indent, 0)
    end.

%-----------------------------------------------------------------------

whitespace(E, S, Line, Indent, White) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            end_of_stream(E, S);

        break ->
            start_of_line(E, yaml_scan:next(S));

        $\s ->
            whitespace(E, yaml_scan:next(S), Line, Indent, White + 1);

        $\t ->
            whitespace(E, yaml_scan:next(S), Line, Indent, White + 1);

        $# ->
            comment(E, yaml_scan:next(S));

        _ ->
            line(E, S, Line, Indent, White)
    end.

%-----------------------------------------------------------------------

comment(E, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            end_of_stream(E, S);

        break ->
            start_of_line(E, yaml_scan:next(S));

        bad_encoding ->
            bad_encoding(E, S);

        _ ->
            comment(E, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

end_of_stream(E, S) ->
    At = yaml_scan:coord(S),
    {{end_of, stream, At}, yaml_event:scan_to(E, S)}.

%-----------------------------------------------------------------------

line(E, S, Line, Indent, White) ->
    {{Line, Indent, White}, yaml_event:scan_to(E, S)}.

%-----------------------------------------------------------------------

bad_encoding(E, S) ->
    {bad_encoding, yaml_event:scan_to(E, S)}.

%=======================================================================

-ifdef(TEST).

-define(_space(T, B, S, A), {T, {?LINE, fun () -> space_tester(B, S, A) end}}).

space_test_() ->
    [ ?_space("No space at start of line",
              {1, 1, <<".">>},
              {indent_line, 0, 0},
              {1, 1, <<".">>})
    , ?_space("No space in middle of line",
              {1, 2, <<".">>},
              {in_line, 0, 0},
              {1, 2, <<".">>})
    , ?_space("Indent at start of line",
              {1, 1, <<"  .">>},
              {indent_line, 2, 0},
              {1, 3, <<".">>})
    , ?_space("Indent in middle of line",
              {1, 2, <<"  .">>},
              {in_line, 2, 0},
              {1, 4, <<".">>})
    , ?_space("Whitespace at start of line",
              {1, 1, <<"\t .">>},
              {indent_line, 0, 2},
              {1, 3, <<".">>})
    , ?_space("Whitespace in middle of line",
              {1, 2, <<"\t .">>},
              {in_line, 0, 2},
              {1, 4, <<".">>})
    , ?_space("Indent on next line",
              {1, 3, <<"\n .">>},
              {indent_line, 1, 0},
              {2, 2, <<".">>})
    , ?_space("Comment goes to next line",
              {1, 3, <<"# comment \n .">>},
              {indent_line, 1, 0},
              {2, 2, <<".">>})
    , ?_space("Skip blank lines",
              {1, 3, <<"\n \t \t\n.">>},
              {indent_line, 0, 0},
              {3, 1, <<".">>})
    , ?_space("Skip comment lines",
              {1, 3, <<"\n#comment\n.">>},
              {indent_line, 0, 0},
              {3, 1, <<".">>})
    , ?_space("Skip comment lines",
              {1, 3, <<"\n \t #comment\n.">>},
              {indent_line, 0, 0},
              {3, 1, <<".">>})
    , ?_space("End of stream",
              {1, 3, <<"\n">>},
              {end_of, stream, {2, 1}},
              {2, 1, <<>>})
    , ?_space("End of stream in comment",
              {1, 2, <<" # ">>},
              {end_of, stream, {1, 5}},
              {1, 5, <<>>})
    , ?_space("End of stream in indent",
              {1, 2, <<" ">>},
              {end_of, stream, {1, 3}},
              {1, 3, <<>>})
    , ?_space("End of stream in whitespace",
              {1, 2, <<" \t">>},
              {end_of, stream, {1, 4}},
              {1, 4, <<>>})
    , ?_space("End of document (break)",
              {1, 2, <<"\n...\n">>},
              {end_of, document, {2, 1}},
              {2, 4, <<"\n">>})
    , ?_space("End of document (space)",
              {1, 2, <<"\n... \n">>},
              {end_of, document, {2, 1}},
              {2, 4, <<" \n">>})
    , ?_space("End of document (content)",
              {1, 2, <<"\n... content\n">>},
              {end_of, document, {2, 1}},
              {2, 4, <<" content\n">>})
    , ?_space("End of document (stream)",
              {1, 2, <<"\n...">>},
              {end_of, document, {2, 1}},
              {2, 4, <<"">>})
    , ?_space("End of directives (break)",
              {1, 2, <<"\n---\n">>},
              {end_of, directives, {2, 1}},
              {2, 4, <<"\n">>})
    , ?_space("End of directives (space)",
              {1, 2, <<"\n--- \n">>},
              {end_of, directives, {2, 1}},
              {2, 4, <<" \n">>})
    , ?_space("End of directives (content)",
              {1, 2, <<"\n--- content\n">>},
              {end_of, directives, {2, 1}},
              {2, 4, <<" content\n">>})
    , ?_space("End of directives (stream)",
              {1, 2, <<"\n---">>},
              {end_of, directives, {2, 1}},
              {2, 4, <<>>})
    , ?_space("Bad encoding in middle of line",
              {1, 2, <<"  ", 128>>},
              {in_line, 2, 0},
              {1, 4, <<128>>})
    , ?_space("Bad encoding in comment",
              {1, 2, <<" # ", 128>>},
              bad_encoding,
              {1, 5, <<128>>})
    ].

%-----------------------------------------------------------------------

space_simplify({Space, Event}) ->
    {Space, yaml_event:scan(Event)}.

%-----------------------------------------------------------------------

space_tester({Br, Bc, Bb}, Space, {Ar, Ac, Ab}) ->
    Before = yaml_scan:mock(Bb, Br, Bc),
    State = yaml_event:mock(Before),
    After = yaml_scan:mock(Ab, Ar, Ac),
    ?assertEqual({Space, After}, space_simplify(space(State))).

%-----------------------------------------------------------------------

-endif.

