%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_scan).

-export([ start/1
        , next/1
        % INFO
        , coord/1
        , consumed/2
        , grapheme/1
        , indented/2
        % SPACE
        , space/1
        ]).

-export_type([ space/0
             , space_line/0
             , space_indent/0
             , space_white/0
             , space_end_of/0
             ]).

-export_type([ state/0 ]).

-include("yaml_private.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([ mock/3 ]).
-endif.

%=======================================================================

-define(SCAN_END(E, S), (E)#event{scan = (S)}).

-define(IS_BREAK(G),
            (       (G =:= $\n)
             orelse (G =:= $\r)
             orelse (G =:= [$\r, $\n])
            )).

-define(IS_SEPARATE(C),
            (       (C =:= $\s)
             orelse (C =:= $\t)
             orelse (C =:= $\n)
             orelse (C =:= $\r)
            )).

%=======================================================================

-record(scan,
        { b = <<>>
            :: binary()
        , g = end_of_stream
            ::  string:grapheme_cluster() |
                break |
                end_of_stream |
                bad_encoding
        , n = <<>>
            :: binary()
        , r = 1
            :: pos_integer()
        , c = 1
            :: pos_integer()
        }).

-opaque state() :: #scan{}.

-type space_line() :: in_line | indent_line.

-type space_indent() :: non_neg_integer().

-type space_white() :: non_neg_integer().

-type space_end_of() :: stream | document | directives.

-type space() ::
    {space_line(), space_indent(), space_white()} |
    {end_of, space_end_of(), yaml:coord()} |
    bad_encoding.

%=======================================================================

-spec start(binary()) -> state().

start(Binary) ->
    prepare(Binary, 1, 1).

%=======================================================================

-ifdef(TEST).

-define(_start(B, G, N), {?LINE, fun () -> start_tester(B, G, N) end}).

start_test_() ->
    [ ?_start(<<>>, end_of_stream, <<>>)
    , ?_start(<<"arest">>, $a, <<"rest">>)
    , ?_start(<<"\nrest">>, break, <<"rest">>)
    , ?_start(<<128, "rest">>, bad_encoding, <<>>)
    ].

%-----------------------------------------------------------------------

start_tester(Binary, Grapheme, Next) ->
    Expect = #scan{b = Binary, g = Grapheme, n = Next, r = 1, c = 1},
    ?assertEqual(Expect, start(Binary)).

-endif.

%=======================================================================

-ifdef(TEST).

-spec mock(binary(), non_neg_integer(), non_neg_integer()) -> state().

mock(Binary, Row, Column) ->
    prepare(Binary, Row, Column).

-endif.

%=======================================================================

-spec next(state()) -> state().

next(#scan{g = break, n = Next, r = Row}) ->
    prepare(Next, Row + 1, 1);
next(S = #scan{g = end_of_stream}) ->
    S;
next(S = #scan{g = bad_encoding}) ->
    S;
next(#scan{n = Next, r = Row, c = Column}) ->
    prepare(Next, Row, Column + 1).

%=======================================================================

-ifdef(TEST).

-define(_next(S, B, G, N, R, C),
        {?LINE, fun () -> next_tester(S, B, G, N, R, C) end}).

next_test_() ->
    Bad = <<128, "rest">>,
    [ ?_next(<<>>, <<>>, end_of_stream, <<>>, 1, 1)
    , ?_next(<<"arest">>, <<"rest">>, $r, <<"est">>, 1, 2)
    , ?_next(<<"\nrest">>, <<"rest">>, $r, <<"est">>, 2, 1)
    , ?_next(Bad, Bad, bad_encoding, <<>>, 1, 1)
    ].

%-----------------------------------------------------------------------

next_tester(Source, Binary, Grapheme, Next, Row, Column) ->
    Expect = #scan{b = Binary, g = Grapheme, n = Next, r = Row, c = Column},
    ?assertEqual(Expect, next(start(Source))).

-endif.

%=======================================================================

-spec prepare(binary(), non_neg_integer(), non_neg_integer()) -> state().

prepare(B, R, C) ->
    case string:next_grapheme(B) of
        [] ->
            #scan{b = B, g = end_of_stream, n = B, r = R, c = C};

        [G] ->
            #scan{b = B, g = G, n = <<>>, r = R, c = C};

        [G| N] when is_binary(N) andalso ?IS_BREAK(G) ->
            #scan{b = B, g = break, n = N, r = R, c = C};

        [G| N] when is_binary(N) ->
            #scan{b = B, g = G, n = N, r = R, c = C};

        [G, N] when is_binary(N) ->
            #scan{b = B, g = G, n = N, r = R, c = C};

        {error, _} ->
            #scan{b = B, g = bad_encoding, n = <<>>, r = R, c = C}
    end.

%=======================================================================

-ifdef(TEST).

-define(_prepare(T, B, N, E),
        {T, {?LINE, fun () -> prepare_tester(?LINE, B, N, E) end}}).

%-----------------------------------------------------------------------

-dialyzer({no_improper_lists, prepare_test_/0}).

prepare_test_() ->
    [ ?_prepare("Common case",
                 <<"arest">>,
                 [$a | <<"rest">>],
                 #scan{g = $a, n = <<"rest">>})
    , ?_prepare("Break (unix)",
                 <<"\nrest">>,
                 [$\n | <<"rest">>],
                 #scan{g = break, n = <<"rest">>})
    , ?_prepare("Break (mac)",
                 <<"\rrest">>,
                 [$\r | <<"rest">>],
                 #scan{g = break, n = <<"rest">>})
    , ?_prepare("Break (dos)",
                 <<"\r\nrest">>,
                 ["\r\n" | <<"rest">>],
                 #scan{g = break, n = <<"rest">>})
    , ?_prepare("Last prepare",
                 <<"a">>,
                 [$a],
                 #scan{g = $a, n = <<>>})
    , ?_prepare("Last prepare is break (unix)",
                 <<"\n">>,
                 [$\n | <<>>],
                 #scan{g = break, n = <<>>})
    , ?_prepare("Last prepare is break (mac)",
                 <<"\r">>,
                 [$\r | <<>>],
                 #scan{g = break, n = <<>>})
    , ?_prepare("Last prepare is break (dos)",
                 <<"\r\n">>,
                 ["\r\n" | <<>>],
                 #scan{g = break, n = <<>>})
    , ?_prepare("Grapheme then bad encoding",
                 <<"a", 128, "rest">>,
                 [$a, <<128, "rest">>],
                 #scan{g = $a, n = <<128, "rest">>})
    , ?_prepare("Break then bad encoding",
                 <<"\n", 128, "rest">>,
                 [$\n | <<128, "rest">>],
                 #scan{g = break, n = <<128, "rest">>})
    , ?_prepare("Bad encoding",
                 <<128, "rest">>,
                 {error, <<128, "rest">>},
                 #scan{g = bad_encoding, n = <<>>})
    ].

%-----------------------------------------------------------------------

prepare_tester(Line, Binary, Next, Scan = #scan{}) when is_binary(Binary) ->
    ?assertEqual(Next, string:next_grapheme(Binary)),
    R = Line rem 9,
    C = (Line div 9) rem 9,
    Expect = Scan#scan{b = Binary, r = R, c = C},
    ?assertEqual(Expect, prepare(Binary, R, C)).

-endif.

%=======================================================================

-spec coord(state()) -> yaml:coord().

coord(#scan{r = R, c = C}) ->
    {R, C}.

%=======================================================================

-spec consumed(state(), state()) -> binary().

consumed(#scan{b = From}, #scan{b = Thru}) when size(From) >= size(Thru) ->
    N = size(From) - size(Thru),
    <<T:N/binary, _/binary>> = From,
    T.

%=======================================================================

-spec grapheme(state()) -> string:grapheme_cluster() | break | end_of_stream.

grapheme(#scan{g = G}) ->
    G.

%=======================================================================

-spec indented(state(), integer()) -> boolean().

indented(#scan{c = C}, I) ->
    C >= I.

%=======================================================================

-spec space(#event{}) -> {space(), #event{}}.

space(SS = #event{scan = S = #scan{c = 1}}) ->
    start_of_line(SS, S);
space(SS = #event{scan = S = #scan{c = C}}) when C > 1 ->
    start_of_space(SS, S, in_line).

%-----------------------------------------------------------------------

start_of_line(SS, S = #scan{b = <<"...">>}) ->
    end_of_document(SS, S);
start_of_line(SS, S = #scan{b = <<"...", X, _/binary>>})
        when ?IS_SEPARATE(X) ->
    end_of_document(SS, S);
start_of_line(SS, S = #scan{b = <<"---">>}) ->
    end_of_directives(SS, S);
start_of_line(SS, S = #scan{b = <<"---", X, _/binary>>})
        when ?IS_SEPARATE(X) ->
    end_of_directives(SS, S);
start_of_line(SS, S) ->
    start_of_space(SS, S, indent_line).

%-----------------------------------------------------------------------

start_of_space(SS, S = #scan{}, Line) ->
    case S#scan.g of
        end_of_stream ->
            end_of_stream(SS, S);

        break ->
            start_of_line(SS, next(S));

        $\s ->
            indent(SS, next(S), Line, 1);

        $\t ->
            whitespace(SS, next(S), Line, 0, 1);

        $# ->
            comment(SS, next(S));

        _ ->
            line(SS, S, Line, 0, 0)
    end.

%-----------------------------------------------------------------------

indent(SS, S = #scan{}, Line, Indent) ->
    case S#scan.g of
        end_of_stream ->
            end_of_stream(SS, S);

        break ->
            start_of_line(SS, next(S));

        $\s ->
            indent(SS, next(S), Line, Indent + 1);

        $\t ->
            whitespace(SS, next(S), Line, Indent, 1);

        $# ->
            comment(SS, next(S));

        _ ->
            line(SS, S, Line, Indent, 0)
    end.

%-----------------------------------------------------------------------

whitespace(SS, S = #scan{}, Line, Indent, White) ->
    case S#scan.g of
        end_of_stream ->
            end_of_stream(SS, S);

        break ->
            start_of_line(SS, next(S));

        $\s ->
            whitespace(SS, next(S), Line, Indent, White + 1);

        $\t ->
            whitespace(SS, next(S), Line, Indent, White + 1);

        $# ->
            comment(SS, next(S));

        _ ->
            line(SS, S, Line, Indent, White)
    end.

%-----------------------------------------------------------------------

comment(SS, S = #scan{}) ->
    case S#scan.g of
        end_of_stream ->
            end_of_stream(SS, S);

        break ->
            start_of_line(SS, next(S));

        bad_encoding ->
            bad_encoding(SS, S);

        _ ->
            comment(SS, next(S))
    end.

%-----------------------------------------------------------------------

end_of_stream(SS = #event{}, S = #scan{}) ->
    #scan{r = R, c = C} = S,
    {{end_of, stream, {R, C}}, ?SCAN_END(SS, S)}.

%-----------------------------------------------------------------------

end_of_document(SS = #event{}, S = #scan{}) ->
    #scan{b = <<"...", B/binary>>, r = R, c = 1} = S,
    End = prepare(B, R, 4),
    {{end_of, document, {R, 1}}, ?SCAN_END(SS, End)}.

%-----------------------------------------------------------------------

end_of_directives(SS = #event{}, S = #scan{}) ->
    #scan{b = <<"---", B/binary>>, r = R, c = 1} = S,
    End = prepare(B, R, 4),
    {{end_of, directives, {R, 1}}, ?SCAN_END(SS, End)}.

%-----------------------------------------------------------------------

line(SS = #event{}, S = #scan{}, Line, Indent, White) ->
    {{Line, Indent, White}, ?SCAN_END(SS, S)}.

%-----------------------------------------------------------------------

bad_encoding(SS = #event{}, S = #scan{}) ->
    {bad_encoding, ?SCAN_END(SS, S)}.

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

space_simplify({Space, #event{scan = Scan}}) ->
    {Space, Scan}.

%-----------------------------------------------------------------------

space_tester({Br, Bc, Bb}, Space, {Ar, Ac, Ab}) ->
    Before = prepare(Bb, Br, Bc),
    State = yaml_event:mock(Before),
    After = prepare(Ab, Ar, Ac),
    ?assertEqual({Space, After}, space_simplify(space(State))).

%-----------------------------------------------------------------------

-endif.

