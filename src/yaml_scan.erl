%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_scan).

-export([ start/1
        , next/1
        % INFO
        , coord/1
        , consumed/2
        , grapheme/1
        , is_indented_at_least_by/2
        , is_start_of_line/1
        , end_of/1
        ]).

-export_type([ state/0
             , grapheme/0
             ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([ mock/3 ]).
-endif.

%=======================================================================

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

-type grapheme() ::
    string:grapheme_cluster() |
    break |
    end_of_stream |
    bad_encoding.

-record(scan,
        { b = <<>>
            :: binary()
        , g = end_of_stream
            :: grapheme()
        , n = <<>>
            :: binary()
        , r = 1
            :: pos_integer()
        , c = 1
            :: pos_integer()
        }).

-opaque state() :: #scan{}.

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

-spec grapheme(state()) -> grapheme().

grapheme(#scan{g = G}) ->
    G.

%=======================================================================

-spec is_indented_at_least_by(state(), integer()) -> boolean().

is_indented_at_least_by(#scan{c = C}, I) ->
    % NOTE:
    % - C is one (1) based
    % - I is zero (0) based
    % For example:
    %   To be indented at least by 2 (spaces),
    %   Column must be 3 or more
    C > I.

%=======================================================================

-spec is_start_of_line(state()) -> boolean().

is_start_of_line(#scan{c = C}) ->
    C =:= 1.

%=======================================================================

-spec end_of(state()) ->
    {document | directives, yaml:coord(), state()} |
    false.

end_of(S = #scan{b = <<"...", _/binary>>}) ->
    end_of(S, document);
end_of(S = #scan{b = <<"...", X, _/binary>>}) when ?IS_SEPARATE(X) ->
    end_of(S, document);
end_of(S = #scan{b = <<"---", _/binary>>}) ->
    end_of(S, directives);
end_of(S = #scan{b = <<"---", X, _/binary>>}) when ?IS_SEPARATE(X) ->
    end_of(S, directives);
end_of(#scan{}) ->
    false.

%-----------------------------------------------------------------------

end_of(#scan{b = <<_:3/binary, B/binary>>, r = R, c = 1}, What) ->
    {What, {R, 1}, prepare(B, R, 4)}.

