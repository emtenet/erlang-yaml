%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_plain).

-export([ scalar/3
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-type style() :: block | flow.

%=======================================================================

-spec scalar(yaml_event:state(), style(), yaml:props()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

scalar(Event, Style, Props)
        when (Style =:= block orelse Style =:= flow) andalso
             is_map(Props) ->
    {T, S} = yaml_token:start(Event, plain, fun construct/2, Props),
    first(Style, T, S).

%=======================================================================

-spec construct(list(), map()) -> {yaml_event:event(), list()}.

construct([], _) ->
    % must not be empty
    error(pre_condition);
construct(Ps, #{ from := From, thru := Thru, anchor := Anchor, tag := Tag}) ->
    Token = {plain, From, Thru, Anchor, Tag, Ps},
    {Token, []}.

%=======================================================================

first(Style, T, S) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_WHITE(G) ->
            error(pre_condition);

        G when ?IS_PLAIN_CHECK_INDICATOR(G) ->
            first_check(Style, T, yaml_scan:next(S));

        G when ?IS_INDICATOR(G) ->
            error(pre_condition);

        _ ->
            text(Style, T, S)
    end.

%-----------------------------------------------------------------------

first_check(Style, T, S) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_WHITE(G) ->
            error(pre_condition);

        G when ?IS_PRINTABLE(G) ->
            text(Style, T, S);

        _ ->
            error(pre_condition)
    end.

%=======================================================================

text(Style, T, S) ->
    case yaml_scan:grapheme(S) of
        $: ->
            text_colon(Style, T, S, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            text_white(Style, T, S, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            text(Style, T, yaml_scan:next(S));

        _ ->
            yaml_token:finish(T, S)
    end.

%-----------------------------------------------------------------------

text_colon(Style, T, White, S) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_WHITE(G) ->
            yaml_token:finish(T, White);

        G when ?IS_PRINTABLE(G) ->
            text(Style, T, yaml_scan:next(S));

        _ ->
            yaml_token:finish(T, White)
    end.

%-----------------------------------------------------------------------

text_white(Style, T, White, S) ->
    case yaml_scan:grapheme(S) of
        $: ->
            text_colon(Style, T, White, yaml_scan:next(S));

        $# ->
            yaml_token:finish(T, White);

        G when ?IS_WHITE(G) ->
            text_white(Style, T, White, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            text(Style, T, yaml_scan:next(S));

        _ ->
            yaml_token:finish(T, White)
    end.

%=======================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CASE(C, X, F, T, P), {X, {?LINE, test_case(C, F, T, P, [])}}).
-define(TEST_CASE(C, X, F, T, P, E), {X, {?LINE, test_case(C, F, T, P, E)}}).
-define(TEST_PRE_CONDITION(C, X, F), {X, {?LINE, test_pre_condition(C, F)}}).

%-----------------------------------------------------------------------

test_case(Style, From = {Fr, Fc, _}, Thru = {Tr, Tc, _}, Parts, Errors) ->
    Anchor = no_anchor,
    Tag = no_tag,
    Props = #{ anchor => Anchor, tag => Tag },
    Token = {plain, {Fr, Fc}, {Tr, Tc}, Anchor, Tag, Parts},
    Test = fun (Event) -> scalar(Event, Style, Props) end,
    fun () -> yaml_token:test_case(Test, From, Thru, Token, Errors) end.

%-----------------------------------------------------------------------

test_pre_condition(Style, From) ->
    Anchor = no_anchor,
    Tag = no_tag,
    Props = #{ anchor => Anchor, tag => Tag },
    Test = fun (Event) -> scalar(Event, Style, Props) end,
    fun () -> yaml_token:test_pre_condition(Test, From) end.

%-----------------------------------------------------------------------

simple_test_() ->
    [ ?TEST_CASE(block, "single alpha word"
        , {1, 1, <<"plain">>}
        , {1, 6, <<>>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )

    , ?TEST_PRE_CONDITION(block, "must not be empty"
        , {1, 1, <<>>}
        )

    , ?TEST_PRE_CONDITION(block, "must not contain leading space"
        , {1, 1, <<" word">>}
        )

    , ?TEST_PRE_CONDITION(block, "must not contain leading tab"
        , {1, 1, <<"\tword">>}
        )

    , [ [ ?TEST_PRE_CONDITION(block, "must not begin with most indicators"
            , {1, 1, <<G, " word">>}
            )
        , ?TEST_PRE_CONDITION(block, "must not begin with most indicators"
            , {1, 1, <<G, "word">>}
            )
        ]
      || G <- ?INDICATOR, not ?IS_PLAIN_CHECK_INDICATOR(G) ]

    , [ [ ?TEST_PRE_CONDITION(block, "some indicators safe with non-space"
            , {1, 1, <<G>>}
            )
        , ?TEST_PRE_CONDITION(block, "some indicators safe with non-space"
            , {1, 1, <<G, " word">>}
            )
        , ?TEST_PRE_CONDITION(block, "some indicators safe with non-space"
            , {1, 1, <<G, "\tword">>}
            )
        , ?TEST_CASE(block, "some indicators safe with non-space"
            , {1, 1, <<G, "word">>}
            , {1, 6, <<>>}
            , [ {{1, 1}, {1, 6}, <<G, "word">>} ]
            )
        ]
      || G <- ?PLAIN_CHECK_INDICATOR ]

    , ?TEST_CASE(block, "must not contain trailing space"
        , {1, 1, <<"plain \t ">>}
        , {1, 6, <<" \t ">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )
    , ?TEST_CASE(block, "must not contain trailing space"
        , {1, 1, <<"plain \t \n">>}
        , {1, 6, <<" \t \n">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )

    , ?TEST_CASE(block, "must not contain implicit key combination"
        , {1, 1, <<"plain:">>}
        , {1, 6, <<":">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )
    , ?TEST_CASE(block, "must not contain implicit key combination"
        , {1, 1, <<"plain: ">>}
        , {1, 6, <<": ">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )
    , ?TEST_CASE(block, "must not contain implicit key combination"
        , {1, 1, <<"plain :">>}
        , {1, 6, <<" :">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )
    , ?TEST_CASE(block, "must not contain implicit key combination"
        , {1, 1, <<"plain : ">>}
        , {1, 6, <<" : ">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )

    , ?TEST_CASE(block, "must not contain comment"
        , {1, 1, <<"plain #">>}
        , {1, 6, <<" #">>}
        , [ {{1, 1}, {1, 6}, <<"plain">>} ]
        )
    , ?TEST_CASE(block, "must not contain comment"
        , {1, 1, <<"plain# ">>}
        , {1, 7, <<" ">>}
        , [ {{1, 1}, {1, 7}, <<"plain#">>} ]
        )
    ].

-endif.

