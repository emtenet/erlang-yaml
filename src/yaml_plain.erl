%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_plain).

-export([ scalar/3
        ]).

-include("yaml_event.hrl").

%=======================================================================

-type context() :: block | flow.

%=======================================================================

-spec scalar(#event{}, context(), props()) -> {term(), list(), #event{}}.

scalar(Event = #event{scan = S}, Context, Props)
        when (Context =:= block orelse Context =:= flow) andalso
             is_map(Props) ->
    T = yaml_token:start(Event, plain, fun construct/2, Props),
    first(Context, T, S).

%=======================================================================

construct([], _) ->
    % must not be empty
    error(pre_condition);
construct(Ps, #{ from := From, thru := Thru, anchor := Anchor, tag := Tag}) ->
    Token = {plain, From, Thru, Anchor, Tag, Ps},
    {Token, []}.

%=======================================================================

first(Context, T, S) ->
    case yaml_scan:grapheme(S) of
        C when ?IS_WHITE(C) ->
            error(pre_condition);

        _ ->
            text(Context, T, S)
    end.

%=======================================================================

text(Context, T, S) ->
    case yaml_scan:grapheme(S) of
        C when ?IS_PRINTABLE(C) ->
            text(Context, T, yaml_scan:next(S));

        _ ->
            yaml_token:finish(T, S)
    end.

%=======================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_CASE(C, X, F, T, P), {X, {?LINE, test_case(C, F, T, P, [])}}).
-define(TEST_CASE(C, X, F, T, P, E), {X, {?LINE, test_case(C, F, T, P, E)}}).
-define(TEST_PRE_CONDITION(C, X, F), {X, {?LINE, test_pre_condition(C, F)}}).

%-----------------------------------------------------------------------

test_case(Context, From = {Fr, Fc, _}, Thru = {Tr, Tc, _}, Parts, Errors) ->
    Anchor = no_anchor,
    Tag = no_tag,
    Props = #{ anchor => Anchor, tag => Tag },
    Token = {plain, {Fr, Fc}, {Tr, Tc}, Anchor, Tag, Parts},
    Test = fun (Event) -> scalar(Event, Context, Props) end,
    fun () -> yaml_token:test_case(Test, From, Thru, Token, Errors) end.

%-----------------------------------------------------------------------

test_pre_condition(Context, From) ->
    Anchor = no_anchor,
    Tag = no_tag,
    Props = #{ anchor => Anchor, tag => Tag },
    Test = fun (Event) -> scalar(Event, Context, Props) end,
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
    ].

-endif.

