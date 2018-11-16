%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_tag).

-export([ property/1
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-spec property(yaml_event:state()) ->
    {yaml:tag(), list(), yaml_event:state()}.

property(E) ->
    Construct = fun property_construct/2,
    {T, S} = yaml_token:start(E, tag, Construct, #{}),
    $! = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    property_start(T, S, Z).

%=======================================================================

-spec property_construct(list(), map()) -> {yaml:tag(), list()}.

property_construct(Ps, #{ from := From, thru := Thru }) ->
    Tag = property_construct(From, Thru, Ps),
    Errors = [],
    {Tag, Errors}.

%-----------------------------------------------------------------------

property_construct(From, Thru, [{_, _, Suffix}, {_, _, Handle}]) ->
    {tag, From, Thru, Handle, Suffix};
property_construct(From, Thru, [{_, _, P} | Ps]) ->
    property_construct(From, Thru, Ps, P).

%-----------------------------------------------------------------------

property_construct(From, Thru, [{_, _, P}, {_, _, Handle}], Acc) ->
    {tag, From, Thru, Handle, <<P/binary, Acc/binary>>};
property_construct(From, Thru, [{_, _, P} | Ps], Acc) ->
    property_construct(From, Thru, Ps, <<P/binary, Acc/binary>>).

%=======================================================================

property_start(T, Start, S) ->
    case yaml_scan:grapheme(S) of
        $! ->
            Z = yaml_scan:next(S),
            property_suffix_empty(yaml_token:keep(T, Z), Z);

        $< ->
            Z = yaml_scan:next(S),
            T1 = yaml_token:keep(T, <<>>, Start),
            property_verbatim(yaml_token:skip(T1, Z), Z, Z);

        G when ?IS_FLOW_INDICATOR(G) ->
            property_non_specific(T, Start, S, finish);

        G when ?IS_WORD_CHAR(G) ->
            property_handle(T, S, yaml_scan:next(S));

        _ ->
            property_non_specific(T, Start, S, trailing)
    end.

%-----------------------------------------------------------------------

property_non_specific(T, Handle, S, Finish) ->
    T1 = yaml_token:keep(T, <<>>, Handle),
    T2 = yaml_token:keep(T1, S),
    case Finish of
        finish ->
            yaml_token:finish(T2, S);

        trailing ->
            property_trailing_check(T2, S)
    end.

%-----------------------------------------------------------------------

property_handle(T, Handle, S) ->
    case yaml_scan:grapheme(S) of
        $! ->
            Z = yaml_scan:next(S),
            property_suffix_empty(yaml_token:keep(T, Z), Z);

        G when ?IS_FLOW_INDICATOR(G) ->
            property_primary_suffix(T, Handle, S, finish);

        G when ?IS_WORD_CHAR(G) ->
            property_handle(T, Handle, yaml_scan:next(S));

        $% ->
            T1 = yaml_token:keep(T, Handle),
            property_suffix_escape(T1, S, yaml_scan:next(S));

        G when ?IS_URI_CHAR(G) ->
            T1 = yaml_token:keep(T, Handle),
            property_suffix(T1, S);

        _ ->
            property_primary_suffix(T, Handle, S, trailing)
    end.

%-----------------------------------------------------------------------

property_primary_suffix(T, Handle, S, Finish) ->
    T1 = yaml_token:keep(T, Handle),
    T2 = yaml_token:keep(T1, S),
    case Finish of
        finish ->
            yaml_token:finish(T2, S);

        trailing ->
            property_trailing_check(T2, S)
    end.

%-----------------------------------------------------------------------

property_suffix_empty(T, S) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_FLOW_INDICATOR(G) ->
            property_suffix_empty_error(T, S, finish);

        $% ->
            property_suffix_escape(T, S, yaml_scan:next(S));

        G when ?IS_URI_CHAR(G) ->
            property_suffix(T, yaml_scan:next(S));

        _ ->
            property_suffix_empty_error(T, S, trailing)
    end.

%-----------------------------------------------------------------------

property_suffix_empty_error(T, S, Finish) ->
    T1 = yaml_token:keep(T, <<>>, S),
    T2 = yaml_token:error(T1, expecting_suffix, S),
    case Finish of
        finish ->
            yaml_token:finish(T2, S);

        _ ->
            property_trailing_check(T2, S)
    end.

%-----------------------------------------------------------------------

property_suffix(T, S) ->
    case yaml_scan:grapheme(S) of
        $! ->
            Z = yaml_scan:next(S),
            T1 = yaml_token:error_range(T, invalid_in_tag_suffix, S, Z),
            property_suffix(T1, Z);

        G when ?IS_FLOW_INDICATOR(G) ->
            yaml_token:finish(T, S);

        $% ->
            property_suffix_escape(T, S, yaml_scan:next(S));

        G when ?IS_URI_CHAR(G) ->
            property_suffix(T, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:keep(T, S),
            property_trailing_check(T1, S)
    end.

%-----------------------------------------------------------------------

property_suffix_escape(T, Start, S) ->
    property_suffix_escape(T, Start, 2, 0, S).

%-----------------------------------------------------------------------

property_suffix_escape(T, Start, 0, $!, S) ->
    T1 = yaml_token:keep(T, Start),
    T2 = yaml_token:keep(T1, <<$!>>, S),
    property_suffix(T2, S);
property_suffix_escape(T, _, 0, _, S) ->
    property_suffix(T, S);
property_suffix_escape(T, Start, N, Acc, S) ->
    case yaml_scan:grapheme(S) of
        G when (G >= $0 andalso G =< $9) ->
            Calc = (Acc * 16) + (G - $0),
            property_suffix_escape(T, Start, N - 1, Calc, yaml_scan:next(S));

        G when (G >= $a andalso G =< $f) ->
            Calc = (Acc * 16) + (G - $a + 10),
            property_suffix_escape(T, Start, N - 1, Calc, yaml_scan:next(S));

        G when (G >= $A andalso G =< $F) ->
            Calc = (Acc * 16) + (G - $A + 10),
            property_suffix_escape(T, Start, N - 1, Calc, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:error_range(T, bad_escape, Start, S),
            property_suffix(T1, S)
    end.

%-----------------------------------------------------------------------

property_verbatim(T, Start, S) ->
    case yaml_scan:grapheme(S) of
        $> ->
            Z = yaml_scan:next(S),
            T1 = property_verbatim_check(T, Start, S),
            T2 = yaml_token:keep(T1, S),
            T3 = yaml_token:skip(T2, Z),
            property_trailing_check(T3, Z);

        $% ->
            throw(verbatim_escape);

        G when ?IS_URI_CHAR(G) ->
            property_verbatim(T, Start, yaml_scan:next(S));

        _ ->
            T1 = property_verbatim_check(T, Start, S),
            T2 = yaml_token:keep(T1, S),
            T3 = yaml_token:error(T2, expecting_verbatim_close_character, S),
            property_trailing_check(T3, S)
    end.

%-----------------------------------------------------------------------

property_verbatim_check(T, Start, End) ->
    case yaml_scan:consumed(Start, End) of
        <<>> ->
            yaml_token:error_range(T, empty_verbatim_tag, Start, End);

        <<"!">> ->
            yaml_token:error_range(T, invalid_local_tag, Start, End);

        _ ->
            T
    end.

%-----------------------------------------------------------------------

property_trailing_check(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            yaml_token:finish(T, S);

        G when ?IS_WHITE(G) ->
            yaml_token:finish(T, S);

        end_of_stream ->
            yaml_token:finish(T, S)
    end.

