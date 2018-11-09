%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_implicit).

-export([ detect/2
        ]).

-export_type([ implicit/0 ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%=======================================================================

-type implicit() ::
    implicit_key |
    explicit_key |
    explicit_value |
    sequence |
    false.

%=======================================================================

-spec detect(yaml_event:state(), block | flow) -> implicit().

detect(E, Context) when Context =:= block orelse Context =:= flow ->
    S = yaml_event:scan(E),
    detect_start(S, Context).

%-----------------------------------------------------------------------

detect_start(S, Context) ->
    case yaml_scan:grapheme(S) of
        $? ->
            detect_indicator(S, Context, explicit_key);

        $: ->
            detect_indicator(S, Context, explicit_value);

        $- ->
            detect_indicator(S, Context, sequence);

        _ ->
            detect_scalar(S, [Context])
    end.

%=======================================================================

detect_indicator(S0, Context, Indicator) ->
    S = yaml_scan:next(S0),
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            Indicator;

        break ->
            Indicator;

        G when ?IS_WHITE(G) ->
            Indicator;

        _ ->
            detect_plain(S, [Context])
    end.

%=======================================================================

detect_scalar(S, Stack) ->
    case yaml_scan:grapheme(S) of
        $' ->
            detect_single(yaml_scan:next(S), Stack);

        $" ->
            detect_double(yaml_scan:next(S), Stack);

        $* ->
            throw(alias);

        $& ->
            throw(anchor);

        $! ->
            throw(property);

        ${ ->
            detect_collection(yaml_scan:next(S), [$} | Stack]);

        $[ ->
            detect_collection(yaml_scan:next(S), [$} | Stack]);

        G when ?IS_RESERVED_INDICATOR(G) ->
            detect_plain_white(S, Stack);

        G when ?IS_PLAIN_ERROR_INDICATOR(G) ->
            detect_plain_white(S, Stack);

        G when ?IS_INDICATOR(G) ->
            detect_continue(S, Stack);

        G when ?IS_PRINTABLE(G) ->
            detect_plain_white(S, Stack);

        _ ->
            false
    end.

%=======================================================================

detect_continue(S, [Context]) ->
    detect_implicit(S, Context);
detect_continue(S, Stack) ->
    detect_collection(S, Stack).

%=======================================================================

detect_implicit(S, Context) ->
    case yaml_scan:grapheme(S) of
        G when is_atom(G) ->
            false;

        $# ->
            false;

        $: when Context =:= flow ->
            implicit_key;

        $: ->
            detect_implicit_colon(yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            detect_implicit(S, Context);

        _ ->
            false
    end.

%-----------------------------------------------------------------------

detect_implicit_colon(S) ->
    case yaml_scan:grapheme(S) of
        G when is_atom(G) ->
            implicit_key;

        G when ?IS_WHITE(G) ->
            implicit_key;

        _ ->
            false
    end.

%=======================================================================

detect_plain(S, Stack) ->
    case yaml_scan:grapheme(S) of
        G when is_atom(G) ->
            false;

        $: ->
            detect_plain_colon(S, yaml_scan:next(S), Stack);

        G when ?IS_WHITE(G) ->
            detect_plain_white(yaml_scan:next(S), Stack);

        G when ?IS_PRINTABLE(G) ->
            detect_plain(yaml_scan:next(S), Stack);

        _ ->
            false
    end.

%-----------------------------------------------------------------------

detect_plain_white(S, Stack) ->
    case yaml_scan:grapheme(S) of
        G when is_atom(G) ->
            false;

        $# ->
            false;

        $: ->
            detect_plain_colon(S, yaml_scan:next(S), Stack);

        G when ?IS_WHITE(G) ->
            detect_plain_white(yaml_scan:next(S), Stack);

        G when ?IS_PRINTABLE(G) ->
            detect_plain(yaml_scan:next(S), Stack);

        _ ->
            false
    end.

%-----------------------------------------------------------------------

detect_plain_colon(Colon, S, Stack) ->
    case yaml_scan:grapheme(S) of
        G when is_atom(G) ->
            detect_continue(Colon, Stack);

        $: ->
            detect_plain_colon(S, yaml_scan:next(S), Stack);

        G when ?IS_WHITE(G) ->
            detect_continue(Colon, Stack);

        G when ?IS_PRINTABLE(G) ->
            detect_plain(yaml_scan:next(S), Stack);

        _ ->
            false
    end.

%=======================================================================

detect_single(S, Stack) ->
    case yaml_scan:grapheme(S) of
        $' ->
            Z = yaml_scan:next(S),
            case yaml_scan:grapheme(Z) of
                $' ->
                    detect_single(yaml_scan:next(Z), Stack);

                _ ->
                    detect_continue(Z, Stack)
            end;

        G when ?IS_PRINTABLE(G) ->
            detect_single(yaml_scan:next(S), Stack);

        _ ->
            false
    end.

%=======================================================================

detect_double(S, Stack) ->
    case yaml_scan:grapheme(S) of
        $\" ->
            detect_continue(yaml_scan:next(S), Stack);

        $\\ ->
            Z = yaml_scan:next(S),
            case yaml_scan:grapheme(Z) of
                G when ?IS_PRINTABLE(G) ->
                    detect_double(yaml_scan:next(Z), Stack);

                _ ->
                    false
            end;

        G when ?IS_PRINTABLE(G) ->
            detect_double(yaml_scan:next(S), Stack);

        _ ->
            false
    end.

%=======================================================================

detect_collection(S, Stack) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_WHITE(G) ->
            detect_collection(yaml_scan:next(S), Stack);

        G when G =:= hd(Stack) ->
            detect_continue(yaml_scan:next(S), tl(Stack));

        $, ->
            detect_collection(yaml_scan:next(S), Stack);

        $? ->
            detect_mapping(yaml_scan:next(S), Stack);

        $: ->
            detect_mapping(yaml_scan:next(S), Stack);

        _ ->
            detect_scalar(S, Stack)
    end.

%=======================================================================

detect_mapping(S, Stack) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_WHITE(G) ->
            detect_collection(yaml_scan:next(S), Stack);

        G when ?IS_FLOW_INDICATOR(G) ->
            detect_continue(S, Stack);

        _ ->
            detect_plain(S, Stack)
    end.

