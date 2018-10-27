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
            detect_plain_or_separate(S, Context, explicit_key);

        $: ->
            detect_plain_or_separate(S, Context, explicit_value);

        $- ->
            detect_plain_or_separate(S, Context, sequence);

        _ ->
            detect_plain(S, [Context])
    end.

%=======================================================================

detect_plain_or_separate(S0, Context, Separate) ->
    S = yaml_scan:next(S0),
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            Separate;

        break ->
            Separate;

        G when ?IS_WHITE(G) ->
            Separate;

        _ ->
            detect_plain(S, [Context])
    end.

%=======================================================================

detect_continue(S, [Context]) ->
    detect_implicit(S, Context).

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

