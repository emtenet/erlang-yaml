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
            false
    end.

%=======================================================================

detect_plain_or_separate(S0, _Context, Separate) ->
    S = yaml_scan:next(S0),
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            Separate;

        break ->
            Separate;

        G when ?IS_WHITE(G) ->
            Separate;

        _ ->
            false
    end.

