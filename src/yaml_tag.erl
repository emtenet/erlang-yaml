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
    {T, S} = yaml_token:start(E, tag_property, Construct, #{}),
    $! = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    property_start(T, Z).

%=======================================================================

-spec property_construct(list(), map()) -> {yaml:tag(), list()}.

property_construct([{_, _, Binary}], #{ from := From, thru := Thru }) ->
    Tag = {Binary, From, Thru},
    Errors = [],
    {Tag, Errors}.

%=======================================================================

property_start(T, S) ->
    case yaml_scan:grapheme(S) of
        $! ->
            property_shorthand_suffix(T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

property_shorthand_suffix(T, S) ->
    case yaml_scan:grapheme(S) of
        $% ->
            throw(escape);

        $! ->
            throw(bang);

        G when ?IS_FLOW_INDICATOR(G) ->
            yaml_token:finish(T, S);

        G when ?IS_URI_CHAR(G) ->
            property_shorthand_suffix(T, yaml_scan:next(S));

        _ ->
            property_finish(T, S)
    end.

%-----------------------------------------------------------------------

property_finish(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            yaml_token:finish(T, S);

        G when ?IS_WHITE(G) ->
            yaml_token:finish(T, S);

        end_of_stream ->
            yaml_token:finish(T, S)
    end.

