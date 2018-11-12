%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_anchor).

-export([ alias/1
        , property/1
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-spec alias(yaml_event:state()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

alias(E) ->
    Construct = fun alias_construct/2,
    {T, S} = yaml_token:start(E, alias, Construct, #{}),
    $* = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    alias_name(yaml_token:skip(T, Z), Z).

%=======================================================================

-spec alias_construct(list(), map()) -> {yaml_event:event(), list()}.

alias_construct([{_, _, Name}], #{ from := From, thru := Thru }) ->
    Anchor = {alias, From, Thru, Name},
    Errors = [],
    {Anchor, Errors}.

%=======================================================================

alias_name(T, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            yaml_token:finish(T, S);

        break ->
            yaml_token:finish(T, S);

        G when ?IS_WHITE(G) ->
            yaml_token:finish(T, S);

        G when ?IS_FLOW_INDICATOR(G) ->
            yaml_token:finish(T, S);

        G when ?IS_PRINTABLE(G) ->
            alias_name(T, yaml_scan:next(S))
    end.

%=======================================================================

-spec property(yaml_event:state()) ->
    {yaml:anchor(), list(), yaml_event:state()}.

property(E) ->
    Construct = fun property_construct/2,
    {T, S} = yaml_token:start(E, anchor, Construct, #{}),
    $& = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    property_name(yaml_token:skip(T, Z), Z).

%=======================================================================

-spec property_construct(list(), map()) -> {yaml:anchor(), list()}.

property_construct([{_, _, Name}], #{ from := From, thru := Thru }) ->
    Anchor = {anchor, From, Thru, Name},
    Errors = [],
    {Anchor, Errors}.

%=======================================================================

property_name(T, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            yaml_token:finish(T, S);

        break ->
            yaml_token:finish(T, S);

        G when ?IS_WHITE(G) ->
            yaml_token:finish(T, S);

        G when ?IS_FLOW_INDICATOR(G) ->
            yaml_token:finish(T, S);

        G when ?IS_PRINTABLE(G) ->
            property_name(T, yaml_scan:next(S))
    end.

