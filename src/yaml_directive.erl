%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_directive).

-export([ document/1
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-spec document(yaml_event:state()) -> yaml_event:emit().

document(E) ->
    {T, S} = yaml_token:start(E, directive, fun construct/2, #{}),
    $% = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    name(yaml_token:skip(T, Z), Z).

%=======================================================================

-spec construct(list(), map()) -> {yaml_event:event(), list()}.

construct(Ws, #{ from := From, thru := Thru }) ->
    [{_, _, Name} | Words] = lists:reverse(Ws),
    Directive = {reserved_directive, From, Thru, Name, Words},
    Errors = [],
    {Directive, Errors}.

%=======================================================================

name(T, S) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_WHITE(G) ->
            name_end(T, S);

        G when ?IS_PRINTABLE(G) ->
            name(T, yaml_scan:next(S));

        _ ->
            name_end(T, S)
    end.

%-----------------------------------------------------------------------

name_end(T, S) ->
    case yaml_token:consumed(T, S) of
        {_, _, <<"YAML">>} ->
            throw(yaml);

        {_, _, <<"TAG">>} ->
            throw(tag);

        {From, Thru, <<>>} ->
            name_error(T, S, expecting_directive_name, From, Thru);

        {From, Thru, _} ->
            name_error(T, S, reserved_directive, From, Thru)
    end.

%-----------------------------------------------------------------------

name_error(T, S, Error, From, Thru) ->
    T1 = yaml_token:error_range(T, Error, From, Thru),
    T2 = yaml_token:keep(T1, S),
    reserved_space(T2, S, S).

%=======================================================================

reserved_space(T, White, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            yaml_token:finish(T, White);

        G when ?IS_WHITE(G) ->
            reserved_space(T, White, yaml_scan:next(S));

        $# ->
            yaml_token:finish(T, White);

        G when ?IS_PRINTABLE(G) ->
            reserved_word(yaml_token:skip(T, S), yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

reserved_word(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            yaml_token:finish(T, S);

        G when ?IS_WHITE(G) ->
            reserved_space(yaml_token:keep(T, S), S, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            reserved_word(T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------
