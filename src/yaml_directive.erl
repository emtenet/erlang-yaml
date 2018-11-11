%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_directive).

-export([ document/1
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-spec document(yaml_event:state()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

document(E) ->
    {T, S} = yaml_token:start(E, directive, fun construct/2, #{}),
    $% = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    name(yaml_token:skip(T, Z), Z).

%=======================================================================

-spec construct(list(), map()) -> {yaml_event:event(), list()}.

construct([{_, _, yaml}], Info) ->
    {construct_yaml({1, 2}, Info), []};
construct([V = {_, _, Version}, {_, _, yaml}], Info) ->
    {construct_yaml(Version, Info), construct_yaml_errors(V)};
construct(Ws, #{ from := From, thru := Thru }) ->
    [{_, _, Name} | Words] = lists:reverse(Ws),
    Directive = {reserved_directive, From, Thru, Name, Words},
    Errors = [],
    {Directive, Errors}.

%-----------------------------------------------------------------------

construct_yaml(Version, #{ from := From, thru := Thru }) ->
    {yaml_directive, From, Thru, Version}.

%-----------------------------------------------------------------------

construct_yaml_errors({_, _, {1, 2}}) ->
    [];
construct_yaml_errors({F, T, _}) ->
    [{expecting_yaml_version_1_2, F, T}].

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
            yaml_space(yaml_token:keep(T, yaml, S), S, S); 

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

%=======================================================================

yaml_space(T, White, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            yaml_expecting_version(T, White);

        break ->
            yaml_expecting_version(T, White);

        G when ?IS_WHITE(G) ->
            yaml_space(T, White, yaml_scan:next(S));

        $# ->
            yaml_expecting_version(T, White);

        G when ?IS_DIGIT(G) ->
            yaml_dot(T, G - $0, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            yaml_unknown_version(yaml_token:skip(T, S), yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

yaml_dot(T, Major, S) ->
    case yaml_scan:grapheme(S) of
        $. ->
            yaml_maybe_minor(T, Major, yaml_scan:next(S));

        G when ?IS_DIGIT(G) ->
            yaml_dot(T, (10 * Major) + (G - $0), yaml_scan:next(S));

        _ ->
            yaml_unknown_version(T, S)
    end.

%-----------------------------------------------------------------------

yaml_maybe_minor(T, Major, S) ->
    case yaml_scan:grapheme(S) of
        G when ?IS_DIGIT(G) ->
            yaml_minor(T, Major, G - $0, yaml_scan:next(S));

        _ ->
            yaml_unknown_version(T, S)
    end.

%-----------------------------------------------------------------------

yaml_minor(T, Major, Minor, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            yaml_version(T, {Major, Minor}, S);

        break ->
            yaml_version(T, {Major, Minor}, S);

        G when ?IS_WHITE(G) ->
            yaml_version(T, {Major, Minor}, S, yaml_scan:next(S));

        G when ?IS_DIGIT(G) ->
            yaml_minor(T, Major, (10 * Minor) + (G - $0), yaml_scan:next(S));

        _ ->
            yaml_unknown_version(T, S)
    end.

%-----------------------------------------------------------------------

yaml_version(T, Version, White) ->
    T1 = yaml_token:keep(T, Version, White),
    yaml_token:finish(T1, White).

%-----------------------------------------------------------------------

yaml_version(T, Version, White, S) ->
    T1 = yaml_token:keep(T, Version, White),
    extra_space(T1, White, S).

%-----------------------------------------------------------------------

yaml_unknown_version(T, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            yaml_bad_version(T, S);

        break ->
            yaml_bad_version(T, S);

        G when ?IS_WHITE(G) ->
            yaml_bad_version(T, S, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            yaml_unknown_version(T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

yaml_expecting_version(T, White) ->
    T1 = yaml_token:error(T, expecting_yaml_version, White),
    yaml_token:finish(T1, White).

%-----------------------------------------------------------------------

yaml_bad_version(T, White) ->
    T1 = yaml_token:error(T, bad_yaml_version, White),
    yaml_token:finish(T1, White).

%-----------------------------------------------------------------------

yaml_bad_version(T, White, S) ->
    T1 = yaml_token:error(T, bad_yaml_version, White),
    extra_space(T1, White, S).

%=======================================================================

extra_space(T, White, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            yaml_token:finish(T, White);

        G when ?IS_WHITE(G) ->
            extra_space(T, White, yaml_scan:next(S));

        $# ->
            yaml_token:finish(T, White);

        G when ?IS_PRINTABLE(G) ->
            extra_word(yaml_token:skip(T, S), yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

extra_word(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            yaml_token:finish(T, S);

        G when ?IS_WHITE(G) ->
            T1 = yaml_token:error(T, bad_directive, S),
            extra_space(T1, S, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            extra_word(T, yaml_scan:next(S))
    end.

