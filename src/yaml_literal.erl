%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_literal).

-export([ block/3
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-type style() :: literal | folded.

%=======================================================================

-spec block(yaml_event:state(), style(), yaml:props()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

block(Event, Style, Props0)
        when (Style =:= literal orelse Style =:= folded) andalso
             is_map(Props0) ->
    Props = Props0#{ style => Style },
    {T, S} = yaml_token:start(Event, Style, fun construct/2, Props),
    first(Style, T, S).

%=======================================================================

-spec construct(list(), map()) -> {yaml_event:event(), list()}.

construct(Ps, Props = #{ style := literal, chomp := Chomp }) ->
    Errors = errors(Ps, Props),
    #{ from := From, thru := Thru, anchor := Anchor, tag := Tag} = Props,
    Parts = to_literal(Ps, Chomp),
    Token = {literal, From, Thru, Anchor, Tag, Parts},
    {Token, Errors};
construct(Ps, Props = #{ style := folded, chomp := Chomp }) ->
    Errors = errors(Ps, Props),
    #{ from := From, thru := Thru, anchor := Anchor, tag := Tag} = Props,
    Parts = to_folded(Ps, Chomp),
    Token = {folded, From, Thru, Anchor, Tag, Parts},
    {Token, Errors}.

%=======================================================================

first(literal, T, S) ->
    $| = yaml_scan:grapheme(S),
    header(T, yaml_scan:next(S));
first(folded, T, S) ->
    $> = yaml_scan:grapheme(S),
    header(T, yaml_scan:next(S)).

%=======================================================================

header(T, S) ->
    case yaml_scan:grapheme(S) of
        $+ ->
            T1 = yaml_token:set(T, chomp, keep),
            header_chomp(T1, yaml_scan:next(S));

        $- ->
            T1 = yaml_token:set(T, chomp, strip),
            header_chomp(T1, yaml_scan:next(S));

        G when G >= $1 andalso G =< $9 ->
            T1 = yaml_token:set(T, indent, G - $0),
            header_indent(T1, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:set(T, chomp, clip),
            T2 = yaml_token:set(T1, indent, auto),
            header_after(T2, S)
    end.

%-----------------------------------------------------------------------

header_chomp(T, S) ->
    case yaml_scan:grapheme(S) of
        G when G >= $1 andalso G =< $9 ->
            T1 = yaml_token:set(T, indent, G - $0),
            header_after(T1, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:set(T, indent, auto),
            header_after(T1, S)
    end.

%-----------------------------------------------------------------------

header_indent(T, S) ->
    case yaml_scan:grapheme(S) of
        $+ ->
            T1 = yaml_token:set(T, chomp, keep),
            header_after(T1, yaml_scan:next(S));

        $- ->
            T1 = yaml_token:set(T, chomp, strip),
            header_after(T1, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:set(T, chomp, clip),
            header_after(T1, S)
    end.

%-----------------------------------------------------------------------

header_after(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            Z = yaml_scan:next(S),
            header_end(yaml_token:skip(T, S), Z);

        G when ?IS_WHITE(G) ->
            header_white(T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

header_white(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            Z = yaml_scan:next(S),
            header_end(yaml_token:skip(T, S), Z);

        G when ?IS_WHITE(G) ->
            header_white(T, yaml_scan:next(S));

        $# ->
            header_comment(T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

header_comment(T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            Z = yaml_scan:next(S),
            header_end(yaml_token:skip(T, S), Z);

        G when not is_atom(G) ->
            header_comment(T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

header_end(T, S) ->
    case yaml_token:get(T, indent) of
        {ok, auto} ->
            auto_break(T, S);

        {ok, Indent} when is_integer(Indent) ->
            Column = yaml_token:indent_to_column(T, Indent),
            T1 = yaml_token:set(T, column, Column),
            line_break(T1, Indent, break, S)
    end.

%=======================================================================

auto_break(T, S) ->
    case yaml_scan:end_of(S) of
        false ->
            auto_is_indented(T, S, S)
    end.

%-----------------------------------------------------------------------

auto_is_indented(T, Start, S) ->
    case yaml_token:is_indented(T, S) of
        true ->
            auto_indented(T, Start, 1, S);

        false ->
            auto_not_indented(T, Start, S)
    end.

%-----------------------------------------------------------------------

auto_not_indented(T, Start, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, break, S),
            auto_break(T1, yaml_scan:next(S));

        $\s ->
            auto_is_indented(T, Start, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:keep(T, break, Start),
            yaml_token:finish(T1, Start)
    end.

%-----------------------------------------------------------------------

auto_indented(T, Start, Indent, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, break, S),
            auto_break(T1, yaml_scan:next(S));

        $\s ->
            auto_indented(T, Start, Indent + 1, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            Column = yaml_token:indent_to_column(T, Indent),
            T1 = yaml_token:set(T, indent, Indent),
            T2 = yaml_token:set(T1, column, Column),
            T3 = yaml_token:keep(T2, break, S),
            line_first(T3, Indent, S)
    end.

%=======================================================================

line_break(T, Indent, Break, S) ->
    case yaml_scan:end_of(S) of
        false ->
            line_indent(T, Indent, Break, S, S)
    end.

%-----------------------------------------------------------------------

line_indent(T, Indent, Break, Start, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            T1 = yaml_token:keep(T, Break, Start),
            yaml_token:finish(T1, Start);

        break ->
            T1 = yaml_token:keep(T, Break, S),
            line_break(T1, Indent, break, yaml_scan:next(S));

        $\s ->
            line_is_indented(T, Indent, Break, Start, yaml_scan:next(S));

        $# ->
            T1 = yaml_token:keep(T, Break, S),
            comment_text(T1, Indent, yaml_scan:next(S));

        _ ->
            line_less_indented(T, Indent, Break, Start, S)
    end.

%-----------------------------------------------------------------------

line_is_indented(T, Indent, Break, Start, S) ->
    case yaml_token:is_indented(T, S, Indent) of
        true ->
            T1 = yaml_token:keep(T, Break, S),
            line_first(T1, Indent, S);

        false ->
            line_indent(T, Indent, Break, Start, S)
    end.

%-----------------------------------------------------------------------

line_less_indented(T, Indent, Break, Start, S) ->
    case yaml_token:is_indented(T, S) of
        true ->
            T1 = yaml_token:keep(T, Break, S),
            line_first(T1, Indent, S);

        false ->
            T1 = yaml_token:keep(T, Break, Start),
            yaml_token:finish(T1, Start)
    end.

%-----------------------------------------------------------------------

line_first(T, Indent, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, S),
            line_break(T1, Indent, break, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            line_text(T, Indent, empty, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            line_text(T, Indent, break, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

line_text(T, Indent, Break, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, S),
            line_break(T1, Indent, Break, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            line_text(T, Indent, Break, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            line_text(T, Indent, break, yaml_scan:next(S))
    end.

%=======================================================================

comment_break(T, Indent, S) ->
    case yaml_scan:end_of(S) of
        false ->
            comment_indent(T, Indent, S, S)
    end.

%-----------------------------------------------------------------------

comment_indent(T, Indent, Start, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            T1 = yaml_token:keep(T, comment, S),
            yaml_token:finish(T1, S);

        break ->
            T1 = yaml_token:keep(T, comment, S),
            comment_break(T1, Indent, yaml_scan:next(S));

        $\s ->
            comment_is_indented(T, Indent, Start, yaml_scan:next(S));

        $# ->
            T1 = yaml_token:keep(T, comment, S),
            comment_text(T1, Indent, yaml_scan:next(S));

        _ ->
            T1 = yaml_token:keep(T, comment, Start),
            yaml_token:finish(T1, Start)
    end.

%-----------------------------------------------------------------------

comment_is_indented(T, Indent, Start, S) ->
    case yaml_token:is_indented(T, S, Indent) of
        true ->
            T1 = yaml_token:keep(T, comment, S),
            comment_indented(T1, Indent, S);

        false ->
            comment_indent(T, Indent, Start, S)
    end.

%-----------------------------------------------------------------------

comment_indented(T, Indent, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, S),
            comment_break(T1, Indent, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            comment_indented(T, Indent, yaml_scan:next(S));

        $# ->
            comment_text(T, Indent, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            line_text(T, Indent, break, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

comment_text(T, Indent, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, S),
            comment_break(T1, Indent, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            comment_text(T, Indent, yaml_scan:next(S))
    end.

%=======================================================================

-define(BREAK(X), X = {_, _, break}).
-define(EMPTY(X), X = {_, _, empty}).
-define(COMMENT(X), X = {_, _, comment}).
-define(TEXT(X), X = {_, _, <<_/binary>>}).
-define(SPACED(X), X = {_, _, <<$\s, _/binary>>}).
-define(TABBED(X), X = {_, _, <<$\t, _/binary>>}).

to_literal([?COMMENT(_), ?TEXT(_) | Ps], Chomp) ->
    to_literal(Ps, Chomp);
to_literal([?COMMENT(_) | Ps], Chomp) ->
    to_literal(Ps, Chomp);
to_literal(Ps, keep) ->
    to_literal_folded(Ps, []);
to_literal(Ps, Chomp) ->
    to_literal_chomp(Ps, Chomp).

%-----------------------------------------------------------------------

to_literal_chomp([?BREAK(_), ?TEXT(T) | Rest], strip) ->
    to_literal_folded(Rest, [T]);
to_literal_chomp([?BREAK(B), ?TEXT(T) | Rest], clip) ->
    to_literal_folded(Rest, [T, to_break(B)]);
to_literal_chomp([?BREAK(_) | Rest], Chomp) ->
    to_literal_chomp(Rest, Chomp).

%-----------------------------------------------------------------------

to_literal_folded([?BREAK(_)], Acc) ->
    Acc;
to_literal_folded([?BREAK(B) | Rest], Acc) ->
    to_literal_folded(Rest, [to_break(B) | Acc]);
to_literal_folded([?EMPTY(B), ?TEXT(T) | Rest], Acc) ->
    to_literal_folded(Rest, [T, to_break(B) | Acc]);
to_literal_folded([?TEXT(T) | Rest], Acc) ->
    to_literal_folded(Rest, [T | Acc]).

%=======================================================================

to_folded([?COMMENT(_), ?TEXT(_) | Ps], Chomp) ->
    to_folded(Ps, Chomp);
to_folded([?COMMENT(_) | Ps], Chomp) ->
    to_folded(Ps, Chomp);
to_folded(Ps, keep) ->
    to_folded_keep(Ps, []);
to_folded(Ps, Chomp) ->
    to_folded_chomp(Ps, Chomp).

%-----------------------------------------------------------------------

to_folded_keep([?BREAK(B), ?SPACED(T) | Rest], Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_keep([?BREAK(B), ?TABBED(T) | Rest], Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_keep([?BREAK(B), ?TEXT(T) | Rest], Acc) ->
    to_folded_folded(Rest, [T, to_break(B) | Acc]);
to_folded_keep([?BREAK(B) | Rest], Acc) ->
    to_folded_keep(Rest, [to_break(B) | Acc]).

%-----------------------------------------------------------------------

to_folded_chomp([{F, T, break}, ?BREAK(_)], _) ->
    [{F, T, <<>>}];
to_folded_chomp([?BREAK(_), ?SPACED(T) | Rest], strip) ->
    to_folded_spaced(Rest, [T]);
to_folded_chomp([?BREAK(_), ?TABBED(T) | Rest], strip) ->
    to_folded_spaced(Rest, [T]);
to_folded_chomp([?BREAK(_), ?TEXT(T) | Rest], strip) ->
    to_folded_folded(Rest, [T]);
to_folded_chomp([?BREAK(B), ?SPACED(T) | Rest], clip) ->
    to_folded_spaced(Rest, [T, to_break(B)]);
to_folded_chomp([?BREAK(B), ?TABBED(T) | Rest], clip) ->
    to_folded_spaced(Rest, [T, to_break(B)]);
to_folded_chomp([?BREAK(B), ?TEXT(T) | Rest], clip) ->
    to_folded_folded(Rest, [T, to_break(B)]);
to_folded_chomp([?BREAK(_) | Rest], Chomp) ->
    to_folded_chomp(Rest, Chomp).

%-----------------------------------------------------------------------

to_folded_folded([?BREAK(_)], Acc) ->
    Acc;
to_folded_folded([?BREAK(B), ?SPACED(T) | Rest], Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_folded([?BREAK(B), ?TABBED(T) | Rest], Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_folded([?BREAK(B), ?TEXT(T) | Rest], Acc) ->
    to_folded_folded(Rest, [T, to_space(B) | Acc]);
to_folded_folded([?EMPTY(B), ?TEXT(T) | Rest], Acc) ->
    to_folded_break(Rest, folded, [T, to_space(B) | Acc]);
to_folded_folded([?BREAK(B) | Rest], Acc) ->
    to_folded_break(Rest, folded, [to_break(B) | Acc]).

%-----------------------------------------------------------------------

to_folded_spaced([?BREAK(_)], Acc) ->
    Acc;
to_folded_spaced([?BREAK(B), ?SPACED(T) | Rest], Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_spaced([?BREAK(B), ?TABBED(T) | Rest], Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_spaced([?BREAK(B), ?TEXT(T) | Rest], Acc) ->
    to_folded_folded(Rest, [T, to_break(B) | Acc]);
to_folded_spaced([?BREAK(B) | Rest], Acc) ->
    to_folded_break(Rest, spaced, [to_break(B) | Acc]).

%-----------------------------------------------------------------------

to_folded_break([?BREAK(_)], _, Acc) ->
    Acc;
to_folded_break([?BREAK(B), ?SPACED(T) | Rest], _, Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_break([?BREAK(B), ?TABBED(T) | Rest], _, Acc) ->
    to_folded_spaced(Rest, [T, to_break(B) | Acc]);
to_folded_break([?BREAK(B), ?TEXT(T) | Rest], spaced, Acc) ->
    to_folded_folded(Rest, [T, to_break(B) | Acc]);
to_folded_break([?BREAK(_), ?TEXT(T) | Rest], folded, Acc) ->
    to_folded_folded(Rest, [T | Acc]);
to_folded_break([?BREAK(B) | Rest], Was, Acc) ->
    to_folded_break(Rest, Was, [to_break(B) | Acc]).

%=======================================================================

to_break({F, T, break}) ->
    {F, T, <<"\n">>};
to_break({F, T, empty}) ->
    {F, T, <<"\n">>}.

%-----------------------------------------------------------------------

to_space({F, T, break}) ->
    {F, T, <<" ">>};
to_space({F, T, empty}) ->
    {F, T, <<" ">>}.


%=======================================================================

errors(Ps, #{ column := Column }) ->
    error_empty(Ps, Column, []);
errors(_, _) ->
    [].

%-----------------------------------------------------------------------

error_empty([?COMMENT(_), ?TEXT(_) | Ps], Column, Errors) ->
    error_empty(Ps, Column, Errors);
error_empty([?COMMENT(_) | Ps], Column, Errors) ->
    error_empty(Ps, Column, Errors);
error_empty([?BREAK(_) | Ps], Column, Errors) ->
    error_empty(Ps, Column, Errors);
error_empty(Ps, Column, Errors) ->
    error_rest(Ps, Column, Errors).

%-----------------------------------------------------------------------

error_rest([], _, Errors) ->
    lists:reverse(Errors);
error_rest([{{R, C}, _, <<_/binary>>} | Ps], Column, Errors) when C < Column ->
    Error = {less_indented, {R, C}, {R, Column}},
    error_rest(Ps, Column, [Error | Errors]);
error_rest([{_, {R, C}, break} | Ps], Column, Errors) when C > Column ->
    Error = {leading_spaces, {R, Column}, {R, C}},
    error_rest(Ps, Column, [Error | Errors]);
error_rest([_ | Ps], Column, Errors) ->
    error_rest(Ps, Column, Errors).

