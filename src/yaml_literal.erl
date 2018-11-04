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
    #{ from := From, thru := Thru, anchor := Anchor, tag := Tag} = Props,
    Parts = to_literal(Ps, Chomp),
    Token = {literal, From, Thru, Anchor, Tag, Parts},
    {Token, []};
construct(Ps, Props = #{ style := folded, chomp := Chomp }) ->
    #{ from := From, thru := Thru, anchor := Anchor, tag := Tag} = Props,
    Parts = to_folded(Ps, Chomp),
    Token = {folded, From, Thru, Anchor, Tag, Parts},
    {Token, []}.

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
            T1 = yaml_token:set(T, chomp, keep),
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
            header_end(yaml_token:skip(T, S), Z)
    end.

%-----------------------------------------------------------------------

header_end(T, S) ->
    case yaml_token:get(T, indent) of
        {ok, auto} ->
            auto_break(T, S);

        {ok, Indent} when is_integer(Indent) ->
            line_break(T, Indent, S)
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
            auto_indented(T, Start, 0, S);

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
        $\s ->
            auto_indented(T, Start, Indent + 1, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) andalso (Indent > 0) ->
            T1 = yaml_token:set(T, indent, Indent),
            T2 = yaml_token:keep(T1, break, S),
            line_text(T2, Indent, S)
    end.

%=======================================================================

line_break(T, Indent, S) ->
    case yaml_scan:end_of(S) of
        false ->
            line_indent(T, Indent, S, S)
    end.

%-----------------------------------------------------------------------

line_indent(T, Indent, Start, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            T1 = yaml_token:keep(T, break, Start),
            yaml_token:finish(T1, Start);

        G when ?IS_PRINTABLE(G) ->
            case yaml_token:is_indented(T, S, Indent) of
                false ->
                    T1 = yaml_token:keep(T, break, Start),
                    yaml_token:finish(T1, Start)
            end
    end.

%-----------------------------------------------------------------------

line_text(T, Indent, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            T1 = yaml_token:keep(T, S),
            line_break(T1, Indent, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            line_text(T, Indent, yaml_scan:next(S))
    end.

%=======================================================================

to_literal(Ps, keep) ->
    to_literal_folded(Ps, []);
to_literal(Ps, Chomp) ->
    to_literal_chomp(Ps, Chomp).

%-----------------------------------------------------------------------

to_literal_chomp(
          [     {_, _, break}
          , T = {_, _, Text}
          | Rest]
        , strip) when is_binary(Text) ->
    to_literal_folded(Rest, [T]);
to_literal_chomp(
          [ B = {_, _, break}
          , T = {_, _, Text}
          | Rest]
        , clip) when is_binary(Text) ->
    to_literal_folded(Rest, [T, to_break(B)]).

%-----------------------------------------------------------------------

to_literal_folded([    {_, _, break}], Acc) ->
    Acc;
to_literal_folded([B = {_, _, break} | Rest], Acc) ->
    to_literal_folded(Rest, [to_break(B) | Acc]);
to_literal_folded([T = {_, _, Text} | Rest], Acc) when is_binary(Text) ->
    to_literal_folded(Rest, [T | Acc]).

%=======================================================================

to_folded(Ps, keep) ->
    to_folded_folded(Ps, []);
to_folded(Ps, Chomp) ->
    to_folded_chomp(Ps, Chomp).

%-----------------------------------------------------------------------

to_folded_chomp(
          [     {F, T, break}
          ,     {_, _, break}
          ]
        , _) ->
    [{F, T, <<>>}];
to_folded_chomp(
          [     {_, _, break}
          , T = {_, _, Text}
          | Rest]
        , strip) when is_binary(Text) ->
    to_folded_folded(Rest, [T]);
to_folded_chomp(
          [ B = {_, _, break}
          , T = {_, _, Text}
          | Rest]
        , clip) when is_binary(Text) ->
    to_folded_folded(Rest, [T, to_break(B)]).

%-----------------------------------------------------------------------

to_folded_folded([    {_, _, break}], Acc) ->
    Acc;
to_folded_folded([B = {_, _, break} | Rest], Acc) ->
    to_folded_folded(Rest, [to_break(B) | Acc]);
to_folded_folded([T = {_, _, Text} | Rest], Acc) when is_binary(Text) ->
    to_folded_folded(Rest, [T | Acc]).

%=======================================================================

to_break({F, T, break}) ->
    {F, T, <<"\n">>}.

