%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_double).

-export([ scalar/3
        ]).

-include("yaml_grapheme.hrl").

%=======================================================================

-type style() :: block | flow.

%=======================================================================

-spec scalar(yaml_event:state(), style(), yaml:props()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

scalar(Event, Style, Props)
        when (Style =:= block orelse Style =:= flow) andalso
             is_map(Props) ->
    {T, S} = yaml_token:start(Event, plain, fun construct/2, Props),
    first(Style, T, S).

%=======================================================================

-spec construct(list(), map()) -> {yaml_event:event(), list()}.

construct(Ps, #{ from := From, thru := Thru, anchor := Anchor, tag := Tag}) ->
    Folds = fold_by_space(Ps, []),
    Token = {double, From, Thru, Anchor, Tag, Folds},
    {Token, []}.

%-----------------------------------------------------------------------

fold_by_space([{_, _, escape} | Rest], Acc) ->
    fold_by_space(Rest, Acc);
fold_by_space(Rest = [{_, _, fold}, {_, _, fold} | _], Acc) ->
    fold_by_line(Rest, Acc);
fold_by_space([{F, T, fold} | Rest], Acc) ->
    fold_by_space(Rest, [{F, T, <<" ">>} | Acc]);
fold_by_space([Text | Rest], Acc) ->
    fold_by_space(Rest, [Text | Acc]);
fold_by_space([], Acc) ->
    Acc.

%-----------------------------------------------------------------------

fold_by_line([{F, T, fold} | Rest = [{_, _, fold} | _]], Acc) ->
    fold_by_line(Rest, [{F, T, <<"\n">>} | Acc]);
fold_by_line([{_, _, fold} | Rest], Acc) ->
    fold_by_space(Rest, Acc).

%=======================================================================

first(Style, T, S) ->
    $\" = yaml_scan:grapheme(S),
    Z = yaml_scan:next(S),
    text(Style, yaml_token:skip(T, Z), Z).

%=======================================================================

text(Style, T, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            fold(Style, fold, T, S, yaml_scan:next(S));

        $\" ->
            text_finish(Style, T, S);
            
        $\\ ->
            text_escape(Style, T, S, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            text_white(Style, T, S, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            text(Style, T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

text_white(Style, T, White, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            fold(Style, fold, T, White, yaml_scan:next(S));

        $\" ->
            text_finish(Style, T, S);
            
        $\\ ->
            text_escape(Style, T, S, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            text_white(Style, T, White, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            text(Style, T, yaml_scan:next(S))
    end.

%-----------------------------------------------------------------------

text_escape(Style, T, Escape, S) ->
    case yaml_scan:grapheme(S) of
        break ->
            fold(Style, escape, T, Escape, yaml_scan:next(S));

        G when G =:= $\s orelse G =:= $\t ->
            Z = yaml_scan:next(S),
            Kept = yaml_token:keep(T, Escape),
            Skip = yaml_token:skip(Kept, S),
            text(Style, Skip, Z)
    end.

%-----------------------------------------------------------------------

text_finish(_Style, T, S) ->
    Z = yaml_scan:next(S),
    Kept = yaml_token:keep(T, S),
    Skip = yaml_token:skip(Kept, Z),
    yaml_token:finish(Skip, Z).

%=======================================================================

fold(Style, Fold, T, White, S) ->
    Abort = {T, White},
    fold_break(Style, Abort, Fold, yaml_token:keep(T, White), White, S).

%-----------------------------------------------------------------------

fold_abort({T, White}) ->
    yaml_token:finish(T, White).

%-----------------------------------------------------------------------

fold_break(Style, Abort, Fold, T, White, S) ->
    case yaml_scan:end_of(S) of
        {_, _, _} ->
            fold_abort(Abort);

        false ->
            fold_indent(Style, Abort, Fold, T, White, S)
    end.

%-----------------------------------------------------------------------

fold_indent(Style, Abort, Fold, T, White, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            fold_abort(Abort);

        break ->
            Kept = yaml_token:keep(T, Fold, S),
            fold_break(Style, Abort, fold, Kept, S, yaml_scan:next(S));

        $\s ->
            fold_indent(Style, Abort, Fold, T, White, yaml_scan:next(S));

        $\t ->
            Indented = yaml_token:is_indented(T, S),
            fold_white(Style, Abort, Fold, T, White, Indented, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            case yaml_token:is_indented(T, S) of
                true ->
                    Kept = yaml_token:keep(T, Fold, S),
                    text(Style, Kept, S);

                false ->
                    fold_abort(Abort)
            end
    end.

%-----------------------------------------------------------------------

fold_white(Style, Abort, Fold, T, White, Indented, S) ->
    case yaml_scan:grapheme(S) of
        end_of_stream ->
            fold_abort(Abort);

        break ->
            Kept = yaml_token:keep(T, Fold, S),
            fold_break(Style, Abort, fold, Kept, S, yaml_scan:next(S));

        G when ?IS_WHITE(G) ->
            fold_white(Style, Abort, Fold, T, White, Indented, yaml_scan:next(S));

        G when ?IS_PRINTABLE(G) ->
            case Indented of
                true ->
                    Kept = yaml_token:keep(T, Fold, S),
                    text(Style, Kept, S);

                false ->
                    fold_abort(Abort)
            end
    end.

