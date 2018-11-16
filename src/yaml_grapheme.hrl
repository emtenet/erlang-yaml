%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-define(IS_PRINTABLE(G), (is_integer(G) orelse is_list(G))).

-define(IS_WHITE(G),
    (       (G =:= $\s)
     orelse (G =:= $\t)
    )).

-define(IS_DIGIT(G), (G >= $0) andalso (G =< $9)).

-define(IS_HEX_DIGIT(C),
    (       (C >= $0 andalso C =< $9)
     orelse (C >= $A andalso C =< $F)
     orelse (C >= $a andalso C =< $F)
    )).

-define(INDICATOR, "-?:,[]{}#&*!|>'\"%@`").

-define(IS_INDICATOR(G),
    (       (G =:= $-)
     orelse (G =:= $?)
     orelse (G =:= $:)
     orelse (G =:= $,)
     orelse (G =:= $[)
     orelse (G =:= $])
     orelse (G =:= ${)
     orelse (G =:= $})
     orelse (G =:= $#)
     orelse (G =:= $&)
     orelse (G =:= $*)
     orelse (G =:= $!)
     orelse (G =:= $|)
     orelse (G =:= $>)
     orelse (G =:= $')
     orelse (G =:= $\")
     orelse (G =:= $%)
     orelse (G =:= $@)
     orelse (G =:= $`)
    )).

-define(PLAIN_CHECK_INDICATOR, "-?:").

-define(IS_PLAIN_CHECK_INDICATOR(G),
    (       (G =:= $-)
     orelse (G =:= $?)
     orelse (G =:= $:)
    )).

-define(FLOW_INDICATOR, ",[]{}").

-define(IS_FLOW_INDICATOR(G),
    (       (G =:= $,)
     orelse (G =:= $[)
     orelse (G =:= $])
     orelse (G =:= ${)
     orelse (G =:= $})
    )).

-define(IS_RESERVED_INDICATOR(C),
    (       (C =:= $@)
     orelse (C =:= $`)
    )).

% Spec "Plain scalars must not begin with most indicators, ..."
% Allow these indicators but with an error
-define(IS_PLAIN_ERROR_INDICATOR(C),
    (       (C =:= $,)
     orelse (C =:= $])
     orelse (C =:= $})
     orelse (C =:= $%)
    )).

-define(IS_WORD_CHAR(C),
    (       (C >= $0 andalso C =< $9)
     orelse (C >= $A andalso C =< $Z)
     orelse (C >= $a andalso C =< $z)
     orelse (C =:= $-)
    )).

-define(IS_URI_CHAR(C),
    (       (C =:= $%)
     orelse (C >= $0 andalso C =< $9)
     orelse (C >= $A andalso C =< $Z)
     orelse (C >= $a andalso C =< $z)
     orelse (C =:= $-)
     orelse (C =:= $#)
     orelse (C =:= $;)
     orelse (C =:= $/)
     orelse (C =:= $?)
     orelse (C =:= $:)
     orelse (C =:= $@)
     orelse (C =:= $&)
     orelse (C =:= $=)
     orelse (C =:= $+)
     orelse (C =:= $$)
     orelse (C =:= $,)
     orelse (C =:= $_)
     orelse (C =:= $.)
     orelse (C =:= $!)
     orelse (C =:= $~)
     orelse (C =:= $*)
     orelse (C =:= $')
     orelse (C =:= $()
     orelse (C =:= $))
     orelse (C =:= $[)
     orelse (C =:= $])
    )).

