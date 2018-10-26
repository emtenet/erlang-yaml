%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-type props() ::
       #{ from := yaml:coord()
        , anchor := term()
        , tag := term()
        }.

-define(IS_WHITE(G),
    (       (G =:= $\s)
     orelse (G =:= $\t)
    )).

-define(IS_PRINTABLE(G),
    (       (G =:= $\t)
     orelse ((G >= $\s) andalso (G =< $~))
     orelse (G =:= 16#85)
     orelse ((G >= 16#A0) andalso (G =< 16#D7FF))
     orelse ((G >= 16#E000) andalso (G =< 16#FFFD))
     orelse ((G >= 16#10000) andalso (G =< 16#10FFFF))
     orelse (hd(G))
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

