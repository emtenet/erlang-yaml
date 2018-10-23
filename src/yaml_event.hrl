%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-record(event,
        { scan :: yaml_scan:state()
        , next :: fun((yaml_event:state()) -> yaml_event:next())
        , i :: integer()
        }).

-type props() ::
       #{ from := yaml:coord()
        , anchor := term()
        , tag := term()
        }.

-define(IS_WHITE(C),
    (       (C =:= $\s)
     orelse (C =:= $\t)
    )).

-define(IS_PRINTABLE(C),
    (       (C =:= $\t)
     orelse ((C >= $\s) andalso (C =< $~))
     orelse (C =:= 16#85)
     orelse ((C >= 16#A0) andalso (C =< 16#D7FF))
     orelse ((C >= 16#E000) andalso (C =< 16#FFFD))
     orelse ((C >= 16#10000) andalso (C =< 16#10FFFF))
     orelse (hd(C))
    )).

-define(IS_FLOW_INDICATOR(C),
    (       (C =:= $,)
     orelse (C =:= $[)
     orelse (C =:= $])
     orelse (C =:= ${)
     orelse (C =:= $})
    )).

