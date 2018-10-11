%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-record(scan,
        { b :: binary()
        , r :: pos_integer() % row
        , c :: pos_integer() % col
        }).

-record(event,
        { scan :: #scan{}
        , next :: fun((yaml_event:state()) -> yaml_event:next())
        }).

-define(COORD(S), {(S)#scan.r, (S)#scan.c}).

