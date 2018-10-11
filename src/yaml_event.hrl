%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-record(scan,
        { b :: binary()
        , r :: non_neg_integer() % row
        , c :: non_neg_integer() % col
        }).

-record(event,
        { scan :: #scan{}
        , next :: fun((yaml_event:state()) -> yaml_event:next())
        }).

