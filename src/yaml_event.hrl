%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
%=======================================================================

-record(event,
        { scan :: yaml_scan:state()
        , next :: fun((yaml_event:state()) -> yaml_event:next())
        , i :: integer()
        }).

