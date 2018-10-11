%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml).

-export_type([ coord/0
             ]).

%=======================================================================

-type coord() :: {non_neg_integer(), non_neg_integer()}.

%=======================================================================

