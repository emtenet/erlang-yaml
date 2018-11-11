%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml).

-export_type([ coord/0
             , maybe_anchor/0
             , maybe_tag/0
             , props/0
             , version/0
             ]).

%=======================================================================

-type coord() :: {non_neg_integer(), non_neg_integer()}.
-type maybe_anchor() :: no_anchor.
-type maybe_tag() :: no_tag.

-type props() ::
       #{ from := coord()
        , anchor := maybe_anchor()
        , tag := maybe_tag()
        }.

-type version() :: {non_neg_integer(), non_neg_integer()}.

%=======================================================================

