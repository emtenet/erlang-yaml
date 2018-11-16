%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml).

-export_type([ coord/0
             , anchor/0
             , maybe_anchor/0
             , tag/0
             , maybe_tag/0
             , props/0
             , version/0
             ]).

%=======================================================================

-type coord() :: {non_neg_integer(), non_neg_integer()}.

-type anchor() :: {anchor, coord(), coord(), binary()}.
-type maybe_anchor() :: anchor() | no_anchor.

-type tag() :: {tag, coord(), coord(), binary(), binary()}.
-type maybe_tag() :: tag() | no_tag.

-type props() ::
       #{ from := coord()
        , anchor := maybe_anchor()
        , tag := maybe_tag()
        }.

-type version() :: {non_neg_integer(), non_neg_integer()}.

%=======================================================================

