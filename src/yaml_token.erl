%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_token).

-export([ start/3
        , start/4
        % FINISH
        , finish/1
        , finish/2
        % STORE
        , set/3
        , get/2
        % ERROR
        , error_range/4
        % CONSUME
        , keep/2
        , keep/3
        , skip/2
        , error/3
        % INFO
        , consumed/2
        , indent_to_column/2
        , is_indented/2
        , is_indented/3
        ]).

-export_type([ state/0
             , construct/0
             ]).

-include("yaml_grapheme.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([ test_case/5
        , test_pre_condition/2
        ]).
-endif.

%=======================================================================

-record(token,
        { event :: yaml_event:state()
        , scan :: yaml_scan:state()
        , from :: yaml:coord()
        , parts :: list()
        , errors :: list()
        , context :: atom()
        , construct :: construct()
        , store :: map()
        }).

-opaque state() :: #token{}.

-type construct() :: fun((list(), map()) -> {yaml_event:event(), list()}).

%=======================================================================
% START
%=======================================================================

-spec start(yaml_event:state(), atom(), construct()) ->
    {state(), yaml_scan:state()}.

start(Event, Context, Build) ->
    start(Event, Context, Build, #{}).

%=======================================================================

-spec start(yaml_event:state(), atom(), construct(), map()) ->
    {state(), yaml_scan:state()}.

start(Event, Context, Construct, Store)
        when is_atom(Context) andalso
             is_function(Construct, 2) andalso
             is_map(Store) ->
    Scan = yaml_event:scan(Event),
    Token = #token
        { event = Event
        , scan = Scan
        , from = start_from(Scan, Store)
        , parts = []
        , errors = []
        , context = Context
        , construct = Construct
        , store = Store
        },
    {Token, Scan}.

%-----------------------------------------------------------------------

start_from(_, #{ from := From  }) ->
    From;
start_from(Scan, _) ->
    yaml_scan:coord(Scan).

%=======================================================================
% FINISH
%=======================================================================

-spec finish(state(), yaml_scan:state()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

finish(T = #token{}, End) ->
    finish(keep(T, End)).

%=======================================================================

-spec finish(state()) ->
    {yaml_event:event(), list(), yaml_event:state()}.

finish(T = #token{}) ->
    #token
        { event = Event
        , scan = End
        , from = From
        , parts = Parts
        , errors = Errors0
        , context = Context0
        , construct = Construct
        , store = Store
        } = T,
    Thru = yaml_scan:coord(End),
    {Token, Es} = Construct(Parts, Store#{ from => From, thru => Thru }),
    Context = {Context0, From , Thru},
    Errors = error_context(Errors0, error_context(Es, [], Context), Context),
    {Token, Errors, yaml_event:scan_to(Event, End)}.

%-----------------------------------------------------------------------

error_context([], Acc, _) ->
    Acc;
error_context([{E, From, Thru} | Es], Acc, Context) ->
    error_context(Es, [{E, From , Thru, Context} | Acc], Context).

%=======================================================================
% STORE
%=======================================================================

-spec set(state(), any(), any()) -> state().

set(SS = #token{store = Store = #{}}, Key, Value) ->
    SS#token{store = Store#{ Key => Value }}.

%=======================================================================

-spec get(state(), any()) -> {ok, any()} | false.

get(#token{store = Store}, Key) ->
    case Store of
        #{ Key := Value } ->
            {ok, Value};

        _ ->
            false
    end.

%=======================================================================
% ERROR
%=======================================================================

-spec error_range(state(), any(), From, Thru) -> state()
    when
        From :: yaml:coord() | yaml_scan:state(),
        Thru :: yaml:coord() | yaml_scan:state().

error_range(T = #token{errors = Es}, E, From = {_, _}, Thru = {_, _}) ->
    T#token{errors = [{E, From, Thru} | Es]};
error_range(T, E, From = {_, _}, Thru) ->
    error_range(T, E, From, yaml_scan:coord(Thru));
error_range(T, E, From, Thru) ->
    error_range(T, E, yaml_scan:coord(From), Thru).

%=======================================================================
% CONSUME
%=======================================================================

-spec keep(state(), yaml_scan:state()) -> state().

keep(T = #token{scan = End}, End) ->
    T;
keep(T = #token{parts = Ps}, End) ->
     P = consumed(T, End),
     T#token{scan = End, parts = [P | Ps]}.

%=======================================================================

-spec keep(state(), any(), yaml_scan:state()) -> state().

keep(T = #token{scan = Start, parts = Ps}, Const, End) ->
    From = yaml_scan:coord(Start),
    Thru = yaml_scan:coord(End),
    P = {From, Thru, Const},
    T#token{scan = End, parts = [P | Ps]}.

%=======================================================================

-spec skip(state(), yaml_scan:state()) -> state().

skip(T = #token{}, End) ->
    T#token{scan = End}.

%=======================================================================

-spec error(state(), any(), yaml_scan:state()) -> state().

error(T = #token{scan = Start, errors = Es}, Error, End) ->
    From = yaml_scan:coord(Start),
    Thru = yaml_scan:coord(End),
    E = {Error, From, Thru},
    T#token{errors = [E | Es], scan = End}.

%=======================================================================
% INFO
%=======================================================================

-spec consumed(state(), yaml_scan:state()) ->
        {yaml:coord(), yaml:coord(), binary()}.

consumed(#token{scan = Start}, End) ->
    Text = yaml_scan:consumed(Start, End),
    From = yaml_scan:coord(Start),
    Thru = yaml_scan:coord(End),
    {From, Thru, Text}.

%=======================================================================

-spec indent_to_column(state(), pos_integer()) -> pos_integer().

indent_to_column(#token{event = Event}, By) ->
    yaml_event:indent_to_column(Event, By).

%=======================================================================

-spec is_indented(state(), yaml_scan:state()) -> boolean().

is_indented(#token{event = Event}, Scan) ->
    yaml_event:is_indented(Event, Scan).

%=======================================================================

-spec is_indented(state(), yaml_scan:state(), pos_integer()) -> boolean().

is_indented(#token{event = Event}, Scan, By) ->
    yaml_event:is_indented(Event, Scan, By).

%=======================================================================

-ifdef(TEST).

test_case(Test, {Fr, Fc, Fb}, {Tr, Tc, Tb}, Token, Errors)
        when is_function(Test, 1) andalso
             is_list(Errors) ->
    From = yaml_scan:mock(Fb, Fr, Fc),
    Thru = yaml_scan:mock(Tb, Tr, Tc),
    Event = yaml_event:mock(From),
    RawResult = Test(Event),
    ?assertMatch({_, _, _}, RawResult),
    {TokenResult, ErrorsResult, ThruEvent} = RawResult,
    ThruResult = yaml_event:scan(ThruEvent),
    Result = {TokenResult, ErrorsResult, ThruResult},
    ?assertEqual({Token, Errors, Thru}, Result).

%-----------------------------------------------------------------------

test_pre_condition(Test, {Fr, Fc, Fb}) when is_function(Test, 1) ->
    From = yaml_scan:mock(Fb, Fr, Fc),
    Event = yaml_event:mock(From),
    ?assertError(pre_condition, Test(Event)).

-endif.

