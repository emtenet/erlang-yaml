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
        , error_at/4
        % CONSUME
        , keep/2
        , keep/3
        , skip/2
        , error/3
        % INFO
        , consumed/2
        , indented/2
        , indent_plus/2
        ]).

-export_type([ state/0
             , construct/0
             ]).

-include("yaml_private.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([ test_case/5
        , test_pre_condition/2
        ]).
-endif.

%=======================================================================

-record(token,
        { event :: #event{}
        , scan :: yaml_scan:state()
        , from :: yaml:coord()
        , parts :: list()
        , errors :: list()
        , context :: atom()
        , construct :: construct()
        , store :: map()
        }).

-opaque state() :: #token{}.

-type construct() :: fun((list(), map()) -> term()).

%=======================================================================
% START
%=======================================================================

-spec start(#event{}, atom(), construct()) -> state().

start(Event, Context, Build) ->
    start(Event, Context, Build, #{}).

%=======================================================================

-spec start(#event{}, atom(), construct(), map()) -> state().

start(Event = #event{scan = Scan}, Context, Construct, Store)
        when is_atom(Context) andalso
             is_function(Construct, 2) andalso
             is_map(Store) ->
    #token
        { event = Event
        , scan = Scan
        , from = yaml_scan:coord(Scan)
        , parts = []
        , errors = []
        , context = Context
        , construct = Construct
        , store = Store
        }.

%=======================================================================
% FINISH
%=======================================================================

-spec finish(state(), yaml_scan:state()) -> {term(), list(), #event{}}.

finish(T = #token{}, End) ->
    finish(keep(T, End)).

%=======================================================================

-spec finish(state()) -> {term(), list(), #event{}}.

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
    {Token, Errors, Event#event{scan = End}}.

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

-spec error_at(state(), any(), yaml:coord(), yaml:coord()) -> state().

error_at(T = #token{errors = Es}, E, From, Thru) ->
    T#token{errors = [{E, From, Thru} | Es]}.

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

-spec indented(state(), yaml_scan:state()) -> boolean().

indented(#token{event = #event{i = I}}, Scan) ->
    yaml_scan:indented(Scan, I).

%=======================================================================

-spec indent_plus(state(), integer()) -> integer().

indent_plus(#token{event = #event{i = N}}, M) ->
    % N was already incremented when {content, N, _} was pushed onto stack
    N + M - 1.

%=======================================================================

-ifdef(TEST).

test_case(Test, {Fr, Fc, Fb}, {Tr, Tc, Tb}, Token, Errors)
        when is_function(Test, 1) andalso
             is_list(Errors) ->
    From = yaml_scan:mock(Fb, Fr, Fc),
    Thru = yaml_scan:mock(Tb, Tr, Tc),
    Event = yaml_event:mock(From),
    RawResult = Test(Event),
    ?assertMatch({_, _, #event{}}, RawResult),
    {TokenResult, ErrorsResult, #event{scan = ThruResult}} = RawResult,
    Result = {TokenResult, ErrorsResult, ThruResult},
    ?assertEqual({Token, Errors, Thru}, Result).

%-----------------------------------------------------------------------

test_pre_condition(Test, {Fr, Fc, Fb}) when is_function(Test, 1) ->
    From = yaml_scan:mock(Fb, Fr, Fc),
    Event = yaml_event:mock(From),
    ?assertError(pre_condition, Test(Event)).

-endif.

