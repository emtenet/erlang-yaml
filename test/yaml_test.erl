%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_test).
-include_lib("eunit/include/eunit.hrl").

-export([ manual/1 ]).

%=======================================================================

manual(File) ->
    {Dir, _} = yaml_test_case:list(),
    execute_test_case(Dir, File).

%=======================================================================

all_test_() ->
    {Dir, Files} = yaml_test_case:list(),
    lists:map(fun (File) -> generate_test_case(Dir, File) end, Files).

%-----------------------------------------------------------------------

generate_test_case(Dir, File) ->
    {File, {?LINE, fun () -> execute_test_case(Dir, File) end}}.

%-----------------------------------------------------------------------

execute_test_case(Dir, File) ->
    {ok, TestCase} = yaml_test_case:open(Dir, File),
    {ok, Source} = yaml_test_case:lookup(<<"source">>, TestCase),
    {ok, Events} = yaml_test_case:lookup_terms(<<"events">>, TestCase),
    State = yaml_event:start(Source),
    test_events(State, Events).

%-----------------------------------------------------------------------

test_events(State, []) ->
    case yaml_event:next(State) of
        end_of_events ->
            ok;

        Value ->
            erlang:error({assertEqual,
                [ {module, ?MODULE}
                , {line, ?LINE}
                , {expression, "yaml_event:next(State)"}
                , {expected, end_of_events}
                , {value, Value}
                ]})
    end;
test_events(State, [{error, Expect} | Events]) ->
    case yaml_event:next(State) of
        {error, Expect, Next} ->
            test_events(Next, Events);

        Value ->
            erlang:error({assertEqual,
                [ {module, ?MODULE}
                , {line, ?LINE}
                , {expression, "yaml_event:next(State)"}
                , {expected, {error, Expect, '_'}}
                , {value, Value}
                ]})
    end;
test_events(State, [Expect | Events]) ->
    case yaml_event:next(State) of
        {event, Expect, Next} ->
            test_events(Next, Events);

        Value ->
            erlang:error({assertEqual,
                [ {module, ?MODULE}
                , {line, ?LINE}
                , {expression, "yaml_event:next(State)"}
                , {expected, {event, Expect, '_'}}
                , {value, Value}
                ]})
    end.

