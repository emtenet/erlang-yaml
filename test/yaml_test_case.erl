%% vim: set ai et sw=4 sts=4:

%% See LICENSE for licensing information.
-module(yaml_test_case).

-export([ list/0
        , open/2
        , lookup/2
        , lookup_terms/2
        ]).

-export_type([ test_case/0 ]).

-opaque test_case() :: [{Key :: binary(), Value :: binary()}].

%=======================================================================

-spec list() -> {Dir, [File]}
    when
        Dir :: file:filename(),
        File :: file:filename().

list() ->
    TestDir = code:lib_dir(yaml, test),
    {ok, AllFiles} = file:list_dir(TestDir),
    IsTestCase = fun (File) -> ".test-case" == filename:extension(File) end,
    TestCases = lists:filter(IsTestCase, AllFiles),
    {TestDir, TestCases}.

%=======================================================================

-spec open(Dir, File) -> TestCase
    when
        Dir :: file:filename(),
        File :: file:filename(),
        TestCase :: test_case().

open(Dir, File) ->
    Path = filename:join(Dir, File),
    {ok, Binary} = file:read_file(Path),
    parse_test_case(Binary).

%=======================================================================

parse_test_case(<<"@@@", Rest/binary>>) ->
    parse_test_case_key_start([], Rest).

%-----------------------------------------------------------------------

parse_test_case_key_start(Tests, <<" ", Key/binary>>) ->
    parse_test_case_key(Tests, Key, Key);
parse_test_case_key_start(Tests, Key) ->
    parse_test_case_key(Tests, Key, Key).

%-----------------------------------------------------------------------

parse_test_case_key(Tests, Key, <<>>) ->
    {ok, lists:reverse([{Key, <<>>} | Tests])};
parse_test_case_key(Tests, From, Thru = <<$\n, Rest/binary>>) ->
    Size = erlang:size(From) - erlang:size(Thru),
    <<Key:Size/binary, _/binary>> = From,
    parse_test_case_value_start(Tests, Key, Rest);
parse_test_case_key(Tests, From, <<_, Rest/binary>>) ->
    parse_test_case_key(Tests, From, Rest).

%-----------------------------------------------------------------------

parse_test_case_value_start(Tests, Key, <<"\n@@@", Rest/binary>>) ->
    parse_test_case_key_start([{Key, <<>>} | Tests], Rest);
parse_test_case_value_start(Tests, Key, <<$\n, Value/binary>>) ->
    parse_test_case_value(Tests, Key, Value, Value);
parse_test_case_value_start(Tests, Key, Value) ->
    parse_test_case_value(Tests, Key, Value, Value).

%-----------------------------------------------------------------------

parse_test_case_value(Tests, Key, Value, <<>>) ->
    {ok, lists:reverse([{Key, Value} | Tests])};
parse_test_case_value(Tests, Key, From, Thru = <<"\n@@@", Rest/binary>>) ->
    Size = erlang:size(From) - erlang:size(Thru),
    <<Value:Size/binary, _/binary>> = From,
    parse_test_case_key_start([{Key, Value} | Tests], Rest);
parse_test_case_value(Tests, Key, From, <<_, Rest/binary>>) ->
    parse_test_case_value(Tests, Key, From, Rest).

%=======================================================================

-spec lookup(Key, TestCase) -> {ok, Value} | error
    when
        Key :: binary(),
        TestCase :: test_case(),
        Value :: binary().

lookup(Key, TestCase) ->
    case proplists:lookup(Key, TestCase) of
        {_, Value} ->
            {ok, Value};
        
        none ->
            error
    end.

%=======================================================================

-spec lookup_terms(Key, TestCase) -> {ok, Terms} | error
    when
        Key :: binary(),
        TestCase :: test_case(),
        Terms :: [term()].

lookup_terms(Key, TestCase) ->
    case lookup(Key, TestCase) of
        {_, Value} ->
            consult(Value);
        
        error ->
            error
    end.

%=======================================================================

-spec consult(binary()) -> list(term()).

consult(Binary) ->
    String = unicode:characters_to_list(Binary, utf8),
    consult(String, {1, 1}, []).

%-----------------------------------------------------------------------

consult(String, StartLocation, Events) ->
    case erl_scan:tokens([], String, StartLocation) of
        {done, {ok, Tokens, EndLocation}, RestOfString} ->
            {ok, Event} = erl_parse:parse_term(Tokens),
            consult(RestOfString, EndLocation, [Event | Events]);

        {more, More} ->
            {done, {eof, _}, eof} = erl_scan:tokens(More, eof, StartLocation),
            {ok, lists:reverse(Events)}
    end.

