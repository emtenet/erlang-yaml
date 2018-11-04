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

-spec open(Dir, File) -> {ok, TestCase}
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
    parse_key_start([], Rest);
parse_test_case(Rest) ->
    parse_leading(Rest).

%-----------------------------------------------------------------------

parse_leading(<<"\n@@@", Rest/binary>>) ->
    parse_key_start([], Rest);
parse_leading(<<_, Rest/binary>>) ->
    parse_leading(Rest);
parse_leading(<<>>) ->
    {ok, []}.

%-----------------------------------------------------------------------

parse_key_start(Tests, <<" ", Key/binary>>) ->
    parse_key(Tests, Key, Key);
parse_key_start(Tests, Key) ->
    parse_key(Tests, Key, Key).

%-----------------------------------------------------------------------

parse_key(Tests, Key, <<>>) ->
    {ok, lists:reverse([{Key, <<>>} | Tests])};
parse_key(Tests, From, Thru = <<$\n, Rest/binary>>) ->
    Key = parse_range(From, Thru),
    parse_value_start(Tests, Key, Rest);
parse_key(Tests, From, <<_, Rest/binary>>) ->
    parse_key(Tests, From, Rest).

%-----------------------------------------------------------------------

parse_value_start(Tests, Key, <<"@@@", Rest/binary>>) ->
    parse_key_start([{Key, <<>>} | Tests], Rest);
parse_value_start(Tests, Key, <<"\n@@@", Rest/binary>>) ->
    parse_key_start([{Key, <<>>} | Tests], Rest);
parse_value_start(Tests, Key, <<$\n, Value/binary>>) ->
    parse_value(Tests, Key, [], Value, Value);
parse_value_start(Tests, Key, Value) ->
    parse_value(Tests, Key, [], Value, Value).

%-----------------------------------------------------------------------

parse_value(Tests, Key, Acc, From, <<>>) ->
    Value = parse_value_acc(Acc, From),
    {ok, lists:reverse([{Key, Value} | Tests])};
parse_value(Tests, Key, Acc, From, Thru = <<"\n@@@", Rest/binary>>) ->
    Binary = parse_range(From, Thru),
    Value = parse_value_acc(Acc, Binary),
    parse_key_start([{Key, Value} | Tests], Rest);
parse_value(Tests, Key, Acc, From, Thru = <<"@TAB@", Rest/binary>>) ->
    Size = erlang:size(From) - erlang:size(Thru),
    <<Binary:Size/binary, _/binary>> = From,
    parse_value(Tests, Key, [<<"\t">>, Binary | Acc], Rest, Rest);
parse_value(Tests, Key, Acc, From, Thru = <<"@SPACE@", Rest/binary>>) ->
    Binary = parse_range(From, Thru),
    parse_value(Tests, Key, [<<"\s">>, Binary | Acc], Rest, Rest);
parse_value(Tests, Key, Acc, From, <<_, Rest/binary>>) ->
    parse_value(Tests, Key, Acc, From, Rest).

%=======================================================================

parse_value_acc([], Binary) ->
    Binary;
parse_value_acc(Acc, Binary) ->
    iolist_to_binary(lists:reverse([Binary | Acc])).

%=======================================================================

parse_range(From, Thru) ->
    Size = erlang:size(From) - erlang:size(Thru),
    <<Binary:Size/binary, _/binary>> = From,
    Binary.

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

-spec consult(binary()) -> {ok, list(term())}.

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

%=======================================================================

-include_lib("eunit/include/eunit.hrl").

parse_empty_test() ->
    Source = <<>>,
    Expect = [],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_empty_leading_test() ->
    Source = <<"leading\n">>,
    Expect = [],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_leading_test() ->
    Source =
        <<"leading\n"
          "\n"
          "@@@ key\n"
          "\n"
          "value\n"
        >>,
    Expect =
        [ {<<"key">>, <<"value\n">>}
        ],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_keys_test() ->
    Source =
        <<"@@@ space before key\n"
          "@@@no space before key\n"
          "@@@ \n"
          "@@@\n"
          "@@@"
        >>,
    Expect =
        [ {<<"space before key">>, <<>>}
        , {<<"no space before key">>, <<>>}
        , {<<>>, <<>>}
        , {<<>>, <<>>}
        , {<<>>, <<>>}
        ],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_empty_values_test() ->
    Source =
        <<"@@@ zero blank lines\n"
          "@@@ one blank line\n"
          "\n"
          "@@@ two blank lines\n"
          "\n"
          "\n"
          "@@@"
        >>,
    Expect =
        [ {<<"zero blank lines">>, <<>>}
        , {<<"one blank line">>, <<>>}
        , {<<"two blank lines">>, <<>>}
        , {<<>>, <<>>}
        ],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_no_blank_line_after_key_test() ->
    Source =
        <<"@@@ key\n"
          "two\n"
          "lines\n"
          "\n"
          "@@@"
        >>,
    Expect =
        [ {<<"key">>, <<"two\nlines\n">>}
        , {<<>>, <<>>}
        ],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_no_blank_line_before_key_test() ->
    Source =
        <<"@@@ key\n"
          "\n"
          "two\n"
          "lines\n"
          "@@@"
        >>,
    Expect =
        [ {<<"key">>, <<"two\nlines">>}
        , {<<>>, <<>>}
        ],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

parse_escaped_test() ->
    Source =
        <<"@@@ key\n"
          "\n"
          "Lone escape @ in text\n"
          "Trailing @SPACE@\n"
          "Tab @TAB@ in text\n"
          "Ignored @tab@ and @space@ @ESCAPE@.\n"
          "\n"
          "@@@"
        >>,
    Value =
        <<"Lone escape @ in text\n"
          "Trailing \s\n"
          "Tab \t in text\n"
          "Ignored @tab@ and @space@ @ESCAPE@.\n"
        >>,
    Expect =
        [ {<<"key">>, Value}
        , {<<>>, <<>>}
        ],
    ?assertEqual({ok, Expect}, parse_test_case(Source)).

%-----------------------------------------------------------------------

consult_test() ->
    Source =
        <<"{start_of_stream, {1, 1}}.\n"
          "{end_of_stream, {1, 1}}.\n"
        >>,
    Expect =
        [ {start_of_stream, {1, 1}}
        , {end_of_stream, {1, 1}}
        ],
    ?assertEqual({ok, Expect}, consult(Source)).

