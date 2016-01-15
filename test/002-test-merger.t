#! /usr/bin/env escript

num_cycles() -> 5000.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),
    random:seed(erlang:now()),

    etap:plan(14),

    test_basic(),
    test_large_keys(),
    test_mapreduce_kv(),

    etap:end_tests().


test_basic() ->
    {ok, C} = merger:new(),
    etap:is(merger:in(C, "foo", "bar"), ok, "Stored a key"),
    etap:is(merger:in(C, "bar", "foo"), ok, "Stored a key"),
    etap:is(merger:size(C), 2, "Correct size for heap"),
    etap:is(merger:out(C), {ok, {<<"bar">>,"foo"}}, "Retrieved a key"),
    etap:is(merger:size(C), 1, "Correct size after delete"),
    etap:is(merger:out(C), {ok, {<<"foo">>,"bar"}}, "Retrieved another key"),
    etap:is(merger:size(C), 0, "Emptied the heap"),
    etap:is(merger:out(C), {error,heap_empty}, "Empty heap").

test_large_keys() ->
    % Test 4KB keys
    {ok, C} = merger:new(),
    random:seed(erlang:now()),
    Key1 = get_random_string(4096),
    etap:is(merger:in(C, Key1, "foo"), ok, "Stored a key"),
    etap:is(merger:size(C), 1, "Correct size for heap"),
    etap:is(merger:out(C), {ok, {list_to_binary(Key1),"foo"}}, "Retrieved a key").

test_mapreduce_kv() ->
    Key = <<"[\"pymc100\",\"VTKGNKUHMP\"]">>,
    Val = <<"{\"id\":\"pymc100\",\"key\":[\"pymc100\",\"VTKGNKUHMP\"],\"value\":[100,100,\"1-00005bf\"]}">>,
    {ok, C} = merger:new(),
    etap:is(merger:in(C, Key, Val), ok, "Stored a key"),
    etap:is(merger:size(C), 1, "Correct size for heap"),
    etap:is(merger:out(C), {ok,{<<"[\"pymc100\",\"VTKGNKUHMP\"]">>,
         <<"{\"id\":\"pymc100\",\"key\":[\"pymc100\",\"VTKGNKUHMP\"],\"value\":[100,100,\"1-00005bf\"]}">>}}, "Retrieved a key").

get_random_string(Length) ->
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    lists:foldl(fun(_, Acc) ->
                    [lists:nth(random:uniform(length(AllowedChars)),
                                AllowedChars)]  ++ Acc
                end, [], lists:seq(1, Length)).
