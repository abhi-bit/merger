#! /usr/bin/env escript
% This file is part of Johnny released under the MIT license.
% See the LICENSE file for more information.

num_cycles() -> 5000.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),
    random:seed(erlang:now()),

    etap:plan(7),

    test_basic(),

    etap:end_tests().


test_basic() ->
    {ok, C} = merger:new(),
    etap:is(merger:put(C, <<"foo">>, "bar"), ok, "Stored a key"),
    etap:is(merger:put(C, <<"bar">>, "foo"), ok, "Stored a key"),
    etap:is(merger:size(C), {ok, 2}, "Correct size for heap"),
    etap:is(merger:get(C), {ok, {<<"bar">>,"foo"}}, "Retrieved a key"),
    etap:is(merger:size(C), {ok, 1}, "Correct size after delete"),
    etap:is(merger:get(C), {ok, {<<"foo">>,"bar"}}, "Retrieved another key"),
    etap:is(merger:size(C), {ok, 0}, "Emptied the heap").

