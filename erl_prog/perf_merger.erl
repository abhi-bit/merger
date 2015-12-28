-module(perf_merger).

-export([main/0]).

main() ->
    {ok, C} = merger:new(),
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    RandomStrings = [get_random_strings(200000, 20, AllowedChars)],
    io:format("~p~n", [erlang:localtime()]),
    C = bench_in(C, lists:nth(1, RandomStrings)),
    io:format("Queue size: ~p~n", [merger:size(C)]),
    bench_out(C),
    io:format("~p~n", [erlang:localtime()]).

get_random_strings(Count, Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    [get_random_string(Length, AllowedChars)] ++ Acc
                end, [], lists:seq(1, Count)).

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    [lists:nth(random:uniform(length(AllowedChars)),
                                AllowedChars)]  ++ Acc
    end, [], lists:seq(1, Length)).

bench_in(C, []) ->
    C;
bench_in(C, [H|T]) ->
    ok = merger:in(C, list_to_binary(H), "foo"),
    bench_in(C, T).

bench_out(C) ->
    case merger:size(C) > 0 of
    true ->
            try merger:out(C) of
                {ok, _Row} ->
                    % io:format("Output row: ~p~n", [Row]),
                    bench_out(C);
                {error, internal_error} ->
                    C
            catch
                _:_ -> C
            end;
    false ->
        C
    end.
