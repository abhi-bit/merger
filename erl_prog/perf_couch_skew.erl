-module(perf_couch_skew).

-export([main/0, get_random_strings/3]).

-record(state, {
    rows,
    less_fun,
    poped = []
}).

main() ->
    {ok, State} = init(),
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    RandomStrings = [get_random_strings(1000000, 20, AllowedChars)],
    io:format("~p~n", [erlang:localtime()]),
    NewState = bench_in(State, lists:nth(1, RandomStrings)),
    io:format("Queue size: ~p~n", [couch_skew:size(NewState#state.rows)]),
    _NNewState = bench_out(NewState),
    io:format("~p~n", [erlang:localtime()]).

init() ->
    State = #state{
        rows = couch_skew:new(),
        less_fun = fun({_, _, A}, {_, _, B}) -> A > B end
    },
    {ok, State}.

get_random_strings(Count, Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    [get_random_string(Length, AllowedChars)] ++ Acc
                end, [], lists:seq(1, Count)).

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    [lists:nth(random:uniform(length(AllowedChars)),
                                AllowedChars)]  ++ Acc
    end, [], lists:seq(1, Length)).

bench_in(State, []) ->
    State;
bench_in(State, [H|T]) ->
    #state{
        less_fun = LessFun,
        rows = Rows,
        poped = _Poped
      } = State,
    Rows2 = couch_skew:in({pid, ref, H}, LessFun, Rows),
    NewState = State#state{
        rows = Rows2
    },
    bench_in(NewState, T).

bench_out(State) ->
    #state {
       less_fun = LessFun,
        rows = Rows,
        poped = Poped
    } = State,
    case couch_skew:size(Rows) > 0 of
    true ->
        {{Pid, Ref, MinRow}, Rows2} = couch_skew:out(LessFun, Rows),
        Poped2 = [{Pid, Ref, MinRow} | Poped],
        NewState = State#state{
            rows = Rows2,
            poped = Poped2
        },
        bench_out(NewState);
    false ->
        State
    end.
