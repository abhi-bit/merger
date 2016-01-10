-module(perf_couch_skew).

-export([main/1]).

-record(state, {
    rows,
    less_fun,
    poped = []
}).

main(Count) ->
    %case whereis(merger) of
    %    undefined ->
    %        ok;
    %    Pid ->
    %        catch exit(Pid, kill)
    %end,
    %erlang:register(merger, self()),
    %% eprof tracing
    %eprof:start(),
    %eprof:start_profiling([erlang:whereis(merger)]),

    {ok, State} = init(),
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Size = list_to_integer(atom_to_list(lists:nth(1, Count))),
    RandomStrings = [get_random_strings(Size, 20, AllowedChars)],
    Start = now_us(erlang:now()),
    NewState = bench_in(State, lists:nth(1, RandomStrings)),
    io:format("Queue size: ~p~n", [couch_skew:size(NewState#state.rows)]),
    _NNewState = bench_out(NewState),
    End = now_us(erlang:now()),
    io:format("~p ms~n", [(End - Start) / 1000]).
    %eprof:stop_profiling(),
    %eprof:analyze(total).

now_us({MegaSecs,Secs,MicroSecs}) ->
        (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

init() ->
    State = #state{
        rows = couch_skew:new(),
        %%less_fun = fun({_, _, A}, {_, _, B}) -> A > B end
        less_fun = fun couch_ejson_compare:less/2
    },
    {ok, State}.

get_random_strings(Count, Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    ["[\"" ++ get_random_string(Length, AllowedChars) ++ "\"]"] ++ Acc
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
    Rows2 = couch_skew:in(H, LessFun, Rows),
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
        {MinRow, Rows2} = couch_skew:out(LessFun, Rows),
        Poped2 = [MinRow | Poped],
        NewState = State#state{
            rows = Rows2,
            poped = Poped2
        },
        bench_out(NewState);
    false ->
        State
    end.
