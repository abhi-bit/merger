-module(perf_merger).

-export([main/1, get_random_string/2]).

main(Count) ->
    % case whereis(merger) of
    %     undefined ->
    %         ok;
    %     Pid ->
    %         catch exit(Pid, kill)
    % end,
    % erlang:register(merger, self()),
    % eprof tracing
    % eprof:start(),
    % eprof:start_profiling([erlang:whereis(merger)]),

    % fprof tracing
    % fprof:trace([start, {file, "fprof.trace"}, verbose, {procs, all}]),

    % flame graph
    % code:add_pathz("/tmp"),

    % spawn(fun() ->
    %               io:format("Tracing started...\n"),
    %               eflame2:write_trace(global_calls_plus_new_procs, "/tmp/ef.test.0", all, 10*1000),
    %               io:format("Tracing finished!\n"),
    %               eflame2:format_trace("/tmp/ef.test.0", "/tmp/ef.test.0.out")
    %       end),
    {ok, C} = merger:new(),
    AllowedChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    Size = list_to_integer(atom_to_list(lists:nth(1, Count))),
    random:seed(erlang:now()),
    RandomStrings = [get_random_strings(Size, 20, AllowedChars)],
    Start = now_us(erlang:now()),
    C = bench_in(C, lists:nth(1, RandomStrings)),
    QueueSize = merger:size(C),
    io:format("Queue size: ~p~n", [QueueSize]),
    bench_out(C),
    End = now_us(erlang:now()),
    io:format("~p ms~n", [(End - Start) / 1000]).

    % fprof tracing
    % fprof:trace([stop]),
    % fprof:profile({file, "fprof.trace"}),
    % fprof:analyse([totals, {dest, "fprof.analysis"}]).

    % eprof:stop_profiling(),
    % eprof:analyze(total).

now_us({MegaSecs,Secs,MicroSecs}) ->
        (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

get_random_strings(Count, Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    ["[\"" ++ get_random_string(Length, AllowedChars) ++ "\"]"] ++ Acc
                end, [], lists:seq(1, Count)).

get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                    [lists:nth(random:uniform(length(AllowedChars)),
                                AllowedChars)]  ++ Acc
    end, [], lists:seq(1, Length)).

bench_in(C, []) ->
    C;
bench_in(C, [H|T]) ->
    % io:format("Inserting ~p~n", [H]),
    % ok = merger:in(C, H, "foo"),
    Ref = make_ref(),
    Pid = self(),
    ok = merger:in(C, H, {"foo", Ref, Pid}),
    bench_in(C, T).

bench_out(C) ->
    case merger:size(C) > 0 of
    true ->
            try merger:out(C) of
                {ok, _Row} ->
                    %io:format("Output row: ~p~n", [Row]),
                    bench_out(C);
                {error, internal_error} ->
                    C
            catch
                _:_ -> C
            end;
    false ->
        C
    end.
