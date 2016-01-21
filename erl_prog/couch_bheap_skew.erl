-module(couch_bheap_skew).

-export([
	new/0,
	is_empty/1,
	in/3,
	insert_all/3,
	min/1,
	delete_min/2,
	out/2,
	order/1,
	size/1
]).

new() ->
	[].

is_empty([]) -> true;
is_empty(_) -> false.

size(T) ->
    lists:foldl(fun(X, Sum) -> {Sz, _, _, _, _} = X, Sz+Sum end, 0, T).

order({_Sz, R, _X, _Xs, _C}) -> R.

root({_Sz, _R, X, _Xs, _C}) -> X.

%% Insert a tree into binomial heap.
insert_tree(_LessFun, T, []) -> [T];
insert_tree(LessFun, T1, [T2 | Ts]) ->
	case order(T1) < order(T2) of
		true ->
			[T1 | [T2 | Ts]];
		false ->
			insert_tree(LessFun, link(LessFun, T1, T2), Ts)
	end.

%% join two heaps together.
join_trees(_LessFun, H, []) -> H;
join_trees(_LessFun, [], H) -> H;
join_trees(LessFun, [X|Xs] = H1, [Y|Ys] = H2) ->
	case order(X) < order(Y) of
		true ->
			%% Prepend X
			[X | join_trees(LessFun, Xs, H2)];
		false ->
			case order(Y) < order(X) of
				true ->
					%% Prepend Y
					[Y | join_trees(LessFun, H1, Ys)];
				false ->
					%% Equal orders
					insert_tree(LessFun, link(LessFun, X, Y), join_trees(LessFun, Xs, Ys))
			end
	end.

normalize(_LessFun, []) -> [];
normalize(LessFun, [Head | Tail]) ->
	insert_tree(LessFun, Head, Tail).

%% bulk insert
insert_all(_LessFun, [], Ts) ->
	Ts;
insert_all(LessFun, [X | Xs], Ts) ->
	insert_all(LessFun, Xs, in(X, LessFun, Ts)).

%% insert an element into heap
in(X, LessFun, [T1 | [T2 | Rest]] = Ts) ->
	%% io:format("~p ~p T1: ~p~n", [?MODULE, ?LINE, T1]),
	%% io:format("~p ~p T2: ~p~n", [?MODULE, ?LINE, T2]),
	%% io:format("~p ~p Rest: ~p~n", [?MODULE, ?LINE, Rest]),
	%% io:format("~p ~p Ts: ~p~n", [?MODULE, ?LINE, Ts]),
	%% io:format("~p ~p X: ~p LessFun: ~p~n", [?MODULE, ?LINE, X, LessFun]),
    case order(T1) == order(T2) of
		true ->
			[skew_link(X, LessFun, T1, T2) | Rest];
		false ->
			[{1, 0, X, [], []} | Ts]
	end;
in(X, _LessFun, Ts) ->
    %% ToAppend = {1, 0, X, [], []},
	%% io:format("~p ~p Ts: ~p ", [?MODULE, ?LINE, Ts]),
	%% io:format("~p ~p ToAppend: ~p~n", [?MODULE, ?LINE, ToAppend]),
    [{1, 0, X, [], []} | Ts].

%% Link T1 and T2 together with a new root X.
skew_link(X, LessFun, {_, _, _, _, _} = T1, {_, _, _, _, _} = T2) ->
	%% io:format("~p ~p T1: ~p~n", [?MODULE, ?LINE, T1]),
	%% io:format("~p ~p T2: ~p~n", [?MODULE, ?LINE, T2]),
	%% io:format("~p ~p X: ~p LessFun: ~p~n", [?MODULE, ?LINE, X, LessFun]),
	{Sz, R, Y, Ys, C} = link(LessFun, T1, T2),
	%% Data = {Sz, R, Y, Ys, C},
	%% io:format("~p ~p Data: ~p LessFun res: ~p~n", [?MODULE, ?LINE, Data, LessFun(X, Y)]),
    case LessFun(X, Y) of
        1 ->
			{Sz+1, R, Y, [X | Ys], C};
		-1 ->
			{Sz+1, R, X, [Y | Ys], C}
	end.

%% Link two trees together.
link(LessFun, {Sz1, R, X1, Xs1, C1} = T1, {Sz2, _, X2, Xs2, C2} = T2) ->
	%% io:format("~p ~p X1: ~p X2: ~p~n", [?MODULE, ?LINE, X1, X2]),
    %% io:format("LessFun res: ~p~n", [LessFun(X1, X2)]),
	case LessFun(X1, X2) of
        1 ->
			{Sz1+Sz2, R+1, X2, Xs2, [T1 | C2]};
		-1 ->
			{Sz1+Sz2, R+1, X1, Xs1, [T2 | C1]}
	end.

%% join two heaps.
join(LessFun, T1, T2) ->
	join_trees(LessFun, normalize(LessFun, T1), normalize(LessFun, T2)).

%% Remove minimum tree from a heap.
remove_min_tree([]) ->
	throw(empty);
remove_min_tree([T]) ->
	{T, []};
remove_min_tree([T|Ts]) ->
	{T1, Ts1} = remove_min_tree(Ts),
	case root(T) =< root(T1) of
		true ->
			{T, Ts};
		false ->
			{T1, [T | Ts1]}
	end.

%% Find minimum element.
min(Ts) ->
	{T, _} = remove_min_tree(Ts),
	root(T).

%% Delete minimum element.
delete_min(LessFun, Ts) ->
	{_Min, Deleted} = out(LessFun, Ts),
	Deleted.

%% Finds and deletes minimum element.
out(LessFun, Ts) ->
	{{_, _, X, Xs, Ts1}, Ts2} = remove_min_tree(Ts),
	{X, insert_all(LessFun, Xs, join(LessFun, lists:reverse(Ts1), Ts2))}.
