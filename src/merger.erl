-module(merger).

-on_load(init/0).

-export([
    new/0, new/1, new/2,
    out/1,
    peek/1,
    in/3,
    size/1,
    keys/1
]).

-include("merger.hrl").


init() ->
    NumScheds = erlang:system_info(schedulers),
    ?NIF_INIT(?MODULE, "merger", NumScheds).
    %?NIF_INIT(?MODULE, "merger", 0).


new() ->
    new([], []).

new(KVs) ->
    new(KVs, []).

new(KVs, Opts) ->
    case new_nif(Opts) of
        {ok, Heap} ->
            lists:foreach(fun({K, V}) -> ?MODULE:put(Heap, K, V) end, KVs),
            {ok, Heap};
        Else ->
            Else
    end.


new_nif(_Options) ->
    ?NOT_LOADED.


out(_Heap) ->
    ?NOT_LOADED.

peek(_Heap) ->
    ?NOT_LOADED.

in(_Heap, _Key, _Val) ->
    ?NOT_LOADED.


size(_Heap) ->
    ?NOT_LOADED.


keys(_Heap) ->
    ?NOT_LOADED.
