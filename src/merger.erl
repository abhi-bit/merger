-module(merger).

-on_load(init/0).

-export([
    new/0, new/1, new/2,
    get/1,
    put/3,
    del/2,
    clear/1,
    size/1,
    keys/1,
    to_list/1
]).

-include("merger.hrl").


init() ->
    ?NIF_INIT(?MODULE, "merger", 0).


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


get(_Heap) ->
    ?NOT_LOADED.


put(_Heap, _Key, _Val) ->
    ?NOT_LOADED.


del(_Heap, _Key) ->
    ?NOT_LOADED.


clear(_Heap) ->
    ?NOT_LOADED.


size(_Heap) ->
    ?NOT_LOADED.


keys(_Heap) ->
    ?NOT_LOADED.


to_list(_Heap) ->
    ?NOT_LOADED.
