Streaming K-way merger queue
==

To build the project:

```
$ make
```

To run test cases:

```
$ make check
```

To compare Pure Erlang version vs NIF version:

Running NIF version:

```
$ cd merger
$ erlc *.erl; ERL_LIBS=”..:$ERL_LIBS” erl -noshell -s perf_merger main 1000000 -s init stop
```

In above case, 1M entries will be inserted into NIF based priority queue.
Each entry(Key-Value pair) has 20 bytes key and probably 10-20 value by default.
Can change the defaults by tweaking `perf_merger.erl` to something like 4KB key
and maybe 1MB value size.

Note: `perf_merger.erl` tries inserting string i.e. list of bytes. Binary values being
    along to NIF is way faster.

Running Pure Erlang version:

```
$ erlc *.erl; ERL_LIBS=”..:$ERL_LIBS” erl -noshell -s perf_couch_skew main 1000 -s init stop
```

Unfortunately, I wasn’t able to get `rebar` buildable nif ready for `LessFun`(couch_ejson_compare/less_fun),
so you could maybe use byte comparision in Erlang in this case.


