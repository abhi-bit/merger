#!/bin/bash

erlc *.erl

for i in 1 10 100 1000 10000 100000 1000000 10000000
do
    ERL_LIBS="..:$ERL_LIBS" erl -noshell -s perf_couch_skew main $i -s init stop > $i.couch_skew.out
done

for i in 1 10 100 1000 10000 100000 1000000 10000000
do
    ERL_LIBS="..:$ERL_LIBS" erl -noshell -s perf_merger main $i -s init stop > $i.merger.out
done
