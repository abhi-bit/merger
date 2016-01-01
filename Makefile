CC=cc
CFLAGS=-Wall -O3 -fPIC
FLAGS=-bundle -flat_namespace -undefined suppress
ICU_CONFIG_EXECUTABLE=icu-config
ICU_INCLUDE_DIR=$(shell $(ICU_CONFIG_EXECUTABLE) --cppflags-searchpath)
ICU_LIB_SEARCHPATH=$(shell $(ICU_CONFIG_EXECUTABLE) --ldflags-searchpath)
ICU_LIBS=$(shell $(ICU_CONFIG_EXECUTABLE) --ldflags-libsonly)

DEPENDENCIES=c_src/merger.o c_src/utf8_collation/collate_json.o \
			 c_src/lib/atoms.o c_src/lib/min_heap.o c_src/lib/util.o

ERTS_HEADER=/Users/asingh/repo/cb/master/install/lib/erlang/erts-5.10.4.0.0.1/include
ERL_INTERFACE_LIBS=/Users/asingh/repo/cb/master/install/lib/erlang/lib/erl_interface-3.7.15/lib

TEST_MODULES = \
    test/etap.beam \
    test/gen_term.beam \
    test/util.beam

all: build

utf: merger collate_json atoms util
	$(CC) -O3 $(DEPENDENCIES) $(FLAGS) -L$(ERL_INTERFACE_LIBS) -lerl_interface \
		$(ICU_LIB_SEARCHPATH) -lei -licui18n -licuuc -licudata -o priv/merger.so
	erlc -o ebin src/*.erl
	
merger:
	$(CC) -c -g $(CFLAGS) -I$(ERTS_HEADER) c_src/merger.c -o c_src/merger.o

collate_json:
	$(CC) -c -g $(CFLAGS) -I$(ERTS_HEADER) $(ICU_INCLUDE_DIR) \
		c_src/utf8_collation/collate_json.c -o c_src/utf8_collation/collate_json.o

atoms:
	$(CC) -c -g $(CFLAGS) -I$(ERTS_HEADER) c_src/lib/atoms.c -o c_src/lib/atoms.o

min_heap:
	$(CC) -c -g $(CFLAGS) -I$(ERTS_HEADER) c_src/lib/min_heap.c -o c_src/lib/min_heap.o

util:
	$(CC) -c -g $(CFLAGS) -I$(ERTS_HEADER) c_src/lib/util.c -o c_src/lib/util.o

clean:
	./rebar clean
	rm -rf logs
	rm -rf test/*.beam
	rm -rf erl_prog/*.beam
	rm -rf c_src/utf8_collation/*.o

deps: ./deps/
	./rebar get-deps update-deps


build: deps
	./rebar compile


etap: $(TEST_MODULES)
	prove test/*.t


check: etap


%.beam: %.erl
	erlc -o test/ $<
