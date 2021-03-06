#ifndef MERGER_UTIL_H
#define MERGER_UTIL_H

#include <stdio.h>

#include "merger.h"
#include "merger_ftypes.h"

#define PRINT_DEBUG printf("%s %s %d\n", __FILE__, __FUNCTION__, __LINE__)

typedef struct _sized_buf {
    char                *buf;
    size_t              size;
} sized_buf;

typedef struct _merger_item_t
{
    ErlNifEnv           *env;
    ErlNifBinary        *key;
    ERL_NIF_TERM        val;
} merger_item_t;

void merger_item_create(ErlNifBinary key, ErlNifBinary val, merger_item_t **item);
void merger_item_destroy(merger_item_t* item);

ERL_NIF_TERM merger_make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM merger_make_ok(ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM merger_make_error(ErlNifEnv* env, ERL_NIF_TERM data);

ErlNifResourceType* merger_init_res(
    ErlNifEnv* env, const char* name, merger_nif_dtor_t* dtor);
#endif
