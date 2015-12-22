#ifndef MERGER_UTIL_H
#define MERGER_UTIL_H

#include "merger.h"
#include "merger_ftypes.h"

typedef struct _merger_item_t
{
    ErlNifEnv*              env;
    ERL_NIF_TERM            data;
} merger_item_t;


merger_item_t* merger_item_create(ERL_NIF_TERM val);
void merger_item_destroy(merger_item_t* item);

ERL_NIF_TERM merger_make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM merger_make_ok(ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM merger_make_error(ErlNifEnv* env, ERL_NIF_TERM data);

ErlNifResourceType* merger_init_res(
    ErlNifEnv* env, const char* name, merger_nif_dtor_t* dtor);
#endif
