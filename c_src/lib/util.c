#include <stdio.h>
#include <string.h>

#include "atoms.h"
#include "util.h"

ERL_NIF_TERM
merger_make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;

    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1))
        return ret;

    return enif_make_atom(env, name);
}

ERL_NIF_TERM
merger_make_ok(ErlNifEnv* env, ERL_NIF_TERM data)
{
    return enif_make_tuple2(env, MERGER_ATOM_OK, data);
}

ERL_NIF_TERM
merger_make_error(ErlNifEnv* env, ERL_NIF_TERM data)
{
    return enif_make_tuple2(env, MERGER_ATOM_ERROR, data);
}

void
merger_item_create(ErlNifBinary keyBin, ErlNifBinary valBin, merger_item_t* *ret)
{
    char *key = NULL, *val = NULL;

    key = (char *) enif_alloc(keyBin.size + 2);
    key[0] = '"';
    memcpy(key + 1, keyBin.data, keyBin.size);
    key[keyBin.size + 1] = '"';

    val = (char *) enif_alloc(valBin.size + 2);
    val[0] ='"';
    memcpy(val + 1, valBin.data, valBin.size);
    val[valBin.size + 1] = '"';

    (*ret)->key->buf = key;
    (*ret)->key->size = keyBin.size + 2;

    (*ret)->val->buf = val;
    (*ret)->val->size = valBin.size + 2;
}

void
merger_item_destroy(merger_item_t* item)
{
    if(!item) return;
    if(item->env) enif_free_env(item->env);
    free(item);
}


ErlNifResourceType*
merger_init_res(ErlNifEnv* env, const char* name, merger_nif_dtor_t* dtor)
{
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    return enif_open_resource_type(env, NULL, name, dtor, flags, NULL);
}
