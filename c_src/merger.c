#include "lib/atoms.h"
#include "lib/merger.h"
#include "lib/min_heap.h"
#include "lib/util.h"

#include <string.h>

typedef struct {
    min_heap_t*     heap;
    ErlNifMutex*    lock;
} merger_nif_heap_t;


typedef struct {
    ErlNifEnv*      env;
    ERL_NIF_TERM          val;
} merger_nif_hash_iter_t;


ErlNifResourceType* MERGER_NIF_RES;


void merger_nif_heap_destroy(ErlNifEnv* env, void* obj);


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    merger_init_atoms(env);

    MERGER_NIF_RES = merger_init_res(
            env, "merger", &merger_nif_heap_destroy
        );

    if(MERGER_NIF_RES == NULL) {
        return 1;
    }

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}


ERL_NIF_TERM
merger_nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t* mh;
    ERL_NIF_TERM ret;

    mh = (merger_nif_heap_t*) enif_alloc_resource(
            MERGER_NIF_RES, sizeof(merger_nif_heap_t));

    if(mh == NULL)
        goto error;

    mh->heap = NULL;
    mh->lock = NULL;

    mh->heap = min_heap_create();
    if(mh->heap == NULL)
        goto error;

    mh->lock = enif_mutex_create("hp_lock");
    if(mh->lock == NULL)
        goto error;

    ret = enif_make_resource(env, mh);
    enif_release_resource(mh);
    return merger_make_ok(env, ret);

error:
    if(mh != NULL) {
        enif_release_resource(mh);
    }
    return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);
}


ERL_NIF_TERM
merger_nif_heap_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t* mh = NULL;
    merger_item_t* item = NULL;
    ErlNifBinary keyBin, valBin;
    ERL_NIF_TERM ret;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);

    if (!min_heap_get(mh->heap, &item))
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);

    if (!enif_alloc_binary(item->key->size, &keyBin))
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);
    memcpy(keyBin.data, item->key->buf, item->key->size);

    if (!enif_alloc_binary(item->val->size, &valBin))
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);
    memcpy(valBin.data, item->val->buf, item->val->size);

    ret = merger_make_ok(env, enif_make_tuple2(env,
                                               enif_make_binary(env, &keyBin),
                                               enif_make_binary(env, &valBin)));

    enif_release_binary(&keyBin);
    enif_release_binary(&valBin);
    enif_free(item);

    return ret;
}

ERL_NIF_TERM
merger_nif_heap_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t* mh = NULL;
    ErlNifBinary keyBin, valBin;
    int ret;

    merger_item_t* item = (merger_item_t *) enif_alloc(sizeof(merger_item_t));
    if (!item)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);

    item->env = enif_alloc_env();
    if(!item->env) {
        enif_free(item);
        return 0;
    }

    item->key = (sized_buf *) enif_alloc(sizeof(sized_buf));
    item->val = (sized_buf *) enif_alloc(sizeof(sized_buf));

    if (argc != 3)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[1], &keyBin))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env, argv[2], &valBin))
        return enif_make_badarg(env);

    merger_item_create(keyBin, valBin, &item);

    ret = min_heap_put(mh->heap, item);

    if(ret) {
        return MERGER_ATOM_OK;
    } else {
        merger_item_destroy(item);
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);
    }
}

ERL_NIF_TERM
merger_nif_heap_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t *mh;
    int size;

    if (argc != 1)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);

    size = min_heap_size(mh->heap);

    return merger_make_ok(env, enif_make_int(env, size));
}


ERL_NIF_TERM
merger_nif_heap_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t *mh;

    if (argc != 1)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void **) &mh))
        return enif_make_badarg(env);

    min_heap_iter(mh->heap, 0);

    return enif_make_atom(env, "ok");
}

void
merger_nif_heap_destroy(ErlNifEnv *env, void *obj)
{
    merger_nif_heap_t *mh = (merger_nif_heap_t *) obj;

    if (mh->lock != NULL)
        enif_mutex_destroy(mh->lock);
}


static ErlNifFunc nif_funcs[] = {
    {"new_nif", 1, merger_nif_new},
    {"out", 1, merger_nif_heap_get},
    {"in", 3, merger_nif_heap_put},
    {"size", 1, merger_nif_heap_size},
    {"keys", 1, merger_nif_heap_list}
};

ERL_NIF_INIT(merger, nif_funcs, &load, &reload, &upgrade, &unload);
