#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lib/atoms.h"
#include "lib/binomial_heap.h"
#include "lib/merger.h"
#include "lib/util.h"
#include "utf8_collation/couch_ejson_compare.h"

typedef struct {
    struct heap         *hp;
    long int            size;
} merger_nif_heap_t;

ErlNifResourceType* MERGER_NIF_RES;

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_NULL;
static ERL_NIF_TERM ATOM_ERROR;

static int load(ErlNifEnv*, void**, ERL_NIF_TERM);
static void unload(ErlNifEnv*, void*);
static __inline ERL_NIF_TERM merger_nif_new(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static __inline ERL_NIF_TERM merger_nif_heap_get(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static __inline ERL_NIF_TERM merger_nif_heap_put(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static __inline ERL_NIF_TERM merger_nif_heap_size(ErlNifEnv*, int, const ERL_NIF_TERM[]);
static __inline ERL_NIF_TERM merger_nif_heap_list(ErlNifEnv*, int, const ERL_NIF_TERM[]);
void merger_nif_heap_destroy(ErlNifEnv* env, void* obj);

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    UErrorCode status = U_ZERO_ERROR;
    int i, j;
    couch_ejson_global_ctx_t *globalCtx;
    merger_init_atoms(env);

    MERGER_NIF_RES = merger_init_res(
            env, "merger", &merger_nif_heap_destroy
        );

    if(MERGER_NIF_RES == NULL) {
        return 1;
    }

    globalCtx = (couch_ejson_global_ctx_t *) enif_alloc(sizeof(couch_ejson_global_ctx_t));

    if (globalCtx == NULL) {
        return 1;
    }

    if (!enif_get_int(env, info, &globalCtx->numCollators)) {
        return 2;
    }

    if (globalCtx->numCollators < 1) {
        return 3;
    }

    globalCtx->collMutex = enif_mutex_create("coll_mutex");

    if (globalCtx->collMutex == NULL) {
        return 4;
    }

    globalCtx->collators = (UCollator **) enif_alloc(sizeof(UCollator *) * globalCtx->numCollators);

    if (globalCtx->collators == NULL) {
        enif_mutex_destroy(globalCtx->collMutex);
        return 5;
    }

    for (i = 0; i < globalCtx->numCollators; i++) {
        globalCtx->collators[i] = ucol_open("", &status);

        if (U_FAILURE(status)) {
            for (j = 0; j < i; j++) {
                ucol_close(globalCtx->collators[j]);
            }

            enif_free(globalCtx->collators);
            enif_mutex_destroy(globalCtx->collMutex);

            return 5;
        }
    }

    globalCtx->collStackTop = 0;
    *priv = globalCtx;

    ATOM_TRUE = enif_make_atom(env, "true");
    ATOM_FALSE = enif_make_atom(env, "false");
    ATOM_NULL = enif_make_atom(env, "null");
    ATOM_ERROR = enif_make_atom(env, "error");

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
unload(ErlNifEnv* env, void* priv_data)
{
    couch_ejson_global_ctx_t *globalCtx = (couch_ejson_global_ctx_t *) priv_data;
    int i;

    for (i = 0; i < globalCtx->numCollators; i++) {
        ucol_close(globalCtx->collators[i]);
    }

    enif_free(globalCtx->collators);
    enif_mutex_destroy(globalCtx->collMutex);
    enif_free(globalCtx);
}

static struct heap*
heap_create()
{
    struct heap *hp = malloc(sizeof(struct heap));
    heap_init(hp);
    return hp;
}

static int
less_fun(struct heap *h, struct heap_node *_a, struct heap_node *_b)
{
    couch_ejson_ctx_t ctx;
    merger_item_t *a, *b;
    int result;
    a = (merger_item_t *) heap_node_value(_a);
    b = (merger_item_t *) heap_node_value(_b);

    char *keys;

    keys = (char *) enif_alloc(a->key->size + b->key->size + 2);
    if (keys == NULL) {
        return enif_make_tuple2(h->env,
                                ATOM_ERROR,
                                enif_make_atom(h->env, "mem_alloc_failure"));
    }

    memcpy(keys, a->key->data, a->key->size);
    keys[a->key->size] = '\0';
    memcpy(keys + a->key->size + 1, b->key->data, b->key->size);
    keys[a->key->size + 1 + b->key->size] = '\0';

    ctx.env = h->env;
    ctx.error = 0;
    ctx.errorMsg = NULL;
    ctx.coll = NULL;
    ctx.globalCtx = (couch_ejson_global_ctx_t *) enif_priv_data(h->env);

    result = less_json(keys, keys + a->key->size + 1, &ctx);
    release_coll(&ctx);
    enif_free(keys);

    return result <= 0;
}

static int
heap_put(struct heap *heap, merger_item_t *item)
{
    struct heap_node *hn = NULL;
    hn = malloc(sizeof(struct heap_node));
    if (!hn)
        return 0;
    heap_node_init(hn, item);
    heap_insert(less_fun, heap, hn);
    return 1;
}

static void
heap_get(struct heap *hp, merger_item_t **item)
{
    struct heap_node *hn;
    hn = heap_take(less_fun, hp);
    *item = heap_node_value(hn);
    free(hn);
}

static void
heap_peek(struct heap *hp, merger_item_t **item)
{
    struct heap_node *hn;
    hn = heap_sneak(less_fun, hp);
    *item = heap_node_value(hn);
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

    mh->hp = heap_create();
    if(mh->hp == NULL)
        goto error;
    mh->size = 0;
    mh->hp->env = env;

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
    ERL_NIF_TERM ret;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);

    if (mh->size <= 0)
        return enif_make_tuple2(env,
                                ATOM_ERROR,
                                enif_make_atom(env, "heap_empty"));

    heap_get(mh->hp, &item);
    if(!item)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);

    if(!mh->size)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);
    mh->size--;

    ret = merger_make_ok(env, enif_make_tuple2(env,
                                               enif_make_binary(env, item->key),
                                               item->val));

    //TODO: looks like key and val free calls are slowing down things
    //verify if enif_free_env will take care of it already
    enif_free(item->key);
    enif_free_env(item->env);
    enif_free(item);

    return ret;
}

ERL_NIF_TERM
merger_nif_heap_peek(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t* mh = NULL;
    merger_item_t* item = NULL;
    ERL_NIF_TERM ret;

    if(argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);

    if (mh->size <= 0)
        return enif_make_tuple2(env,
                                ATOM_ERROR,
                                enif_make_atom(env, "heap_empty"));

    heap_peek(mh->hp, &item);
    if(!item)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);

    ret = merger_make_ok(env, enif_make_tuple2(env,
                                               enif_make_binary(env, item->key),
                                               item->val));

    return ret;
}

ERL_NIF_TERM
merger_nif_heap_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t* mh = NULL;
    int ret;

    merger_item_t* item = (merger_item_t *) enif_alloc(sizeof(merger_item_t));
    if (!item)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);

    item->env = enif_alloc_env();
    if(!item->env) {
        enif_free(item);
        return enif_make_tuple2(env,
                                ATOM_ERROR,
                                enif_make_atom(env, "mem_alloc_failure"));
    }
    item->key = (ErlNifBinary *)enif_alloc(sizeof(ErlNifBinary));

    if (argc != 3)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(item->env, argv[1], item->key))
        return enif_make_badarg(env);
    item->val = argv[2];

    ret = heap_put(mh->hp, item);

    if(ret) {
        mh->size++;
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

    if (argc != 1)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);
    return enif_make_long(env, mh->size);
}


ERL_NIF_TERM
merger_nif_heap_list(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    merger_nif_heap_t *mh;

    if (argc != 1)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void **) &mh))
        return enif_make_badarg(env);

    //min_heap_iter(mh->hp, 0);

    return enif_make_atom(env, "ok");
}

void
merger_nif_heap_destroy(ErlNifEnv *env, void *obj)
{
    return;
}


static ErlNifFunc nif_funcs[] = {
    {"new_nif", 1, merger_nif_new},
    {"out", 1, merger_nif_heap_get},
    {"peek", 1, merger_nif_heap_peek},
    {"in", 3, merger_nif_heap_put},
    {"size", 1, merger_nif_heap_size},
    {"keys", 1, merger_nif_heap_list}
};

#ifndef _MSC_VER
#if defined (__SUNPRO_C) && (__SUNPRO_C >= 0x550)
__global
#elif defined __GNUC__
__attribute__ ((visibility("default")))
#endif

#endif
ERL_NIF_INIT(merger, nif_funcs, &load, &reload, &upgrade, &unload)
