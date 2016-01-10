#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lib/atoms.h"
#include "lib/binomial_heap.h"
#include "lib/merger.h"
#include "utf8_collation/collate_json.h"
#include "lib/util.h"

typedef struct {
    struct heap     *hp;
    long int        size;
} merger_nif_heap_t;

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

static struct heap*
heap_create()
{
    struct heap *hp = malloc(sizeof(struct heap));
    heap_init(hp);
    return hp;
}

static int
less_fun(struct heap_node *_a, struct heap_node *_b)
{
    sized_buf *key1Buf = NULL, *key2Buf = NULL;
    merger_item_t *a, *b;
    a = (merger_item_t *) heap_node_value(_a);
    b = (merger_item_t *) heap_node_value(_b);
    int result = UINT_MAX;

    key1Buf = enif_alloc(sizeof(sized_buf));
    key2Buf = enif_alloc(sizeof(sized_buf));

    key1Buf->buf = (char *) a->key->data;
    key1Buf->size = a->key->size;

    key2Buf->buf = (char *) b->key->data;
    key2Buf->size = b->key->size;

    result = CollateJSON(key1Buf,
                        key2Buf,
                        kCollateJSON_Unicode);

    enif_free(key1Buf);
    enif_free(key2Buf);

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

    heap_get(mh->hp, &item);
    if(!item)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);

    if(!mh->size)
        return merger_make_error(env, MERGER_ATOM_INTERNAL_ERROR);
    mh->size--;

    ret = merger_make_ok(env, enif_make_tuple2(env,
                                               enif_make_binary(env, item->key),
                                               enif_make_binary(env, item->val)));

    //TODO: looks like key and val free calls are slowing down things
    //verify if enif_free_env will take care of it already
    enif_free(item->key);
    enif_free(item->val);
    enif_free_env(item->env);
    enif_free(item);

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
        return 0;
    }
    item->key = (ErlNifBinary *)enif_alloc(sizeof(ErlNifBinary));
    item->val = (ErlNifBinary *)enif_alloc(sizeof(ErlNifBinary));

    if (argc != 3)
        return enif_make_badarg(env);
    if (!enif_get_resource(env, argv[0], MERGER_NIF_RES, (void**) &mh))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(item->env, argv[1], item->key))
        return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(item->env, argv[2], item->val))
        return enif_make_badarg(env);

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
    {"in", 3, merger_nif_heap_put},
    {"size", 1, merger_nif_heap_size},
    {"keys", 1, merger_nif_heap_list}
};

ERL_NIF_INIT(merger, nif_funcs, &load, &reload, &upgrade, &unload);
