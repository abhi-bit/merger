#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "min_heap.h"
#include "erl_nif.h"

pthread_mutex_t lock;

int min_heap_size(min_heap_t *hp)
{
    return hp->size;
}

min_heap_t* min_heap_create()
{
    min_heap_t *hp;
    hp = malloc(sizeof(min_heap_t));
    if (!hp) return NULL;

    hp->size = 0;
    return hp;
}

void swap(merger_item_t *n1, merger_item_t *n2)
{
    merger_item_t temp = *n1;
    *n1 = *n2;
    *n2 = temp;
}

int less_fun(merger_item_t *elem1, merger_item_t *elem2)
{
    ErlNifBinary key1Bin, key2Bin;
    char *key1 = NULL, *key2 = NULL;
    sized_buf *buf1 = malloc(sizeof(sized_buf));
    sized_buf *buf2 = malloc(sizeof(sized_buf));
    int result = 0;

    if(!enif_inspect_binary(elem1->env, elem1->key, &key1Bin)) return 0;
    if(!enif_inspect_binary(elem2->env, elem2->key, &key2Bin)) return 0;

    key1 = (char *) enif_alloc(key1Bin.size + 2);
    key1[0] = '"';
    memcpy(key1 + 1, key1Bin.data, key1Bin.size);
    key1[key1Bin.size + 1] = '"';

    key2 = (char *) enif_alloc(key2Bin.size + 2);
    key2[0] ='"';
    memcpy(key2 + 1, key2Bin.data, key2Bin.size);
    key2[key2Bin.size + 1] = '"';

    printf("key 1: %.*s\n", (int) key1Bin.size, key1);
    printf("key 2: %.*s\n", (int) key2Bin.size, key2);

    buf1->buf = key1;
    buf1->size = key1Bin.size + 2;

    buf2->buf = key2;
    buf2->size = key2Bin.size + 2;

    result = CollateJSON((const sized_buf*) buf1,
                        (const sized_buf*) buf2,
                        kCollateJSON_Unicode);

    free(buf1);
    free(buf2);

    return result;
}

void min_heap_heapify(min_heap_t *hp, int i)
{
    int smallest = (LCHILD(i) < hp->size &&
                    less_fun(&hp->elem[LCHILD(i)], &hp->elem[i]))
                    ? LCHILD(i) : i;

    if (RCHILD(i) < hp->size &&
            less_fun(&hp->elem[RCHILD(i)], &hp->elem[smallest])) {
        smallest = RCHILD(i);
    }

    if (smallest != i) {
        swap(&(hp->elem[i]), &(hp->elem[smallest]));
        min_heap_heapify(hp, smallest);
    }
}

void erl_nif_term_print(merger_item_t term)
{
    ErlNifBinary keyBin, valBin;
    char *key = NULL, *val = NULL;

    if(!enif_inspect_binary(term.env, term.key, &keyBin)) return ;
    if(!enif_inspect_binary(term.env, term.val, &valBin)) return;

    key = (char *) enif_alloc(keyBin.size + 1);
    memcpy(key, keyBin.data, keyBin.size);
    key[keyBin.size] = '\0';

    val = (char *) enif_alloc(valBin.size + 1);
    memcpy(val, valBin.data, valBin.size);
    val[valBin.size] = '\0';

    printf("key: %.*s\n", (int) keyBin.size + 1, key);
    printf("value: %.*s\n", (int) valBin.size + 1, val);
}

int min_heap_put(min_heap_t *hp, merger_item_t *n) {
    if (hp->size) {
        hp->elem = realloc(hp->elem, (hp->size + 1) * sizeof(merger_item_t));
    } else {
        hp->elem = malloc(sizeof(merger_item_t));
    }

    merger_item_t item;
    item.key = n->key;
    item.val = n->val;

    int i = (hp->size)++;
    while (i && less_fun(&item, &hp->elem[PARENT(i)])) {
        hp->elem[i] = hp->elem[PARENT(i)];
        i = PARENT(i);
    }
    hp->elem[i] = item;

    return 1;
}

void min_heap_delete_min_node(min_heap_t *hp) {
    if (hp->size) {
        hp->elem[0] = hp->elem[--(hp->size)];
        hp->elem = realloc(hp->elem, hp->size * sizeof(merger_item_t));
        min_heap_heapify(hp, 0);
    } else {
        printf("\nMin Heap is empty!\n");
        free(hp->elem);
    }
}

int min_heap_get(min_heap_t *hp, void **value) {
    *value = NULL;

    if (min_heap_size(hp) > 0) {
        merger_item_t *n = malloc(sizeof(merger_item_t));
        pthread_mutex_lock(&lock);
        n->key = hp->elem[0].key;
        n->val = hp->elem[0].val;

        *value = n;

        min_heap_delete_min_node(hp);
        pthread_mutex_unlock(&lock);
        return 1;
    } else {
        return 0;
    }
}

void min_heap_iter(min_heap_t *hp, int i) {
    if (LCHILD(i) < hp->size) {
        min_heap_iter(hp, LCHILD(i));
    }
    if (RCHILD(i) < hp->size) {
        min_heap_iter(hp, RCHILD(i));
    }

    printf("%s\n", (const char *) (&hp->elem[i].key));
}
