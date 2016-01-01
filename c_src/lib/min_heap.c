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
    hp = enif_alloc(sizeof(min_heap_t));
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

void min_heap_heapify(min_heap_t *hp, int i)
{
    int smallest = (LCHILD(i) < hp->size && CollateJSON(hp->elem[LCHILD(i)].key,
                                                        hp->elem[i].key,
                                                        kCollateJSON_Unicode))
                                                        ? LCHILD(i) : i;

    if (RCHILD(i) < hp->size && CollateJSON(hp->elem[RCHILD(i)].key,
                                            hp->elem[smallest].key,
                                            kCollateJSON_Unicode)) {
                    smallest = RCHILD(i);
    }

    if (smallest != i) {
        swap(&(hp->elem[i]), &(hp->elem[smallest]));
        min_heap_heapify(hp, smallest);
    }
}

int min_heap_put(min_heap_t *hp, merger_item_t *val) {
    if (hp->size) {
        hp->elem = realloc(hp->elem, (hp->size + 1) * sizeof(merger_item_t));
    } else {
        hp->elem = malloc(sizeof(merger_item_t));
    }

    merger_item_t item;
    item.key = val->key;
    item.val = val->val;

    int i = (hp->size)++;
    while (i) {
        if (CollateJSON(item.key,
                            hp->elem[PARENT(i)].key,
                            kCollateJSON_Unicode)) {
            hp->elem[i] = hp->elem[PARENT(i)];
            i = PARENT(i);
        }
    }
    hp->elem[i] = item;

    return 1;
}

void min_heap_delete_min_node(min_heap_t *hp) {
    if (hp->size) {
        hp->elem[0] = hp->elem[--(hp->size)];
        //Leaking memory
        //hp->elem = realloc(hp->elem, hp->size * sizeof(merger_item_t));
        min_heap_heapify(hp, 0);
    } else {
        printf("\nMin Heap is empty!\n");
        free(hp->elem);
    }
}

int min_heap_get(min_heap_t *hp, merger_item_t **value) {
    *value = NULL;

    if (min_heap_size(hp) > 0) {
        merger_item_t *n = enif_alloc(sizeof(merger_item_t));
        n->key = hp->elem[0].key;
        n->val = hp->elem[0].val;

        *value = n;
        //free(&hp->elem[0]);
        min_heap_delete_min_node(hp);
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
