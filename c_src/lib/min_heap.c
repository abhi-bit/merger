#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "min_heap.h"
#include "erl_nif.h"

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

/*int compare(const sized_buf *buf1, const sized_buf *buf2, collation_mode mode)
{
    size_t length = (buf1->size < buf2->size) ? buf1->size : buf2->size;

    if (memcmp(buf1->buf, buf2->buf, length) < 0)
        return 1;
    else
        return 0;
}*/

void min_heap_heapify(min_heap_t *hp, int i)
{
    int smallest = (LCHILD(i) < hp->size &&
                    enif_compare(hp->elem[LCHILD(i)].key, hp->elem[i].key))
                    ? LCHILD(i) : i;

    if (RCHILD(i) < hp->size &&
            enif_compare(hp->elem[RCHILD(i)].key, hp->elem[smallest].key)) {
        smallest = RCHILD(i);
    }

    if (smallest != i) {
        swap(&(hp->elem[i]), &(hp->elem[smallest]));
        min_heap_heapify(hp, smallest);
    }
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
    while (i && enif_compare(item.key, hp->elem[PARENT(i)].key)) {
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

    merger_item_t *n = malloc(sizeof(merger_item_t));
    n->key = hp->elem[0].key;
    n->val = hp->elem[0].val;

    *value = n;

    min_heap_delete_min_node(hp);
    return 1;
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
