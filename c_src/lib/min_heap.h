#include <stdio.h>

#include "util.h"

#ifndef MINHEAP_H
#define MINHEAP_H

#define LCHILD(x) 2 * x + 1
#define RCHILD(x) 2 * x + 2
#define PARENT(x) (x - 1) / 2

typedef struct _sized_buf {
    char                *buf;
    size_t              size;
} sized_buf;

typedef struct node {
    sized_buf           *data;
    int                 i; //Index of array
    int                 j; //Index of next element to be stored from array
} node;

typedef struct {
    //min_heap_func_t     *func;
    //min_heap_cmp_func   *cmp;
    //min_heap_dtor_t     *dtor;

    int                 num_producers;
} min_heap_opts_t;

typedef struct min_heap {
    merger_item_t           *elem;
    //min_heap_cmp_func     *cmpfun;
    //min_heap_dtor_t       *dtor;

    int                     size;
    int                     num_producers;
} min_heap_t;

typedef enum collation_mode {
    collate_unicode,
    collate_raw
} collation_mode;

min_heap_t *min_heap_create(); //TODO: pass options to it

void    min_heap_destroy(min_heap_t *h);
void    min_heap_clear(min_heap_t *h, int root); //TODO: Fix implementation

int     min_heap_get(min_heap_t *h, void** value); //TODO: Fix implementation
int     min_heap_put(min_heap_t *h, merger_item_t *n);
void    min_heap_delete_min_node(min_heap_t *h);
void    min_heap_iter(min_heap_t *h, int index);
int     min_heap_size(min_heap_t *h);

void    min_heap_heapify(min_heap_t *hp, int index);
void    swap(merger_item_t *n1, merger_item_t *n2);

int     compare(const sized_buf *b1, const sized_buf *b2, collation_mode mode);
void    erl_nif_term_print(merger_item_t term);

#endif
