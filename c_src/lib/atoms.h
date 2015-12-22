#ifndef ATOMS_H
#define ATOMS_H


#include "erl_nif.h"


#define MERGER_ATOM_TABLE_MAP(XX)                       \
    XX(ALREADY_FINALIZED,   "already_finalized")        \
    XX(ERROR,               "error")                    \
    XX(INTERNAL_ERROR,      "internal_error")           \
    XX(NOT_FOUND,           "not_found")                \
    XX(OK,                  "ok")


#define MERGER_ATOM_EXTERN(n, v) extern ERL_NIF_TERM MERGER_ATOM_##n;
MERGER_ATOM_TABLE_MAP(MERGER_ATOM_EXTERN)
#undef MERGER_ATOM_EXTERN


void merger_init_atoms(ErlNifEnv* env);


#endif
