#include "atoms.h"
#include "util.h"


#define MERGER_ATOM_DECL(n, v) ERL_NIF_TERM MERGER_ATOM_##n;
MERGER_ATOM_TABLE_MAP(MERGER_ATOM_DECL)
#undef MERGER_ATOM_DECL


#define MERGER_ATOM_INST(n, v) MERGER_ATOM_##n = merger_make_atom(env, v);
void
merger_init_atoms(ErlNifEnv* env)
{
    MERGER_ATOM_TABLE_MAP(MERGER_ATOM_INST);
}
#undef MERGER_ATOM_INST
