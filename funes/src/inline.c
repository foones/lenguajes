#include "Fu.h"

RES *Inlined_Table;

RES *
fu_inlined(fun)
	RES *fun;
{
	RES *inl;

	/* primero veo si es una secuencia, hash, etc. */
	inl = fu_get_hash_eq(Inlined_Table, fun);

	if (inl == NIL)
		return NIL;
	else
		return CDR(inl);
}

__inline__ RES *
fu_inlined_set(fun, inl)
		RES *fun, *inl;
{
	return fu_set_hash_eq(Inlined_Table, fun, inl);
}

void
fu_init_inline()
{
	Inlined_Table = fu_make_hash(fu_int(INLINEDTABLE_SIZE));
}

