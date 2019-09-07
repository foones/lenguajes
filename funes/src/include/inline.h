/* INLINEDTABLE_SIZE deberia ser un primo */
#define INLINEDTABLE_SIZE	73

RES *fu_inlined(RES *fun);
RES *fu_inlined_set(RES *fun, RES *inl);

void fu_init_inline();
