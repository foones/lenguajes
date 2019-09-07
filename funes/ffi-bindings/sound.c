#include "Fu.h"
#include "modsound.h"
#define VOID_RES(X)	X, T
#define FU_SINGLE_FLOAT(X)	fu_float((double) X)
#define VAL_SINGLE_FLOAT(X)	(float) VAL_FLOAT(X)
#define FU_DOUBLE_FLOAT(X)	fu_float(X)
#define VAL_DOUBLE_FLOAT(X)	VAL_FLOAT(X)

#define FU_BOOL(X)	(X) != NIL
#define VAL_BOOL(X)	((X) ? T : NIL)
#define FU_PTR(X)	((RES *)((long)(X) | 1))
#define VAL_PTR(X) ((void *)((long)(X) & ~1))

int safe_VAL_INT(RES *x) {
	if (!INT_P(x)) fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("foreign_binding -- se esperaba: 'int'"));
	return VAL_INT(x);
}

char safe_VAL_CHAR(RES *x) {
	if (!CHAR_P(x)) fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("foreign_binding -- se esperaba: 'char'"));
	return VAL_CHAR(x);
}

float safe_VAL_SINGLE_FLOAT(RES *x) {
	if (!FLOAT_P(x)) fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("foreign_binding -- se esperaba: 'float'"));
	return VAL_SINGLE_FLOAT(x);
}

double safe_VAL_DOUBLE_FLOAT(RES *x) {
	if (!FLOAT_P(x)) fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("foreign_binding -- se esperaba: 'double'"));
	return VAL_DOUBLE_FLOAT(x);
}

char * safe_VAL_STR(RES *x) {
	if (!STR_P(x)) fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("foreign_binding -- se esperaba: 'char *'"));
	return VAL_STR(x);
}


RES *aux1(RES *var2, RES *var3) {
	return VOID_RES(sound(safe_VAL_SINGLE_FLOAT(var2), safe_VAL_INT(var3)));
}

RES *fu_lib_init(RES *args) {
	RES *h = fu_make_hash(fu_int(11));
	fu_set_hash_eq(h, fu_local_symbol("snd"),
		fu_make_proc(aux1, 2, 0, 0));
	return h;
}
