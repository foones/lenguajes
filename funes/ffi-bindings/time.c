#include "Fu.h"
#include "modtime.h"
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


RES *fu_MK_tm(struct tm *x) {
	return fu_vector("xxxxxxxxx",fu_int(x->tm_sec), fu_int(x->tm_min), fu_int(x->tm_hour), fu_int(x->tm_mday), fu_int(x->tm_mon), fu_int(x->tm_year), fu_int(x->tm_wday), fu_int(x->tm_yday), fu_int(x->tm_isdst));
}

struct tm *fu_AC_tm(RES *x) {
	struct tm *y;
	if (!VECTOR_NO_UNIFORME_P(x))		fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("foreign_binding -- se esperaba: 'struct tm'"));
	y = NEW(struct tm);
	y->tm_sec = safe_VAL_INT(fu_get_vector(x, fu_int(0)));
	y->tm_min = safe_VAL_INT(fu_get_vector(x, fu_int(1)));
	y->tm_hour = safe_VAL_INT(fu_get_vector(x, fu_int(2)));
	y->tm_mday = safe_VAL_INT(fu_get_vector(x, fu_int(3)));
	y->tm_mon = safe_VAL_INT(fu_get_vector(x, fu_int(4)));
	y->tm_year = safe_VAL_INT(fu_get_vector(x, fu_int(5)));
	y->tm_wday = safe_VAL_INT(fu_get_vector(x, fu_int(6)));
	y->tm_yday = safe_VAL_INT(fu_get_vector(x, fu_int(7)));
	y->tm_isdst = safe_VAL_INT(fu_get_vector(x, fu_int(8)));
	return y;
}

RES *aux1() {
	return fu_int(clock());
}

RES *aux2() {
	return FU_PTR(get_time());
}

RES *aux3(RES *var4) {
	return fu_MK_tm(localtime(VAL_PTR(var4)));
}

RES *aux5(RES *var6, RES *var7) {
	return fu_str(fmtime(safe_VAL_STR(var6), fu_AC_tm(var7)));
}

RES *fu_lib_init(RES *args) {
	RES *h = fu_make_hash(fu_int(11));
	fu_set_hash_eq(h, fu_local_symbol("clock"),
		fu_make_proc(aux1, 0, 0, 0));
	fu_set_hash_eq(h, fu_local_symbol("time"),
		fu_make_proc(aux2, 0, 0, 0));
	fu_set_hash_eq(h, fu_local_symbol("localtime"),
		fu_make_proc(aux3, 1, 0, 0));
	fu_set_hash_eq(h, fu_local_symbol("fmtime"),
		fu_make_proc(aux5, 2, 0, 0));
	fu_set_hash_eq(h, fu_local_symbol("ClocksPerSec"),
		fu_int(CLOCKS_PER_SEC));
	return h;
}
