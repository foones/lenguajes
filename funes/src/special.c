#include "Fu.h"

RES *Current_Compile_Env;

RES* special_form_if;
RES* special_form_quote;
RES* special_form_fun;
RES* special_form_do;
RES* special_form_def;
RES* special_form_set;
RES* special_form_dyn;
RES* special_form_dir;
RES* special_form_while;
RES* special_form_or;
RES* special_form_not;
RES* special_form_eq;
RES* special_form_car;
RES* special_form_cdr;
RES* special_form_thunk;
RES* special_form_call;
RES* special_form_set_cc;
RES* special_form_call_cc;
RES* special_form_spawn;
RES* special_form_sem_init;
RES* special_form_sem_p;
RES* special_form_sem_v;
RES* special_form_sem_val;
RES* special_form_sem_set;

RES *
fu_compile_env()
{
	return Current_Compile_Env;
}

RES *
fu_set_compile_env(RES *e)
{
	return Current_Compile_Env = e;
}

RES *
fu_compile_dir()
{
	return CAR(Current_Compile_Env);
}

RES *
fu_compile_value(name)
	RES *name;
{
	RES *r, *prev_env;

	prev_env = fu_env();
	fu_with_env(Current_Compile_Env);
	r = fu_get_env(name);
	fu_with_env(prev_env);
	if (CONS_P(r)) r = CDR(r);
	return r;
}

RES *
fu_set_compile_value(name, value)
	RES *name, *value;
{
	RES *r, *prev_env;

	prev_env = fu_env();
	fu_with_env(Current_Compile_Env);
	r = fu_def_env(name, value);
	fu_with_env(prev_env);
	return r;
}

#define SPECIAL_FORM(X)		{(X) = NEW(RES); TIPO(X) = tipo_special_form;}
#define BIND(SYMB, COSA)	fu_set_compile_value(fu_funes_symbol(SYMB), COSA)
void
fu_init_special()
{
	Current_Compile_Env = fu_cons(fu_make_hash(fu_int(ENV_SIZE)), NIL);

	SPECIAL_FORM(special_form_quote);
	SPECIAL_FORM(special_form_if);
	SPECIAL_FORM(special_form_fun);
	SPECIAL_FORM(special_form_do);
	SPECIAL_FORM(special_form_def);
	SPECIAL_FORM(special_form_set);
	SPECIAL_FORM(special_form_dyn);
	SPECIAL_FORM(special_form_while);
	SPECIAL_FORM(special_form_or);
	SPECIAL_FORM(special_form_thunk);
	SPECIAL_FORM(special_form_call);
	SPECIAL_FORM(special_form_set_cc);
	SPECIAL_FORM(special_form_call_cc);
	SPECIAL_FORM(special_form_spawn);
	SPECIAL_FORM(special_form_sem_init);
	SPECIAL_FORM(special_form_sem_p);
	SPECIAL_FORM(special_form_sem_v);
	SPECIAL_FORM(special_form_sem_val);
	SPECIAL_FORM(special_form_sem_set);

	BIND("quote", special_form_quote);
	BIND("if", special_form_if);
	BIND("fun", special_form_fun);
	BIND("do", special_form_do);
	BIND("def", special_form_def);
	BIND("set", special_form_set);
	BIND("dyn", special_form_dyn);
	BIND("while", special_form_while);
	BIND("or", special_form_or);
	BIND("thunk", special_form_thunk);
	BIND("call", special_form_call);
	BIND("set/cont", special_form_set_cc);
	BIND("call/cont", special_form_call_cc);
	BIND("spawn-closure", special_form_spawn);
	BIND("_sem_init", special_form_sem_init);
	BIND("_sem_p", special_form_sem_p);
	BIND("_sem_v", special_form_sem_v);
	BIND("_sem_val", special_form_sem_val);
	BIND("_sem_set", special_form_sem_set);

	/* para procedimientos inlined */
	SPECIAL_FORM(special_form_not);
	SPECIAL_FORM(special_form_eq);
	SPECIAL_FORM(special_form_car);
	SPECIAL_FORM(special_form_cdr);
	SPECIAL_FORM(special_form_dir);
}
#undef BIND
#undef SPECIAL_FORM
