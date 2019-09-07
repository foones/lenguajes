#include "Fu.h"

RES *
fu_macro_defmacro(nombre, llist, body)
	RES *nombre, *llist, *body;
{
	RES *macro;

	if (!CONS_P(body) && body != NIL)
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("macro -- cuerpo deforme"));

	macro = fu_eval(fu_make_list("xxX", fu_funes_symbol("fun"), llist, body));
	CLOSURE_MACRO_SET(VAL_CLOSURE(macro));

	fu_set_compile_value(nombre, macro);
	return fu_make_list("xxx", fu_funes_symbol("do"),
			fu_make_list("xxx", fu_funes_symbol("def"), nombre, macro),
			fu_make_list("xx", fu_funes_symbol("quote"), nombre));
}

RES *
fu_macro_def_symbol_macro(nombre, body)
	RES *nombre, *body;
{
	RES *macro;

	if (!CONS_P(body) && body != NIL)
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("symbol-macro -- cuerpo deforme"));

	macro = fu_eval(fu_make_list("xxX", fu_funes_symbol("fun"), NIL, body));
	CLOSURE_SYMBOL_MACRO_SET(VAL_CLOSURE(macro));

	fu_set_compile_value(nombre, macro);

	return macro;
}

RES *
fu_macro_to(var, rango, cuerpo)
	RES *var, *rango, *cuerpo;
{
	RES *inicio, *final, *step = fu_int(1);

	if (!CONS_P(rango))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("to -- el rango debe ser una lista"));

	if (CONS_P(CDR(rango))) {
		inicio = CAR(rango);
		final = CADR(rango);
		if (CONS_P(CDDR(rango)))
			step = CADDR(rango);
	} else {
		inicio = fu_int(0);
		final = CAR(rango);
	}

	/*
	 * (let ((<var> <inicio>))
	 *   (while (< <var> <final>)
	 *     <cuerpo>
	 *     (set <var> (+ <var> 1))))
	 *
	 */
	return fu_make_list("xxx",
			fu_funes_symbol("let"),
			fu_cons(fu_make_list("xx", var, inicio), NIL),
			fu_make_list("xxxx",
				fu_funes_symbol("while"),
				fu_make_list("xxx",
					fu_funes_symbol("<"), var, final),
				fu_cons(fu_funes_symbol("do"), cuerpo),
				fu_make_list("xxx",
					fu_funes_symbol("set"),
					var,
					fu_make_list("xxx",
						fu_funes_symbol("+"),
						var,
						step))));
}

RES *
fu_macro_try(expr, handler_case)
	RES *expr, *handler_case;
{
	unsigned char condicion_p = 1;
	RES *p, *resu = NIL;

	for ( p = handler_case; CONS_P(p); p = CDR(p)) {
		RES *car = CAR(p);

		if (!CONS_P(CDR(p))) {
			/* tengo el ultimo termino */
			if (condicion_p)
			/* si tocaba una condicion, es el else */
				resu = fu_cons(car, resu);
			else {
			/* si no, agrego un else (throwar de nuevo la
			 * excepcion */
				resu = fu_cons(car, resu);
				resu = fu_cons(fu_make_list("xxx",
										fu_funes_symbol("err"),
										fu_funes_symbol("_exc"),
										fu_funes_symbol("_errstr")), resu);
			}
			break;
		}
		if (condicion_p) {
			resu = fu_cons(fu_make_list("xxx",
							fu_funes_symbol("member"),
							fu_funes_symbol("_exc"),
							fu_make_list("xx",
								fu_funes_symbol("quote"),
								(CONS_P(car)? car: fu_cons(car, NIL)))),
						resu);
		} else {
			resu = fu_cons(car, resu);
		}
		condicion_p = !condicion_p;
	}
	return fu_make_list("xxxx",
					fu_funes_symbol("handle"),
					T,
					fu_make_list("xxx",
							fu_funes_symbol("fun"),
							fu_make_list("xx", fu_funes_symbol("_exc"),
												fu_funes_symbol("_errstr")),
							fu_make_list("xX",
								fu_funes_symbol("if"),
								fu_xrev(resu))),
					fu_make_list("xxx", fu_funes_symbol("fun"), NIL, expr));
}

RES *
fu_macro_and(body)
	RES *body;
{
	RES *p, *resu, *tmp, *cond;

	resu = tmp = fu_cons(NIL, NIL);
	for (p = body; CONS_P(p); p = CDR(p)) {
		cond = fu_cons(CAR(p), NIL);
		CDR(tmp) = fu_cons(fu_cons(fu_funes_symbol("if"), cond), NIL);
		tmp = cond;
	}
	return CADR(resu);
}

RES *
fu_macro_backquote(expr)
	RES *expr;
{
#if 0
	// `(a . ()) => (cons `a `()) => (cons 'a '()) => (a . ())
	fu_print(expr);
	printf("\n");
	if (expr == NIL) {
		return NIL;
	} else if (CONS_P(expr)) {
		RES *v = fu_macro_backquote(CAR(expr));
		RES *w = fu_macro_backquote(CDR(expr));

		return fu_cons(fu_funes_symbol("cons"), fu_cons(v, fu_cons(w, NIL)));
	} else {
		return fu_cons(fu_funes_symbol("quote"), fu_cons(expr, NIL));
	}
#endif
	if (expr == NIL) {
		return NIL;
	} else if (CONS_P(expr)) {
		if (CAR(expr) == fu_funes_symbol("unquote")) {
			if (!CONS_P(CDR(expr)))
				fu_throw(fu_exception_symbol("bad-argument"),
					fu_str("backquote -- unquote deforme"));
			return CADR(expr);
		} else {
			RES *v = CAR(expr), *w = CDR(expr);

			if (CONS_P(v) && CAR(v) == fu_funes_symbol("unquote-splicing")) {
				/*if (w == NIL) {
					return CADR(v);
				} else*/
					return fu_make_list("xxx", fu_funes_symbol("cat"),
						CADR(v),
						fu_macro_backquote(w));
			} else {
				/*if (w == NIL)
					return fu_make_list("xx", fu_funes_symbol("list"),
						fu_macro_backquote(v));
				else*/
					return fu_make_list("xxx", fu_funes_symbol("cons"),
						fu_macro_backquote(v),
						fu_macro_backquote(w));
			}
		}
	} else
		return fu_cons(fu_funes_symbol("quote"), fu_cons(expr, NIL));
}

RES *
fu_set_macro_flag(fun)
	RES *fun;
{
	if (!TIPO_P(tipo_closure, fun))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("set-macro-flag -- no es una closure"));
	CLOSURE_MACRO_SET(VAL_CLOSURE(fun));
	return fun;
}

RES *
fu_macro_defstruct(nombre, slots)
	RES *nombre, *slots;
{
	RES *true_nombre;
	RES *opciones = NIL;
	RES *p;
	RES *slots_eval = NIL;
	RES *slots_nombres = NIL;
	RES *make_slot_accessors = NIL;
	RES *qt = fu_funes_symbol("quote");
	RES *lst = fu_funes_symbol("list");
	RES *acc = fu_funes_symbol("stype-accessor");
	RES *df = fu_funes_symbol("def");
	RES *opt;
	RES *nombre_constructor = UNDEF;
	RES *args_constructor = UNDEF;
	RES *opciones_reales = NIL;
	RES *prefix = NIL;
	unsigned char arma_args_constructor = TRUE;

	if (CONS_P(nombre)) {
		true_nombre = CAR(nombre);
		opciones = CDR(nombre);
	} else
		true_nombre = nombre;

	prefix = true_nombre;

	for (opt = opciones; CONS_P(opt); opt = CDR(opt)) {
		if (CAR(opt) == fu_keyword("constructor")) {
			arma_args_constructor = FALSE;
			opt = CDR(opt);
			if (!CONS_P(opt))
				fu_throw(fu_exception_symbol("bad-argument"),
					fu_str("defstruct -- se esperaba nombre de constructor"));
			nombre_constructor = CAR(opt);
			if (nombre_constructor != NIL) {
				opt = CDR(opt);
				if (!CONS_P(opt))
					fu_throw(fu_exception_symbol("bad-argument"),
						fu_str("defstruct -- se esperaban argumentos del constructor"));
				args_constructor = CAR(opt);
			}
		} else if (CAR(opt) == fu_keyword("prefix")) {
			opt = CDR(opt);
			if (!CONS_P(opt))
				fu_throw(fu_exception_symbol("bad-argument"),
					fu_str("defstruct -- se esperaba prefijo"));
			prefix = CAR(opt);
		} else {
			opciones_reales = fu_cons(CAR(opt), opciones_reales);
		}
	}
	opciones_reales = fu_xrev(opciones_reales);

	for (p = slots; CONS_P(p); p = CDR(p)) {
		RES *aa;
		if (CONS_P(CAR(p))) {
			aa = CAAR(p);
			aa = fu_mksym(fu_cons(fu_symbol_name(aa), NIL));
			slots_eval = fu_cons(
							fu_make_list("xxX", lst,
									fu_make_list("xx", qt, aa),
									CDAR(p)),
							slots_eval);
		} else {
			aa = CAR(p);
			aa = fu_mksym(fu_cons(fu_symbol_name(aa), NIL));
			slots_eval = fu_cons(fu_make_list("xx", qt, aa),
							slots_eval);
		}
		if (arma_args_constructor)
			slots_nombres = fu_cons(aa, slots_nombres);
		
		make_slot_accessors = fu_cons(
					fu_make_list("xxx", df,
							fu_mksym(fu_make_list("xxx",
											fu_symbol_name(prefix),
											fu_str("-"),
											fu_symbol_name(aa))),
							fu_make_list("xxx",
									acc, true_nombre,
									fu_make_list("xx", qt, aa))),
					make_slot_accessors);
	}
	slots_eval = fu_cons(lst, fu_xrev(slots_eval));
	if (arma_args_constructor)
		slots_nombres = fu_xrev(slots_nombres);

	/*
	** (def nombre (mkstype slots opciones))
	** (def make-nombre (stype-constructor nombre slots))
	** (def nombre-slot1 (stype-accessor nombre slot1))
	** ...
	** (def nombre-slotN (stype-accessor nombre slotN))
	*/
	if (arma_args_constructor) {
		nombre_constructor =
			fu_mksym(fu_make_list("xx", fu_str("make-"),
									fu_symbol_name(prefix)));
		args_constructor = slots_nombres;
	}
	args_constructor = fu_make_list("xx", qt, args_constructor);
	
	return fu_make_list("xxxX",
		fu_funes_symbol("do"),

				fu_make_list("xxx", df, true_nombre,
					fu_make_list("xxX", fu_funes_symbol("mkstype"),
						slots_eval, opciones_reales))
				
				,

				
				(nombre_constructor == NIL ? NIL :
				 fu_make_list("xxx", df,
						 nombre_constructor,
						 fu_make_list("xxx", fu_funes_symbol("stype-constructor"),
								 true_nombre, args_constructor)))
				
				,

				make_slot_accessors
				);
}

#define MBIND(SYMB, COSA)	{ COSA; \
		fu_set_compile_value(fu_funes_symbol(SYMB), p); \
		fu_def_env(fu_funes_symbol(SYMB), p); \
}
#define MAKE_MACRO(A,B,C,D)	{ p = fu_make_proc(A,B,C,D); \
									PROC_MACRO_SET(VAL_PROC(p)); }
#define READ_MACRO(A,B,C,D)	{ p = fu_make_proc(A,B,C,D); \
									PROC_READ_MACRO_SET(VAL_PROC(p)); }
void
fu_init_macro()
{
	RES *p;

	/* MACROS */
	MBIND("macro", MAKE_MACRO(fu_macro_defmacro,2,0,1));
	MBIND("symbol-macro", MAKE_MACRO(fu_macro_def_symbol_macro,1,0,1));

	/* van a quedar */
	MBIND("and", MAKE_MACRO(fu_macro_and,0,0,1));
	MBIND("backquote", MAKE_MACRO(fu_macro_backquote,1,0,0));

	/* deberian ir desapareciendo */
	MBIND("try", MAKE_MACRO(fu_macro_try,1,0,1));
	MBIND("to", MAKE_MACRO(fu_macro_to,2,0,1));
	MBIND("defstruct", MAKE_MACRO(fu_macro_defstruct,1,0,1));
}
#undef MBIND
#undef MAKE_MACRO
