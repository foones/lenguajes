#include "Fu.h"

/* compila s-expressions a codigo para la vm */
BLIST *bl;
unsigned n_consts;
unsigned n_labels;
RES *consts, *consts_last;

__inline__ unsigned
constante(c)
	RES *c;
{
	RES *p;
	unsigned i = 0;

	for (p = consts; CONS_P(p); p = CDR(p), i++) {
		if (CAR(p) == c)
			return i;
	}
	CAR(consts_last) = c;
	consts_last = CDR(consts_last) = fu_cons(NIL, NIL);
	return n_consts++;
}

#define ADD_CONST(C) {\
	CAR(consts_last) = C;\
		consts_last = CDR(consts_last) = fu_cons(NIL, NIL);\
		n_consts++;\
		}

#define CMD(OPCODE,OPT) \
	FU_PUSH_BLIST(bl, FU_BCONS(fu_make_bcode((OPCODE), (OPT))));

#define NEW_LABEL(L)	{L = n_labels; n_labels++;}

void fu_compile_expr(RES *expr, char push_value);

/* compila un redex 
 * un redex es de la forma
 * ((fun <args> cuerpo) . <vals>)
 * y si es asi lo inlinea
 */
void
compile_redex(expr, push_value)
	RES *expr;
	char push_value;
{
	/* expr = ((fun <args> cuerpo) . <vals>) */
	/* es un redex */
	RES *argumentos;
	RES *cuerpo;
	RES *valores;
	RES *arg, *val;
	RES *lista_identificadores = NIL, *p;

	if (!CONS_P(CDAR(expr))) {
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("fun -- redex deforme"));
	}
	argumentos = CADAR(expr);
	cuerpo = CDDAR(expr);
	valores = CDR(expr);

	for ( arg = argumentos, val = valores;
			CONS_P(val);
			arg = CDR(arg), val = CDR(val)) {
		RES *a, *b;
		if (CONS_P(arg)) {
			a = CAR(arg);
			b = CAR(val);
		} else {
			a = arg;
			b = fu_cons(fu_funes_symbol("list"), val);
		}
		if (!TIPO_P(tipo_sym, a)) {
			fu_throw(fu_exception_symbol("bad-argument"),
				fu_str("fun -- (redex) el parametro no es un simbolo"));
		}
		fu_compile_expr(b, 1);
		lista_identificadores = fu_cons(a, lista_identificadores);
		if (!CONS_P(arg))
			break;
	}

	if (CONS_P(arg) && !CONS_P(val))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("fun -- (redex) faltan parametros"));

	CMD(PUSH_ENV, 0);
	for (p = lista_identificadores; CONS_P(p); p = CDR(p)) 
		CMD(DEF, constante(CAR(p)));

	fu_compile_expr(fu_cons(fu_funes_symbol("do"), cuerpo), push_value);
	CMD(POP_ENV, 0);
}

void
compile_quote(expr, push_value)
	RES *expr;
	char push_value;
{
	if (! CONS_P(CDR(expr))) {
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("quote -- falta expresion"));
	}
	CMD(push_value ? CONST_PUSH : OP_CONST, constante(CAR(CDR(expr))));
}

void
compile_argset(expr, push_value)
	RES *expr;
	char push_value;
{
	if (! CONS_P(CDR(expr)) || ! CONS_P(CDR(CDR(expr)))) {
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("argset -- falta expresion"));
	}
	fu_compile_expr(fu_make_list("xxxxx",
				fu_funes_symbol("list"), ARGSET,
				fu_make_list("xx", fu_funes_symbol("quote"), CADR(expr)),
				fu_make_list("xx", fu_funes_symbol("quote"), CADDR(expr)),
				CAR(CDDDR(expr))),
			push_value);
}


void
compile_if(expr, push_value)
	RES *expr;
	char push_value;
{
	RES *p;
	unsigned char condicion_p = 1, hay_defa = 0;
	unsigned last, siguiente;

	NEW_LABEL(last);
	NEW_LABEL(siguiente);
	for ( p = CDR(expr); CONS_P(p); p = CDR(p) ) {
		if ( condicion_p && !CONS_P(CDR(p))) {
		/* es el default */
			CMD(LABEL, siguiente);
			fu_compile_expr(CAR(p), push_value);
			hay_defa = 1;
			break;
		}

		if ( condicion_p ) {
			CMD(LABEL, siguiente);
			fu_compile_expr(CAR(p), 0);
			NEW_LABEL(siguiente);
			CMD(JF_LABEL, siguiente);
		} else {
			fu_compile_expr(CAR(p), push_value);
			CMD(JMP_LABEL, last);
		}

		condicion_p = !condicion_p;
	}
	if (!hay_defa) {
		CMD(LABEL, siguiente);
		fu_compile_expr(NIL, push_value);
	}
	CMD(LABEL, last);
}

void
compile_while(expr, push_value)
	RES *expr;
	char push_value;
{
	RES *p, *cond;
	unsigned inicio, final;

	NEW_LABEL(inicio);
	NEW_LABEL(final);

	p = CDR(expr);
	if ( !CONS_P(p) )
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("while -- debe darse una condicion"));

	CMD(LABEL, inicio);
	cond = CAR(p);
	fu_compile_expr(cond, 0);
	CMD(JF_LABEL, final);
	fu_compile_expr(fu_cons(fu_funes_symbol("do"), CDR(p)), 0);
	CMD(JMP_LABEL, inicio);
	CMD(LABEL, final);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_or(expr, push_value)
	RES *expr;
	char push_value;
{
	RES *p;
	unsigned last;

	NEW_LABEL(last);
	for ( p = CDR(expr); CONS_P(p); p = CDR(p) ) {
			fu_compile_expr(CAR(p), 0);
			CMD(JNF_LABEL, last);
	}
	CMD(LABEL, last);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_not(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("not -- requiere un argumento"));

	fu_compile_expr(CADR(expr), 0);
	CMD(NOT, 0);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_eq(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)) || !CONS_P(CDDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("eq -- requiere dos argumentos"));

	fu_compile_expr(CADR(expr), 1);
	fu_compile_expr(CADDR(expr), 0);
	CMD(EQ, 0);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_car(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("car -- requiere un argumento"));

	fu_compile_expr(CADR(expr), 0);
	CMD(OP_CAR, 0);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_cdr(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("car -- requiere un argumento"));

	fu_compile_expr(CADR(expr), 0);
	CMD(OP_CDR, 0);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_fun(expr, push_value)
	RES *expr;
	char push_value;
{
	/* compila la expresion dada
	 * en una closure y la agrega
	 * como constante
	 */
	RES *arglist, *body;

	if (CONS_P(CDR(expr)) && CONS_P(CDDR(expr))) ;
	else
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("fun -- falta expresion"));

	/* lista de argumentos */
	arglist = CADR(expr);
	/* el cuerpo es todo el resto encerrado
	 * en un `do'
	 */
	body = fu_cons(fu_funes_symbol("do"), CDDR(expr));
	CMD(OP_CONST, n_consts);
	CMD(CLOSE, n_consts);
	if (push_value)
		CMD(PUSH, 0);
	ADD_CONST(fu_compile(body, arglist, 0, 1));
}

void
compile_dyn(expr, push_value)
	RES *expr;
	char push_value;
{
	/* compila la expresion dada
	 * NO closada y la agrega como
	 * constante */
	RES *arglist, *body, *dyn_clos;

	if (CONS_P(CDR(expr)) && CONS_P(CDDR(expr))) ;
	else {
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("dyn -- falta expresion"));
	}
	/* lista de argumentos */
	arglist = CADR(expr);
	/* el cuerpo es todo el resto encerrado
	 * en un `do'
	 */
	body = fu_cons(fu_funes_symbol("do"), CDDR(expr));
	CMD(OP_CONST, n_consts);
	if (push_value)
		CMD(PUSH, 0);
	dyn_clos = fu_compile(body, arglist, 0, 1);
	CLOSURE_DYN_SET(VAL_CLOSURE(dyn_clos));
	ADD_CONST(dyn_clos);
} 

void
compile_thunk(expr, push_value)
	RES *expr;
	char push_value;
{
	/* compila la expresion dada
	 * NO closada y la agrega como
	 * constante */
	RES *body, *thunk;

	if (CONS_P(CDR(expr))) ;
	else {
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("dyn -- falta expresion"));
	}
	/* el cuerpo es todo el resto encerrado
	 * en un `do'
	 */
	body = fu_cons(fu_funes_symbol("do"), CDR(expr));
	CMD(OP_CONST, n_consts);
	if (push_value)
		CMD(PUSH, 0);
	thunk = fu_compile(body, NIL, 0, 0); /* new_env = 0 */
	CLOSURE_DYN_SET(VAL_CLOSURE(thunk));
	ADD_CONST(thunk);
} 

void
compile_do(expr, push_value)
	RES *expr;
	char push_value;
{
	/* compila varios secuencialmente
	 * importando el valor sólo del último
	 */
	RES *p = CDR(expr);

	if (!CONS_P(p)) {
		CMD(NOP, 0);
	} else {
		for ( ; CONS_P(CDR(p)); p = CDR(p) ) {
			fu_compile_expr(CAR(p), 0);
		}
		fu_compile_expr(CAR(p), push_value);
	}
}

void
compile_def(expr, push_value)
	RES *expr;
	char push_value;
{
	RES *p = CDR(expr);
	RES *asignado;
	RES *asignando;

	if (!CONS_P(p) || !CONS_P(CDR(p))) {
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("def -- falta expresion"));
	}

	asignado = CAR(p);
	asignando = CDR(p);
	if (TIPO_P(tipo_sym, asignado)) {
		/* asigno a un simbolo */
		fu_compile_expr(CAR(asignando), 1);
		CMD(DEF, constante(asignado));
		if (push_value)
			CMD(PUSH, 0);
	} else if (CONS_P(asignado)) {
		/* puede ser una lista
		 * lo transformo en (def F (fun ARGS ...))
		 */
		fu_compile_expr(
			fu_make_list("xxx",
				fu_funes_symbol(CONS_P(CAR(asignado)) ? "set" : "def"),
				CAR(asignado),
				fu_make_list("xxX",
					fu_funes_symbol("fun"),
					CDR(asignado),
					asignando)),
			push_value);
	} else {
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("def -- debe ser un simbolo o una lista"));
	}
}

void
compile_set(expr, push_value)
	RES *expr;
	char push_value;
{
	RES *p = CDR(expr);
	RES *asignado;
	RES *asignando;

	if (!CONS_P(p) || !CONS_P(CDR(p))) {
		fu_throw(fu_exception_symbol("missing-expression"),
			fu_str("set -- falta expresion"));
	}

	asignado = CAR(p);
	asignando = CADR(p);
	if (TIPO_P(tipo_sym, asignado)) {
		RES *value = fu_compile_value(asignado);
		if (value != NIL && SYMBOL_MACROP(value)) {
			/* es un symbol-macro */
			compile_set(fu_make_list("xxx",
								fu_funes_symbol("set"),
								fu_apply(value, NIL),
								asignando),
						push_value);
		} else {
			/* asigno a un simbolo */
			fu_compile_expr(asignando, 1);
			CMD(SET, constante(asignado));
			if (push_value)
				CMD(PUSH, 0);
		}
	} else if (CONS_P(asignado)) {
		/* setf
		 * (set (operador . argumentos) . valores)
		 * => ((setter operador) argumentos valores)
		 */
		RES *q, *opvalue;
		unsigned i = 0;

		opvalue = fu_compile_value(CAR(asignado));
		if ( opvalue != NIL && MACROP(opvalue)) {
			/* es un macro */
			compile_set(fu_make_list("xxx",
								fu_funes_symbol("set"),
								fu_apply(opvalue, CDR(asignado)),
								asignando),
						push_value);
			return;
		}

		/* empujo los argumentos */
		for ( q = CDR(asignado); CONS_P(q); q = CDR(q) ) {
			fu_compile_expr(CAR(q), 1);
			i++;
		}
		/* empujo los valores */
		for ( q = CDR(p); CONS_P(q); q = CDR(q) ) {
			fu_compile_expr(CAR(q), 1);
			i++;
		}

		/* empujo el operador */
		fu_compile_expr(CAR(asignado), 0);
		/* aplico el setter */
		CMD(SETTER, 0);
		CMD(APPLY, i);
		if (push_value)
			CMD(PUSH, 0);
	} else {
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("set -- debe ser un simbolo o variable generalizada"));
	}
}

void
compile_dir(expr, push_value)
	RES *expr;
	char push_value;
{
	CMD(ENV, 0);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_set_cc(expr)
	RES *expr;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("set-cc -- requiere un argumento"));

	/* argumentos */
	if (CONS_P(CDR(CDR(expr))))
		fu_compile_expr(CAR(CDR(CDR(expr))), 1);
	else
		fu_compile_expr(NIL, 1);

	/* funcion */
	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(SET_CC, 0);
}

void
compile_call_cc(expr)
	RES *expr;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("call-cc -- requiere un argumento"));

	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(CALL_CC, 0);
}

void
compile_spawn(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)) || !CONS_P(CDR(CDR(expr))))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("spawn -- requiere dos argumentos"));

	/* argumentos */
	fu_compile_expr(CAR(CDR(CDR(expr))), 1);
	/* funcion */
	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(SPAWN, 0);
	if (push_value)
		fu_compile_expr(NIL, 1);
}

void
compile_sem_init(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)) || !CONS_P(CDR(CDR(expr))))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("sem-init -- requiere dos argumentos"));

	/* valor */
	fu_compile_expr(CAR(CDR(CDR(expr))), 1);
	/* semaforo */
	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(SEM_SET, SEM_init);
	if (push_value)
		fu_compile_expr(T, 1);
}

void
compile_sem_pv(expr, push_value, pov)
	RES *expr;
	char push_value;
	char pov;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("sem-acquire/sem-release -- requiere un argumentos"));

	/* semaforo */
	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(SEM_SET, (pov == 'p' ? SEM_p : SEM_v));
	if (push_value)
		fu_compile_expr(T, 1);
}

void
compile_sem_val(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("sem-val -- requiere un argumentos"));

	/* semaforo */
	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(SEM_SET, SEM_val);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_sem_set(expr, push_value)
	RES *expr;
	char push_value;
{
	if (!CONS_P(CDR(expr)) || !CONS_P(CDR(CDR(expr))))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("sem-set -- requiere dos argumentos"));

	/* semaforo */
	fu_compile_expr(CAR(CDR(CDR(expr))), 1);
	fu_compile_expr(CAR(CDR(expr)), 1);
	CMD(SEM_SET, SEM_set);
	if (push_value)
		CMD(PUSH, 0);
}

void
compile_call(expr, push_value)
	RES *expr;
	char push_value;
{
	RES *p;
	unsigned i = 0;

	if (!CONS_P(expr))
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("call -- requiere al menos un argumento"));

	for ( p = CDR(expr); CONS_P(p); p = CDR(p) ) {
		/* argumentos */
		fu_compile_expr(CAR(p), 1);
		i++;
	}
	/* operador */
	fu_compile_expr(CAR(expr), 0);
	/* aplicacion */
	CMD(APPLY_CALL, i);
	if (push_value)
		CMD(PUSH, 0);
}

/* auxiliar: compila una expresion y la va agregando a la
 * bl actual
 * esta recursivamente definida
 */
void
fu_compile_expr(expr, push_value)
	RES *expr;
	char push_value;	/* hace un push del valor? */
{
	if ( TIPO_P(tipo_sym, expr) && fu_keyword_p(expr) == NIL) {
		/* es un simbolo representando una variable (NO debe ser keyword) */
		RES *value = fu_compile_value(expr);
		if (value != NIL && SYMBOL_MACROP(value)) {
			/* es un symbol-macro */
			fu_compile_expr(fu_apply(value, NIL), push_value);
		} else {
			CMD(GET, constante(expr));
			if (push_value)
				CMD(PUSH, 0);
		}
	} else if ( CONS_P(expr) ) {
		/* es una lista */
		RES *op, *opvalue, *p, *en_linea;
		unsigned i = 0;

		op = CAR(expr);

		if ( CONS_P(op) ) {
			/* comprueba si es un redex
			 * no es excluyente con lo demás!
			 */
			RES *opvalue2;
			opvalue2 = fu_compile_value(CAR(op));
			if ( opvalue2 != NIL && opvalue2 == special_form_fun) {
				compile_redex(expr, push_value);
				return;
			}
		}

		/* comprueba si es un macro */
		opvalue = fu_compile_value(op);
		if ( opvalue == NIL ) opvalue = op;

		en_linea = fu_inlined(opvalue);
		if ( en_linea != NIL ) {
			fu_compile_expr(fu_cons(en_linea, CDR(expr)), push_value);
		} else if (opvalue == ARGSET) {
			/* es argset */
			compile_argset(expr, push_value);
		} else if ( TIPO_P(tipo_special_form, opvalue)) {
			/* es una special form */
			if ( opvalue == special_form_quote ) {
				compile_quote(expr, push_value);
			} else if ( opvalue == special_form_if ) {
				compile_if(expr, push_value);
			} else if ( opvalue == special_form_while ) {
				compile_while(expr, push_value);
			} else if ( opvalue == special_form_or ) {
				compile_or(expr, push_value);
			} else if ( opvalue == special_form_fun ) {
				compile_fun(expr, push_value);
			} else if ( opvalue == special_form_dyn ) {
				compile_dyn(expr, push_value);
			} else if ( opvalue == special_form_do ) {
				compile_do(expr, push_value);
			} else if ( opvalue == special_form_def ) {
				compile_def(expr, push_value);
			} else if ( opvalue == special_form_set ) {
				compile_set(expr, push_value);
			} else if ( opvalue == special_form_not ) {
				compile_not(expr, push_value);
			} else if ( opvalue == special_form_eq ) {
				compile_eq(expr, push_value);
			} else if ( opvalue == special_form_car ) {
				compile_car(expr, push_value);
			} else if ( opvalue == special_form_cdr ) {
				compile_cdr(expr, push_value);
			} else if ( opvalue == special_form_dir ) {
				compile_dir(expr, push_value);
			} else if ( opvalue == special_form_thunk ) {
				compile_thunk(expr, push_value);
			} else if ( opvalue == special_form_set_cc ) {
				compile_set_cc(expr);
			} else if ( opvalue == special_form_call_cc ) {
				compile_call_cc(expr);
			} else if ( opvalue == special_form_spawn ) {
				compile_spawn(expr, push_value);
			} else if ( opvalue == special_form_sem_init ) {
				compile_sem_init(expr, push_value);
			} else if ( opvalue == special_form_sem_p ) {
				compile_sem_pv(expr, push_value, 'p');
			} else if ( opvalue == special_form_sem_v ) {
				compile_sem_pv(expr, push_value, 'v');
			} else if ( opvalue == special_form_sem_val ) {
				compile_sem_val(expr, push_value);
			} else if ( opvalue == special_form_sem_set ) {
				compile_sem_set(expr, push_value);
			} else if ( opvalue == special_form_call ) {
				compile_call(CDR(expr), push_value);
			}
		} else if ( MACROP(opvalue) ) {
			/* es un macro */
			fu_compile_expr(fu_apply(opvalue, CDR(expr)), push_value);
			return;
		} else {
			/* no es una special form ni un macro */
			for ( p = CDR(expr); CONS_P(p); p = CDR(p) ) {
				/* argumentos */
				fu_compile_expr(CAR(p), 1);
				i++;
			}
			/* operador */
			fu_compile_expr(op, 0);
			/* aplicacion */
			CMD(APPLY, i);
			if (push_value)
				CMD(PUSH, 0);
		}
	} else {
		/* es una constante */
		CMD(push_value ? CONST_PUSH : OP_CONST, constante(expr));
		/*CMD(push_value ? CONST_PUSH : OP_CONST, n_consts);
		ADD_CONST(expr);*/
	}
}
#undef CMD
#undef ADD_CONST

/* segunda pasada del compilador
 * transforma la BLIST en un array de BCODE
 * resuelve etiquetas
 * transforma
 *   LABEL     -> <nada>
 *   JMP_LABEL -> JMP
 *   JNF_LABEL -> JNF
 *   JF_LABEL  -> JF
 * reconoce tail calls y transforma el opcode APPLY
 * en TAIL_APPLY si corresponde
 */
CLOSURE *
fu_compile_2(arglist, new_env)
	RES *arglist;
	char new_env;
{
	CLOSURE *clos = NEW(CLOSURE);
	BCONS *p;
	BCODE **codevec;
	unsigned idx = 0;
	unsigned length = 0;
	unsigned *etiquetas;
	char flag_cambio;

	/* guardo las posiciones de las etiquetas */
	etiquetas = NEWQ(unsigned, n_labels);
	for ( p = BLIST_FIRST(bl); p; p = BCONS_NEXT(p) ) {
		BCODE *cod;

		cod = BCONS_CODE(p);
		if (BCODE_OPCODE(cod) == LABEL)
			etiquetas[BCODE_OPT(cod)] = length;
		length++;
	}

	/* transformo las opcodes con _LABEL en opcodes
	 * sin _LABEL y con posiciones absolutas */
	codevec = NEWQ(BCODE *, length);
	for ( p = BLIST_FIRST(bl); p; p = BCONS_NEXT(p) ) {
		BCODE *cod;

		cod = BCONS_CODE(p);
		switch (BCODE_OPCODE(cod)) {
		case JMP_LABEL:
			BCODE_OPCODE(cod) = JMP;
			BCODE_OPT(cod) = etiquetas[BCODE_OPT(cod)];
			break;
		case JNF_LABEL:
			BCODE_OPCODE(cod) = JNF;
			BCODE_OPT(cod) = etiquetas[BCODE_OPT(cod)];
			break;
		case JF_LABEL:
			BCODE_OPCODE(cod) = JF;
			BCODE_OPT(cod) = etiquetas[BCODE_OPT(cod)];
			break;
		}
		codevec[idx++] = BCONS_CODE(p);
	}

	/* hago el codevec tail recursive */
	/* XXX: algoritmo pedorrisimo */
	flag_cambio = TRUE;
	while (flag_cambio) {
			flag_cambio = FALSE;
			/* recorro todo el codevec desde el final hacia el principio
			 * (porque en general los saltos se dan hacia adelante)
			 */ 
			for (idx = length - 1; idx > 0; idx--) {
					switch (BCODE_OPCODE(codevec[idx])) {
					/* si el opcode es un JMP (no condicional)
					 * y el destino es un RET, lo transformo
					 * en un RET */
					case JMP:
							if (BCODE_OPCODE(codevec[BCODE_OPT(codevec[idx])])
											== RET) {
									BCODE_OPCODE(codevec[idx]) = RET;
									flag_cambio = TRUE;
							}
							break;
					/* si es una etiqueta y el siguiente es un RET,
					 * lo transformo en un RET */
					case LABEL:
							if (BCODE_OPCODE(codevec[idx+1]) == RET) {
									BCODE_OPCODE(codevec[idx]) = RET;
									flag_cambio = TRUE;
							}
							break;
					/* si es un APPLY y el siguiente es un RET,
					 * lo transformo en un TAIL_APPLY */
					case APPLY:
						if (BCODE_OPCODE(codevec[idx+1]) == RET) {
							BCODE_OPCODE(codevec[idx]) = TAIL_APPLY;
							flag_cambio = TRUE;
						}
						break;
					}
			}
	}
	
	CLOSURE_CODEVEC(clos) = codevec;
	CLOSURE_LENGTH(clos) = length;
	CLOSURE_CONSTANTS(clos) = fu_list_to_vector(consts);
	CLOSURE_ARGLIST(clos) = arglist;
	CLOSURE_ENVPADRE(clos) = fu_env();
	if ( new_env )
		CLOSURE_NEWENV_SET(clos);
	else
		CLOSURE_NEWENV_UNSET(clos);
	CLOSURE_MACRO_UNSET(clos);
	CLOSURE_DYN_UNSET(clos);
	return clos;
}

/*
 * compila una expresion a una closure
 */
RES *
fu_compile(expr, arglist, push_value, new_env)
	RES *expr, *arglist;
	char push_value, new_env;
{
	/* Guarda los valores anteriores para poder anidar
	 * las llamadas a fu_compile
	 */
	BLIST *prev_bl = bl;
	unsigned prev_n_consts = n_consts;
	unsigned prev_n_labels = n_labels;
	RES *prev_consts = consts, *prev_consts_last = consts_last;
	/**/

	RES *p;
	CLOSURE *resultado;

	p = NEW(RES);
	TIPO(p) = tipo_closure;

	bl = fu_make_blist(FU_BCONS(fu_make_bcode(NOP,0)));
	n_labels = 0;
	n_consts = 1;
	consts_last = fu_cons(NIL,NIL);
	consts = fu_cons(NIL, consts_last);
	fu_compile_expr(expr, push_value);

	/*FU_PUSH_BLIST(bl, FU_BCONS(fu_make_bcode(PRINT, 0)));*/
	FU_PUSH_BLIST(bl, FU_BCONS(fu_make_bcode(RET, 0)));

	resultado = fu_compile_2(arglist, new_env);

	/* Vuelve los valores a su estado anterior */
	bl = prev_bl;
	n_consts = prev_n_consts;
	n_labels = prev_n_labels;
	consts = prev_consts;
	consts_last = prev_consts_last;
	/**/

	VAL(p) = resultado;
	return p;
}

RES *
fu_fenv(closure)
		RES *closure;
{
	if (!TIPO_P(tipo_closure, closure)) {
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("fenv -- no es una closure"));
	}
	return CLOSURE_ENVPADRE(VAL_CLOSURE(closure));
}

RES *
fu_fenv_set(closure, env)
		RES *closure, *env;
{
	if (!TIPO_P(tipo_closure, closure)) {
		fu_throw(fu_exception_symbol("bad-argument"),
			fu_str("fenv -- no es una closure"));
	}
	CLOSURE_ENVPADRE(VAL_CLOSURE(closure)) = (void *) env;
	return closure;
}

