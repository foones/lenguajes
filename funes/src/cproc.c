#include "Fu.h"

/*
 * ojo:
 *  algunos de los casos siguientes estan considerados
 *  APARTE en la VM para permitir que todas las ejecuciones
 *  se mantengan en un mismo contexto.
 *
 *  (El asunto es que llamar a fu_apply no permite controlar
 *  las continuaciones en ejecucion).
 */
RES *
fu_apply(val, args)
	RES *val, *args;
{
	if (TIPO_P(tipo_proc, val))
		return fu_apply_proc(val, args);
	else if (TIPO_P(tipo_closure, val))
		return fu_interpret(val, args);
	else if ( val == T ) {
		if ( CONS_P(args) )
			return CAR(args);
		else
			return NIL;
	} else if (CONS_P(args) && (CONS_P(val))) {
		if ( !CONS_P(args) )
			fu_throw(fu_exception_symbol("arity-error"),
					fu_str("apply -- requiere un indice"));
		return fu_nth(val, CAR(args));
	} else if (TIPO_P(tipo_vector, val)) {
		if ( !CONS_P(args) )
			fu_throw(fu_exception_symbol("arity-error"),
					fu_str("apply -- requiere un indice"));
		return fu_elt(CAR(args), val);
	} else if ((TIPO_P(tipo_tipo, val) || TIPO_P(tipo_stype, val))
					&& CONS_P(args) ) {
		if ( !CONS_P(args) )
			fu_throw(fu_exception_symbol("arity-error"),
					fu_str("apply -- requiere un indice"));
		return fu_typep(val, CAR(args));
	} else if (TIPO_P(tipo_struct, val)) {
		RES *f = STYPE_GET_FUNCTION(VAL_STYPE(STRUCT_STYPE(VAL_STRUCT(val))));
		if (f == NIL) {
				if ( !CONS_P(args) )
					fu_throw(fu_exception_symbol("arity-error"),
							fu_str("apply -- requiere un indice"));
				return fu_slot_value(val, CAR(args));
		} else {
				return fu_apply(f, fu_cons(val, args));
		}
	} else if (val == ARGSET) {
		return fu_cons(ARGSET, args);
	} else {
		fu_throw(fu_exception_symbol("wrong-type"),
				fu_str("apply -- no es aplicable"));
	}
	return NIL;
}

RES *
fu_make_proc(funcion, nargs, nopts, restp)
	RES *(*funcion)();
	unsigned char nargs, nopts, restp;
{
	RES *v;
	CPROC *p;

	v = NEW(RES);
	TIPO(v) = tipo_proc;
	p = NEW(CPROC);
	PROC_FUNCION(p) = funcion;
	if ( nargs + nopts + (restp? 1: 0) <= MAX_PROC_ARGS ) {
		PROC_NARGS(p) = nargs;
		PROC_NOPTS(p) = nopts;
		if (restp)
			PROC_REST_SET(p);
		else
			PROC_REST_UNSET(p);
		PROC_MACRO_UNSET(p);
	} else {
		fu_throw(fu_exception_symbol("misc-error"),
				fu_str("make_proc -- proc con demasiados argumentos"));
	}
	VAL(v) = (void *) p;
	return v;
}

RES *
fu_apply_proc(proc, args)
	RES *proc, *args;
{
	char mal;
	unsigned i;
	unsigned char nargs, nopts, restp;
	RES *va[MAX_PROC_ARGS], *v;
	CPROC *p;

	if (!TIPO_P(tipo_proc, proc)) {
		fu_throw(fu_exception_symbol("wrong-type"),
				fu_str("apply_proc -- no es un proc"));
	}
	p = VAL_PROC(proc);

	/* comprueba que coincidan la cantidad de argumentos
	 * esperados con la cantidad de argumentos recibidos
	 */
	nargs = PROC_NARGS(p);
	nopts = PROC_NOPTS(p);
	restp = PROC_RESTP(p);
	mal = TRUE;
	for ( v = args, i = 0; ; i++, v = CDR(v) ) {
		if ( i >= nargs && i < nargs + nopts + (restp? 0: 1) && !CONS_P(v) ) {
			unsigned j;
			for ( j = i; j < nargs + nopts + (restp? 0: 1); j++ )
				va[j] = UNDEF;
			mal = FALSE;
			break;
		} else if ( i == nargs + nopts ) {
			if ( restp )
					va[i] = v;
			if ( restp || NULL_P(v) ) {
					mal = FALSE;
					break;
			}
		} else if ( i == nargs + nopts || !CONS_P(v) )
			break;
		va[i] = CAR(v);
	}
	if ( mal ) {
		fu_throw(fu_exception_symbol("arity-error"),
				fu_str("apply_proc -- no coinciden las aridades"));
	}

	/* llama a la funcion con la cantidad de argumentos que
	 * corresponda. la cantidad de entradas del switch tiene
	 * que coincidir con MAX_PROC_ARGS definido en proc.h
	 */ 
	switch (nargs + nopts + (restp? 1: 0)) {
		case 0: return PROC_FUNCION(p)();
		case 1: return PROC_FUNCION(p)(va[0]);
		case 2: return PROC_FUNCION(p)(va[0], va[1]);
		case 3: return PROC_FUNCION(p)(va[0], va[1], va[2]);
		case 4: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3]);
		case 5: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3], va[4]);
		case 6: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3], va[4], va[5]);
		case 7: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3], va[4], va[5], va[6]);
		case 8: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3], va[4], va[5], va[6], va[7]);
		case 9: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3], va[4], va[5], va[6], va[7], va[8]);
		case 10: return PROC_FUNCION(p)(va[0], va[1], va[2], va[3], va[4], va[5], va[6], va[7], va[8], va[9]);
		default:
				 fu_throw(fu_exception_symbol("internal-error"),
					 fu_str("apply_proc -- proc con demasiados argumentos"));
			 break;
	}
	return NIL;
}
