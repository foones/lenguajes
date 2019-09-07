#include "Fu.h"
#include <string.h>

int
pjw_hash(s,modulo)
	char *s;
	int modulo;
{
	char *p;
	unsigned h = 0, g;

	for ( p = s; *p; p++ ) {
		h = (h << 4) + (*p);
		if ( (g = h & 0xf0000000) ) {
			h ^= (g >> 24);
			h ^= g;
		}
	}
	return h % modulo;
}

/* En VECTOR_TAG => la cantidad de elementos actualmente
   guardados en el hash */
__inline__ RES *
fu_make_hash(len)
	RES *len;
{
	RES *p;
	if (len == UNDEF)
		len = fu_int(SYMTABLE_SIZE);
	p = fu_make_vector(len, NIL);
	VECTOR_TIPO(VAL_VECTOR(p)) = vector_hash;
	VECTOR_TAG(VAL_VECTOR(p)) = 0;
	return p;
}

RES *
fu_db(args)
	RES *args;
{
	RES *h, *p;

	h = fu_make_hash(UNDEF);
	for (p = args; CONS_P(p) && CONS_P(CDR(p)); p = CDDR(p))
		fu_set_hash_eq(h, CAR(p), CADR(p));
	return h;
}

RES *
fu_hash_eq(x, len) 
	RES *x, *len;
{
	if (!INT_P(len))		
		fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("hash-eq -- la longitud debe ser un entero"));
	return fu_int(((fuint) x) % VAL_INT(len));
}

#define EL VECTOR_ELEMENTO(h, k)

/* set y get genericos dadas funciones de
 * igualdad y hash
 */
RES *
fu_set_hash_f(eqfun,hashfun,hash,clave,valor)
	RES *eqfun, *hashfun, *hash, *clave, *valor;
{
	int k;
	VECTOR *h;
	RES *clave_valor;

 	h = VAL_VECTOR(hash);
	k = VAL_INT(fu_apply(hashfun,
					fu_cons(clave, fu_cons(fu_int(VECTOR_LENGTH(h)), NIL))));
	if (EL == NIL) {
		/* El lugar estaba vacio */
		clave_valor = fu_cons(clave, valor);
		EL = fu_cons(clave_valor, NIL);
		++VECTOR_TAG(h);
	} else {
		RES *q;
		RES *c = fu_cons(NIL, fu_cons(clave, NIL));
		/* El lugar estaba ocupado */
		/* Reviso si ya existe la clave */
		for ( q = EL; q != NIL; q = CDR(q) ) {
			CAR(c) = CAR(CAR(q));
			if ( fu_apply(eqfun, c) != NIL ) {
				/* En este caso ya existia */
				CDR(CAR(q)) = valor;
				return CAR(q);
			}
		}
		/* En este caso, no existia en ese compartimento un dato con
		 * la misma clave */
		clave_valor = fu_cons(clave, valor);
		EL = fu_cons(clave_valor, EL);
		++VECTOR_TAG(h);
	}
	return clave_valor;
}

RES *
fu_get_hash_f(eqfun,hashfun,hash,clave)
	RES *eqfun, *hashfun, *hash, *clave;
{
	int k;
	VECTOR *h;

	h = VAL_VECTOR(hash);
	k = VAL_INT(fu_apply(hashfun, fu_cons(clave, fu_cons(fu_int(VECTOR_LENGTH(h)), NIL))));
	if (EL != NIL) {
		RES *q;
		RES *c = fu_cons(NIL, fu_cons(clave, NIL));
		for ( q = EL; q != NIL; q = CDR(q) ) {
			CAR(c) = CAR(CAR(q));
			if ( fu_apply(eqfun, c) != NIL )
				return CAR(q);
		}
	}
	return NIL;
}

RES *
fu_del_hash_f(eqfun,hashfun,hash,clave)
	RES *eqfun, *hashfun, *hash, *clave;
{
	int k;
	VECTOR *h;

	h = VAL_VECTOR(hash);
	k = VAL_INT(fu_apply(hashfun,
					fu_cons(clave, fu_cons(fu_int(VECTOR_LENGTH(h)), NIL))));
	if (EL != NIL) {
		RES **q;
		RES *c = fu_cons(NIL, fu_cons(clave, NIL));
		for ( q = &EL; *q != NIL; q = &CDR(*q) ) {
			CAR(c) = CAR(CAR(*q));
			if ( fu_apply(eqfun, c) != NIL ) {
				*q = CDR(*q);
				--VECTOR_TAG(h);
				return T;
			}
		}
	}
	return NIL;
}

/* funciones rapidas con eq */
RES *
fu_set_hash_eq(hash,clave,valor)
	RES *hash, *clave, *valor;
{
	fuint k;
	VECTOR *h;
	RES *clave_valor;

 	h = VAL_VECTOR(hash);
	k = ((fuint) clave) % VECTOR_LENGTH(h);
	if (EL == NIL) {
		/* El lugar estaba vacio */
		clave_valor = fu_cons(clave, valor);
		EL = fu_cons(clave_valor, NIL);
		++VECTOR_TAG(h);
	} else {
		RES *q;
		/* El lugar estaba ocupado */
		/* Reviso si ya existe la clave */
		for ( q = EL; q != NIL; q = CDR(q) ) {
			if ( CAR(CAR(q)) == clave ) {
				/* En este caso ya existia */
				CDR(CAR(q)) = valor;
				return CAR(q);
			}
		}
		/* En este caso, no existia en ese compartimento un dato con
		 * la misma clave */
		clave_valor = fu_cons(clave, valor);
		EL = fu_cons(clave_valor, EL);
		++VECTOR_TAG(h);
	}
	return clave_valor;
}

RES *
fu_get_hash_eq(hash,clave)
	RES *hash, *clave;
{
	fuint k;
	VECTOR *h;

	h = VAL_VECTOR(hash);
	k = ((fuint) clave) % VECTOR_LENGTH(h);
	if (EL != NIL) {
		RES *q;
		for ( q = EL; q != NIL; q = CDR(q) )
			if ( CAR(CAR(q)) == clave)
				return CAR(q);
	}
	return NIL;
}

RES *
fu_del_hash_eq(hash,clave)
	RES *hash, *clave;
{
	fuint k;
	VECTOR *h;

	h = VAL_VECTOR(hash);
	k = ((fuint) clave) % VECTOR_LENGTH(h);
	if (EL != NIL) {
		RES **q;
		for ( q = &EL; *q != NIL; q = &CDR(*q) )
			if ( CAR(CAR(*q)) == clave) {
				*q = CDR(*q);
				--VECTOR_TAG(h);
				return T;
			}
	}
	return NIL;
}

/* funciones con cadenas para uso interno */
RES *
fu_set_hash(hash,clave,valor)
	RES *hash, *clave, *valor;
{
	char *str_clave;
	int k;
	VECTOR *h;
	RES *clave_valor;

	if ( !TIPO_P(tipo_vector, hash)) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("set_hash -- no es un vector"));
	}
	if ( !STR_P(clave)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("set_hash -- la clave no es una cadena"));
	}
 	h = VAL_VECTOR(hash);
 	str_clave = VAL_STR(clave);
	k = pjw_hash(str_clave, VECTOR_LENGTH(h));
	if (EL == NIL) {
		/* El lugar estaba vacio */
		clave_valor = fu_cons(clave, valor);
		EL = fu_cons(clave_valor, NIL);
		++VECTOR_TAG(h);
	} else {
		RES *q;
		/* El lugar estaba ocupado */
		/* Reviso si ya existe la clave */
		for ( q = EL; q != NIL; q = CDR(q) ) {
			if ( strcmp( VAL_STR(CAR(CAR(q))), str_clave ) == 0 ) {
				/* En este caso ya existia */
				CDR(CAR(q)) = valor;
				return CAR(q);
			}
		}
		/* En este caso, no existia en ese compartimento un dato con
		 * la misma clave */
		clave_valor = fu_cons(clave, valor);
		EL = fu_cons(clave_valor, EL);
		++VECTOR_TAG(h);
	}
	return clave_valor;
}

RES *
fu_get_hash(hash,clave)
	RES *hash, *clave;
{
	char *str_clave;
	int k;
	VECTOR *h;

	if ( !TIPO_P(tipo_vector, hash)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("get_hash -- no es un vector"));
	}
	if ( !STR_P(clave)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("get_hash -- la clave no es una cadena"));
	}
#if 0
#endif
	h = VAL_VECTOR(hash);
 	str_clave = VAL_STR(clave);
	k = pjw_hash(str_clave, VECTOR_LENGTH(h));
	if (EL != NIL) {
		RES *q;
		for ( q = EL; q != NIL; q = CDR(q) )
			if ( ! strcmp(VAL_STR(CAR(CAR(q))), str_clave))
				return CAR(q);
	}
	return NIL;
}

RES *
fu_del_hash(hash,clave)
	RES *hash, *clave;
{
	char *str_clave;
	int k;
	VECTOR *h;

	if ( !TIPO_P(tipo_vector, hash)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("del_hash -- no es un vector"));
	}
	if ( !STR_P(clave)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("del_hash -- la clave no es una cadena"));
	}
	h = VAL_VECTOR(hash);
 	str_clave = VAL_STR(clave);
	k = pjw_hash(str_clave, VECTOR_LENGTH(h));
	if (EL != NIL) {
		RES **q;
		for ( q = &EL; *q != NIL; q = &CDR(*q) ) {
			if ( ! strcmp(VAL_STR(CAR(CAR(*q))), str_clave)) {
				*q = CDR(*q);
				return T;
			}
		}
	}
	return NIL;
}

/* "map" para hashes */
RES *
fu_db_data(hash, func)
	RES *hash, *func;
{
	RES *resultado = NIL;
	VECTOR *v;
	unsigned i;

	if ( !TIPO_P(tipo_vector, hash)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("data -- no es un vector"));
	}
	v = VAL_VECTOR(hash);
	for (i = 0; i < VECTOR_LENGTH(v); i++) {
		RES *l1; 

		l1 = VECTOR_ELEMENTO(v, i);
		if (l1 == NIL) {
			/* nada */
		} else if (CONS_P(l1)) {
			RES *p;

			for ( p = l1; CONS_P(p); p = CDR(p))
				resultado = fu_cons((func == UNDEF ?
										CAR(p) :
										fu_apply(func, fu_cons(CAR(p), NIL))),
								resultado);
		} else
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("data -- el hash es deforme"));
	}
	return resultado;
}

/* devuelve el vector interno */
RES *
fu_db_vec(hash)
	RES *hash;
{
	RES *r;

	if ( !TIPO_P(tipo_vector, hash)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("data -- no es un vector"));
	}
	r = fu_empty_vector(VECTOR_LENGTH(VAL_VECTOR(hash)));
	VECTOR_TABLA(VAL_VECTOR(r)) = VECTOR_TABLA(VAL_VECTOR(hash));
	VECTOR_TIPO(VAL_VECTOR(r)) = vector_no_uniforme;
	return r;
}

/* devuelve el conjunto de claves */
RES *
fu_db_keys(hash)
	RES *hash;
{
	RES *r, *p;
	VECTOR *v;
	unsigned i;

	if ( !TIPO_P(tipo_vector, hash)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("data -- no es un vector"));
	}

	r = NIL;
	v = VAL_VECTOR(hash);
	for (i = 0; i < VECTOR_LENGTH(v); i++) {
		p = NIL;
		for ( p = VECTOR_ELEMENTO(v, i); CONS_P(p); p = CDR(p) ) {
			if ( !CONS_P(CAR(p))) {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("keys -- hash deforme"));
			}
			r = fu_cons(CAR(CAR(p)), r);
		}
	}

	return r;
}

/* copia un hash */
RES *
fu_copy_hash(hash)
		RES *hash;
{
	RES *copia;
	VECTOR *v;
	VECTOR *vcopia;
	RES *p, *q;
	unsigned i;

	if ( !TIPO_P(tipo_vector, hash)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("copy-hash -- no es un hash"));
	}
	v = VAL_VECTOR(hash);
	copia = fu_make_hash(fu_int(VECTOR_LENGTH(v)));
	vcopia = VAL_VECTOR(copia);
	for (i = 0; i < VECTOR_LENGTH(v); i++) {
		q = NIL;
		for ( p = VECTOR_ELEMENTO(v, i); CONS_P(p); p = CDR(p) ) {
			if ( !CONS_P(CAR(p))) {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("copy-hash -- hash deforme"));
			}
			q = fu_acons(CAR(CAR(p)), CDR(CAR(p)), q);
		}
		VECTOR_ELEMENTO(vcopia, i) = q;
	}
	VECTOR_TAG(vcopia) = VECTOR_TAG(v);
	return copia;
}

RES *
fu_db_size(hash)
		RES *hash;
{
		if ( !TIPO_P(tipo_vector, hash)) {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("dbsize -- no es un hash"));
		}
		return fu_int(VECTOR_TAG(VAL_VECTOR(hash)));
}

RES *
fu_rehash_eq(hash, tam)
		RES *hash, *tam;
{
		RES *copia;
		VECTOR *v;
		VECTOR *h;
		RES *p, *q;
		unsigned i, lcopia;
		int k;

		if ( !TIPO_P(tipo_vector, hash)) {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("rehash -- no es un hash"));
		}
		v = VAL_VECTOR(hash);
		lcopia = VAL_INT(tam);
		copia = fu_make_hash(fu_int(lcopia));
		h = VAL_VECTOR(copia);
		for (i = 0; i < VECTOR_LENGTH(v); i++) {
				for ( p = VECTOR_ELEMENTO(v, i); CONS_P(p); ) {
						if ( !CONS_P(CAR(p))) {
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("rehash -- hash deforme"));
						}
						k = ((fuint) CAR(CAR(p))) % lcopia;
						q = CDR(p);
						CDR(p) = EL;
						EL = p;
						p = q;
				}
		}
		VECTOR_TABLA(v) = VECTOR_TABLA(h);
		VECTOR_LENGTH(v) = lcopia;
		return hash;
}

RES *
fu_rehash_f(eqfun, hashfun, hash, tam)
		RES *eqfun, *hashfun, *hash, *tam;
{
		RES *copia;
		VECTOR *v;
		VECTOR *h;
		RES *p, *q;
		unsigned i, lcopia;
		fuint k;
		RES *c;

		if ( !TIPO_P(tipo_vector, hash)) {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("rehash -- no es un hash"));
		}
		v = VAL_VECTOR(hash);
		lcopia = VAL_INT(tam);
		copia = fu_make_hash(fu_int(lcopia));
		h = VAL_VECTOR(copia);
		c = fu_cons(NIL, fu_cons(fu_int(lcopia), NIL));
		for (i = 0; i < VECTOR_LENGTH(v); i++) {
				for ( p = VECTOR_ELEMENTO(v, i); CONS_P(p); ) {
						if ( !CONS_P(CAR(p))) {
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("rehash -- hash deforme"));
						}
						CAR(c) = CAR(CAR(p));
						k = VAL_INT(fu_apply(hashfun, c));
						q = CDR(p);
						CDR(p) = EL;
						EL = p;
						p = q;
				}
		}
		VECTOR_TABLA(v) = VECTOR_TABLA(h);
		VECTOR_LENGTH(v) = lcopia;
		return hash;
}


RES *
fu_rehash(hash, tam)
		RES *hash, *tam;
{
		RES *copia;
		VECTOR *v;
		VECTOR *h;
		RES *p, *q;
		unsigned i, lcopia;
		int k;

		if ( !TIPO_P(tipo_vector, hash)) {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("rehash -- no es un hash"));
		}
		v = VAL_VECTOR(hash);
		lcopia = VAL_INT(tam);
		copia = fu_make_hash(fu_int(lcopia));
		h = VAL_VECTOR(copia);
		for (i = 0; i < VECTOR_LENGTH(v); i++) {
				for ( p = VECTOR_ELEMENTO(v, i); CONS_P(p); ) {
						if ( !CONS_P(CAR(p))) {
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("rehash -- hash deforme"));
						}
						k = pjw_hash(VAL_STR(CAR(CAR(p))), lcopia);
						q = CDR(p);
						CDR(p) = EL;
						EL = p;
						p = q;
				}
		}
		VECTOR_TABLA(v) = VECTOR_TABLA(h);
		VECTOR_LENGTH(v) = lcopia;
		return hash;
}

#undef EL
