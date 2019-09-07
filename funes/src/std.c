#include "Fu.h"
#include <stdlib.h>
#include <string.h>

#if _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

/* algunas funciones comunes */

RES *
fu_eq(v,w)
		RES *v, *w;
{
	return ( v == w ) ? T : NIL;
}

RES *
fu_null(v)
		RES *v;
{
	return ( v == NIL ) ? T : NIL;
}

RES *
map_seq(f, ls, guardap, car_cdr, preserva_tipo)
	RES *f, *ls;
	unsigned char guardap, car_cdr, preserva_tipo;
{
	unsigned i = 0, tipo = 0;
	unsigned char macrop = MACROP(f);
	RES *resultado = NIL, *tmp, *args, *p;
	RES *listas = fu_copy_shallow(ls);

	if (!CONS_P(listas)) return NIL;
	while (TRUE) {
		args = NIL;
		for ( p = listas; CONS_P(p); p = CDR(p) ) {
			RES *v = CAR(p);
			if ( CONS_P(v) ) {
				args = fu_cons((car_cdr ? CAR(v) : v), args);
				CAR(p) = CDR(v);
			} else if (NINMEDIATO_P(v)) {
				switch ( TIPO(v) ) {
					case tipo_vector:
					{
						VECTOR *w = VAL_VECTOR(v);
						unsigned l = VECTOR_LENGTH(w);

						if ( i >= l )
							goto fin_map;
						switch (VECTOR_TIPO(w)) {
						case vector_no_uniforme:
							tipo = 1;
							args = fu_cons(VECTOR_ELEMENTO(w,i), args);
							break;
						case vector_uniforme_char:
							tipo = 2;
							args = fu_cons(fu_char(VAL_STR(v)[i]), args);
							break;
						default:
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("map -- no es una secuencia"));
							break;
						}
						break;
					}
					default:
						fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("map -- no es una secuencia"));
						break;
				}
			} else {
				goto fin_map;
			}
		}
		tmp = fu_apply(f, fu_xrev(args));
		if (macrop)
			tmp = fu_eval(tmp);
		if (guardap)
			resultado = fu_cons(tmp, resultado);
		i++;
	}
fin_map:
	if (guardap) {
		if (preserva_tipo && tipo == 1)
			return fu_list_to_vec(fu_xrev(resultado), fu_int(i));
		else if (preserva_tipo && tipo == 2)
			return fu_list_to_str(fu_xrev(resultado), fu_int(i));
		else
			return fu_xrev(resultado);
	} else
		return CAR(ls);
}

RES *
fu_map(f,ls)
	RES *f, *ls;
{
	return map_seq(f, ls, TRUE, TRUE, TRUE);
}

RES *
fu_mapc(f,ls)
	RES *f, *ls;
{
	return map_seq(f, ls, FALSE, TRUE, FALSE);
}

RES *
fu_maplist(f,ls)
	RES *f, *ls;
{
	return map_seq(f, ls, TRUE, FALSE, FALSE);
}

RES *
fu_mapl(f,ls)
	RES *f, *ls;
{
	return map_seq(f, ls, FALSE, FALSE, FALSE);
}

RES *
fu_mapt(f,ls)
	RES *f, *ls;
{
	return map_seq(f, ls, TRUE, TRUE, FALSE);
}

RES *
fu_length(v)
		RES *v;
{
	if (NULL_P(v)) {
			return fu_int(0);
	} else if (CONS_P(v)) {
			unsigned l = 0;
			RES *p;

			for (p = v; CONS_P(p); p = CDR(p)) l++;
			return fu_int(l);
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				return fu_int(VECTOR_LENGTH(VAL_VECTOR(v)));
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("len -- no es una secuencia"));

	return NIL;
}

RES *
fu_xrev(v)
	RES *v;
{
	if (NULL_P(v)) {
			return NIL;
	} else if (CONS_P(v)) {
			RES *p = v, *anterior = NIL, *siguiente;

			while (TRUE) {
				siguiente = CDR(p);
				CDR(p) = anterior;
				anterior = p;
				if (!CONS_P(siguiente)) break;
				p = siguiente;
			}
			return p;
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				VECTOR *w;

				w = VAL_VECTOR(v);
				switch (VECTOR_TIPO(w)) {
				case vector_no_uniforme: {
					unsigned i, l;
					RES **tabla = VECTOR_TABLA(w);
					RES *t;

					l = VECTOR_LENGTH(w) - 1;
					for ( i = 0; i < l; i++, l--) {
						t = tabla[l];
						tabla[l] = tabla[i];
						tabla[i] = t;
					}
					return v;
				}
				case vector_uniforme_char: {
					char *p = VAL_STR(v), *q, c;

					q = p + VECTOR_LENGTH(w) - 1;
					for ( ; p < q; p++, q--) {
						c = *p;
						*p = *q;
						*q = c;
					}
					return v;
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("xrev -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("xrev -- no es una secuencia"));

	return NIL;
}

RES *
fu_rev(v)
	RES *v;
{
	if (NULL_P(v)) {
			return NIL;
	} else if (CONS_P(v)) {
			RES *p, *final = NIL;

			for ( p = v; CONS_P(p); p = CDR(p))
				final = fu_cons(CAR(p), final);
			return final;
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				VECTOR *w;

				w = VAL_VECTOR(v);
				switch (VECTOR_TIPO(w)) {
				case vector_no_uniforme: {
					unsigned i, l;
					RES **copia, *resultado;

					l = VECTOR_LENGTH(w);
					copia = NEWQ(RES *, l);
					for ( i = 0; i < l; i++ )
						copia[i] = VECTOR_ELEMENTO(w, l-i-1);
					resultado = fu_empty_vector(l);
					VECTOR_TABLA(VAL_VECTOR(resultado)) = copia;
					VECTOR_TIPO(VAL_VECTOR(resultado)) = vector_no_uniforme;
					return resultado;
				}
				case vector_uniforme_char: {
					unsigned i, l;
					char *original = VAL_STR(v), *copia;

					l = VECTOR_LENGTH(w);
					copia = NEWQ(char, l);
					for ( i = 0; i < l; i++)
						copia[i] = original[l-i-1];
					return fu_str(copia);
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("rev -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("rev -- no es una secuencia"));

	return NIL;
}

RES *
fu_elt(idx, v)
	RES *idx, *v;
{
	if ( NULL_P(v) || CONS_P(v)) {
		return fu_nth(v, idx);
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				switch (VECTOR_TIPO(VAL_VECTOR(v))) {
				case vector_uniforme_char:
				case vector_no_uniforme:
					return fu_get_vector(v, idx);
				case vector_hash: {
					RES *r;
					
					r = fu_get_hash_eq(v, idx);
					if (CONS_P(r))
						return CDR(r);
					else
						fu_throw(fu_exception_symbol("key-error"),
							fu_mkstr(
								fu_make_list("xx",
						 		fu_str("elt -- el hash no contiene la clave: "),
								idx)));
				}
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("elt -- no es accesible"));
	return NIL;
}

RES *
fu_elt_set(idx, v, valor)
	RES *idx, *v, *valor;
{
	if ( NULL_P(v) || CONS_P(v) ) {
		return fu_nth_set(v, idx, valor);
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				switch (VECTOR_TIPO(VAL_VECTOR(v))) {
				case vector_uniforme_char:
				case vector_no_uniforme:
					fu_set_vector(v, idx, valor);
					return valor;
				case vector_hash:
					fu_set_hash_eq(v, idx, valor);
					return valor;
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("elt_set -- no es accesible"));

	return NIL;
}

RES *
fu_copy_shallow(v)
		RES *v;
{
	if (NULL_P(v)) {
			return NIL;
	} else if (CONS_P(v)) {
			RES *p, *final = NIL, *ultimo;

			for ( p = v; CONS_P(p); p = CDR(p) ) {
				final = fu_cons(CAR(p), final);
			}
			ultimo = final;
			final = fu_xrev(final);
			CDR(ultimo) = p;
			return final;
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				VECTOR *w;

				w = VAL_VECTOR(v);
				switch (VECTOR_TIPO(w)) {
				case vector_no_uniforme: {
					RES *p;
					unsigned l;
					RES **copia;

					l = VECTOR_LENGTH(w);
					p = fu_empty_vector(l);
					copia = NEWQ(RES *, l);
					memcpy(copia, VECTOR_TABLA(w), sizeof(RES *) * l);
					VECTOR_TABLA(VAL_VECTOR(p)) = copia;
					VECTOR_TIPO(VAL_VECTOR(p)) = vector_no_uniforme;
					return p;
				}
				case vector_uniforme_char: {
					RES *p;
					unsigned l;
					char *copia;

					l = VECTOR_LENGTH(w);
					p = fu_empty_vector(l);
					copia = NEWQ(char, l+1);
					strncpy(copia, VAL_STR(v), l);
					copia[l] = '\0';
					VECTOR_TABLA(VAL_VECTOR(p)) = (RES **) copia;
					VECTOR_TIPO(VAL_VECTOR(p)) = vector_uniforme_char;
					return p;
				}
				case vector_hash: {
					return fu_copy_hash(v);
				}
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("copy -- no es una secuencia"));

	return NIL;
}

RES *
fu_slice(v, inicio, final, step)
	RES *v, *inicio, *final, *step;
{
	int i, f, s;
	unsigned char desde_i = FALSE, hasta_f = FALSE;

	if (TIPO_P(tipo_vector, v)) {
		switch (VECTOR_TIPO(VAL_VECTOR(v))) {
		case vector_uniforme_char:
			return fu_list_to_str(
							fu_slice(fu_vec_to_list(v),
									inicio, final, step), UNDEF);
		case vector_no_uniforme:
			return fu_list_to_vec(
							fu_slice(fu_vec_to_list(v),
									inicio, final, step), UNDEF);
		default:
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("slice -- tipo de vector desconocido"));
		}
	}

	/* lee los argumentos */
	if (inicio == UNDEF || inicio == T) {
		desde_i = TRUE;
		i = 0;
	} else {
		if (!INT_P(inicio))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("slice -- especificacion de final invalida"));
		i = VAL_INT(inicio);
	}

	if (final == UNDEF || final == T)
		hasta_f = TRUE;
	else {
		if (!INT_P(final))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("slice -- especificacion de inicio invalida"));
		f = VAL_INT(final);
	}

	if (step == UNDEF || step == T)
		s = 1;
	else {
		if (!INT_P(step))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("slice -- especificacion de step invalida"));
		s = VAL_INT(step);
	}

	/* si el step es negativo, cambio los ordenes de los indices */
	if (s < 0) {
		int tmp;

		tmp = i;
		i = f;
		f = tmp;
		if (hasta_f) i = 0;
	}

	/* si algun indice es negativo lo ajusto modulo
	 * la longitud de la secuencia
	 * si no es negativo no hago nada, porque puede
	 * ser una lista circular!
	 */
	if (i < 0 || f < 0) {
		int l = VAL_INT(fu_length(v));

		i = (i % l);
		if ( i < 0 ) i += l;

		f = (f % l);
		if ( f < 0 ) f += l;
	}

	/* SLICE */
	if ( NULL_P(v) || CONS_P(v)) {
		RES *resu = NIL, *p;
		int pos = 0, st = 0;

		for ( p = v; CONS_P(p); p = CDR(p), pos++) {
			if (pos >= f && !hasta_f)
				break;
			if (pos >= i) {
				if (st % s == 0)
					resu = fu_cons(CAR(p), resu);
				st++;
			}
		}

		if (s < 0)
			return resu;
		else
			return fu_xrev(resu);
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("slice -- no es una secuencia"));

	return NIL;
}

RES *
fu_cat(seqs)
	RES *seqs;
{
	/* el tipo de la primera secuencia determina el tipo
	 * que deben tener las restantes y el tipo del resultado
	 */
	RES *v = CAR(seqs);

	if (NULL_P(v) || CONS_P(v)) {
		RES *l, *p, *final = NIL;

		for ( l = seqs; CONS_P(l); l = CDR(l) ) {
			if (!CONS_P(CAR(l)) && !NULL_P(CAR(l)))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("cat -- secuencias de distintos tipos"));
			for ( p = CAR(l); CONS_P(p); p = CDR(p) ) {
				final = fu_cons(CAR(p), final);
			}
		}
		return fu_xrev(final);
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				switch (VECTOR_TIPO(VAL_VECTOR(v))) {
				case vector_no_uniforme: {
					RES *v1, *p;
					unsigned l = 0;
					RES **datos, **actual;

					for ( v1 = seqs; CONS_P(v1); v1 = CDR(v1)) {
						if (!VECTOR_NO_UNIFORME_P(CAR(v1)))
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("cat -- secuencias de distintos tipos"));
						l += VECTOR_LENGTH(VAL_VECTOR(CAR(v1)));
					}
					p = fu_empty_vector(l);
					actual = datos = NEWQ(RES *, l);
					for ( v1 = seqs; CONS_P(v1); v1 = CDR(v1)) {
						VECTOR *w1 = VAL_VECTOR(CAR(v1));
						unsigned l1 = VECTOR_LENGTH(w1);

						memcpy(actual,
								VECTOR_TABLA(w1),
								sizeof(RES *) * l1);
						actual += l1;
					}
					VECTOR_TABLA(VAL_VECTOR(p)) = datos;
					VECTOR_TIPO(VAL_VECTOR(p)) = vector_no_uniforme;
					return p;
				}
				case vector_uniforme_char: {
					RES *v1, *p;
					unsigned l = 0;
					char *datos, *actual;

					for ( v1 = seqs; CONS_P(v1); v1 = CDR(v1)) {
						if (!STR_P(CAR(v1)))
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("cat -- secuencias de distintos tipos"));
						l += VECTOR_LENGTH(VAL_VECTOR(CAR(v1)));
					}
					p = fu_empty_vector(l);
					actual = datos = NEWQ(char, l+1);
					for ( v1 = seqs; CONS_P(v1); v1 = CDR(v1)) {
						unsigned l1 = VECTOR_LENGTH(VAL_VECTOR(CAR(v1)));

						strncpy(actual, VAL_STR(CAR(v1)), l1);
						actual[l1] = '\0';
						actual += l1;
					}
					VECTOR_TABLA(VAL_VECTOR(p)) = (RES **) datos;
					VECTOR_TIPO(VAL_VECTOR(p)) = vector_uniforme_char;
					return p;
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("cat -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("cat -- no es una secuencia"));

	return NIL;
}

RES *
fu_join(sep, seqs)
	RES *sep, *seqs;
{
	/* el tipo de la primera secuencia determina el tipo
	 * que deben tener las restantes y el tipo del resultado
	 */
	RES *v;
	
	if (CONS_P(seqs))
		v = CAR(seqs);
	else {
		if (CONS_P(sep))
			return NIL;
		else if (VECTOR_NO_UNIFORME_P(sep))
			return fu_empty_vector(0);
		else if (STR_P(sep))
			return fu_str("");
		else
			return sep;
	}

	if (NULL_P(v) || CONS_P(v)) {
		RES *l, *p, *final = NIL;
		char separa = TRUE;

		for ( l = seqs; CONS_P(l); l = (separa ? l : CDR(l)) ) {
			RES *sig;

			if (separa) {
				if (!CONS_P(CAR(l)) && !NULL_P(CAR(l)))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("join -- secuencias de distintos tipos"));
				sig = CAR(l);
			} else
				sig = sep;

			for ( p = sig; CONS_P(p); p = CDR(p) ) {
				final = fu_cons(CAR(p), final);
			}
			separa = !separa;
		}
		return fu_xrev(final);
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				switch (VECTOR_TIPO(VAL_VECTOR(v))) {
				case vector_no_uniforme: {
					RES *v1, *p;
					unsigned l = 0, cant_elts = 0;
					RES **datos, **actual;
					char separa = TRUE;


					if (!VECTOR_NO_UNIFORME_P(sep))
						fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("join -- el separador no es del tipo de las secuencias"));

					for ( v1 = seqs; CONS_P(v1); v1 = CDR(v1)) {
						if (!VECTOR_NO_UNIFORME_P(CAR(v1)))
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("join -- secuencias de distintos tipos"));
						l += VECTOR_LENGTH(VAL_VECTOR(CAR(v1)));
						cant_elts++;
					}

					l += (cant_elts - 1) * VECTOR_LENGTH(VAL_VECTOR(sep));
					p = fu_empty_vector(l);
					actual = datos = NEWQ(RES *, l);
					for ( v1 = seqs; CONS_P(v1); v1 = (separa ? v1 : CDR(v1))) {
						RES *sig;
						VECTOR *w1;
						unsigned l1;

						sig = separa ? CAR(v1) : sep;
						w1 = VAL_VECTOR(sig);
						l1 = VECTOR_LENGTH(w1);

						memcpy(actual,
								VECTOR_TABLA(w1),
								sizeof(RES *) * l1);
						actual += l1;
						separa = !separa;
					}
					VECTOR_TABLA(VAL_VECTOR(p)) = datos;
					VECTOR_TIPO(VAL_VECTOR(p)) = vector_no_uniforme;
					return p;
				}
				case vector_uniforme_char: {
					RES *v1, *p;
					unsigned l = 0, cant_elts = 0;
					char *datos = "", *actual;
					char separa = TRUE;

					if (!STR_P(sep))
						fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("join -- el separador no es del tipo de las secuencias"));
					for ( v1 = seqs; CONS_P(v1); v1 = CDR(v1)) {
						if (!STR_P(CAR(v1)))
							fu_throw(fu_exception_symbol("wrong-type-arg"),
								fu_str("join -- secuencias de distintos tipos"));
						l += VECTOR_LENGTH(VAL_VECTOR(CAR(v1)));
						cant_elts++;
					}

					l += (cant_elts - 1) * VECTOR_LENGTH(VAL_VECTOR(sep));
					p = fu_empty_vector(l);
					actual = datos = NEWQ(char, l+1);
					for ( v1 = seqs; CONS_P(v1); v1 = (separa ? v1 : CDR(v1))) {
						RES *sig;
						unsigned l1;

						sig = separa ? CAR(v1) : sep;
						l1 = VECTOR_LENGTH(VAL_VECTOR(sig));

						strncpy(actual, VAL_STR(sig), l1);
						actual[l1] = '\0';
						actual += l1;
						separa = !separa;
					}
					VECTOR_TABLA(VAL_VECTOR(p)) = (RES **) datos;
					VECTOR_TIPO(VAL_VECTOR(p)) = vector_uniforme_char;
					return p;
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("join -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("join -- no es una secuencia"));

	return NIL;
}

#define COMPARAR(E1, E2, PRED) \
	((PRED == UNDEF)?\
	 (E1 == E2):\
	 !NULL_P(fu_apply(PRED, fu_cons(E1,fu_cons(E2,NIL)))))
RES *
fu_index(elt, v, pred)
	RES *elt, *v, *pred;
{
	if (NULL_P(v)) {
		fu_throw(fu_exception_symbol("index-error"),
			fu_str("index -- el elemento no esta en la secuencia"));
	} else if (CONS_P(v)) {
			unsigned i = 0;
			RES *p;

			for ( p = v; CONS_P(p); p = CDR(p), i++ ) {
				if (COMPARAR(elt, CAR(p), pred))
					return fu_int(i);
			}
			fu_throw(fu_exception_symbol("index-error"),
				fu_str("index -- el elemento no esta en la secuencia"));
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				VECTOR *w;

				w = VAL_VECTOR(v);
				switch (VECTOR_TIPO(w)) {
				case vector_no_uniforme: {
					unsigned i, l;

					l = VECTOR_LENGTH(w);
					for ( i = 0; i < l; i++ ) {
						if (COMPARAR(elt, VECTOR_ELEMENTO(w, i), pred))
							return fu_int(i);
					}
					fu_throw(fu_exception_symbol("index-error"),
						fu_str("index -- el elemento no esta en la secuencia"));
				}
				case vector_uniforme_char: {
					char *c, *s;

					if (!CHAR_P(elt))
						fu_throw(fu_exception_symbol("wrong-type"),
							fu_str("index -- el elemento no es un caracter"));
					s = VAL_STR(v);
					for ( c = s; *c; c++ )
						if (COMPARAR(elt, fu_char(*c), pred))
							return fu_int(c-s);
					fu_throw(fu_exception_symbol("index-error"),
						fu_str("index -- el elemento no esta en la secuencia"));
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("index -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("index -- no es una secuencia"));

	return NIL;
}

RES *
fu_member(elt, v, pred)
	RES *elt, *v, *pred;
{
	if (NULL_P(v)) {
			return NIL;
	} else if (CONS_P(v)) {
			RES *p;

			for ( p = v; CONS_P(p); p = CDR(p) ) {
				if ( COMPARAR(elt, CAR(p), pred) )
					return p;
			}
			return NIL;
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("member -- no es una lista"));

	return NIL;
}

RES *
fu_assoc(elt, v, pred)
	RES *elt, *v, *pred;
{
	if (NULL_P(v)) {
			return NIL;
	} else if (CONS_P(v)) {
			RES *p;

			for ( p = v; CONS_P(p); p = CDR(p) ) {
				if (!CONS_P(CAR(p)))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("assoc -- no es una lista de asociacion"));
				if (COMPARAR(elt, CAAR(p), pred) )
					return CAR(p);
			}
			return NIL;
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("assoc -- no es una lista"));

	return NIL;
}
#undef COMPARAR

RES *
fu_last(lista)
	RES *lista;
{
	if (NULL_P(lista))
		return NIL;
	else if (CONS_P(lista)) {
		RES *p;

		for (p = lista; CONS_P(CDR(p)); p = CDR(p));
		return p;
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("last -- no es una lista"));

	return NIL;
}

RES *
fu_range(x,y,z)
		RES *x, *y, *z;
{
	RES *resultado = NIL;
	int termina, empieza = 0, step = 1, i;

	if (!INT_P(x))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("range -- el primer argumento no es entero"));

	if (y == UNDEF) {
		termina = VAL_INT(x);
	} else {
		if (!INT_P(y))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("range -- el segundo argumento no es entero"));
		empieza = VAL_INT(x);
		termina = VAL_INT(y);
		if (z != UNDEF) {
				if (!INT_P(z))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("range -- el tercer argumento no es entero"));
				step = VAL_INT(z);
				if (step == 0)
					return NIL;
		}
	}

	if (step > 0)
		for ( i = empieza; i < termina; i += step)
			resultado = fu_cons(fu_int(i), resultado);
	else
		for ( i = empieza; i > termina; i += step)
			resultado = fu_cons(fu_int(i), resultado);

	return fu_xrev(resultado);
}

RES *
fu_iota(x,y,z)
	RES *x, *y, *z;
{
	RES *resultado = NIL;
	int cuenta, empieza = 0, step = 1, i;

	if (!INT_P(x))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("iota -- la cantidad no es entera"));

	cuenta = VAL_INT(x);
	if (y != UNDEF) {
		if (!INT_P(y))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("iota -- el inicio no es un entero"));
		empieza = VAL_INT(y);
		if (z != UNDEF) {
				if (!INT_P(z))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("iota -- el step no es un entero"));
				step = VAL_INT(z);
		}
	}

	for ( i = 0; i < cuenta; i++)
		resultado = fu_cons(fu_int(empieza + i * step), resultado);

	return fu_xrev(resultado);
}

RES *
fu_fread(archivo)
	RES *archivo;
{
		RES *arch, *last = NIL;
		jmp_buf *prev_err_salto;
		FILE *f;

		if (!STR_P(archivo))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("fread -- el archivo debe ser una cadena"));

		if ( !(f = fopen(VAL_STR(archivo),"r"))) {
			fu_throw(fu_exception_symbol("system-error"),
				fu_str("fread -- no se puede leer del archivo"));
		}

		prev_err_salto = Fu_Err_Salto;
		Fu_Err_Salto = NEW(jmp_buf);
		if ( setjmp(*Fu_Err_Salto) ) {
			Fu_Err_Salto = prev_err_salto;
			fclose(f);
			fu_throw(Fu_Err_Excepcion, Fu_Err_Descripcion);
		}

		arch = fu_reader(fu_port(f), FALSE);
		while (f) {
			RES *sexpr;
			sexpr = fu_read(arch);
			if (sexpr == EOF_OBJECT)
				break;
			else
				last = fu_cons(sexpr, last);
		}
		Fu_Err_Salto = prev_err_salto;
		if (fclose(f)) {
			fu_throw(fu_exception_symbol("system-error"),
				fu_str("fread -- no se puede cerrar el archivo"));
		}
		return fu_xrev(last);
}

RES *
fu_fwrite(archivo, str)
	RES *archivo, *str;
{
	FILE *f;

	if (!STR_P(archivo))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("fwrite -- el archivo debe ser una cadena"));

	if (!STR_P(str))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("fwrite -- debe ser una cadena"));

	if ( !(f = fopen(VAL_STR(archivo),"w")))
		fu_throw(fu_exception_symbol("system-error"),
			fu_str("fwrite -- no se puede escribir el archivo"));

	fprintf(f, VAL_STR(str));

	if (fclose(f))
		fu_throw(fu_exception_symbol("system-error"),
			fu_str("fread -- no se puede cerrar el archivo"));

	return str;
}

RES *
fu_load(archivo, args)
	RES *archivo, *args;
{
		RES *arch, *last = NIL, *prev_args;
		FILE *f;

		if (!STR_P(archivo))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("load -- el archivo debe ser una cadena"));

		if ( !(f = fopen(VAL_STR(archivo),"r"))) {
			fu_throw(fu_exception_symbol("system-error"),
				fu_str("load -- no se puede leer del archivo"));
		}
		arch = fu_reader(fu_port(f), FALSE);
		prev_args = Fu_Argv;

		Fu_Argv = args;
		while (f) {
			RES *sexpr;
			sexpr = fu_read(arch);

			if (sexpr == EOF_OBJECT)
				break;
			else
				last = fu_eval(sexpr);
		}

		Fu_Argv = prev_args;
		if (fclose(f)) {
			fu_throw(fu_exception_symbol("system-error"),
				fu_str("load -- no se puede cerrar el archivo"));
		}
		return last;
}

#define TERMINAR() {\
	if (i != 0) {\
		tmp = resultado;\
		resultado = NEWQ(char, longitud+i+1);\
		strncat(resultado, tmp, longitud);\
		resultado[longitud] = '\0';\
		strncat(resultado, buffer, i);\
		longitud += i;\
		resultado[longitud] = '\0';\
	}\
}

#define APPEND(CARACTER) {\
	buffer[i++] = CARACTER;\
	if (i >= MAX_STR_BUF) {\
		i = MAX_STR_BUF;\
		TERMINAR();\
		i = 0;\
	}\
}

RES *
fu_flines(archivo)
	RES *archivo;
{
	RES *last = NIL;
	FILE *f;

	if (!STR_P(archivo))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("flines -- el archivo debe ser una cadena"));

	if ( !(f = fopen(VAL_STR(archivo),"r"))) {
		fu_throw(fu_exception_symbol("system-error"),
			fu_str("flines -- no se puede leer del archivo"));
	}
	while (f) {
		char *resultado, buffer[MAX_READ_BUFFER], *tmp, c;
		unsigned longitud, i;

		c = fgetc(f);
		if (c == '\n') {
			TERMINAR();
			last = fu_cons(fu_str(resultado), last);
			resultado = "";
			i = 0;
		} else if ( c == EOF ) {
			TERMINAR();
			break;
		} else {
			APPEND(c);
		}
	}
	if (fclose(f)) {
		fu_throw(fu_exception_symbol("system-error"),
			fu_str("load -- no se puede cerrar el archivo"));
	}
	return fu_xrev(last);
}
#undef TERMINAR
#undef APPEND

RES *
fu_die(mensaje, valor)
		RES *mensaje, *valor;
{
	int v = 1;
	if (valor != UNDEF && INT_P(valor))
		v = VAL_INT(v);
	if (mensaje != UNDEF)
		fu_pr(mensaje, Std_Error);
	exit(v);
}

RES *
fu_exit(valor)
	RES *valor;
{
	int v = 0;
	if (valor != UNDEF && INT_P(valor))
		v = VAL_INT(v);
	exit(v);
}

RES *
fu_sys(args)
	RES *args;
{
	return fu_int(system(VAL_STR(fu_mkstr(args))));
}

/*
 * 2007-01-03 -- ahora call es una special form
 *
 */
#if 0
RES *
fu_call(val, args)
	RES *val, *args;
{
	RES *p;
	RES *tmp = NIL, *final;

	for (p = args; CONS_P(p) && CONS_P(CDR(p)); p = CDR(p))
		tmp = fu_cons(CAR(p), tmp);

	if (tmp == NIL) {
		if (args == NIL)
			final = NIL;
		else
			final = CAR(args);
	} else {
		final = fu_xrev(tmp);
		CDR(tmp) = CAR(p);
	}

	return fu_apply(val, final);
}
#endif

/* devuelve un entero en el intervalo [0, max) */
RES *
fu_rand(max)
	RES *max;
{
	if (!INT_P(max))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("rand -- no es un entero"));
	return fu_int(rand() * (float) VAL_INT(max) / (RAND_MAX + 1.0));
}

RES *
fu_choice(seq, n, repeat)
	RES *seq, *n, *repeat;
{
	RES *resultado = NIL;

	if (n == UNDEF || n == NIL )
		/* devuelve un solo elemento */
		return fu_elt(fu_rand(fu_length(seq)), seq);
	else {
		/* devuelve una lista de elementos */
		unsigned i;
		RES *seq_actual;
		RES *l;

		if (!INT_P(n))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("choice -- no es un entero"));

		if (TIPO_P(tipo_vector, seq))
			seq_actual = fu_vec_to_list(seq);
		else if (CONS_P(seq) || NULL_P(seq))
			seq_actual = seq;
		else
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("choice -- no es una secuencia valida"));

		l = fu_length(seq);
		if (repeat == NIL || repeat == UNDEF) {
			/* sin repetir */
			RES *elegido;

			if (CONS_P(seq) || NULL_P(seq))
				seq_actual = fu_copy_shallow(seq_actual);
			for ( i = 0; i < VAL_INT(n); i++ ) {
				if (VAL_INT(l) == 0)
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("choice -- secuencia muy corta sin repeat"));
			
				elegido = fu_rand(l);
				/*resultado = fu_cons(fu_nth(seq_actual, elegido), resultado);
				seq_actual = fu_cat(fu_make_list("xx",
								fu_slice(seq_actual, T, elegido, T),
								fu_ntl(seq_actual,
										fu_int(VAL_INT(elegido) + 1))));*/
				if (VAL_INT(elegido) == 0) {
					resultado = fu_cons(CAR(seq_actual), resultado);
					seq_actual = CDR(seq_actual);
				} else {
					RES *sl = fu_ntl(seq_actual, fu_int(VAL_INT(elegido) - 1));
					resultado = fu_cons(CAR(CDR(sl)), resultado);
					CDR(sl) = CDR(CDR(sl));
				}
				l = fu_int(VAL_INT(l) - 1);
			}
		} else {
			/* repitiendo */
			for ( i = 0; i < VAL_INT(n); i++)
				resultado = fu_cons(fu_nth(seq_actual, fu_rand(l)), resultado);
		}
		return resultado;
	}
}

RES *
fu_list_to_vec(v, tamano)
	RES *v, *tamano;
{
	unsigned i, l;
	RES *p, *w, **tabla;

	if (tamano == UNDEF) l = VAL_INT(fu_length(v));
	else l = VAL_INT(tamano);

	w = fu_empty_vector(l);
	tabla = NEWQ(RES *, l);
	for (p = v, i = 0; CONS_P(p); p = CDR(p), i++)
		tabla[i] = CAR(p);
	for (; i < l; i++ )
		tabla[i] = NIL;
	VECTOR_TABLA(VAL_VECTOR(w)) = tabla;
	VECTOR_TIPO(VAL_VECTOR(w)) = vector_no_uniforme;
	return w;
}

RES *
fu_list_to_str(v, tamano)
	RES *v, *tamano;
{
	unsigned i, l;
	RES *p, *w;
	char *tabla;

	if (tamano == UNDEF) l = VAL_INT(fu_length(v));
	else l = VAL_INT(tamano);

	w = fu_empty_vector(l);
	tabla = NEWQ(char, l);
	for (p = v, i = 0; CONS_P(p); p = CDR(p), i++) {
		RES *a = CAR(p);

		if (!CHAR_P(a))
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("list->str -- no es una lista de caracteres"));
		tabla[i] = VAL_CHAR(CAR(p));
	}
	for (; i < l; i++ )
		tabla[i] = ' ';
	VECTOR_TABLA(VAL_VECTOR(w)) = (RES **) tabla;
	VECTOR_TIPO(VAL_VECTOR(w)) = vector_uniforme_char;
	return w;
}

RES *
fu_vec_to_list(v)
	RES *v;
{
	RES *resu = NIL;

	switch ( TIPO(v) ) {
		case tipo_vector:
		{
			VECTOR *w = VAL_VECTOR(v);
			unsigned l = VECTOR_LENGTH(w), i;

			switch (VECTOR_TIPO(w)) {
			case vector_no_uniforme:
				for ( i = 0; i < l; i++)
					resu = fu_cons(VECTOR_ELEMENTO(w, i), resu);
				return fu_xrev(resu);
			case vector_uniforme_char:
				for ( i = 0; i < l; i++)
					resu = fu_cons(fu_char(VAL_STR(v)[i]), resu);
				return fu_xrev(resu);
			default:
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("vec->list -- tipo de vector desconocido"));
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("vec->list -- no es un vector"));

	return NIL;
}

typedef RES *(*funcion)(RES *);

RES *
fu_dynl(arch, args)
	RES *arch, *args;
{
	funcion init;
	RES *resu;

	if (!STR_P(arch))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("dynl -- el archivo debe ser una cadena"));

#ifdef _WIN32
	{
			HANDLE hdl = LoadLibrary(VAL_STR(arch));

			if ((int) hdl <= HINSTANCE_ERROR)
				fu_throw(fu_exception_symbol("system-error"),
					fu_str("dynl -- no se puede abrir biblioteca"));

			init = (funcion) GetProcAddress(hdl, "fu_lib_init");
	}
#else
	{
			void *hdl = dlopen(VAL_STR(arch), RTLD_NOW | RTLD_LOCAL);

			if (!hdl)
				fu_throw(fu_exception_symbol("system-error"),
					fu_str("dynl -- no se puede abrir biblioteca"));

			init = (funcion) dlsym(hdl, "fu_lib_init");
	}
#endif

	resu = (*init)(args);
	return resu;
}

RES *
fu_any(pred, v)
	RES *pred, *v;
{
	if (NULL_P(v)) {
		return NIL;
	} else if (CONS_P(v)) {
		RES *p;

		for (p = v; CONS_P(p); p = CDR(p))
			if (fu_apply(pred, fu_cons(CAR(p), NIL)) != NIL)
				return T;
		return NIL;
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				VECTOR *w;

				w = VAL_VECTOR(v);
				switch (VECTOR_TIPO(w)) {
				case vector_no_uniforme: {
					unsigned i, l;
					RES **tabla = VECTOR_TABLA(w);

					l = VECTOR_LENGTH(w);
					for (i = 0; i < l; i++)
						if (fu_apply(pred, fu_cons(tabla[i], NIL)) != NIL)
							return T;
					return NIL;
				}
				case vector_uniforme_char: {
					unsigned i, l;
					char *p = VAL_STR(v);

					l = VECTOR_LENGTH(w);
					for (i = 0; i < l; i++)
						if (fu_apply(pred, fu_cons(fu_char(p[i]), NIL)) != NIL)
							return T;
					return NIL;
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("any -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("any -- no es una secuencia"));

	return NIL;
}

RES *
fu_all(pred, v)
	RES *pred, *v;
{
	if (NULL_P(v)) {
		return T;
	} else if (CONS_P(v)) {
		RES *p;

		for (p = v; CONS_P(p); p = CDR(p))
			if (fu_apply(pred, fu_cons(CAR(p), NIL)) == NIL)
				return NIL;
		return T;
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_vector:
			{
				VECTOR *w;

				w = VAL_VECTOR(v);
				switch (VECTOR_TIPO(w)) {
				case vector_no_uniforme: {
					unsigned i, l;
					RES **tabla = VECTOR_TABLA(w);

					l = VECTOR_LENGTH(w);
					for (i = 0; i < l; i++)
						if (fu_apply(pred, fu_cons(tabla[i], NIL)) == NIL)
							return NIL;
					return T;
				}
				case vector_uniforme_char: {
					unsigned i, l;
					char *p = VAL_STR(v);

					l = VECTOR_LENGTH(w);
					for (i = 0; i < l; i++)
						if (fu_apply(pred, fu_cons(fu_char(p[i]), NIL)) == NIL)
							return NIL;
					return T;
				}
				default:
					fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("all -- tipo de vector desconocido"));
				}
				break;
			}
		}
	}
	fu_throw(fu_exception_symbol("wrong-type-arg"),
		fu_str("all -- no es una secuencia"));

	return NIL;
}

RES *
fu_not_all(pred, v)
	RES *pred, *v;
{
	return (fu_all(pred, v) == NIL ? T : NIL);
}

RES *
fu_not_any(pred, v)
	RES *pred, *v;
{
	return (fu_any(pred, v) == NIL ? T : NIL);
}

RES *
fu_os_type()
{
	return fu_keyword(
#ifdef _WIN32
	"os-windows"
#else
	"os-generic"
#endif
	);
}

