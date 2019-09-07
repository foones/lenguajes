#include "Fu.h"
#include <string.h>

RES *
fu_str(s)
	const char *s;
{
	RES *p;
	VECTOR *v;

	p = fu_empty_vector(strlen(s));
	v = VAL_VECTOR(p);
	VECTOR_TABLA(v) = (RES **) s;
	VECTOR_TIPO(v) = vector_uniforme_char;

	VAL(p) = (void *) v;
	return p;
}

RES *
fu_str_cat(s1, s2)
	RES *s1, *s2;
{
	char *c;
	unsigned l1, l;

	if (!STR_P(s1) || !STR_P(s2)) {
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("str_cat -- no son cadenas"));
	}
	l1 = VECTOR_LENGTH(VAL_VECTOR(s1));
	l = l1 + VECTOR_LENGTH(VAL_VECTOR(s2));
	c = NEWQ(char, l + 1);
	strncpy(c, VAL_STR(s1), l1);
	c[l1] = 0;
	strncat(c, VAL_STR(s2), l);
	c[l] = 0;
	return fu_str(c);
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
fu_input()
{
	char *resultado = "\0", *tmp;
	char buffer[MAX_STR_BUF];
	int c;
	unsigned longitud = 0;
	unsigned i = 0;

	while ((c = fgetc(stdin)) != '\n') {
		if (c == EOF) {
			fu_throw(fu_exception_symbol("read-error"),
				fu_str("input -- no se puede leer de la entrada"));
		}
		APPEND(c);
	}
	TERMINAR();
	return fu_str(resultado);
}

#define PLUS		1		/* muestra el mas */
#define SPACE		2		/* espacio si hay mas */
#define SPECIAL		4		/* 0x */
#define LARGE		8		/* usar ABCDEF en vez de abcdef */

/* APPEND funciona en el sentido contrario:
 * primero se deben appendear los últimos caracteres
 */
char *
stringify_number(n, base, tipo, devolver_longitud)
	const RES *n, *base;
	int tipo;
	unsigned *devolver_longitud;
{
	const char *digitos;
	const char *digitos_chicos = "0123456789abcdefghijklmnopqrstuvwxyz";
	const char *digitos_grandes = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	char *resultado, *tmp;
	char buffer[MAX_STR_BUF], c;
	char signo;
	int valor;
	unsigned longitud = 0;
	unsigned i = 0, b, digito;
	char *p, *q;

	digitos = (tipo & LARGE)? digitos_grandes : digitos_chicos;

	if (!INT_P(n))
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("formato de numero -- no es un entero"));
	if (!INT_P(base))
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("formato de numero -- la base no es un entero"));

	b = VAL_INT(base);

	if (b < 1 || b > 36)
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("formato de numero -- la base debe estar entre 1 y 37"));

	valor = VAL_INT(n);

	signo = 0;
	if (valor < 0) {
		signo = '-';
		valor = -valor;
	} else if ( tipo & PLUS ) {
		signo = '+';
	} else if ( tipo & SPACE) {
		signo = ' ';
	}

	do {
		digito = valor % b;
		APPEND(digitos[digito]);
	} while ( (valor = valor / b) );

	if (signo)
		APPEND(signo);

	if (tipo & SPECIAL) {
		if (b == 10) ;
		else if (b == 2) {
			APPEND(digitos[11]);
			APPEND('#');
		} else if (b == 8) {
			APPEND(digitos[24]);
			APPEND('#');
		} else if (b == 16) {
			APPEND(digitos[33]);
			APPEND('#');
		} else {
			unsigned *lb = NEW(unsigned);
			char *numero, *c;

			APPEND('>');
			numero = stringify_number(fu_int(b), fu_int(10), 0, lb);
			for ( c = numero+*lb-1; c >= numero; c--) {
				APPEND(*c);
			}
			APPEND('<');
			APPEND('#');
		}
	}

	TERMINAR();

	/* por ultimo invierte la cadena
	 * (swap del primero con el ultimo, el segundo
	 * con el anteultimo, etc.)
	 */
	p = resultado;
	q = resultado + longitud - 1;
	while (q > p) {
		/* swap de los valores */
		c = *p; *p = *q; *q = c;
		p++, q--;
	}
	*devolver_longitud = longitud;
	return resultado;
}
#undef APPEND
#undef TERMINAR

RES *
fu_num_to_str(n, base, plus, space, special, large)
	RES *n, *base, *plus, *space, *special, *large;
{
	unsigned *l = NEW(unsigned);
	int flags = 0;
	if (base == UNDEF)
		base = fu_int(10);
	if (plus != NIL && plus != UNDEF)
		flags |= PLUS;
	if (space != NIL && space != UNDEF)
		flags |= SPACE;
	if (special != NIL && special != UNDEF)
		flags |= SPECIAL;
	if (large != NIL && large != UNDEF)
		flags |= LARGE;
	return fu_str(stringify_number(n, base, flags, l));
}

#define APPEND_STR(CADENA, L2) {\
			char *__tmp;\
			__tmp = (char *)resultado;\
			resultado = (unsigned char *)NEWQ(char, l+L2+1);\
			strncpy((char *)resultado, __tmp, l);\
			resultado[l] = '\0';\
			strncat((char *)resultado, CADENA, L2);\
			l += L2;\
			resultado[l] = '\0';\
}
/* devuelve una cadena dado un objeto */
unsigned char *
stringify(v, style, devolver_longitud)
	RES *v, *style;
	unsigned *devolver_longitud;
{
	unsigned l = 0;
	unsigned char *resultado;
	RES *p;

	if (NULL_P(v)) {
		resultado = (unsigned char *)"()";
		*devolver_longitud = 2;
	} else if ( v == T ) {
		resultado = (unsigned char *)"#t";
		*devolver_longitud = 2;
	} else if ( v == EOF_OBJECT ) {
		resultado = (unsigned char *)"#<eof>";
		*devolver_longitud = 6;
	} else if ( v == UNDEF ) {
		resultado = (unsigned char *)"#u";
		*devolver_longitud = 2;
	} else if (INT_P(v)) {
		resultado = (unsigned char *)stringify_number(v, fu_int(10), 0, devolver_longitud);
	} else if (CHAR_P(v)) {
		unsigned char c = VAL_CHAR(v);
		if ( c == '\n') {
			resultado = (unsigned char *)"\\nl";
			*devolver_longitud = 3;
		} else if ( c == '\r') {
			resultado = (unsigned char *)"\\cr";
			*devolver_longitud = 3;
		} else {
			resultado = (unsigned char *)NEWQ(char, 3);
			sprintf((char *)resultado, "\\%c", c);
			*devolver_longitud = 2;
		}
	} else if (CONS_P(v)) {
		unsigned l2;
		char *s2;

		resultado = (unsigned char *)"(";
		l = 1;
		p = v;
		while (1) {
			s2 = (char *)stringify(CAR(p), style, &l2);
			APPEND_STR(s2, l2);
			p = CDR(p);
			if (!CONS_P(p))
				break;
			APPEND_STR(" ", 1);
		}
		if ( p != NIL ) {
			APPEND_STR(" . ", 3);
			s2 = (char *)stringify(p, style, &l2);
			APPEND_STR(s2, l2);
		}
		APPEND_STR(")", 1);
		*devolver_longitud = l;
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_sym: {
				unsigned l = 0;	
				if (fu_keyword_p(v) != NIL) {
					resultado = (unsigned char *)"=";
					l = 1;
				} else if (VAL_SYMPACK(v) == NIL) {
					resultado = (unsigned char *)":";
					l = 1;
				} else if (VAL_SYMPACK(v) == fu_current_package()) {
					resultado = (unsigned char *)"";
					l = 0;
				} else {
					char *pn = VAL_PACKNAME(VAL_SYMPACK(v));
					resultado = (unsigned char *)"";
					l = 0;
					APPEND_STR(pn, strlen(pn));
					APPEND_STR("::", 2);
				}
				APPEND_STR(VAL_SYMNAME(v), strlen(VAL_SYMNAME(v)));
				*devolver_longitud = l;
				break;
			}
			case tipo_vector:
				{
					int i;
					VECTOR *w = VAL_VECTOR(v);

					switch (VECTOR_TIPO(w)) {
					case vector_hash:
					case vector_no_uniforme:
						APPEND_STR("#(", 2);
						for ( i = 0; i < VECTOR_LENGTH(w); i++) {
							unsigned l2;
							char *s2;
							if ( i != 0 )
								APPEND_STR(" ", 1);
							s2 = (char *)stringify(VECTOR_ELEMENTO(w, i), style, &l2);
							APPEND_STR(s2, l2);
						}
						APPEND_STR(")", 1);
						*devolver_longitud = l;
						break;
					case vector_uniforme_char:
						if (style == T) {
							unsigned l2 = VECTOR_LENGTH(w);
							resultado = (unsigned char *)NEWQ(char, l2+1);
							sprintf((char *)resultado, "%s", VAL_STR(v));
							*devolver_longitud = l2;
						} else {
							unsigned l2 = VECTOR_LENGTH(w);
							resultado = (unsigned char *)NEWQ(char, l2+3);
							sprintf((char *)resultado, "\"%s\"", VAL_STR(v));
							*devolver_longitud = l2+2;
						}
						break;
					}
					break;
				}
			case tipo_proc:
				if (PROC_MACRO(VAL_PROC(v))) {
					unsigned l2;
					char *s2;
					APPEND_STR("#<macro-proc ", 13);
					s2 = stringify_number(fu_int((fuint) v),fu_int(16), 0, &l2);
					APPEND_STR(s2, l2);
					APPEND_STR(">", 1);
					*devolver_longitud = l;
				} else {
					unsigned l2;
					char *s2;
					APPEND_STR("#<proc ", 7);
					s2 = stringify_number(fu_int((fuint) v),fu_int(16), 0, &l2);
					APPEND_STR(s2, l2);
					APPEND_STR(">", 1);
					*devolver_longitud = l;
				}
				break;
			case tipo_special_form: {
				unsigned l2;
				char *s2;
				APPEND_STR("#<forma-especial ", 17);
				s2 = (char *)stringify_number(fu_int((fuint) v),fu_int(16), 0, &l2);
				APPEND_STR(s2, l2);
				APPEND_STR(">", 1);
				*devolver_longitud = l;
				break;
			}
			case tipo_closure: {
				/*fu_closure_print(v);*/
				unsigned l2;
				char *s2;
				if (CLOSURE_MACRO(VAL_CLOSURE(v))) {
					APPEND_STR("#<macro-clausura ", 17);
				} else if (CLOSURE_DYN(VAL_CLOSURE(v))) {
					APPEND_STR("#<clausura-dinamica ", 20);
				} else {
					APPEND_STR("#<clausura ", 11);
				}
				s2 = stringify_number(fu_int((fuint) v),fu_int(16), 0, &l2);
				APPEND_STR(s2, l2);
				APPEND_STR(">", 1);
				*devolver_longitud = l;
				break;
			}
			case tipo_port: {
				unsigned l2;
				char *s2;
				APPEND_STR("#<puerto ", 9);
				s2 = stringify_number(fu_int((fuint) v),fu_int(16), 0, &l2);
				APPEND_STR(s2, l2);
				APPEND_STR(">", 1);
				*devolver_longitud = l;
				break;
			}

			case tipo_tipo: {
				TIPO *t = VAL_TIPO(v);
				unsigned l2;
				char *s2;
				if (TIPO_BUILTIN_P(t)) {
					APPEND_STR(TIPO_NOMBRE(t), strlen(TIPO_NOMBRE(t)));
				} else {
					s2 = (char *)stringify(TIPO_DES(t), style, &l2);
					APPEND_STR(s2, l2);
				}
				*devolver_longitud = l;
				break;
			}
			case tipo_stype: {
				unsigned l2;
				char *s2;
				APPEND_STR("#<stype ", 8);
				s2 = stringify_number(fu_int((fuint) v),fu_int(16), 0, &l2);
				APPEND_STR(s2, l2);
				if (STYPE_LISTP(VAL_STYPE(v))) {
					APPEND_STR(" list>", 6);
				} else {
					APPEND_STR(" vec>", 5);
				}
				*devolver_longitud = l;
				break;
		 	}
			case tipo_struct: {
				RES *t = STRUCT_STYPE(VAL_STRUCT(v));
				unsigned l2;
				char *s2;
				if (TIPO_P(tipo_stype, t)) {
					RES *pf;
					pf = STYPE_PRINT_FUNCTION(VAL_STYPE(t));
					if (pf != NIL) {
						s2 = (char *)stringify(fu_apply(pf, fu_cons(v, NIL)), style, &l2);
						APPEND_STR(s2, l2);
						*devolver_longitud = l;
						break;
					}
				}
				APPEND_STR("#S(", 3);
				s2 = (char *)stringify(STRUCT_STYPE(VAL_STRUCT(v)), style, &l2);
				APPEND_STR(s2, l2);
				APPEND_STR(" ", 1);
				s2 = (char *)stringify(STRUCT_DATA(VAL_STRUCT(v)), style, &l2);
				APPEND_STR(s2, l2);
				APPEND_STR(")", 1);
				*devolver_longitud = l;
				break;
			}
			case tipo_float: {
				break;
			}
			case tipo_cont: {
				break;	
			}
			case tipo_package: {
				APPEND_STR("#<package ", 10);
				APPEND_STR(VAL_PACKNAME(v), strlen(VAL_PACKNAME(v)));
				APPEND_STR(">", 1);
				*devolver_longitud = l;
				break;
			}

			default:
				fu_throw(fu_exception_symbol("wrong-type"),
					fu_str("str -- no se reconoce el tipo"));
		}

	} else {
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("str -- no se reconoce el tipo"));
	}
	return resultado;
}

RES *
fu_mkstr(v)
	RES *v;
{
	unsigned l = 0, *l2 = NEW(unsigned);
	unsigned char *resultado = (unsigned char *)"";
	char *s2;
	RES *p;

	if (!CONS_P(v) && v != NIL)
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("str -- no es una lista de argumentos"));

	for (p = v; CONS_P(p); p = CDR(p)) {
		s2 = (char *)stringify(CAR(p), T, l2);
		APPEND_STR(s2, *l2);
	}
	return fu_str((char *)resultado);
}

#define PUSHBUF(C)	if (c - (char *)buff < MAX_READ_BUFFER - 1) {\
				*c++ = C;\
			} else {\
				*c = '\0';\
				c = (char *)buff;\
				APPEND_STR((char *)buff, MAX_READ_BUFFER);\
				*c++ = C;\
			}
#define FLUSHBUF() if (c - (char *)buff != 0) {\
			*c = '\0';\
			APPEND_STR((char *)buff, c - (char *)buff);\
			c = (char *)buff;\
}

RES *
fu_fmt(fmt_str, args)
	RES *fmt_str, *args;
{
	unsigned l = 0, *l2 = NEW(unsigned);
	unsigned char *resultado = (unsigned char *)"";
	char *s2;
	unsigned char buff[MAX_STR_BUF];
	char *c = (char *)buff;
	char *s;
	RES *p = args;

	if (!STR_P(fmt_str))
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("fmt -- no es una cadena"));
	if (!CONS_P(args) && args != NIL)
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("fmt -- no es una lista de argumentos"));

	for (s = VAL_STR(fmt_str); *s; s++) {
		int flags = 0;
		unsigned base = 10;
		RES *estilo = T;

		if (*s != '%') {
			PUSHBUF(*s);
			continue;
		}

		repeat:
		s++;
		if (!*s)
			fu_throw(fu_exception_symbol("bad-argument"),
				fu_str("fmt -- fin inesperado de la cadena de formato"));

		if (*s == '%') {
			PUSHBUF(*(unsigned char *)s);
			continue;
		}

		switch (*s) {
			case '+': flags |= PLUS; goto repeat;
			case ' ': flags |= SPACE; goto repeat;
			case '#': flags |= SPECIAL; goto repeat;
		}

		FLUSHBUF();

		switch (*s) {
		case 'S':
			estilo = NIL;
		case 's':
			if (!CONS_P(p))
				fu_throw(fu_exception_symbol("arity-error"),
					fu_str("fmt -- la cadena de formato exige mas argumentos"));

			s2 = stringify(CAR(p), estilo, l2);
			p = CDR(p);
			APPEND_STR(s2, *l2);
			break;
		case 'X':
			flags |= LARGE;
		case 'x':
			base = 16;
			if (!CONS_P(p))
				fu_throw(fu_exception_symbol("arity-error"),
					fu_str("fmt -- la cadena de formato exige mas argumentos"));

			s2 = stringify_number(CAR(p), fu_int(base), flags, l2);
			p = CDR(p);
			APPEND_STR(s2, *l2);
			break;
		case 'O':
			flags |= LARGE;
		case 'o':
			base = 8;
			if (!CONS_P(p))
				fu_throw(fu_exception_symbol("arity-error"),
					fu_str("fmt -- la cadena de formato exige mas argumentos"));

			s2 = stringify_number(CAR(p), fu_int(base), flags, l2);
			p = CDR(p);
			APPEND_STR(s2, *l2);
			break;
		case 'B':
			flags |= LARGE;
		case 'b':
			base = 2;
			if (!CONS_P(p))
				fu_throw(fu_exception_symbol("arity-error"),
					fu_str("fmt -- la cadena de formato exige mas argumentos"));

			s2 = stringify_number(CAR(p), fu_int(base), flags, l2);
			p = CDR(p);
			APPEND_STR(s2, *l2);
			break;
		case 'i':
			if (!CONS_P(p))
				fu_throw(fu_exception_symbol("arity-error"),
					fu_str("fmt -- la cadena de formato exige mas argumentos"));

			s2 = stringify_number(CAR(p), fu_int(base), flags, l2);
			p = CDR(p);
			APPEND_STR(s2, *l2);
			break;
		default:
			fu_throw(fu_exception_symbol("bad-argument"),
				fu_str("fmt -- comando de formato desconocido"));
		}
	}

	FLUSHBUF();
	return fu_str(resultado);
}
#undef PUSHBUF
#undef FLUSHBUF

#undef APPEND_STR

#if _WIN32

#include <windows.h>

int fu_val_str(RES *s, LPSTR string, LONG size)
{
	unsigned l = strlen(VAL_STR(s));
	if (size > l) {
		strcpy(string, VAL_STR(s));
		string[l] = '\0';
		return 1;
	} else {
		return 0;
	}
}

#endif
