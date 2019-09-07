#include "Fu.h"
#include <stdarg.h>
#include <string.h>

RES *
fu_empty_vector(l)
	fuint l;
{
	RES *p;
	VECTOR *v;

	p = NEW(RES);
	TIPO(p) = tipo_vector;
	v = NEW(VECTOR);
	VECTOR_LENGTH(v) = l;

	VAL(p) = (void *) v;
	return p;
}

/* funciones para manejar vectores no uniformes */

RES *
fu_make_vector(len, inicial)
	RES *len, *inicial;
{
	RES *p;
	VECTOR *v;
	unsigned i, l;

	l = VAL_INT(len);
	p = fu_empty_vector(l);
	v = VAL_VECTOR(p);
	VECTOR_TABLA(v) = NEWQ(RES *, l);

	if ( inicial == UNDEF )
		inicial = NIL;

	VECTOR_TIPO(v) = vector_no_uniforme;
	for (i = 0; i < l; i++)
		VECTOR_ELEMENTO(v, i) = inicial;
	return p;
}

RES *
fu_set_vector(vector,indice,valor)
	RES *vector, *indice, *valor;
{
	unsigned i;
	VECTOR *v;

	if (! INT_P(indice) )
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("get_vector -- el indice no es entero"));
 	i = VAL_INT(indice);
	if (! TIPO_P(tipo_vector, vector) )
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("set_vector -- no es un vector"));
	
	v = VAL_VECTOR(vector);
	if (i >= VECTOR_LENGTH(v))
		fu_throw(fu_exception_symbol("index-error"),
			 fu_str("set_vector -- indice fuera de rango"));
	switch (VECTOR_TIPO(v)) {
	case vector_hash:
	case vector_no_uniforme:
		VECTOR_ELEMENTO(v, i) = valor;
		break;
	case vector_uniforme_char: {
		if (!CHAR_P(valor))
			fu_throw(fu_exception_symbol("index-error"),
				fu_str("set_vector -- no es un caracter"));
		((char *)VECTOR_TABLA(v))[i] = VAL_CHAR(valor);
		break;
	}
	}
	return valor;
}

RES *fu_get_vector(vector,indice)
	RES *vector, *indice;
{
	unsigned i;
	VECTOR *v;

	if (! INT_P(indice) )
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("get_vector -- el indice no es entero"));
		
 	i = VAL_INT(indice);
	if (! TIPO_P(tipo_vector, vector) )
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("get_vector -- no es un vector"));

	v = VAL_VECTOR(vector);
	if (i >= VECTOR_LENGTH(v))
		fu_throw(fu_exception_symbol("index-error"),
			 fu_str("get_vector -- indice fuera de rango"));
	switch (VECTOR_TIPO(v)) {
	case vector_hash:
	case vector_no_uniforme:
		return VECTOR_ELEMENTO(v, i);
	case vector_uniforme_char:
		return fu_char(((char *) VECTOR_TABLA(v))[i]);
	default:
		fu_throw(fu_exception_symbol("bad-argument"),
			 fu_str("get_vector -- vector de tipo desconocido"));
	}
	return NIL;
}

RES *fu_vector(char *format, ...)
{
	RES *vec;
	RES **t;
	va_list args;
	char *c;

	va_start(args, format);
	vec = fu_make_vector(fu_int(strlen(format)), NIL);
	t = VECTOR_TABLA(VAL_VECTOR(vec));

	for (c = format; *c; c++)
		t[c - format] = va_arg(args, RES *);
	va_end(args);
	return vec;
}

RES *fu_vector_builtin(RES *args)
{
	unsigned i = 0;
	RES *vec, *p;
	RES **t;

	vec = fu_make_vector(fu_length(args), NIL);
	t = VECTOR_TABLA(VAL_VECTOR(vec));

	for (p = args; CONS_P(p); p = CDR(p), i++)
		t[i] = CAR(p);
	return vec;
}
