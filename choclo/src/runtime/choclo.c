#include <stdlib.h>
#include <ctype.h>
#include <assert.h>
#include <string.h>
#include "choclo.h"

void *xmalloc(size_t tam) 
{
	void *p = malloc(tam);

	if (p == NULL) {
		fprintf(stderr, "Memoria insuficiente");
		exit(1);
	}
	return p;
}

Ref *
mk_objeto(t, v)
	Tipo t;
	void *v;
{
	Ref *x = NEW(Ref);

	x->tipo = t;
	x->valor = v;
	return x;
}

Ref *
mk_int(i)
	int i;
{
	return MK_OBJETO(tipo_int, i);
}

Ref *
mk_cons(car, cdr)
	Ref *car, *cdr;
{
	Cons *c = NEW(Cons);

	c->car = car;
	c->cdr = cdr;
	return MK_OBJETO(tipo_cons, c);
}

Ref *
mk_vector(tam)
	unsigned int tam;
{
	Ref **env = NEWQ(Ref *, tam);	

	return MK_OBJETO(tipo_vector, env);
}

Ref *
mk_array(tam)
	unsigned int tam;
{
	unsigned i;
	Array *a = NEW(Array);
	Ref **tabla;

	a->tam = tam;
	a->tabla = tabla = NEWQ(Ref *, tam);	
	for (i = 0; i < tam; ++i) {
		*(tabla++) = NIL;
    }
	return MK_OBJETO(tipo_array, a);
}

Ref *
mk_closure(envs, tam, funcion)
	Ref *envs;
	unsigned int tam;
	Ref *(*funcion) _PROTO((Ref *envs));
{
	Closure *c = NEW(Closure);

	c->envs = envs;
	c->nargs = tam;
	c->funcion = funcion;
	return MK_OBJETO(tipo_closure, c);
}

Ref *
mk_proc(tam, funcion)
	unsigned int tam;
	Ref *(*funcion)();
{
	Proc *p = NEW(Proc);

	p->nargs = tam;
	p->funcion = funcion;
	return MK_OBJETO(tipo_proc, p);
}

int
str_hash(s,modulo)
	char *s;
	int modulo;
{
	char *p;
	unsigned h = 0, g;

	for ( p = s; *p; p++ ) {
		h = (h << 4) + (*p);
		if ( g = h & 0xf0000000 ) {
			h ^= (g >> 24);
			h ^= g;
		}
	}
	return h % modulo;
}

Ref *
mk_hashtable(tam)
	int tam;
{
	return mk_array(tam);
}

Ref *
choclo_hashtable_set_str(array, string, valor)
	Ref *array;
	char *string;
	Ref *valor;
{
	int key = str_hash(string, VAL_ARRAY(array)->tam);
	Ref **bucket;

	assert(ARRAYP(array));

	bucket = &VAL_ARRAY(array)->tabla[key];

	if (CONSP(*bucket)) {
		Ref *p;
		for (p = *bucket; CONSP(p); p = CDR(p)) {
			assert(CONSP(CAR(p)));
			assert(STRINGP(CAAR(p)));
			if (strcmp(VAL_STRING(CAAR(p)), string) == 0) {
				/* el dato estaba */
				CDAR(p) = valor;
				return valor;
			}
		}
	}
	/* el dato no estaba */
	*bucket = mk_cons(mk_cons(mk_string(string), valor), *bucket);
	return valor;
}

Ref *
choclo_hashtable_get_str(array, string)
	Ref *array;
	char *string;
{
	int key = str_hash(string, VAL_ARRAY(array)->tam);
	Ref *bucket;

	assert(ARRAYP(array));

	bucket = VAL_ARRAY(array)->tabla[key];

	if (CONSP(bucket)) {
		Ref *p;
		for (p = bucket; CONSP(p); p = CDR(p)) {
			assert(CONSP(CAR(p)));
			assert(STRINGP(CAAR(p)));
			if (strcmp(VAL_STRING(CAAR(p)), string) == 0)
				/* el dato estaba */
				return CDAR(p);
		}
	}
	/* el dato no estaba */
	return NIL;
}

#define TAM_SYMTAB 211

Ref *TABLA_DE_SIMBOLOS = NIL;

Ref *
mk_simbolo(nombre)
	char *nombre;
{
	Ref *sim;

	if (NULLP(TABLA_DE_SIMBOLOS))
		TABLA_DE_SIMBOLOS = mk_hashtable(TAM_SYMTAB);

	sim = choclo_hashtable_get_str(TABLA_DE_SIMBOLOS, nombre);
	if (NULLP(sim)) {
		int longitud = strlen(nombre);
		char *nueva = NEWQ(char, longitud + 1);
		Simbolo *s = NEW(Simbolo);

		strncpy(nueva, nombre, longitud);
		nueva[longitud] = '\0';
		s->nombre = nueva;
		sim = MK_OBJETO(tipo_simbolo, s);
		choclo_hashtable_set_str(TABLA_DE_SIMBOLOS, nombre, sim);
	}
	return sim;
}

Ref *
choclo_symtab()
{
	return TABLA_DE_SIMBOLOS;
}

Ref *
mk_string(cadena)
	char *cadena;
{
	int longitud = strlen(cadena);
	char *nueva = NEWQ(char, longitud + 1);

	strncpy(nueva, cadena, longitud);
	nueva[longitud] = '\0';
	return MK_OBJETO(tipo_string, nueva);
}

Ref *mk_stream(arch)
	FILE *arch;
{
	return MK_OBJETO(tipo_stream, arch);
}

Ref *
aplicar(functor, args, nargs)
	Ref *functor, *args;
	unsigned int nargs;
{
	Ref *nuevo_entorno;

	assert(VECTORP(args));

	if (functor->tipo == tipo_closure) {
		assert(VAL_CLOSURE(functor)->nargs == nargs);

		nuevo_entorno = mk_cons(args, VAL_CLOSURE(functor)->envs);
		return (*VAL_CLOSURE(functor)->funcion)(nuevo_entorno);
	} else {
		Ref **t = VAL_VECTOR(args);
	   
		assert(PROCP(functor));
		assert(VAL_PROC(functor)->nargs == nargs);
		assert(nargs <= 10);

		switch (nargs) {
		case 0:
			return CALL_PROC(functor,());
		case 1:
			return CALL_PROC(functor,(t[0]));
		case 2:
			return CALL_PROC(functor,(t[0],t[1]));
		case 3:
			return CALL_PROC(functor,(t[0],t[1],t[2]));
		case 4:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3]));
		case 5:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3],t[4]));
		case 6:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3],t[4],t[5]));
		case 7:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3],t[4],t[5],t[6]));
		case 8:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3],t[4],t[5],t[6],
									t[7]));
		case 9:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3],t[4],t[5],t[6],
									t[7],t[8]));
		case 10:
			return CALL_PROC(functor,(t[0],t[1],t[2],t[3],t[4],t[5],t[6],
									t[7],t[8],t[9]));
		}
	}
	return NIL;
}

Ref *
choclo_eq(v, w)
	Ref *v, *w;
{
	if (v == w)
		return mk_simbolo("#t");
	else
		return NIL;
}

Ref *
choclo_car(cons)
	Ref *cons;

{
	assert(CONSP(cons));
	return CAR(cons);
}

Ref *
choclo_cdr(cons)
	Ref *cons;

{
	assert(CONSP(cons));
	return CDR(cons);
}

Ref *
choclo_consp(x)
	Ref *x;

{
	if (CONSP(x))
		return mk_simbolo("#t");
	else
		return NIL;
}

Ref *
choclo_intp(x)
	Ref *x;

{
	if (INTP(x))
		return mk_simbolo("#t");
	else
		return NIL;
}

Ref *
choclo_stringp(x)
	Ref *x;

{
	if (STRINGP(x))
		return mk_simbolo("#t");
	else
		return NIL;
}

Ref *
choclo_symbolp(x)
	Ref *x;

{
	if (SIMBOLOP(x))
		return mk_simbolo("#t");
	else
		return NIL;
}

Ref *
choclo_int_eql(n, m)
	Ref *n, *m;
{
	if (VAL_INT(n) == VAL_INT(m))
		return mk_simbolo("#t");
	else
		return NIL;
}

Ref *
choclo_int_add(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	return mk_int(VAL_INT(n) + VAL_INT(m));
}

Ref *
choclo_int_sub(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	return mk_int(VAL_INT(n) - VAL_INT(m));
}

Ref *
choclo_int_lt(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	if (VAL_INT(n) < VAL_INT(m))
		return n;
	else
		return NIL;
}

Ref *
choclo_int_gt(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	if (VAL_INT(n) > VAL_INT(m))
		return n;
	else
		return NIL;
}

Ref *
choclo_int_le(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	if (VAL_INT(n) <= VAL_INT(m))
		return n;
	else
		return NIL;
}

Ref *
choclo_int_ge(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	if (VAL_INT(n) >= VAL_INT(m))
		return n;
	else
		return NIL;
}

Ref *
choclo_int_mul(n, m)
	Ref *n, *m;
{
	assert(INTP(n) && INTP(m));
	return mk_int(VAL_INT(n) * VAL_INT(m));
}

/* entrada / salida */

void
display(f, x)
	FILE *f;
	Ref *x;
{
	if (NULLP(x)) {
		fprintf(f, "()");
	} else {
		switch (x->tipo) {
		case tipo_int:
			fprintf(f, "%i", VAL_INT(x));
			break;
		case tipo_cons: {
			Ref *p;

			fprintf(f, "(");
			display(f, CAR(x));
			for (p = CDR(x); CONSP(p); p = CDR(p)) {
				fprintf(f, " ");
				display(f, CAR(p));
			}
			if (!NULLP(p)) {
				/* es una lista impropia */
				fprintf(f, " . ");
				display(f, p);
			}
			fprintf(f, ")");
			break;
		}
		case tipo_simbolo:
			fprintf(f, "%s", VAL_SIMBOLO(x)->nombre);
			break;
		case tipo_string:
			fprintf(f, "%s", VAL_STRING(x));
			break;
		case tipo_array: {
			unsigned i;
			Ref **tabla = VAL_ARRAY(x)->tabla;

			fprintf(f, "#(");
			for (i = 0; i < VAL_ARRAY(x)->tam; ++i) {
				if (i != 0) fprintf(f, " ");
				display(f, *(tabla++));
			}
			fprintf(f, ")");
			break;
		}
		default:
			fprintf(f, "#<no imprimible %lx>", x);
			break;
		}
	}
}

Ref *
choclo_pr(x)
	Ref *x;
{
	display(stdout, x);
	fprintf(stdout, "\n");
	return x;
}

Ref *
choclo_pr1(x)
	Ref *x;
{
	display(stdout, x);
	return x;
}

Ref *
choclo_out(f, x)
	Ref *f, *x;
{
	assert(STREAMP(f));
	display(VAL_STREAM(f), x);
	fprintf(VAL_STREAM(f), "\n");
	return x;
}

Ref *
choclo_out1(f, x)
	Ref *f, *x;
{
	assert(STREAMP(f));
	display(VAL_STREAM(f), x);
	return x;
}

Ref *
choclo_out_string(stream, x)
	Ref *stream, *x;
{
	FILE *f;
	char *s;

	assert(STRINGP(x));
	assert(STREAMP(stream));

	f = VAL_STREAM(stream);

	fprintf(f, "\"");
	for (s = VAL_STRING(x); *s != '\0'; ++s) {
		switch (*s) {
		case '\\': case '"':
			fputc('\\', f);
			fputc(*s, f);
			break;
		default:
			fputc(*s, f);
			break;
		}
	}
	fprintf(f, "\"");
	return x;
}

Ref *
choclo_die(x)
	Ref *x;
{
	display(stderr, x);
	fprintf(stderr, "\n\n");
	exit(1);
}

#define TOK_START	0
#define TOK_LPAREN	256
#define TOK_RPAREN	257
#define TOK_STR		258
#define TOK_INT		259
#define TOK_ID		260
#define TOK_DOT		261
#define TOK_QUOTE	262
#define TOK_EOF		263

#define MAX_STR		256

Ref *valor;

char es_espacio(char c) {
	return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

char es_caracter_simbolo(char c) {
	return c != '(' && c != ')' && c != '"' && !es_espacio(c);
}

int gettok(FILE *f) {
	int cc = fgetc(f);
	char c = cc;

	/* se come los espacios y los comentarios */
	for (;;) {
		while (es_espacio(c)) {
			c = (char)(cc = fgetc(f));
		}
		if (c == ';') {
			do {
				c = (char)(cc = fgetc(f));
			}
			while (c != '\n');
		} else
			break;
	}

	if (cc == EOF)
	    return TOK_EOF;

	switch (c) {
	case '(':
		return TOK_LPAREN;
	case ')':
		return TOK_RPAREN;
	case '\'':
		return TOK_QUOTE;
	case '"': {
		char buf[MAX_STR];
		unsigned int i = 0;

		while ((c = fgetc(f)) != '"') {
			assert(i + 1 < MAX_STR);
			if (c == '\\') {
				buf[i] = c;
				++i;
				c = fgetc(f);
				assert(i + 1 < MAX_STR);
			}
			buf[i] = c;
			++i;
		}
		buf[i] = '\0';
		valor = mk_string(buf);
		return TOK_STR;
	}
	case '.':
		return TOK_DOT;
	default:
		if (isdigit(c)) {
			int n = 0;

			do {
				n *= 10;
				n += c - '0';
			} while (isdigit(c = fgetc(f)));
			ungetc(c, f);
			valor = mk_int(n);
			return TOK_INT;
		} else {
			char buf[MAX_STR];
			unsigned int i = 0;

			assert(!es_espacio(c));
			do {
				assert(i + 1 < MAX_STR);
				buf[i] = c;
				++i;
			} while (es_caracter_simbolo(c = fgetc(f)));
			ungetc(c, f);

			buf[i] = '\0';
			valor = mk_simbolo(buf);
			return TOK_ID;
		}
	}
}

Ref *
read_with_token(f, tok)
	FILE *f;
	int tok;
{
	Ref *r;

	if (tok == TOK_START) tok = gettok(f);

	if (tok == TOK_EOF) {
		return CHOCLO_EOF;
	} else if (tok == TOK_QUOTE) {
		r = read_with_token(f, TOK_START);
		assert(r != CHOCLO_EOF); /* check EOF */
		return mk_cons(mk_simbolo("quote"), mk_cons(r, NIL));
	} else if (tok == TOK_LPAREN) {
		Ref *l = NIL;
		Ref **lista;

		lista = &l;
		while ((tok = gettok(f)) != TOK_RPAREN && tok != TOK_DOT) {
			r = read_with_token(f, tok);
			
			assert(r != CHOCLO_EOF); /* check EOF */

			*lista = mk_cons(r, NIL);
			lista = &CDR(*lista);
		}

		if (tok == TOK_DOT) {
			r = read_with_token(f, TOK_START);
			assert(r != CHOCLO_EOF); /* check EOF */

			*lista = r;
			tok = gettok(f);
			assert(tok == TOK_RPAREN);
		}

		return l;
	} else {
		return valor;
	}
}

Ref *
choclo_read(stream)
	Ref *stream;
{
	FILE *f = VAL_STREAM(stream);
	Ref *r = read_with_token(f, TOK_START);
	if (r == CHOCLO_EOF)
		return mk_simbolo("eof");
	else
		return r;
}

Ref *
choclo_stdin()
{
	static Ref *f = NULL;

	if (f == NULL) f = mk_stream(stdin);
	return f;
}

Ref *
choclo_open(nom_arch, mode)
	Ref *nom_arch, *mode;

{
	FILE *f = fopen(VAL_STRING(nom_arch), VAL_STRING(mode));
	if (f == NULL)
		return NIL;
	else
		return mk_stream(f);
}

Ref *
choclo_close(stream)
	Ref *stream;
{
	assert(STREAMP(stream));
	fclose(VAL_STREAM(stream));
	return NIL;
}
