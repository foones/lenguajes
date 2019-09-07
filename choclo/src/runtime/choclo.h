#ifndef _CHOCLO_H_
#define _CHOCLO_H_

#include <stdio.h>

#define _PROTO(X)	X

typedef enum {
	tipo_int, tipo_cons, tipo_vector,
	tipo_closure, tipo_proc, tipo_simbolo,
	tipo_stream, tipo_array, tipo_string,
} Tipo;

typedef struct _fer Ref;
struct _fer {
	Tipo tipo;
	void *valor;
};

typedef struct _cons Cons;
struct _cons {
	Ref *car, *cdr;
};

typedef struct _closure Closure;
struct _closure {
	Ref *envs;
	unsigned nargs;
	Ref *(*funcion) _PROTO((Ref *envs));
};

typedef struct _proc Proc;
struct _proc {
	unsigned nargs;
	Ref *(*funcion)();
};

typedef struct _simbolo Simbolo;
struct _simbolo {
	char *nombre;
};

typedef struct _array Array;
struct _array {
	unsigned int tam;
	Ref **tabla;
};

#define NEW(T)	 	((T *)(xmalloc(sizeof(T))))
#define NEWQ(T,Q) 	((T *)(xmalloc(sizeof(T)*(Q))))
#define MK_OBJETO(T,V)	mk_objeto(T, (void *) (V))

#define VAL_INT(X)	((int)(X)->valor)
#define VAL_CONS(X)	((Cons *)(X)->valor)
#define VAL_VECTOR(X)	((Ref **)(X)->valor)
#define VAL_CLOSURE(X)	((Closure *)(X)->valor)
#define VAL_PROC(X)	((Proc *)(X)->valor)
#define VAL_SIMBOLO(X)	((Simbolo *)(X)->valor)
#define VAL_STRING(X)	((char *)(X)->valor)
#define VAL_STREAM(X)	((FILE *)(X)->valor)
#define VAL_ARRAY(X)	((Array *)(X)->valor)

#define CALL_PROC(X,R)	((VAL_PROC(X)->funcion)R)

#define CAR(X)		(VAL_CONS(X)->car)
#define CDR(X)		(VAL_CONS(X)->cdr)

#define CAAR(X)		CAR(CAR(X))
#define CDAR(X)		CDR(CAR(X))

#define NIL		NULL
#define CHOCLO_EOF	((void *)0x1)
#define NULLP(X)	((X) == NULL)
#define CONSP(X)	(!NULLP(X) && (X)->tipo == tipo_cons)
#define INTP(X)		(!NULLP(X) && (X)->tipo == tipo_int)
#define VECTORP(X)	(!NULLP(X) && (X)->tipo == tipo_vector)
#define ARRAYP(X)	(!NULLP(X) && (X)->tipo == tipo_array)
#define PROCP(X)	(!NULLP(X) && (X)->tipo == tipo_proc)
#define SIMBOLOP(X)	(!NULLP(X) && (X)->tipo == tipo_simbolo)
#define STRINGP(X)	(!NULLP(X) && (X)->tipo == tipo_string)
#define STREAMP(X)	(!NULLP(X) && (X)->tipo == tipo_stream)

Ref *mk_objeto _PROTO((Tipo t, void *v));
Ref *mk_int _PROTO((int i));
Ref *mk_cons _PROTO((Ref *car, Ref *cdr));
Ref *mk_vector _PROTO((unsigned int tam));
Ref *mk_array _PROTO((unsigned int tam));
Ref *mk_closure
 _PROTO((Ref *envs, unsigned int nargs, Ref *(*funcion) _PROTO((Ref *envs))));
Ref *mk_proc _PROTO((unsigned int nargs, Ref *(*funcion)()));
Ref *mk_simbolo _PROTO((char *nombre));
Ref *mk_string _PROTO((char *cadena));
Ref *mk_stream _PROTO((FILE *arch));
Ref *aplicar _PROTO((Ref *closure, Ref *args, unsigned int nargs));

Ref *choclo_eq _PROTO((Ref *v, Ref *w));
Ref *choclo_int_eql _PROTO((Ref *n, Ref *m));
Ref *choclo_int_add _PROTO((Ref *n, Ref *m));
Ref *choclo_int_sub _PROTO((Ref *n, Ref *m));
Ref *choclo_int_mul _PROTO((Ref *n, Ref *m));

Ref *choclo_int_lt _PROTO((Ref *n, Ref *m));
Ref *choclo_int_gt _PROTO((Ref *n, Ref *m));
Ref *choclo_int_le _PROTO((Ref *n, Ref *m));
Ref *choclo_int_ge _PROTO((Ref *n, Ref *m));

Ref *choclo_car(Ref *cons);
Ref *choclo_cdr(Ref *cons);
Ref *choclo_consp(Ref *x);
Ref *choclo_intp(Ref *x);
Ref *choclo_stringp(Ref *x);
Ref *choclo_symbolp(Ref *x);

/* choclib */
Ref *choclo_pr _PROTO((Ref *x));
Ref *choclo_pr1 _PROTO((Ref *x));
Ref *choclo_out _PROTO((Ref *f, Ref *x));
Ref *choclo_out1 _PROTO((Ref *f, Ref *x));
Ref *choclo_out_string _PROTO((Ref *stream, Ref *x));
Ref *choclo_die _PROTO((Ref *x));
Ref *choclo_read _PROTO((Ref *stream));
Ref *choclo_stdin _PROTO(());

Ref *choclo_open(Ref *nom_arch, Ref *mode);
Ref *choclo_close(Ref *stream);

Ref *choclo_symtab();

#endif /*_CHOCLO_H_*/
