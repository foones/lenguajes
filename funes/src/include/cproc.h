typedef struct _cproc CPROC;
struct _cproc {
	struct _res *(*funcion)();
	unsigned char nargs, nopts, flags;
};

/* PROTOS */
RES *fu_apply(RES *val, RES *args);
RES *fu_make_proc(RES *(*funcion)(), unsigned char nargs, unsigned char nopts,
										unsigned char restp);
RES *fu_apply_proc(RES *proc, RES *args);

#define MAX_PROC_ARGS 10

/* MACROS */
#define VAL_PROC(R)	((CPROC *) VAL(R))

#define PROC_FUNCION(P)	((P)->funcion)
#define PROC_NARGS(P)	((P)->nargs)
#define PROC_NOPTS(P)	((P)->nopts)

#define PROC_RESTP(P)		((P)->flags & 1)
#define PROC_REST_SET(P)	((P)->flags |= 1)
#define PROC_REST_UNSET(P)	((P)->flags &= ~1)

#define PROC_MACRO(P)		((P)->flags & 2)
#define PROC_MACRO_SET(P)	((P)->flags |= 2)
#define PROC_MACRO_UNSET(P)	((P)->flags &= ~2)

#define PROC_READ_MACRO(P)		((P)->flags & 4)
#define PROC_READ_MACRO_SET(P)		((P)->flags |= 4)
#define PROC_READ_MACRO_UNSET(P)	((P)->flags &= ~4)

#define MACROP(X)	((TIPO_P(tipo_proc,X) && PROC_MACRO(VAL_PROC(X))) || (TIPO_P(tipo_closure,X) && CLOSURE_MACRO(VAL_CLOSURE(X))))

#define READ_MACROP(X)	((TIPO_P(tipo_proc,X) && PROC_READ_MACRO(VAL_PROC(X))) || (TIPO_P(tipo_closure, X) && CLOSURE_READ_MACRO(VAL_CLOSURE(X))))

#define SYMBOL_MACROP(X) (TIPO_P(tipo_closure, X) && CLOSURE_SYMBOL_MACRO(VAL_CLOSURE(X)))
