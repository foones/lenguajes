/* unidad de codigo
 * tiene un opcode (comando, por ejemplo PUSH, JMP, etc.)
 * y una opcion restringida a entero
 */
typedef struct _bcode BCODE;
struct _bcode {
	char opcode;
	int opt;
};

#define BCODE_OPCODE(C)			((C)->opcode)
#define BCODE_OPT(C)			((C)->opt)

/* par para formar listas enlazadas de bcode
 */
typedef struct _bcons BCONS;
struct _bcons {
	BCODE *code;
	BCONS *next;
};

#define BCONS_CODE(C)	((C)->code)
#define BCONS_NEXT(C)	((C)->next)

/* lista de bcode que contiene un puntero al primer
 * elemento y uno al ultimo para poder agregar un
 * elemento en tiempo constante
 */
typedef struct _blist BLIST;
struct _blist {
	BCONS *first;
	BCONS *last;
};
#define BLIST_FIRST(C)	((C)->first)
#define BLIST_LAST(C)	((C)->last)

BCODE *fu_make_bcode(char opcode, int opt);
BCONS *fu_make_bcons(BCODE *code, BCONS *next);
BLIST *fu_make_blist(BCONS *first);

/* una closure contiene
 *   codigo compilado
 *   vectores de constantes e identificadores
 *   puntero a entorno padre
 *   argumentos que toma
 */
typedef struct _closure CLOSURE;
struct _closure {
	BCODE **codevec;	/* vector de codigo */
	unsigned length;	/* longitud del codevec */
	RES *consts_vector;	/* vector de constantes */
	RES *arglist;		/* lista de argumentos */
	RES *env_padre;		/* puntero al entorno padre */
	unsigned char flags;	/* crea su propio entorno?
				   es un macro?
				   es dyn?
				   es un reader macro?  */
};

#define VAL_CLOSURE(RES)	((CLOSURE *) VAL(RES))

#define CLOSURE_CODEVEC(C)	((C)->codevec)
#define CLOSURE_LENGTH(C)	((C)->length)
#define CLOSURE_CONSTANTS(C)	((C)->consts_vector)
#define CLOSURE_ARGLIST(C)	((C)->arglist)
#define CLOSURE_ENVPADRE(C)	((C)->env_padre)
#define CLOSURE_FLAGS(C)		((C)->flags)

#define CLOSURE_NEWENV(C)		((C)->flags & 1)
#define CLOSURE_NEWENV_SET(C)	((C)->flags |= 1)
#define CLOSURE_NEWENV_UNSET(C)	((C)->flags &= ~1)

#define CLOSURE_MACRO(C)		((C)->flags & 2)
#define CLOSURE_MACRO_SET(C)	((C)->flags |= 2)
#define CLOSURE_MACRO_UNSET(C)	((C)->flags &= ~2)

#define CLOSURE_DYN(C)			((C)->flags & 4)
#define CLOSURE_DYN_SET(C)		((C)->flags |= 4)
#define CLOSURE_DYN_UNSET(C)	((C)->flags &= ~4)

#define CLOSURE_READ_MACRO(C)		((C)->flags & 8)
#define CLOSURE_READ_MACRO_SET(C)	((C)->flags |= 8)
#define CLOSURE_READ_MACRO_UNSET(C)	((C)->flags &= ~8)

#define CLOSURE_SYMBOL_MACRO(C)		((C)->flags & 16)
#define CLOSURE_SYMBOL_MACRO_SET(C)	((C)->flags |= 16)
#define CLOSURE_SYMBOL_MACRO_UNSET(C)	((C)->flags &= ~16)

/* una continuation contiene todo el estado
 * necesario para reflejar un computo pendiente
 */
typedef struct _cont CONT;
struct _cont {
	CLOSURE *clos;		/* current closure */
	unsigned pc;		/* program counter */
	RES *val;		/* VM register */
	RES *stack;		/* VM stack */
	RES *prev_env;		/* the environment before the call */
	RES *env;		/* the associated environment */
	char action;		/* next action to take */
	RES *function;		/* next function to call */
	RES *arguments;		/* arguments to pass */
	CONT *next;		/* continuation to follow */
};

#define VAL_CONT(R)	((CONT *) VAL(R))
#define CONT_P(R)	(TIPO_P(tipo_cont, R))

RES *fu_interpret(RES *c, RES *args);
RES *fu_eval(RES *expr);

void fu_closure_print(RES *c);

RES *fu_current_continuation();

#define FU_PUSH_BLIST(L,C) (BLIST_LAST(L) = (BCONS_NEXT(BLIST_LAST(L)) = C))
#define FU_BCONS(X) fu_make_bcons(X, (BCONS *) 0)

#define QUANTUM		10
