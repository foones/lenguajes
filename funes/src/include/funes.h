
/*
 * para hacer enteros y conses mas baratos
 * en vez de guardar un struct con tipo
 * un puntero a verdadero res termina siempre
 * (en bits) en 00     --> no inmediato
 * a int     en 01     --> inmediatos
 * a cons    en 10
 * a otras   en 11   -- nil #t #f #undef char
 * y no hay problemas porque al allocar,
 * siempre estan alineados a 00
 */
enum tipo {
	tipo_sym=1,
	tipo_vector,
	tipo_proc,
 	tipo_special_form,
	tipo_closure,
	tipo_port,
	tipo_reader,
	tipo_stype,
	tipo_struct,
	tipo_tipo,
	tipo_float,
	tipo_cont,
	tipo_package,

	/* tipos magicos */
	tipo_magic_char,
	tipo_magic_cons,
	tipo_magic_int,
	tipo_magic_null,
	tipo_magic_t,
	tipo_magic_eof,
	tipo_magic_undef,
	tipo_magic_macro,
	tipo_magic_str,
	tipo_magic_hash,
	tipo_magic_any,
};

typedef struct _res RES;
struct _res {
	char tipo;
	void *datos;
};

extern RES *NIL;
extern RES *T;
extern RES *EOF_OBJECT;
extern RES *UNDEF;
extern RES *ARGSET;
extern RES *Fu_Argv;

typedef long long int fuint;

RES *fu_args();

/* Para uso personal... */
#define TRUE 1
#define FALSE 0

/* MACROS */
#define NEW(TIPO)	((TIPO *) GC_MALLOC(sizeof(TIPO)))
#define NEWQ(TIPO,Q)	((TIPO *) GC_MALLOC((Q) * sizeof(TIPO)))
#if 0
#define NEW(TIPO)	((TIPO *) malloc(sizeof(TIPO)))
#define NEWQ(TIPO,Q)	((TIPO *) malloc((Q) * sizeof(TIPO)))
#endif

#define VAL(R)        	((R)->datos)
#define TIPO(R)		((R)->tipo)

#define NINMEDIATO_P(R)	(((fuint) (R) & 3) == 0)
#define INMEDIATO_P(R)	(!NINMEDIATO_P(R))
#define INT_P(R)	(((fuint) (R) & 3) == 1)
#define CONS_P(R)	(((fuint) (R) & 3) == 2)
#define CHAR_P(R)	(((fuint) (R) & 15) == 7)
#define TIPO_P(T,R)	(NINMEDIATO_P(R) && (TIPO(R) == T))

#define NULL_P(R)	((R) == NIL)
