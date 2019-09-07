#include "Fu.h"

/*
 * Un env es un CONS;
 *  su CAR es un hash que mapea simbolos a valores
 *  y su CDR es o bien NIL o bien otro env (el entorno padre).
 */
RES *Current_Env;

void
fu_init_env()
{
	Current_Env = fu_cons(fu_make_hash(fu_int(ENV_SIZE)), NIL);
}

/* hace un def (crea o establece un binding en el entorno
 * actual)
 */
__inline__ RES *
fu_def_env(clave,valor)
	RES *clave, *valor;
{
	RES *h = CAR(Current_Env);
	RES *r = fu_set_hash_eq(h, clave, valor);
	unsigned s = HASH_SIZE(h);
	unsigned l = HASH_LEN(h);

	if (s / l > 1)
		fu_rehash_eq(h, fu_int(2 * l + 1));

	return r;
}

/* hace un get (devuelve el binding para el entorno actual) */
RES *
fu_get_env(clave)
	RES *clave;
{
	RES *e, *r;

	for (e = Current_Env; CONS_P(e); e = CDR(e))
		if ( (r = fu_get_hash_eq(CAR(e), clave)) != NIL )
			return r;
	return NIL;
}

/* hace un set destructivo (si existe el binding lo modifica)
 */
RES *
fu_set_env(clave,valor)
	RES *clave, *valor;
{
	RES *e, *r;

	for (e = Current_Env; e != NIL; e = CDR(e))
		if ( (r = fu_get_hash_eq(CAR(e), clave)) != NIL )
			return (CDR(r) = valor);
	return NIL;
}

/* Crea un nuevo entorno cuyo padre es el entorno actual */
__inline__ void
fu_push_env()
{
	Current_Env = fu_cons(fu_make_hash(fu_int(ENV_SIZE)), Current_Env);
}

/* Vuelve un nivel en entornos (vuelve al padre del entorno actual)
 * XXX: inseguro */
__inline__ void
fu_pop_env()
{
	Current_Env = CDR(Current_Env);
}

RES *
fu_env()
{
	return Current_Env;
}

RES *
fu_dir()
{
	return CAR(Current_Env);
}

RES *
fu_with_env(e)
		RES *e;
{
		return Current_Env = e;
}
