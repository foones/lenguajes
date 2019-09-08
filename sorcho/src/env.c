#include "Sor.h"

int pjw_hash(char *s, int modulo)
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

ListaCV *mk_lista_cv(ClaveValor *cv, ListaCV *sig)
{
	ListaCV *lcv = NEW(ListaCV);

	lcv->cv = cv;
	lcv->siguiente = sig;
	return lcv;
}

ClaveValor *mk_clave_valor(char *c, Objeto *v)
{
	ClaveValor *cv = NEW(ClaveValor);

	cv->clave = c;
	cv->valor = v;
	return cv;
}

Hash *mk_hash(unsigned cant_buckets)
{
	int i;
	Hash *h = NEW(Hash);

	h->buckets = cant_buckets;
	h->tabla = NEWQ(ListaCV *, cant_buckets);
	for (i = 0; i < cant_buckets; i++)
		h->tabla[i] = 0;
	return h;
}

#define EL h->tabla[key]

Objeto *hash_set(Hash *h, char *idx, Objeto *it)
{
	int key;

	key = pjw_hash(idx, h->buckets);
	if (!(EL)) {
		/* El lugar estaba vacio */
		EL = mk_lista_cv(mk_clave_valor(idx, it), 0);
	} else {
		ListaCV *q;
		/* El lugar estaba ocupado */
		/* Reviso si ya existe la clave */
		for ( q = EL; q; q = q->siguiente ) {
			if ( ! strcmp(q->cv->clave, idx) ) {
				/* En este caso ya existia */
				q->cv->valor = it;
				return it;
			}
		}
		/* En este caso, no existia en ese compartimento un dato con
		 * la misma clave */
		EL = mk_lista_cv(mk_clave_valor(idx, it), EL);
	}
	return it;
}

Objeto *hash_get(Hash *h, char *idx)
{
	int key;

	key = pjw_hash(idx, h->buckets);	
	if (EL) {
		ListaCV *q;
		for ( q = EL; q; q = q->siguiente )
			if ( ! strcmp(q->cv->clave, idx) )
				return q->cv->valor;
	}
	return 0;
}
#undef EL

Entorno *mk_entorno(Hash *h, Entorno *padre)
{
	Entorno *e = NEW(Entorno);

	e->hash = h;
	e->padre = padre;
	return e;
}

Objeto *binding(Entorno *e, char *nombre)
{
	Entorno *p;

	for (p = e; p; p = p->padre) {
		Objeto *o; 

		if (o = hash_get(p->hash, nombre))
			return o;
	}
	return 0;
}
