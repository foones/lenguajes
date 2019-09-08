#define HASH_PRIMO 211

typedef struct _entorno Entorno;
struct _entorno {
	struct _hash *hash;
	struct _entorno *padre;
};

typedef struct _hash Hash;
struct _hash {
	unsigned buckets;
	struct _lista_cv **tabla;
};

typedef struct _lista_cv ListaCV;
struct _lista_cv {
	struct _clave_valor *cv;
	struct _lista_cv *siguiente;
};

typedef struct _clave_valor ClaveValor;
struct _clave_valor {
	char *clave;
	struct _objeto *valor;
};

/* PROTOS */
#if 0
int pjw_hash(char *s, int modulo);
ListaCV *mk_lista_cv(ClaveValor *cv, ListaCV *sig);
ClaveValor *mk_clave_valor(char *c, Object *v);
Hash *mk_hash(int cant_buckets);
Objeto *hash_set(Hash *h, char *idx, Objeto *it);
Objeto *hash_get(Hash *h, char *idx);
Entorno *mk_entorno(Hash *h, Entorno *padre);
Objeto *binding(Entorno *e, char *nombre);
#endif
Objeto *binding(Entorno *e, char *nombre);
Entorno *mk_entorno(Hash *h, Entorno *padre);
Hash *mk_hash(unsigned cant_buckets);
Objeto *hash_set(Hash *h, char *idx, struct _objeto *it);
Objeto *hash_get(Hash *h, char *idx);
