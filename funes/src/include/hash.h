/* PROTOS */
RES *fu_make_hash(RES *len);
RES *fu_db(RES *args);

RES *fu_set_hash_f(RES *eqfun, RES *hashfun, RES *hash, RES *clave, RES *valor);
RES *fu_get_hash_f(RES *eqfun, RES *hashfun, RES *hash, RES *clave);
RES *fu_del_hash_f(RES *eqfun, RES *hashfun, RES *hash, RES *clave);
RES *fu_rehash_f(RES *eqfun, RES *hashfun, RES *hash, RES *tam);

RES *fu_set_hash_eq(RES *hash, RES *clave, RES *valor);
RES *fu_get_hash_eq(RES *hash, RES *clave);
RES *fu_del_hash_eq(RES *hash, RES *clave);
RES *fu_rehash_eq(RES *hash, RES *tam);

RES *fu_set_hash(RES *hash, RES *clave, RES *valor);
RES *fu_get_hash(RES *hash, RES *clave);
RES *fu_del_hash(RES *hash, RES *clave);
RES *fu_rehash(RES *hash, RES *tam);

RES *fu_db_data(RES *hash, RES *func);
RES *fu_db_vec(RES *hash);

RES *fu_copy_hash(RES *hash);
RES *fu_db_keys(RES *hash);
RES *fu_db_size(RES *hash);

RES *fu_hash_eq(RES *x, RES *len);

#define HASH_P(R)	(TIPO_P(tipo_vector, R) && (VECTOR_TIPO(VAL_VECTOR(R)) == vector_hash))

#define HASH_LEN(H)	VECTOR_LENGTH(VAL_VECTOR(H))
#define HASH_SIZE(H)	VECTOR_TAG(VAL_VECTOR(H))

