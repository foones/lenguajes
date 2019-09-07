/* si el tipo de un vector es distinto de 0,
 * el vector es uniforme */

typedef enum {
	vector_no_uniforme = 0, vector_uniforme_char,
	vector_hash,
} vector_uniforme_tipo;

typedef struct _vector VECTOR;
struct _vector {
	unsigned len;
	struct _res **tabla;
	vector_uniforme_tipo tipo;
	unsigned tag;
};

/* PROTOS */
RES *fu_empty_vector(fuint l);
RES *fu_make_vector(RES *len, RES *inicial);
RES *fu_set_vector(RES *vector, RES *indice, RES *valor);
RES *fu_get_vector(RES *vector, RES *indice);
RES *fu_vector(char *format, ...);
RES *fu_vector_builtin(RES *args);

/* MACROS */
#define VAL_VECTOR(RES)	((VECTOR *) VAL(RES))

#define VECTOR_TABLA(V)			((V)->tabla)
#define VECTOR_ELEMENTO(V,INDICE)	((V)->tabla[INDICE])
#define VECTOR_LENGTH(V)		((V)->len)
#define VECTOR_TIPO(V)			((V)->tipo)
#define VECTOR_TAG(V)			((V)->tag)

#define VECTOR_NO_UNIFORME_P(V)	(TIPO_P(tipo_vector,V) && (VECTOR_TIPO(VAL_VECTOR(V)) == vector_no_uniforme))
