typedef struct _tipo TIPO;
struct _tipo {
	char *nombre;
	RES *descripcion;
};

/* PROTOS */
void fu_init_tipos();
RES *fu_typep(RES *tipo, RES *expresion);
RES *fu_type(RES *des);
RES *fu_type_of(RES *expr);
RES *fu_check(RES *tipo, RES *expr, RES *errstr, RES *exc);

/* MACROS */
#define VAL_TIPO(R)		((TIPO *) VAL(R))
#define TIPO_DES(T)		((T)->descripcion)
#define TIPO_NOMBRE(T)		((T)->nombre)
#define TIPO_BUILT(T)		((fuint) TIPO_DES(T) >> 2)
#define TIPO_BUILTIN_P(T)	(((fuint) TIPO_DES(T) & 3) == 1)
#define TIPO_COMPUESTO_P(T)	(!TIPO_BUILTIN_P(T))
