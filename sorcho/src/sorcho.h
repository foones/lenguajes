#define GC_malloc malloc
#define NEW(TIPO)	((TIPO *) GC_malloc(sizeof(TIPO)))
#define NEWQ(TIPO, Q)	((TIPO *) GC_malloc((Q) * sizeof(TIPO)))

typedef enum {	tipo_nada, tipo_entero, tipo_simbolo,
		tipo_algebraico, tipo_aplicacion, tipo_constructor,
		tipo_funcion, tipo_cerradura, tipo_generica,
		tipo_construccion, tipo_maquina, tipo_dondura,
		tipo_char, tipo_entorno, } Tipo;

typedef struct _objeto Objeto;
struct _objeto {
	Tipo tipo;
	union {
		int v_int;
		char v_char;
		char *v_nombre;
		struct _algebraico *v_algebraico;
		struct _aplicacion *v_aplicacion;
		struct _constructor *v_constructor;
		struct _funcion *v_funcion;
		struct _cerradura *v_cerradura;
		struct _dondura *v_dondura;
		struct _lista *v_generica;
		struct _construccion *v_construccion;
		struct _maquina *v_maquina;
		struct _entorno *v_entorno;
	} valor;
};
#define VAL_INT(O)		((O)->valor.v_int)
#define VAL_NOMBRE(O)		((O)->valor.v_nombre)
#define VAL_ALGEBRAICO(O)	((O)->valor.v_algebraico)
#define VAL_APLICACION(O)	((O)->valor.v_aplicacion)
#define VAL_CONSTRUCTOR(O)	((O)->valor.v_constructor)
#define VAL_FUNCION(O)		((O)->valor.v_funcion)
#define VAL_CERRADURA(O)	((O)->valor.v_cerradura)
#define VAL_DONDURA(O)		((O)->valor.v_dondura)
#define VAL_GENERICA(O)		((O)->valor.v_generica)
#define VAL_CONSTRUCCION(O)	((O)->valor.v_construccion)
#define VAL_MAQUINA(O)		((O)->valor.v_maquina)
#define VAL_CHAR(O)		((O)->valor.v_char)
#define VAL_ENTORNO(O)		((O)->valor.v_entorno)

typedef struct _algebraico Algebraico;
struct _algebraico {
	struct _objeto *constructor;
	struct _lista *elementos;
};

typedef struct _lista Lista;
struct _lista {
	struct _objeto *elemento;
	struct _lista *siguiente;
};

typedef struct _constructor Constructor;
struct _constructor {
	char *nombre;
	unsigned char aridad;
};

typedef struct _aplicacion Aplicacion;
struct _aplicacion {
	Objeto *functor;
	Objeto *arg;
};

typedef struct _funcion Funcion;
struct _funcion {
	struct _entorno *entorno;
	struct _objeto *arg;
	struct _objeto *cuerpo;
};

typedef struct _cerradura Cerradura;
struct _cerradura {
	struct _entorno *entorno;
	struct _objeto *expresion;
};

typedef struct _dondura Dondura;
struct _dondura {
	struct _hash *hash;
	struct _objeto *expresion;
};

typedef struct _construccion Construccion;
struct _construccion {
	struct _objeto *constructor;
	struct _lista *elementos;
	struct _lista *ultimo;
	unsigned char longitud;
};

extern struct _hash *Actual;
extern struct _entorno *Pila_Actuales;
extern Objeto *Read_Sexpr;
extern jmp_buf Err_Salto;
extern char Charlatan;
extern char Koan;

/* PROTOS */

Lista *mk_par(Objeto *o, Lista *sig);

Objeto *mk_objeto_algebraico(Objeto *cons, Lista *elts);
Objeto *mk_int(int i);
Objeto *mk_char(char c);
Objeto *mk_aplicacion(Objeto *ador, Objeto *ando);
Objeto *mk_construccion(Objeto *ador, Objeto *ando);
Objeto *mk_simbolo(char *nombre);
Objeto *mk_constructor(char *nom, unsigned char ari);
Objeto *mk_nada();
Objeto *mk_funcion(struct _entorno *entorno, struct _objeto *argumentos,
		struct _objeto *cuerpo);
Objeto *mk_cerradura(struct _entorno *entorno, struct _objeto *expresion);
Objeto *mk_dondura(struct _hash *hash, struct _objeto *expresion);
Objeto *mk_generica(struct _lista *l);
Objeto *mk_curry(struct _entorno *e, struct _lista *argumentos,
		struct _objeto *expresion);
Objeto *mk_construccion(Objeto *ador, Objeto *elt);
void construccion_push(Objeto *construccion, Objeto *elt);
Objeto *mk_objeto_entorno(struct _entorno *e);
