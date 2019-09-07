#ifndef RES_H_INCLUDED
#define RES_H_INCLUDED


/* Constantes de compilación */
#define DEBUG

/* ESTRUCTURAS DE DATOS */

#define STR_MAX 255			/* Número máximo de caracteres por cadena */
/* Tipo cadena */
typedef unsigned char *Cadena;

/* Tipo para los opcodes */
typedef unsigned int Op;

/* Tipo numérico */
typedef signed long Num;

/* Para árboles */
typedef struct _arbol {
	Op opcode;
	Cadena nombre;
	Num simbolo;
	struct _arbol *hijo1;
	struct _arbol *hijo2;
} Arbol;

/* Para listas */
typedef struct _lista {
	struct _res *contenido;
	struct _lista *next;
} Lista;

typedef struct _cabecera_lista {
	Lista *listaptr;
	Num longitud;
} CabeceraLista;


/* Para hashes */
#define HASH_PRIMO 211			/* Número primo para hashes */

typedef struct {
	struct _res *head;
	struct _res *tail;
} Cons;

typedef struct {
	struct _res *tabla[HASH_PRIMO];
} Hash;

typedef struct {
	Hash *hashptr;
	Num longitud;
} CabeceraHash;

/* Para código LEN */
typedef struct _codigo {
	Op opcode;
	Cadena nombre;
	Num simbolo;
	struct _codigo *next;
} Codigo;

/* Tipo para valores */
typedef union _valor {
	Num numero;
	float flotante;
	Cadena cadena;
	CabeceraLista *lista;
	Cons *cons;
	CabeceraHash *hash;
	Codigo *codigo;
	FILE *archivo;
	struct _res *(*func)( struct _res *argumentos, struct _res *ambito);
	struct _tipo *tipoptr;
	struct _res *puntero;
} Valor;

/* El tipo Tipo */
typedef struct _tipo {
	Cadena nombre;
	/* Punteros a funciones (métodos). */
	struct _res *(*constructor)(struct _res *pars);
	void (*destructor)();
	struct _res *(*ejecutar)(struct _res *f, struct _res *entorno, struct _res *argumentos,
			struct _res *ambito);
	struct _res *(*cadena)(struct _res *p);
	struct _res *(*comparar)(struct _res *p, struct _res *q);
	struct _res *(*copiar)(struct _res *p);
	struct _res *(*eval)(struct _res *p);
	struct _res *(*anexar)(struct _res *p, Op opcode, Cadena nombre, Num simbolo);
	int (*hash)(struct _res *v);

	/* SECUENCIA */
	struct _res *(*longitud)(struct _res *p);
	void (*push)(struct _res *p, struct _res *it, struct _res *idx);
	struct _res *(*pop)(struct _res *p, struct _res *idx);
	struct _res *(*get)(struct _res *p, struct _res *idx);
	void (*set)(struct _res *p, struct _res *it, struct _res *idx);
	void (*del)(struct _res *p, struct _res *idx);
	
	/* CONS */
	struct _res *(*gethead)(struct _res *p);
	struct _res *(*gettail)(struct _res *p);
	void (*sethead)(struct _res *p, struct _res *q);
	void (*settail)(struct _res *p, struct _res *q);

	/* NUMERO */
	struct _res *(*suma)(struct _res *v, struct _res *w);
	struct _res *(*resta)(struct _res *v, struct _res *w);
	struct _res *(*mult)(struct _res *v, struct _res *w);
	struct _res *(*division)(struct _res *v, struct _res *w);
	struct _res *(*unaryminus)(struct _res *v);
	struct _res *(*modulo)(struct _res *p, struct _res *q);

	/* BOOL */
	struct _res *(*or)(struct _res *v, struct _res *w);
	struct _res *(*and)(struct _res *v, struct _res *w);
	struct _res *(*xor)(struct _res *v, struct _res *w);
	struct _res *(*not)(struct _res *v );
} Tipo;

/* Tipo para objetos propiamente dichos */
typedef struct _res {
	Tipo *tipo;
	Num referencias;
	Valor valor;
} Res;

/* FUNCIONES */
/* Genericas */
Res *ResNuevo(Res *pars);
void ResLiberar(Res *p);
void ResSet(Res *p, Valor n);
Res *ResComparar(Res *p, Res *q);
Res *ResCopiar(Res *p);
Res *ResRef(Res *p);
Res *ResDeref(Res *p);
Res *ResEval(Res *p);
Res *ResCadena(Res *p);

/* Enteros */
Res *IntNuevo(Res *pars);
Res *IntSuma(Res *v, Res *w);
Res *IntResta(Res *v, Res *w);
Res *IntMult(Res *v, Res *w);
Res *IntDivision(Res *v, Res *w);
Res *IntFromInt(Num n);
Res *IntComparar(Res *p, Res *q);
Res *IntEval(Res *p);
Res *IntCadena(Res *p);
Res *IntMenosUnario(Res *v);
int IntHash(Res *v);

/* Flotantes */
Res *FloatNuevo(Res *pars);
Res *FloatSuma(Res *v, Res *w);
Res *FloatResta(Res *v, Res *w);
Res *FloatMult(Res *v, Res *w);
Res *FloatDivision(Res *v, Res *w);
Res *FloatFromFloat(float n);
Res *FloatComparar(Res *p, Res *q);
Res *FloatEval(Res *p);
Res *FloatCadena(Res *p);
Res *FloatMenosUnario(Res *v);
int FloatHash(Res *v);

/* Conses */
Res *ConsNuevo(Res *pars);
void ConsLiberar(Res *p);
void ConsSetHead(Res *p, Res *q);
void ConsSetTail(Res *p, Res *q);
Res *ConsGetHead(Res *p);
Res *ConsGetTail(Res *p);
Res *ConsEval(Res *p);
Res *ConsCadena(Res *p);
Res *ConsCopiar(Res *p);

/* Codigo */
Codigo *_codigo_nuevo();
Res *CodigoNuevo(Res *pars);
void CodigoLiberar(Res *p);
Res *CodigoEjecutar(Res *f, Res *entorno, Res *argumentos, Res *ambito);
Res *CodigoPrograma(Res *f, Res *entorno, Res *argumentos, Res *ambito);
Res *CodigoComparar(Res *p, Res *q);
#ifdef DEBUG
void CodigoVisitar(Res *entorno);
#endif
Res *CodigoAgregar(Res *p, Op opcode, Cadena nombre, Num simbolo);

/* Arrays */
Res *AryNuevo(Res *pars);
void AryLiberar(Res *p);
Res *AryPop(Res *p, Res *idx);
Res *AryGet(Res *p, Res *idx);
void AryPush(Res *p, Res *it, Res *idx);
void ArySet(Res *p, Res *it, Res *idx);
void AryDel(Res *p, Res *idx);
Res *AryComparar(Res *p, Res *q);
Res *AryLongitud(Res *p);
Res *AryEval(Res *p);
Res *AryCadena(Res *p);
Res *ArySuma(Res *v, Res *w);
Res *AryCopiar(Res *p);

/* Hashes */
Res *HashNuevo(Res *pars);
void HashLiberar(Res *p);
Res *HashGet(Res *p, Res *idx);
void HashSet(Res *p, Res *it, Res *idx);
Res *HashEval(Res *p);
Res *HashCadena(Res *p);
Res *HashComparar(Res *p, Res *q);
Res *HashLongitud(Res *p);
Res *HashClaves(Res *p);
Res *HashCopiar(Res *p);
void HashPush(Res *p, Res *it, Res *idx);
Res *HashPop(Res *p, Res *idx);
void HashDel(Res *p, Res *idx);
void HashSetHead(Res *p, Res *q);
void HashSetTail(Res *p, Res *q);
Res *HashGetHead(Res *p);
Res *HashGetTail(Res *p);
Res *HashSumar(Res *v, Res *w);
Res *HashRestar(Res *v, Res *w);
Res *HashMult(Res *v, Res *w);
Res *HashDivision(Res *v, Res *w);
Res *HashMenosUnario(Res *v);
Res *HashOr(Res *v, Res *w);
Res *HashAnd(Res *v, Res *w);
Res *HashXor(Res *v, Res *w);
Res *HashNot(Res *v);
Res *HashEjecutar(Res *f, Res *entorno, Res *argumentos, Res *ambito);
Res *_hash_tiene(Res *p, Cadena funcion);

/* Cadenas */
Res *_str_nuevo(Num longitud);
Res *StrNuevo(Res *pars);
void StrLiberar(Res *p);
Res *StrComparar(Res *p, Res *q);
Res *StrFromStr(Cadena s);
Res *StrLongitud(Res *p);
void StrPush(Res *p, Res *it, Res *idx);
Res *StrSuma(Res *v, Res *w);
Res *StrEval(Res *p);
void StrAppend(Res *p, Res *q);
Res *StrCadena(Res *p);
Res *StrCopiar(Res *p);
Res *StrGet(Res *p, Res *idx);
void StrDel(Res *p, Res *idx);
Res *StrPop(Res *p, Res *idx);
int StrHash(Res *v);
Res *StrModulo( Res *p, Res *q );

/* Global */
Res *GlobalNuevo(Res *pars);

/* Tipo */
Res *TipoNuevo(Res *pars);
Res *TipoHacer(Res *p);

/* Funcion */
Res *FuncNuevo(Res *pars);
Res *FuncEjecutar(Res *f, Res *entorno, Res *argumentos, Res *ambito);

/* Archivos */
Res *FileNuevo(Res *pars);
Res *FileOpen(Res *q, Res *modo);
void FileClose(Res *p);
Res *FileEval(Res *p);
Res *FileReadLine(Res *p);

/* Arboles */
Arbol *ArbolNuevo(Op o, Cadena n, Num s, Arbol *h1, Arbol *h2);
#ifdef DEBUG
void ArbolVisitar( Arbol *ar );
#endif
Res *ArbolCompilar( Arbol *ar );
Codigo *ArbolCompilarRama( Arbol *ar, Codigo *lugar );

/* Booleanas */
Res *BoolNuevo(Res *pars);
Res *BoolComparar(Res *p, Res *q);
Res *BoolEval(Res *p);
Res *BoolCadena(Res *p);
Res *BoolOr(Res *v, Res *w);
Res *BoolXor(Res *v, Res *w);
Res *BoolAnd(Res *v, Res *w);
Res *BoolNot(Res *v);

/* Secuencias en general */
Res *SeqSlice(Res *p, Res *i, Res *f);

/* Errores */
int lose(Res *s);
int lostr(Cadena c);

/* Builtins */
Res *Builtins();

/* Objs */
void ObjsInit();

/* TIPOS */
extern Tipo IntTipo;
extern Tipo FloatTipo;
extern Tipo UndefTipo;
extern Tipo CodigoTipo;
extern Tipo AryTipo;
extern Tipo HashTipo;
extern Tipo BoolTipo;
extern Tipo ConsTipo;
extern Tipo StrTipo;
extern Tipo TipoTipo;
extern Tipo GlobalTipo;
extern Tipo FuncTipo;
extern Tipo FileTipo;

/* OBJETOS */
extern Res *Nulo;
extern Res *Verdadero;
extern Res *Falso;
extern Res *Infinito;
extern Res *Fallo;
extern Res *Excepciones;

extern Res *ExClass;
extern Res *ExType;
extern Res *ExAttr;
extern Res *ExOper;
extern Res *ExSub;
extern Res *ExFunc;
extern Res *ExSeq;

#endif /* RES_H_INCLUDED */
