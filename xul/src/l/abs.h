#define NL printf("\n")

/* Macros para la memoria */
#define CHECK(P)		if (!(P)) { return 0; }	/* XXX: Hacer una excepcion */
#define ALLOC(P,TIPO)		P = (TIPO *) malloc( sizeof( TIPO ) ); CHECK(P)
#define FREE(P)			if ((P)) free(P)

/******************************************************************************/

/* Macros para el manejo de referencias */
#define REF(RES)		(RES)->referencias
#define DECREF(RES)		((RES)->referencias)--
#define INCREF(RES)		if (REF(RES) != -1) ((RES)->referencias)++

/******************************************************************************/

/* Chequeo de tipos */
#define DEBE_SER(RES,TIPO)	if (((RES)->tipo) != &TIPO) {			\
					RAISE(ExType);				\
					return;					\
				} /* XXX: Hacer una excepcion */

#define GET_TIPO(RES)		((RES)->tipo)

/******************************************************************************/
/* XXX: Hacer una excepcion */
#define LLAMAR(RES,FUNCION,ARGS) ( (((RES))->tipo->FUNCION) ?\
					(((RES))->tipo->FUNCION) ARGS :\
					 FALLO )

/* Macros para el acceso a funciones */
#define Nuevo(TIPO)		(TIPO).constructor(0)
#define NuevoPars(TIPO,PARS)	(TIPO).constructor(PARS)
#define Liberar(RES)		LLAMAR(RES,destructor,(RES))
#define Ejecutar(RES,E,P,A)	LLAMAR(RES,ejecutar,(RES,E,P,A))
#define Cadena(RES)		LLAMAR(RES,cadena,(RES))
#define HASH(RES)		(((RES)->tipo->hash)(RES))

#define Imprimir(RES)		printf("%s", StrVal(Cadena(RES)) )
#define ImprArch(ARCH,RES)	fprintf(ARCH,"%s", StrVal(Cadena(RES)) )

#define Comparar(RES,Q)		LLAMAR(RES,comparar,(RES,Q))
#define Copiar(RES)		LLAMAR(RES,copiar,(RES))
#define Eval(RES)		LLAMAR(RES,eval,(RES))
#define Anexar(RES,OP,NOM,SIM)	LLAMAR(RES,anexar,(RES,OP,NOM,SIM))
/* Secuencia */
#define Longitud(RES)		LLAMAR(RES,longitud,(RES))
#define Push(RES,IT,IDX)	LLAMAR(RES,push,(RES,IT,IDX))
#define Pop(RES,IDX)		LLAMAR(RES,pop,(RES,IDX))
#define Get(RES,IDX)		LLAMAR(RES,get,(RES,IDX))
#define Set(RES,IT,IDX)		LLAMAR(RES,set,(RES,IT,IDX))
#define Del(RES,IDX)		LLAMAR(RES,del,(RES,IDX))
/* Cons */
#define GetHead(RES)		LLAMAR(RES,gethead,(RES))
#define GetTail(RES)		LLAMAR(RES,gettail,(RES))
#define SetHead(RES,Q)		LLAMAR(RES,sethead,(RES,Q))
#define SetTail(RES,Q)		LLAMAR(RES,settail,(RES,Q))
/* Numero */
#define Suma(V,W)		LLAMAR(V,suma,(V,W))
#define Resta(V,W)		LLAMAR(V,resta,(V,W))
#define Mult(V,W)		LLAMAR(V,mult,(V,W))
#define Division(V,W)		LLAMAR(V,division,(V,W))
#define MenosUnario(V)		LLAMAR(V,unaryminus,(V))
#define Modulo(RES,FMT)		LLAMAR(RES,modulo,(RES,FMT))
/* Bool */
#define And(V,W)		LLAMAR(V,and,(V,W))
#define Or(V,W)			LLAMAR(V,or,(V,W))
#define Xor(V,W)		LLAMAR(V,xor,(V,W))
#define Not(V)			LLAMAR(V,not,(V))

/* SLICES */
#define SLICE(SEQ,IN,FI)	SeqSlice(SEQ,IN,FI)

#define HASH_SET(H,VAL,CAD)	{\
					Res *tmp;\
					tmp = StrFromStr(CAD);\
					Set( H, VAL, tmp );\
					Liberar( tmp );\
				}

#define HASH_GET(H,COSA,CAD)	{\
					Res *tmp, *z;\
					tmp = StrFromStr(CAD);\
					COSA = Get( H, tmp );\
					Liberar( tmp );\
				}

/******************************************************************************/

/* Objetos */
#define NULO			(Res *)&Nulo
#define VERDADERO		(Res *)&Verdadero
#define FALSO			(Res *)&Falso
#define INFINITO		(Res *)&Infinito
#define FALLO			(Res *)&Fallo

/******************************************************************************/

#define NumVal(RES)		((RES)->valor.numero)
#define StrVal(RES)		((RES)->valor.cadena)
#define LstCabVal(RES)		((RES)->valor.lista)
#define LstVal(RES)		((RES)->valor.lista->listaptr)
#define LstLen(RES)		((RES)->valor.lista->longitud)
#define HshCabVal(RES)		((RES)->valor.hash)
#define HshVal(RES)		((RES)->valor.hash->hashptr)
#define HshLen(RES)		((RES)->valor.hash->longitud)

/******************************************************************************/

/* Para recorrer una secuencia */
#define Iterar(IT,LST)		for ( (IT) = LstVal(LST); (IT); (IT) = (IT)->next )
/* Elemento de un hash */
#define Elemento(HASH,KEY)	((HASH)->valor.hash->hashptr->tabla[KEY])

/******************************************************************************/
/* Manejo de excepciones */

#define RAISE(EXCEPTION)	{\
					Res *final;\
					final = IntFromInt(-1);\
					Push(Excepciones,EXCEPTION,final);\
					Liberar(final);\
				}
