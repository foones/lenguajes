#include "Len.h"

Codigo *
_codigo_nuevo()
{
	Codigo *p;

	ALLOC(p, Codigo);
	p->opcode = OP_NOP;
	p->next = 0;
	return p;
}
	
Res *
CodigoNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &CodigoTipo;
	p->valor.codigo = _codigo_nuevo();
	p->valor.codigo->opcode = OP_NOP;
	return p;
}

void
CodigoLiberar(p)
	Res *p;
{
	Codigo *c, *sig;
	
	DEBE_SER(p,CodigoTipo);
	/* Los objetos con referencia -1 son eternos (Nulo, Infinito, etc.) */
	if ( REF(p) != -1 ) {
		DECREF(p);
		if ( !(REF(p)) ) {
			sig = c->next;
			for ( c = p->valor.codigo; c->next; c = sig ) {
				sig = c->next;
				FREE(c);
			}
			FREE(p);
		}
	}
}

Res *
CodigoAgregar(p,opcode,nombre,simbolo)
	Res *p;
	Op opcode;
	Cadena nombre;
	Num simbolo;
{
	Codigo *c, *q;

	DEBE_SER(p,CodigoTipo);
	for ( c = p->valor.codigo; c->next; c = c->next ) {
		/* vacio */
	}
	q = _codigo_nuevo();
	q->opcode = opcode;
	if ( nombre ) {
		q->nombre = (Cadena) malloc( sizeof(char) * (strlen(nombre) + 1) );
		CHECK( q->nombre );
		strcpy( q->nombre, nombre );
	} else
		q->nombre = 0;
	q->simbolo = simbolo;

	c->next = q;
}

#ifdef DEBUG
void
CodigoVisitar(entorno)
	Res *entorno;
{
	Res *codigo_entorno;			/* Código del entorno */
	Res *const_entorno, *hash_entorno;	/* Símbolos del entorno */
	Res *nombre_archivo;
	Res *final;
	Codigo *c;

	final = IntFromInt(-1);

	/* Desempaqueto el entorno */
	/* Fijarse en el orden de empaquetado en parser.c (inverso por ser una pila) */
	HASH_GET( entorno, codigo_entorno, "codigo_actual" );
/*	const_entorno = Pop( entorno, final );
	hash_entorno = Pop( entorno, final );
	nombre_archivo = Pop( entorno, final );*/
	/**/

	printf("----------COMIENZO-----------\n");
	for ( c = codigo_entorno->valor.codigo; c->opcode != OP_FIN && c; c = c->next ) {
		switch ( c->opcode ) {
		case OP_FIN:			
			printf("fin");
			break;
		case OP_NOP:
			printf("nop");
			break;
		case OP_IMPRIMIR:
			printf("imprimir");
			break;
		case OP_NUEVO:
			printf("nuevo");
			break;
		case OP_ID:
			printf("identificador %s", c->nombre);
			break;
		case OP_NUM:
			printf("constante numero %i", c->simbolo);
			break;
		case OP_SET:
			printf("set");
			break;
		case OP_DEL:
			printf("borrar");
			break;
		case OP_PLUS:
			printf("mas");
			break;
		case OP_MINUS:
			printf("menos");
			break;
		case OP_MULT:
			printf("por");
			break;			
		case OP_DIV:
			printf("dividido");
			break;
		case OP_JMPF:
			printf("salta si falso a %i", c->simbolo );			
			break;
		case OP_JMP:
			printf("salta a %i", c->simbolo );			
			break;
		case OP_LINENO:
			printf("linea número %i", c->simbolo );
			break;
		case OP_BUILDLIST:
			printf("hacer lista %i", c->simbolo );
			break;
		case OP_SUB:
			printf("subíndice");
			break;
		case OP_CALL:
			printf("llamar");
			break;
		case OP_RETURN:
			printf("devolver");
			break;
		case OP_SHIFT:
			printf("shift");
			break;
		case OP_OR:
			printf("or");
			break;
		case OP_XOR:
			printf("xor");
			break;
		case OP_AND:
			printf("and");
			break;
		case OP_NOT:
			printf("not");
			break;
		case OP_TEST:
			printf("test %i", c->simbolo);
			break;
		case OP_GLOBAL:
			printf("global");
			break;
		case OP_NAME:
			printf("name");
			break;
		case OP_UMINUS:
			printf("menos unario");
			break;
		case OP_SLICE:
			printf("slice");
			break;
		case OP_BUILDCONS:
			printf("hacer cons");
			break;
		case OP_BUILDHASH:
			printf("hacer hash");
			break;
		case OP_REF:
			printf("referencia");
			break;
		case OP_PUSH:
			printf("push");
			break;
		case OP_COPY:
			printf("copiar");
			break;
		case OP_READARGS:
			printf("lee parámetros");
			break;
		case OP_MOD:
			printf("modulo");
			break;
		case OP_EXEC:
			printf("exec");
			break;
		case OP_TRY:
			printf("probar");
			break;
		case OP_RAISE:
			printf("elevar");
			break;
		case OP_HANDLE:
			printf("manejar");
			break;
		case OP_DERIV:
			printf("derivar");
			break;
		}
		NL;
	}
	printf("-------------FIN-------------\n");
	Liberar( final );
}
#endif

#define PUSH(X)		Push(pila,X,final)
#define POP()		Pop(pila,final)
#define SALTAR()	{\
	Codigo *r;\
	for ( r = codigo_entorno->valor.codigo; r->opcode != OP_FIN && r; r = r->next ) {\
		if ( r->opcode == OP_LINENO && r->simbolo == c->simbolo ) {\
			c = r;\
			break;\
		}\
	}\
}

Res *
CodigoPrograma(f,entorno,argumentos,ambito)
	Res *f,*entorno, *argumentos, *ambito;
{
	CodigoEjecutar( f, entorno, argumentos, ambito );
}

Res *
CodigoEjecutar(f,entorno,argumentos,ambito)
	Res *f,*entorno, *argumentos, *ambito;
{
	Res *codigo_entorno;			/* Código del entorno */
	Res *const_entorno, *hash_entorno;	/* Símbolos del entorno */
	Res *nombre_archivo;
	Res *tabla_local;
	Res *es_modulo;
	Res *pila, *principio, *final;
	Res *x1, *x2, *x3, *x4;
	Res *tmp1, *tmp2;
	Res *ultimo_id;
	Res *tries;		/* Etiquetas para los try */
	Res *exc_actual;	/* La última excepción elevada */
	Lista *l;
	Codigo *c;
	int i;
	int EsModulo;
	int TryFlag = 0;	/* Verdadero si es el primer paso después de una excepción */

	principio = IntFromInt(0);
	final = IntFromInt(-1);

	codigo_entorno = f;
	/* Desempaqueto el entorno */
	/* Fijarse en el orden de empaquetado en parser.c */
	HASH_GET( entorno, const_entorno, "const_actual" );
	HASH_GET( entorno, hash_entorno, "__CURR_HASH" );
	HASH_GET( entorno, nombre_archivo, "nombre_archivo" );
	HASH_GET( entorno, es_modulo, "es_modulo" );
	/**/

	EsModulo = NumVal(es_modulo);
	ultimo_id = StrFromStr("");		/* Guardo el último identificador para mostrar
							en el debug */
	tries = Nuevo( AryTipo );		/* Guardo las etiquetas a las cuales tienen que
							ir los try ante una excepcion */

	pila = Nuevo( AryTipo );

	if ( EsModulo )
		tabla_local = hash_entorno;
	else
		tabla_local = Copiar( hash_entorno );
	
	for ( c = codigo_entorno->valor.codigo; c->opcode != OP_FIN && c; c = c->next ) {
		switch ( c->opcode ) {
		case OP_FIN:
			break;
		case OP_NOP:
			break;
		case OP_IMPRIMIR:
			switch ( c->simbolo ) {
			case 1:
				x1 = POP();
				Imprimir( x1 );
				break;
			case 0:
				printf("\n");
				break;
			}
			break;
		case OP_NUEVO:
			x1 = POP();				/* Parámetros */
			x2 = POP();				/* Tipo */
			if ( GET_TIPO(x2) == &TipoTipo )	/* Nuevo de un tipo */
				x3 = TipoHacer( x2 );
			else if ( GET_TIPO(x2) == &HashTipo ) {	/* Es un 'bless' */
				Push( x1, x2, principio );
				x3 = NuevoPars( HashTipo, x1 ); /* Creo el objeto */
			} else {
				RAISE( ExType ); break;
			}
			PUSH(x3);
			break;
		case OP_ID:
			tmp1 = StrFromStr( c->nombre );
			Liberar( ultimo_id );
			ultimo_id = tmp1; 
			/* Ámbito */

			/* Reviso la tabla de más arriba (la del final) que es
			 * la de builtins */
			{
				Res *l;
				l = Get(ambito, final);
				if ( l != NULO ) {
					x1 = Get( l, tmp1 );
					if ( x1 != NULO )
						goto CodigoEjecutar_Id_Fin;
				}
			}
			/* Reviso la tabla local */
			x1 = Get( tabla_local, tmp1 );
			/* Si el id es global, reviso de abajo hacia arriba el
			 * resto de las tablas */
			if ( GET_TIPO( x1 ) == &GlobalTipo ) {
				Lista *l;
				Iterar( l, ambito ) {
					x1 = Get( l->contenido, tmp1 );
					/* Hasta encontrar un id no global */
					if ( GET_TIPO( x1 ) != &GlobalTipo && x1 != NULO ) {
						break;
					}
				}
			}
CodigoEjecutar_Id_Fin:
			PUSH( x1 );
			break;
		case OP_NAME:
			x1 = POP();				/* Nombre del sub */
			x2 = StrFromStr(c->nombre);		/* Nombre del padre */
			x3 = Get( x1, x2 );
			if ( x3 == FALLO ) {
				RAISE( ExAttr );
				break;
			}
			if ( x3 == NULO ) {
				x4 = ResNuevo(0);
				Set( x1, x4, x2 );
				x3 = Get( x1, x2 );
			}
			PUSH( x3 );
			break;
		case OP_REF:
			x1 = POP();
			x2 = StrFromStr(c->nombre);
			x3 = _hash_tiene( x1, StrVal(x2) );
			if ( x3 == FALLO ) {
				RAISE( ExAttr );
				break;
			}
			if ( x3 == NULO ) {
				x3 = ResNuevo(0);
				x3->valor.puntero = NULO;
				PUSH(x3);
				break;
			}
			PUSH( x3 );
			break;
		case OP_NUM:
			/* Empujo el símbolo número c->simbolo de la lista de constantes */
			tmp1 = IntFromInt( c->simbolo );
			PUSH( Get( const_entorno, tmp1 ) );
			Liberar( tmp1 );
			break;
		case OP_SET:
			x1 = POP();
			x2 = POP();
			x2->tipo = x1->tipo;
			x2->valor = x1->valor;
			PUSH( x1 );
			break;
		case OP_DEL:
			x1 = POP();
			Liberar(x1);
			break;
		case OP_PLUS:
			x1 = POP();
			x2 = POP();
			x3 = Suma( x2, x1 );
			if ( x3 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x3 );
			break;
		case OP_MINUS:
			x1 = POP();
			x2 = POP();
			x3 = Resta( x2, x1 );
			if ( x3 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x3 );
			break;
		case OP_UMINUS:
			x1 = POP();
			x2 = MenosUnario( x1 );
			if ( x2 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x2 );
			break;
		case OP_MULT:
			x1 = POP();
			x2 = POP();
			x3 = Mult( x2, x1 );
			if ( x3 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x3 );
			break;
		case OP_DIV:
			x1 = POP();
			x2 = POP();
			x3 = Division( x2, x1 );
			if ( x3 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x3 );
			break;
		case OP_LINENO:
			break;
		case OP_JMPF:
			x1 = POP();
			if ( Eval(x1)->valor.puntero == FALSO )
				SALTAR();
			break;
		case OP_JMP:
			SALTAR();
			break;
		case OP_BUILDLIST:
			x1 = Nuevo( AryTipo );
			for ( i = 0; i < c->simbolo; i++ ) {
				x2 = POP();				
				Push( x1, x2, principio );
			}
			PUSH( x1 );
			break;
		case OP_BUILDCONS:
			x1 = POP();
			x2 = POP();
			x3 = Nuevo( ConsTipo );
			SetHead( x3, x2 );
			SetTail( x3, x1 );
			PUSH(x3);
			break;
		case OP_BUILDHASH:
			x1 = Nuevo( HashTipo );
			for ( i = 0; i < c->simbolo; i++ ) {
				x2 = POP();
				Set( x1, GetTail(x2), GetHead(x2) );
			}
			PUSH(x1);
			break;
		case OP_SUB:
			x2 = POP();
			x1 = POP();
			x3 = Get( x1, x2 );
			if ( x3 == FALLO ) {
				RAISE( ExSub );
				break;
			}
			if ( x3 == NULO ) {
				x4 = ResNuevo(0);
				Set( x1, x4, x2 );
				x3 = Get( x1, x2 );
			}
			PUSH( x3 );
			break;
		case OP_SLICE:
			switch ( c->simbolo ) {
			case 0:
				x1 = POP();	/*final*/
				x2 = POP();	/*inicial*/
				x3 = POP();	/*objeto*/
				PUSH( SLICE( x3, x2, x1 ) );
				break;
			case 1:
				x1 = POP();	/*inicial*/
				x2 = POP();	/*objeto*/
				PUSH( SLICE( x2, x1, final ) );
				break;
			case 2:
				x1 = POP();	/*final*/
				x2 = POP();	/*objeto*/
				PUSH( SLICE( x2, principio, x1 ) );
				break;
			case 3:
				x1 = POP();	/*objeto*/
				PUSH( Copiar( x1 ) );
				break;
			}
			break;
		case OP_CALL:
			x1 = POP();
			x2 = POP();
			if ( GET_TIPO(x2) == &HashTipo || GET_TIPO(x2) == &FuncTipo ) {
				/* El ámbito del hijo contiene la tabla de símbolos
				 * del padre y el ambito del padre. El padre se agrega
				 * al principio para ir revisandola en forma de pila
				 * en ID */
				/* Pongo la tabla local en el ambito para pasarsela al hijo */
				Push( ambito, tabla_local, principio );
				x3 = Ejecutar( x2, 0, x1, ambito );
				if ( x3 == FALLO ) {
					RAISE( ExFunc );
					break;
				}
				/* Saco la tabla local del ambito despues de volver */
				Pop( ambito, principio );
				PUSH( x3 );
			} else
				RAISE( ExFunc );
			break;
		case OP_EXEC:
			/* Ejecuta un modulo */
			x1 = POP();
			if ( GET_TIPO(x1) == &HashTipo ) {
				/* El ámbito del hijo contiene la tabla de símbolos
				 * del padre y el ambito del padre. El padre se agrega
				 * al principio para ir revisandola en forma de pila
				 * en ID */
				/* Pongo la tabla local en el ambito para pasarsela al hijo */
				Push( ambito, tabla_local, principio );
				HASH_GET( x1, x4, "codigo_actual" );
				x3 = Ejecutar( x4, x1, Nuevo( AryTipo ), ambito );
				if ( x3 == FALLO ) {
					RAISE( ExFunc );
					break;
				}
				/* Saco la tabla local del ambito despues de volver */
				Pop( ambito, principio );
				PUSH( x3 );
			} else
				RAISE( ExFunc );
			break;
		case OP_RETURN:
			if ( EsModulo ) {
				return tabla_local;
			} else {
				x1 = POP();
				PUSH( IntFromInt(0) );
				Liberar( pila );
				Liberar( final );
				Liberar( principio );
				return x1;
			}
			break;
		case OP_SHIFT:
			if ( c->simbolo ) {
				x1 = POP();
				x2 = Pop( x1, principio );
				if ( x2 == FALLO ) {
					RAISE( ExSeq );
					break;
				}
				PUSH(x2);
			} else {
				x1 = Pop( argumentos, principio );
				PUSH(x1);
			}
			break;
		case OP_PUSH:
			x1 = POP();
			x2 = POP();
			Push( x2, x1, principio );
			PUSH(x2);
			break;
		case OP_OR:
			x1 = POP();
			x2 = POP();
			x3 = Or( Eval(x2), Eval(x1) );
			PUSH( x3 );
			break;
		case OP_XOR:
			x1 = POP();
			x2 = POP();
			x3 = Xor( Eval(x2), Eval(x1) );
			PUSH( x3 );
			break;
		case OP_AND:
			x1 = POP();
			x2 = POP();
			x3 = And( Eval(x2), Eval(x1) );
			PUSH( x3 );
			break;
		case OP_NOT:
			x1 = POP();
			x2 = Not( Eval(x1) );
			PUSH( x2 );
			break;
		case OP_TEST:
			x1 = POP();
			x2 = POP();
			x3 = Comparar( x2, x1 );
			if ( x3 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			x4 = Nuevo( BoolTipo );
			if (	( c->simbolo == 0 && NumVal(x3) ==  0 ) ||	/*Igualdad*/
				( c->simbolo == 2 && NumVal(x3) == -1 ) ||	/*Menor*/
				( c->simbolo == 1 && NumVal(x3) ==  1 ) ||	/*Mayor*/
				( c->simbolo == 3 &&
				  (NumVal(x3) == 0 || NumVal(x3) ==  1)) ||	/*Mayor o igual*/
				( c->simbolo == 4 &&
				  (NumVal(x3) == 0 || NumVal(x3) == -1)) ||	/*Menor o igual*/
				( c->simbolo == 5 &&
				  (NumVal(x3) == 1 || NumVal(x3) == -1)) ) {	/*Desigual*/
				x4->valor.puntero = VERDADERO;
			} else
				x4->valor.puntero = FALSO;
			PUSH( x4 );
			break;
		case OP_GLOBAL:	
			x1 = POP();
			x2 = Nuevo( GlobalTipo );
			Iterar( l, x1 ) {
				l->contenido->tipo = x2->tipo;
				l->contenido->valor = x2->valor;
			}
			break;		
		case OP_COPY:
			x1 = POP();
			x2 = Copiar(x1);
			if ( x2 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x2 );
			break;
		case OP_READARGS:
			/* Llena la lista de parámetros con los shifts de argumentos */
			x1 = POP();	/* La lista de parámetros */
			Iterar( l, x1 ) {
				x2 = Pop( argumentos, principio );
				l->contenido->tipo = x2->tipo;
				l->contenido->valor = x2->valor;
			}
			break;
		case OP_MOD:
			x1 = POP();
			x2 = POP();
			x3 = Modulo( x2, x1 );
			if ( x3 == FALLO ) {
				RAISE( ExOper );
				break;
			}
			PUSH( x3 );
			break;
		case OP_TRY:
			Push( tries, IntFromInt( c->simbolo ), principio );
			break;
		case OP_HANDLE:
			Pop( tries, principio );
			if ( !TryFlag ) {
				SALTAR();
			} else {
				x1 = POP();
				if ( x1 == exc_actual ) {
					TryFlag = 0;
				} else {
					Push( Excepciones, exc_actual, principio );
					TryFlag = 0;
				}
			}
			/*{
				Codigo *r;
				for ( r = codigo_entorno->valor.codigo;
						r->opcode != OP_FIN && r;
						r = r->next ) {
					if ( r->opcode == OP_HANDLE ) {
						c = r;
						break;
					}
				}
			}*/
			break;
		case OP_RAISE:
			x1 = POP();
			RAISE(x1);
			break;
		case OP_DERIV:
			x1 = POP();		/* Lista de clases de las que se deriva */
			x2 = POP();		/* Subclase */
			HASH_SET(x2, x1, "__BASE");
			PUSH(x2);
			break;
		}

		/* Chequeo de excepciones */
		if ( LstLen(Excepciones) > 0 && !TryFlag ) {
			Res *e, *etiq;
			Codigo *r;
			
			TryFlag = 1;
			etiq = Pop( tries, principio );

			/* Salto a la etiqueta try, SI LA HAY */
			if ( etiq != NULO && etiq->valor.puntero != NULO ) {
				/* Obtengo la excepción actual */
				exc_actual = Pop( Excepciones, principio );
				/* Salto */
                int cont = 0;
				for ( r = codigo_entorno->valor.codigo;
						r->opcode != OP_FIN && r;
						r = r->next ) {
					if ( r->opcode == OP_LINENO &&
							r->simbolo == NumVal(etiq) ) {
						c = r;
                        cont = 1;
						break;
					}
				}
                if (cont) {
                    continue;
                }
			}
			/* Si no hay muestro la excepción en la salida de errores */
			while ( ( e = Pop( Excepciones, principio ) )->valor.puntero != NULO ) {
				fprintf(stderr, "Excepción en %s cerca de \"%s\":\n"
						, StrVal(nombre_archivo)
						, StrVal(ultimo_id) );
				ImprArch( stderr, e );
				fprintf(stderr, ".\n");
			}
			fprintf(stderr, "Ejecución de %s abortada.\n", StrVal(nombre_archivo) );
			printf("\n");
			exit(-1);
		}
	}

	
	if ( EsModulo ) {
		return tabla_local;
	} else {
		PUSH( IntFromInt(0) );
		/*Liberar( pila );*/
		Liberar( final );
		Liberar( principio );
		return NULO;
	}
	
}

Res *
CodigoComparar(p,q)
	Res *p, *q;
{
	DEBE_SER(p,CodigoTipo);
	if ( GET_TIPO(p) != GET_TIPO(q) ) return NULO;
	return IntFromInt(1);
}
