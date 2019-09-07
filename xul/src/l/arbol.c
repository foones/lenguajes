#include "Len.h"

Arbol *
ArbolNuevo(o,n,s,h1,h2)
	Op o;
	Cadena n;
	Num s;
	Arbol *h1, *h2;
{
	Arbol *p;

	ALLOC(p, Arbol);
	p->opcode = o;
	if ( n ) {
		p->nombre = (Cadena) malloc( sizeof(char) * ( strlen( n ) + 1) );
		CHECK(p->nombre);
		strcpy(p->nombre, n);
	} else
		p->nombre = 0;
	p->simbolo = s;
	p->hijo1 = h1;
	p->hijo2 = h2;
	return p;
}

#ifdef DEBUG
void
ArbolVisitar(ar)
	Arbol *ar;
{
	static int barritas = 0;
	int i;

	printf("\t ");
	for ( i = 0; i < barritas; i++ )
		printf("---");
	switch ( ar->opcode ) {
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
			printf("identificador %s", ar->nombre);
			break;
		case OP_NUM:
			printf("constante numero %i", ar->simbolo);
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
		case OP_SET:
			printf("set");
			break;
		case OP_DEL:
			printf("borrar");
			break;
		case OP_JMP:
			printf("salta a %i", ar->simbolo );			
			break;
		case OP_JMPF:
			printf("salta si falso a %i", ar->simbolo );			
			break;
		case OP_LINENO:
			printf("linea número %i", ar->simbolo );
			break;
		case OP_BUILDLIST:
			printf("hacer lista %i", ar->simbolo );
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
			printf("test %i", ar->simbolo);
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
	barritas++;
	if ( ar->hijo1 ) ArbolVisitar(ar->hijo1);
	if ( ar->hijo2 ) ArbolVisitar(ar->hijo2);
	barritas--;
}
#endif

Res *
ArbolCompilar(ar)
	Arbol *ar;
{	
	Res *fun;

	fun = Nuevo( CodigoTipo );
	ArbolCompilarRama( ar, fun->valor.codigo );
	CodigoAgregar( fun, OP_FIN, 0, 0 );
	return fun;
}

Codigo *
ArbolCompilarRama(ar, lugar)
	Arbol *ar;
	Codigo *lugar;
{
	Codigo *q, *n;

	q = lugar;
	
	if ( ar->hijo1 ) {
		q = ArbolCompilarRama( ar->hijo1, q );
	}
	if ( ar->hijo2 ) {
		q = ArbolCompilarRama( ar->hijo2, q );
	}
	
	q->opcode = ar->opcode;
	q->nombre = ar->nombre;
	q->simbolo = ar->simbolo;
	q->next = _codigo_nuevo();

	return q->next;
}
