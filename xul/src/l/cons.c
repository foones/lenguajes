#include "Len.h"

Res *
ConsNuevo(pars)
	Res *pars;
{
	Res *p, *n;

	p = ResNuevo(0);
	p->tipo = &ConsTipo;
	ALLOC(p->valor.cons, Cons);
	p->valor.cons->head = NULO;
	p->valor.cons->tail = NULO;	
	return p;
}

void
ConsLiberar(p)
	Res *p;
{
	Lista *q, *sig;

	DEBE_SER(p,ConsTipo);
	/* Los objetos con referencia -1 son eternos (Nulo, Infinito, etc.) */
	if ( REF(p) != -1 ) {
		DECREF(p);
		if ( !(REF(p)) ) {
			Liberar(p->valor.cons->head);
			Liberar(p->valor.cons->tail);
			FREE(p->valor.cons);
			FREE(p);
		}
	}
}

Res *
ConsCadena(p)
	Res *p;
{
	Res *z, *tmp1;

	DEBE_SER(p,ConsTipo);
	z = StrFromStr("(");
	StrAppend( z, Cadena(p->valor.cons->head) );
	tmp1 = StrFromStr(" : ");	
	StrAppend( z, tmp1 );
	Liberar( tmp1 );
	StrAppend( z, Cadena(p->valor.cons->tail) );
	tmp1 = StrFromStr(")");	
	StrAppend( z, tmp1 );
	Liberar( tmp1 );
	return z;
}

void
ConsSetHead(p,q)
	Res *p, *q;
{
	DEBE_SER(p,ConsTipo);
	INCREF(q);
	p->valor.cons->head = q;
}

void
ConsSetTail(p,q)
	Res *p, *q;
{
	DEBE_SER(p,ConsTipo);
	INCREF(q);
	p->valor.cons->tail = q;
}

Res *
ConsGetHead(p)
	Res *p;
{
	DEBE_SER(p,ConsTipo);
	return p->valor.cons->head;
}

Res *
ConsGetTail(p)
	Res *p;
{
	DEBE_SER(p,ConsTipo);
	return p->valor.cons->tail;
}

Res *
ConsEval(p)
	Res *p;
{
	Res *z;
	
	z = Nuevo( BoolTipo );
	DEBE_SER(p, ConsTipo);
	if ( (Eval(GetHead(p)) == FALSO) && (Eval(GetTail(p)) == FALSO) )
		z->valor.puntero = FALSO;
	else
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
ConsCopiar(p)
	Res *p;
{
	Res *n;

	DEBE_SER(p, ConsTipo);
	n = Nuevo( ConsTipo );
	SetHead( n, Copiar( GetHead(p) ) );
	SetTail( n, Copiar( GetTail(p) ) );
	return n;
}
