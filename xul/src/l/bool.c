#include "Len.h"

Res *
BoolNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &BoolTipo;
	p->valor.puntero = FALSO;
	return p;
}

Res *
BoolCadena(p)
	Res *p;
{
	Res *z;
	
	DEBE_SER(p,BoolTipo);
	if ( p->valor.puntero == FALSO )
		z = StrFromStr("falso");
	else if ( p->valor.puntero == VERDADERO )
		z = StrFromStr("verdadero");
	return z;
}


Res *
BoolComparar(p, q)
	Res *p, *q;
{
	DEBE_SER(p,BoolTipo);
	if ( GET_TIPO(p) != GET_TIPO(q) ) return NULO;
	if ( p->valor.puntero == q->valor.puntero )
		return IntFromInt(0);
	else
		return IntFromInt(1);
}

Res *
BoolEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(p, BoolTipo);
	if ( p->valor.puntero == FALSO )
		z->valor.puntero = FALSO;
	else
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
BoolAnd(v,w)
	Res *v, *w;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(v, BoolTipo);
	DEBE_SER(w, BoolTipo);
	
	if ( v->valor.puntero == FALSO )
		z->valor.puntero = FALSO;
	else if ( v->valor.puntero == VERDADERO )
		if ( w->valor.puntero == VERDADERO )
			z->valor.puntero = VERDADERO;
		else
			z->valor.puntero = FALSO;
	return z;
}

Res *
BoolOr(v,w)
	Res *v, *w;
{
	Res *z;
	
	z = Nuevo( BoolTipo );
	DEBE_SER(v, BoolTipo);
	DEBE_SER(w, BoolTipo);
	
	if ( v->valor.puntero == FALSO )
		if ( w->valor.puntero == VERDADERO )
			z->valor.puntero = VERDADERO;
		else
			z->valor.puntero = FALSO;
	else if ( v->valor.puntero == VERDADERO )
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
BoolXor(v,w)
	Res *v, *w;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(v, BoolTipo);
	DEBE_SER(w, BoolTipo);
	
	if ( v->valor.puntero == FALSO )
		if ( w->valor.puntero == VERDADERO )
			z->valor.puntero = VERDADERO;
		else
			z->valor.puntero = FALSO;
	else if ( v->valor.puntero == VERDADERO )
		if ( w->valor.puntero == VERDADERO )
			z->valor.puntero = FALSO;
		else
			z->valor.puntero = VERDADERO;
	return z;
}

Res *
BoolNot(v)
	Res *v;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(v, BoolTipo);
	
	if ( v->valor.puntero == FALSO )
		z->valor.puntero = VERDADERO;
	else if ( v->valor.puntero == VERDADERO )
		z->valor.puntero = FALSO;
	return z;
}
