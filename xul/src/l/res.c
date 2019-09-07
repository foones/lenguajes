#include "Len.h"

Res *
ResNuevo(pars)
	Res *pars;
{
	Res *p;

	ALLOC(p, Res);
	p->tipo = &UndefTipo;
	p->referencias = 0;
	INCREF(p);			/* A los que usan ResNuevo no tengo
					que hacerles INCREF porque ya lo hago
					acá */
	return p;
}

void
ResLiberar(p)
	Res *p;
{
	/* Los objetos con referencia -1 son eternos (Nulo, Infinito, etc.) */
	if ( REF(p) != -1 ) {
		DECREF(p);
		if ( !(REF(p)) ) {
			FREE(p);
		}
	}
}

Res *
ResCadena(p)
	Res *p;
{
	Res *z, *tmp1;
	char s[ STR_MAX ];
	
	if ( p == NULO || p->valor.puntero == NULO )
		z = StrFromStr("nulo");
	else {
		z = StrFromStr("<");
		tmp1 = StrFromStr( p->tipo->nombre );
		StrAppend( z, tmp1 );
		Liberar( tmp1 );
		tmp1 = StrFromStr(" en ");
		StrAppend( z, tmp1 );
		Liberar( tmp1 );
		sprintf( s, "%lx", &p );
		tmp1 = StrFromStr( s );
		StrAppend( z, tmp1 );
		Liberar( tmp1 );
		tmp1 = StrFromStr( ">" );
		StrAppend( z, tmp1 );
		Liberar( tmp1 );
	}
	return z;
}

void
ResSet(p,n)
	Res *p;
	Valor n;
{
	p->valor = n;
}

Res *
ResComparar(p, q)
	Res *p, *q;
{
	return IntFromInt(1);
	/*if ( p->valor.puntero == q->valor.puntero )
		return IntFromInt(1);
	else
		return IntFromInt(0);*/
/*	return IntFromInt( strcmp(GET_TIPO(p)->nombre, GET_TIPO(q)->nombre) );*/
}

Res *
ResRef(p)
	Res *p;
{
	Res *z;
	
	z = ResNuevo(0);
	z->valor.puntero = p;
	return z;
}

Res *
ResDeref(p)
	Res *p;
{
	Res *z;
	z = p->valor.puntero;
	return z;
}

Res *
ResEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo( BoolTipo );
	if ( p == NULO || p->valor.puntero == NULO )
		z->valor.puntero = FALSO;
	else if ( p == FALLO )
		z = FALLO;
	else
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
ResCopiar(p)
	Res *p;
{
	Res *z;

	z = ResNuevo(0);
	z->tipo = p->tipo;
	z->valor = p->valor;
	return z;
}
