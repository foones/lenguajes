#include "Len.h"

Res *
IntNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &IntTipo;
	p->valor.numero = 0;
	return p;
}

Res *
IntCadena(p)
	Res *p;
{
	Res *z;
	char s[ STR_MAX ];
	
	DEBE_SER(p,IntTipo);
	if ( p->valor.puntero == INFINITO )
		sprintf(s, "infinito" );
	else
		sprintf(s, "%i\0", p->valor.numero );
	z = StrFromStr( s );
	return z;
}

Res *
IntSuma(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,IntTipo);
	if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;	
	} else if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(IntTipo);
		z->valor.numero = v->valor.numero + w->valor.numero;
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Suma( w, FloatFromFloat( (float) v->valor.numero ) );
	}
	return z;
}

Res *
IntResta(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,IntTipo);
	if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(IntTipo);
		z->valor.numero = v->valor.numero - w->valor.numero;
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Resta( w, FloatFromFloat( (float) v->valor.numero ) );
	}
	return z;
}

Res *
IntMenosUnario(v)
	Res *v;
{
	Res *z;

	DEBE_SER(v,IntTipo);
	z = Nuevo(IntTipo);
	z->valor.numero = -v->valor.numero;
	return z;
}

Res *
IntMult(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,IntTipo);
	if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(IntTipo);
		z->valor.numero = v->valor.numero * w->valor.numero;
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Mult( w, FloatFromFloat( (float) v->valor.numero ) );
	}
	return z;
}

Res *
IntDivision(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,IntTipo);
	if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(IntTipo);
		z->valor.numero = 0;
	} else if ( w->valor.numero == 0 ) {
		z = Nuevo(IntTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		float n;

		n = (float)v->valor.numero / (float)w->valor.numero;
		if ( n == (int) n )
			z = IntFromInt( (int) n );
		else
			z = FloatFromFloat( n );
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Division( w, FloatFromFloat((float) v->valor.numero) );
	}
	return z;
}

Res *
IntFromInt(n)
	Num n;
{
	Res *p;

	p = Nuevo(IntTipo);
	p->valor.numero = n;
	return p;
}

Res *
IntComparar(p, q)
	Res *p, *q;
{
	DEBE_SER(p,IntTipo);
	if ( GET_TIPO(p) != GET_TIPO(q) ) return NULO;
	if ( p->valor.numero == q->valor.numero )
		return IntFromInt(0);
	else if ( p->valor.numero < q->valor.numero )
		return IntFromInt(-1);
	else if ( p->valor.numero > q->valor.numero )
		return IntFromInt(1);
}

Res *
IntEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(p, IntTipo);
/*	if ( NumVal(p) == 0 )
		z->valor.puntero = FALSO;
	else*/
	z->valor.puntero = VERDADERO;
	return z;
}

int
IntHash(v)
	Res *v;
{	
	unsigned n;

	DEBE_SER(v,IntTipo);
	n = v->valor.numero;
	return n % HASH_PRIMO;
}
