#include "Len.h"

Res *
FloatNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &FloatTipo;
	p->valor.flotante = 0;
	return p;
}

Res *
FloatCadena(p)
	Res *p;
{
	Res *z;
	char s[ STR_MAX ];
	int i;
	float val, val2;
	
	DEBE_SER(p,FloatTipo);
	if ( p->valor.puntero == INFINITO )
		sprintf(s, "infinito" );
	else {
		val = p->valor.flotante;
		val2 = val;
		for ( i = 1; i < 10; val2 *= 10, i++ )
			if ( val2 == (int) val2 ) {
				i--;
				break;
			}
		sprintf(s, "%.*f\0", i, val );
	}
	z = StrFromStr( s );
	return z;
}

Res *
FloatSuma(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,FloatTipo);
	if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante + (float) NumVal(w);
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante + w->valor.flotante;
	}
	return z;
}

Res *
FloatResta(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,FloatTipo);
	if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante - (float) NumVal(w);
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante - w->valor.flotante;
	}
	return z;
}

Res *
FloatMenosUnario(v)
	Res *v;
{
	Res *z;

	DEBE_SER(v,FloatTipo);
	z = Nuevo(FloatTipo);
	z->valor.flotante = -v->valor.flotante;
	return z;
}

Res *
FloatMult(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,FloatTipo);
	if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante * (float) NumVal(w);
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante * w->valor.flotante;
	}
	return z;
}

Res *
FloatDivision(v,w)
	Res *v, *w;
{
	Res *z;

	DEBE_SER(v,FloatTipo);
	if ( w->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = 0;
	} else if ( v->valor.puntero == INFINITO ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( w->valor.flotante == 0 ) {
		z = Nuevo(FloatTipo);
		z->valor.puntero = INFINITO;
	} else if ( GET_TIPO(w) == &IntTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante / (float) NumVal(w);
	} else if ( GET_TIPO(w) == &FloatTipo ) {
		z = Nuevo(FloatTipo);
		z->valor.flotante = v->valor.flotante / w->valor.flotante;
	}
	return z;
}

Res *
FloatFromFloat(n)
	float n;
{
	Res *p;

	p = Nuevo(FloatTipo);
	p->valor.flotante = n;
	return p;
}

Res *
FloatComparar(p, q)
	Res *p, *q;
{
	DEBE_SER(p,FloatTipo);
	if ( GET_TIPO(p) != GET_TIPO(q) ) return NULO;
	if ( p->valor.flotante == q->valor.flotante )
		return FloatFromFloat(0);
	else if ( p->valor.flotante < q->valor.flotante )
		return FloatFromFloat(-1);
	else if ( p->valor.flotante > q->valor.flotante )
		return FloatFromFloat(1);
}

Res *
FloatEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(p, FloatTipo);
/*	if ( p->valor.flotante == 0 )
		z->valor.puntero = FALSO;
	else*/
	z->valor.puntero = VERDADERO;
	return z;
}

int
FloatHash(v)
	Res *v;
{	
	unsigned n;

	DEBE_SER(v,FloatTipo);
	n = (unsigned) v->valor.flotante;
	return n % HASH_PRIMO;
}
