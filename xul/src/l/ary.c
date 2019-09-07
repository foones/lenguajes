#include "Len.h"

Lista *
_ary_nuevo()
{
	Lista *p;
	ALLOC(p, Lista);
	p->contenido = NULO;
	p->next = 0;
	return p;
}

Res *
AryNuevo(pars)
	Res *pars;
{
	Res *p, *n;

	p = ResNuevo(0);
	p->tipo = &AryTipo;
	ALLOC(LstCabVal(p), CabeceraLista);
	
	LstVal(p) = 0;
	LstLen(p) = 0;
	/*LstVal(p) = _ary_nuevo();	
	LstLen(p) = 1;

	n = IntFromInt(0);
	Del(p, n);
	Liberar(n);*/
	return p;
}

void
AryLiberar(p)
	Res *p;
{
	Lista *q, *sig;

	DEBE_SER(p,AryTipo);
	/* Los objetos con referencia -1 son eternos (Nulo, Infinito, etc.) */
	if ( REF(p) != -1 ) {
		DECREF(p);
		if ( !(REF(p)) ) {
			if ( LstVal(p) ) {
				sig = q->next;
				for ( q = LstVal(p); q->next; q = sig ) {
					sig = q->next;
					Liberar(q->contenido);
				}
				FREE(LstVal(p));
			}
			FREE(LstCabVal(p));
			FREE(p);
		}
	}
}

void
AryPush(p,it,idx)
	Res *p, *it, *idx;
{
	Lista *q, *r;
	Num j = 1, ind;
	
	DEBE_SER(p,AryTipo);
	ind = idx->valor.numero;
	r = _ary_nuevo();
	r->contenido = it;
	INCREF(it);	/* Aumenta el contador de referencias del objeto. IMPORTANTE */
	LstLen(p)++;	/* Aumento el tamaño de la lista */
	/* Comprobar si la lista está vacía del todo */
	if ( !(q = LstVal(p))) {
		r->next = 0;
		LstVal(p) = r;
		return;
	}
	
	switch ( ind ) {
	case 0:
		r->next = q;
		LstVal(p) = r;
		break;
	case -1:
		for ( q = LstVal(p); q->next; q = q->next ) /* vacio */;
		r->next = q->next;
		q->next = r;
		break;
	default:		
		if ( ind < 0 ) ind = LstLen(p) - 1 + ind;	
		for ( q = LstVal(p); q->next && j < ind; j++, q = q->next ) /* vacio */;
		r->next = q->next;
		q->next = r;
		break;
	}
}

Res *
AryPop(p,idx)
	Res *p, *idx;
{
	Res *z;
	Lista *q, *ant;
	Num j = 1, ind;
	
	DEBE_SER(p,AryTipo);
	ind = idx->valor.numero;
	/* Comprobar si la lista está vacia del todo */
	if ( ! (q = LstVal(p) ) ) {
		/* XXX: elevar excepcion o algo así */
		z = ResNuevo(0);
		z->valor.puntero = NULO;
		return z;
	}
	LstLen(p)--;	/* Reduzco el tamaño de la lista */

	switch ( ind ) {
	case 0:
		LstVal(p) = q->next;
		break;
	case -1:
		for ( q = LstVal(p), ant = q; q->next; q = q->next ) {
			ant = q;
		}
		ant->next = 0;
		break;
	default:
		if ( ind < 0 ) ind = LstLen(p) - 1 + ind;	
		for ( q = LstVal(p), ant = q; q->next && j <= ind; j++, q = q->next ) {
			ant = q;
		}
		ant->next = q->next;
		break;
	}
	return q->contenido;
}

void
AryDel(p,idx)
	Res *p, *idx;
{
	Res *q;
	
	DEBE_SER(p,AryTipo);
	q = AryPop(p, idx);
	Liberar(q);
}

Res *
AryGet(p,idx)
	Res *p, *idx;
{
	Lista *q;
	Num j = 1, ind;
	
	DEBE_SER(p,AryTipo);
	ind = idx->valor.numero;	
	/* Comprobar si la lista está vacia del todo */
	if ( ! (q = LstVal(p) ) ) {
		/* XXX: elevar excepcion o algo así */
		return NULO;
	}
	
	switch ( ind ) {
	case 0:
		break;
	case -1:
		for ( q = LstVal(p); q->next; q = q->next ) /* vacio */;
		break;
	default:		
		if ( ind < 0 ) ind = LstLen(p) - 1 + ind;	
		for ( q = LstVal(p); q->next && j <= ind; j++, q = q->next ) /* vacio */;
		break;
	}
	return q->contenido;
}

void
ArySet(p,it,idx)
	Res *p, *it, *idx;
{
	DEBE_SER(p,AryTipo);
	AryDel(p,idx);
	AryPush(p,it,idx);
}

Res *
AryCadena(p)
	Res *p;
{
	Res *z, *zz, *final, *tmp1, *tmp2;
	char priflag = 1;
	Lista *q;

	DEBE_SER(p,AryTipo);
	z = StrFromStr("(");
	tmp1 = StrFromStr(", ");
	tmp2 = StrFromStr("\"");
	Iterar (q, p) {
		if ( priflag ) priflag = 0;
		else StrAppend(z, tmp1);
		if (q->contenido) {
			if ( GET_TIPO(q->contenido) == &StrTipo )
				StrAppend( z, tmp2 );
			StrAppend( z, Cadena(q->contenido) );
			if ( GET_TIPO(q->contenido) == &StrTipo )
				StrAppend( z, tmp2 );
		}
	}
	Liberar(tmp1);
	tmp1 = StrFromStr(")");
	StrAppend(z, tmp1);
	Liberar(tmp1);
	Liberar(tmp2);
	return z;
}

Res *
AryLongitud(p)
	Res *p;
{
	DEBE_SER(p, AryTipo);
	return IntFromInt( LstLen(p) );
}

Res *
AryEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(p, AryTipo);
	if ( LstLen(p) == 0 )
		z->valor.puntero = FALSO;
	else
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
AryComparar(p,q)
	Res *p, *q;
{
	Res *z;
	Lista *v, *w;
	
	DEBE_SER(p, AryTipo);
	DEBE_SER(q, AryTipo);
	for ( v = LstVal(p), w = LstVal(q); ; v = v->next, w = w->next ) {
		z = Comparar( v->contenido, w->contenido );
		if ( NumVal( z ) != 0 )
			return z;
		else if ( !(v->next) ) {
			if ( !(w->next) )
				return IntFromInt(0);
			else
				return IntFromInt(-1);
		} else if ( !(w->next) )
			return IntFromInt(1);
	}
	return NULO;
}

Res *
ArySuma(v,w)
	Res *v, *w;
{
	Res *z, *n;	
	Lista *l;
	
	DEBE_SER(v,AryTipo);
	DEBE_SER(w,AryTipo);
	z = Nuevo( AryTipo );
	n = IntFromInt(-1);
	Iterar ( l, v ) {		
		Push(z, l->contenido, n);
	}
	Iterar ( l, w ) {		
		Push(z, l->contenido, n);
	}
	Liberar(n);
	return z;
}

Res *
AryCopiar(p)
	Res *p;
{
	Res *n, *final;
	Lista *l;

	DEBE_SER(p,AryTipo);
	final = IntFromInt(-1);
	n = Nuevo( AryTipo );
	Iterar (l, p) {
		Push( n, Copiar( l->contenido ), final );
	}
	Liberar( final );
	return n;
}
