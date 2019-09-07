#include "Len.h"

Res *
_str_nuevo(longitud)
	Num longitud;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &StrTipo;
	p->valor.cadena = (Cadena) malloc( sizeof(char) * (longitud+1) );
	CHECK( p->valor.cadena );
	strcpy( p->valor.cadena, "\0" );
	return p;
}

Res *
StrNuevo(pars)
	Res *pars;
{
	return _str_nuevo( STR_MAX );
}

void
StrLiberar(p)
	Res *p;
{
	DEBE_SER(p,StrTipo);
	/* Los objetos con referencia -1 son eternos (Nulo, Infinito, etc.) */
	if ( REF(p) != -1 ) {
		DECREF(p);
		if ( !(REF(p)) ) {
			FREE(p->valor.cadena);
			FREE(p);
		}
	}
}

Res *
StrCadena(p)
	Res *p;
{
	DEBE_SER(p,StrTipo);
	return StrFromStr( p->valor.cadena );
}

Res *
StrComparar(p,q)
	Res *p, *q;
{
	Res *z;
	Num i;

	DEBE_SER(p,StrTipo);
	if ( GET_TIPO(p) != GET_TIPO(q) ) return NULO;
	i = strcmp(p->valor.cadena, q->valor.cadena);
	z = IntFromInt(i);
	return z;
}

Res *
StrFromStr(s)
	Cadena s;
{
	Res *p;

	p = _str_nuevo( strlen(s) );
	strcpy( p->valor.cadena, s);
	return p;
}

Res *
StrLongitud(p)
	Res *p;
{
	Res *z;
	
	DEBE_SER(p,StrTipo);
	z = IntFromInt( strlen( p->valor.cadena ) );
	return z;
}

Res *
StrPop(p,idx)
	Res *p, *idx;
{
	Res *z;

	DEBE_SER(p,StrTipo);

	z = Get(p,idx);
	Del(p,idx);
	return z;
}

void
StrDel(p,idx)
	Res *p, *idx;
{
	Cadena s, t, origs, origt ;
	int i, j;
	
	DEBE_SER(p,StrTipo);
	i = NumVal(idx);

	if ( i < 0 ) i = strlen(p->valor.cadena) + i;
	origs = p->valor.cadena;
	origt = (Cadena) malloc( strlen(p->valor.cadena) );
	for ( t = origt, s = origs, j = 0; j < i; j++ )
		*t++ = *s++;
	s++;
	while ( *t++ = *s++ ) ;
	FREE(p->valor.cadena);
	p->valor.cadena = origt;
}

void
StrPush(p,it,idx)
	Res *p, *it, *idx;
{
	Cadena z, c, d;
	int i, l;
	
	DEBE_SER(p,StrTipo);
	DEBE_SER(it,StrTipo);
	c = p->valor.cadena;
	d = it->valor.cadena;
	i = NumVal(idx);
	l = strlen(c);
	z = (Cadena) malloc( sizeof(char) * ( l + strlen(d) + 1) );
	
	if ( i == -1 ) {
		strcpy( z, c );
		strcat( z, d );
	} else {
		strncpy( z, c, i );
		c += i;
		strcat( z, d );
		strncat( z, c, l - i );
	}
	FREE(p->valor.cadena);
	p->valor.cadena = z;
}

Res *
StrGet(p,idx)
	Res *p, *idx;
{
	Cadena x;
	int i;

	DEBE_SER(p,StrTipo);
	x = (Cadena) malloc( sizeof(char) * 2 );
	i = NumVal(idx);
	if ( i < 0 ) i = strlen(p->valor.cadena) + i;	
	if ( i > strlen(p->valor.cadena) ) return NULO;	
	sprintf(x, "%c\0", p->valor.cadena[i]);
	return StrFromStr(x);
}

Res *
StrSuma(v,w)
	Res *v, *w;
{
	Res *z, *n;
	
	DEBE_SER(v,StrTipo);
	z = StrFromStr(StrVal(v));
	n = IntFromInt(-1);	
	Push(z, w, n);
	Liberar(n);
	return z;
}

void
StrAppend(p,q)
	Res *p, *q;
{
	Res *final;

	final = IntFromInt(-1);
	Push( p, q, final );
	Liberar(final);
}

Res *
StrEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo( BoolTipo );
	DEBE_SER(p, StrTipo);
	if ( strlen(p->valor.cadena) == 0 )
		z->valor.puntero = FALSO;
	else
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
StrCopiar(p)
	Res *p;
{
	Res *n;

	DEBE_SER(p,StrTipo);
	n = StrFromStr( StrVal( p ) );
	return n;
}

int
StrHash(v)
	Res *v;
{	
	char *p, *c;
	unsigned h = 0, g;

	DEBE_SER(v,StrTipo);
	c = v->valor.cadena;
	for ( p = c; *p; p++ ) {
		h = (h << 4) + (*p);
		if ( g = h & 0xf0000000 ) {
			h ^= (g >> 24);
			h ^= g;
		}
	}
	return h % HASH_PRIMO;
}

Res *
StrModulo(p,q)
	Res *p, *q;
{
	Res *indice, *cad, *resultado;
	Cadena c;
	char dupla[2] = "\0", temp[STR_MAX] = "\0";
	int modiflag = 0;
	
	resultado = Nuevo( StrTipo );
	indice = IntFromInt(0);
	if ( GET_TIPO(q) != &AryTipo ) {
		Res *final, *arr;
		
		arr = Nuevo( AryTipo );
		final = IntFromInt( -1 );
		Push( arr, q, final );
		Liberar( final );
		q = arr;
	}
	for ( c = StrVal(Cadena(p)); *c; c++ ) {
		
		if ( *c == '%' && !modiflag) {
			modiflag = 1;
			
		} else if ( *c == '%' && modiflag ) {
			dupla[0] = *c;
			StrAppend( resultado, StrFromStr( dupla ) );
			modiflag = 0;
			
		} else if ( *c == 's' && modiflag ) {			
			cad = Cadena(Get(q,indice));
			StrAppend( resultado, cad );
			NumVal(indice)++;
			modiflag = 0;

/*		} else if ( *c == 'i' && modiflag ) {
			Res *x;

			x = Get(q,indice);
			if ( GET_TIPO(x) == &IntTipo )
				sprintf( temp, "%i\0", x->valor.numero );
			else if ( GET_TIPO(x) == &FloatTipo )
				sprintf( temp, "%i\0", (int)x->valor.flotante );
			StrAppend( resultado, StrFromStr(temp) );
			NumVal(indice)++;
			modiflag = 0;
			
		} else if ( *c == 'f' && modiflag ) {
			Res *x;

			x = Get(q,indice);
			if ( GET_TIPO(x) == &IntTipo )
				sprintf( temp, "%f\0", (float)x->valor.numero );
			else if ( GET_TIPO(x) == &FloatTipo )
				sprintf( temp, "%f\0", x->valor.flotante );
			StrAppend( resultado, StrFromStr(temp) );
			NumVal(indice)++;
			modiflag = 0;		*/
			
		} else if ( !modiflag ) {
			dupla[0] = *c;
			StrAppend( resultado, StrFromStr( dupla ) );
		}
		if ( NumVal(indice) >= NumVal(Longitud(q)) )
			NumVal(indice) = 0;
	}
	Liberar(indice);
	return resultado;
}
