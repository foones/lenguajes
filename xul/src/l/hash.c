#include "Len.h"

Res *
_hash_get( p, idx )
	Res *p, *idx;
{
	Res *q, *r, *z;
	int Key;
	Lista *l;

	DEBE_SER(p,HashTipo);
	Key = HASH(idx);
	if ( Elemento( p, Key ) ) {
		Iterar ( l, Elemento( p, Key ) ) {
			q = GetHead( l->contenido );
			/* Si q es igual a la clave buscada */
			if ( NumVal(Comparar( q, idx )) == 0 ) {
				r = GetTail( l->contenido );
				return r;
			}
		}
	}
	return NULO;
}

Res *
_hash_tiene( p, funcion )
	Res *p;
	Cadena funcion;
{
	/*
	 * Se fija si el hash como objeto pertenece a una clase, y si esa
	 * clase contiene el objeto solicitado. En ese caso devuelve ese
	 * objeto.
	 */
	Res *nup, *cl, *f, *key;
	
	if ( GET_TIPO(p) == &TipoTipo ) {
		f = Nuevo(FuncTipo);
		/*if ( strcmp(funcion, "__NEW" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->constructor);
		else if ( strcmp(funcion, "__SET" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->set);
		else if ( strcmp(funcion, "__GET" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->get);
		else if ( strcmp(funcion, "__EVAL" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->eval);
		else if ( strcmp(funcion, "__COMP" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->comparar);
		else if ( strcmp(funcion, "__LEN" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->longitud);
		else if ( strcmp(funcion, "__COPY" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->copiar);
		else if ( strcmp(funcion, "__ADD" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->suma);
		else if ( strcmp(funcion, "__POP" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->pop);
		else if ( strcmp(funcion, "__PUSH" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->push);
		else if ( strcmp(funcion, "__DEL" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->del);
		else if ( strcmp(funcion, "__SETHEAD" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->sethead);
		else if ( strcmp(funcion, "__SETTAIL" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->settail);
		else if ( strcmp(funcion, "__GETHEAD" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->gethead);
		else if ( strcmp(funcion, "__GETTAIL" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->gettail);
		else if ( strcmp(funcion, "__SUB" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->resta);
		else if ( strcmp(funcion, "__MULT" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->mult);
		else if ( strcmp(funcion, "__DIV" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->division);
		else if ( strcmp(funcion, "__UMINUS" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->unaryminus);
		else if ( strcmp(funcion, "__MOD" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->modulo);
		else if ( strcmp(funcion, "__AND" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->and);
		else if ( strcmp(funcion, "__OR" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->or);
		else if ( strcmp(funcion, "__XOR" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->xor);
		else if ( strcmp(funcion, "__NOT" ) == 0 )
			f->valor.func = &(p->valor.tipoptr->not);
		else*/
		f = NULO;
		return f;
	} else {
		DEBE_SER( p, HashTipo );
		nup = p;

		key = StrFromStr(funcion);		/* Busca el objeto en el propio hash */
		f = _hash_get( nup, key );
		Liberar(key);
		if ( f != NULO )
			return f;
		
		key = StrFromStr("__CLASS");	/* Busca el objeto en la clase del hash */
		cl = _hash_get( nup, key );
		Liberar( key );
		if ( cl != NULO ) {
			f = _hash_tiene( cl, funcion );
			if ( f != NULO )
				return f;
		}
		key = StrFromStr("__BASE");	/* Busca el objeto en las bases de una clase */
		cl = _hash_get( nup, key );
		Liberar( key );
		if ( cl != NULO ) {
			Lista *l;
		
			if ( GET_TIPO(cl) != &AryTipo )
				return FALLO;
			Iterar( l, cl ) {
				f = _hash_tiene( l->contenido, funcion );
				if ( f != NULO )
					return f;
			}
		}
	}

	return NULO;
}

Res *
HashNuevo(pars)
	Res *pars;
{
	Res *p, *f;
	int i;

	p = ResNuevo(0);
	p->tipo = &HashTipo;	
	ALLOC( HshCabVal(p), CabeceraHash );
	ALLOC( HshVal(p), Hash );
	HshLen(p) = 1;
	for ( i = 0; i < HASH_PRIMO; i++ )
		Elemento( p, i ) = 0;
	
	if ( pars != 0 ) {
		Res *principio, *final, *clase;
		/*Res *lstclase;*/

		principio = IntFromInt(0);
		final = IntFromInt(-1);
		clase = Pop( pars, principio );
		Liberar(principio);

		/*lstclase = Nuevo( AryTipo );
		Push( lstclase, clase, final );
		HASH_SET( p, lstclase, "__CLASS" );*/
		HASH_SET( p, clase, "__CLASS" );

		if ( ( f = _hash_tiene(p, "__NEW") ) != NULO ) {
			Lista *l;
			Res *args, *z;

			args = Nuevo( AryTipo );
			Push( args, p, final );
			Iterar ( l, pars ) {
				Push( args, l->contenido, final );
			}
			z = Ejecutar( f, 0, args, Builtins() );
			return z;
		}
		Liberar(final);
	}

	return p;
}

void
HashLiberar(p)
	Res *p;
{
	Num i;
	
	DEBE_SER(p,HashTipo);
	/* Los objetos con referencia -1 son eternos (Nulo, Infinito, etc.) */
	if ( REF(p) != -1 ) {
		DECREF(p);
		if ( !(REF(p)) ) {
			for ( i = 0; i < HASH_PRIMO; i++ ) {
				if ( Elemento( p, i ) )
					Liberar( Elemento( p, i ) );
			}
			FREE(HshCabVal(p));
			FREE(p);
		}
	}
}

void
HashSet(p,it,idx)
	Res *p, *it, *idx;
{
	int Key, i;
	Res *n, *kd, *q, *f;
	Lista *l;

	DEBE_SER(p,HashTipo);
	if ( ( f = _hash_tiene(p,"__SET") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, it, final );
		Push( args, idx, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
	}
	Key = HASH(idx);
	kd = Nuevo(ConsTipo);	/* Cons clave-dato */
	SetHead(kd, idx);
	SetTail(kd, it);	
	if ( ! ( Elemento( p, Key ) ) ) {
		/* El lugar estaba vacio */
		n = IntFromInt(-1);	/* En el lugar -1 (al final) voy a agregar cosas */
		HshLen(p)++;
		Elemento( p, Key ) = Nuevo(AryTipo);
		Push( Elemento( p, Key ), kd, n );
	} else {
		/* El lugar estaba ocupado */
		/* Reviso si ya existe la clave */
		i = 0;
		Iterar( l, Elemento( p, Key ) ) {
			q = GetHead( l->contenido );
			if ( NumVal(Comparar( q, idx )) == 0 ) {
				/* En este caso ya existia */
				n = IntFromInt(i);	/* En el lugar i voy a ubicar cosas */
				Set( Elemento( p, Key ), kd, n );
				goto HashSet_Fin;
			}
			i++;
		}
		/* En este caso, no existia en ese compartimento un dato con
		 * la misma clave */
		n = IntFromInt(-1);	/* En el lugar i voy a agregar cosas */
		HshLen(p)++;
		Push( Elemento( p, Key ), kd, n );
	}
HashSet_Fin:
	Liberar(n);
}

Res *
HashGet(p,idx)
	Res *p, *idx;
{
	Res *f;

	DEBE_SER(p,HashTipo);
	if ( ( f = _hash_tiene(p,"__GET") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, idx, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return _hash_get( p, idx );
}

Res *
HashCadena(p)
	Res *p;
{
	Res *z, *tmp1, *tmp2, *f;
	Lista *l;
	int i, priflag = 1;
	
	DEBE_SER(p,HashTipo);
	
	if ( ( f = _hash_tiene(p,"__STR") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}		
	z = StrFromStr("(");
	tmp1 = StrFromStr(", ");
	tmp2 = StrFromStr(": ");
	for ( i = 0; i < HASH_PRIMO; i++ ) {
		if ( Elemento( p, i ) ) {
			Iterar ( l, Elemento( p, i ) ) {
				if ( priflag ) priflag = 0;
				else StrAppend( z, tmp1 );
				StrAppend( z, Cadena( GetHead( l->contenido ) ) );
				StrAppend( z, tmp2 );
				StrAppend( z, Cadena( GetTail( l->contenido ) ) );
			}
		}
	}
	Liberar( tmp1 );
	Liberar( tmp2 );
	tmp1 = StrFromStr(")");
	StrAppend( z, tmp1 );
	Liberar( tmp1 );
	return z;
}

Res *
HashEval(p)
	Res *p;
{
	Res *z, *f;

	z = Nuevo( BoolTipo );
	DEBE_SER(p, HashTipo);
	
	if ( ( f = _hash_tiene(p,"__EVAL") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	if ( NumVal(Longitud(p)) == 0 )
		z->valor.puntero = FALSO;
	else
		z->valor.puntero = VERDADERO;
	return z;
}

Res *
HashComparar(p,q)
	Res *p, *q;
{
	Res *f;
	DEBE_SER(p, HashTipo);

	if ( ( f = _hash_tiene(p,"__COMP") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, q, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	if ( GET_TIPO(p) != GET_TIPO(q) ) return NULO;
	return IntFromInt(1);
}

Res *
HashLongitud(p)
	Res *p;
{
	Res *f;

	DEBE_SER(p, HashTipo);
	if ( ( f = _hash_tiene(p,"__LEN") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return IntFromInt( HshLen(p) );
}

Res *
HashClaves(p)
	Res *p;
{
	int i;
	Res *n, *final;
	Lista *l;
		
	DEBE_SER(p, HashTipo);
	final = IntFromInt( -1 );
	n = Nuevo( AryTipo );
	for ( i = 0; i < HASH_PRIMO; i++ ) {
		if ( Elemento( p, i ) ) {
			Iterar ( l, Elemento( p, i ) ) {
				Push( n, GetHead( l->contenido ), final );
			}
		}
	}
	Liberar( final );
	return n;
}

Res *
HashCopiar(p)
	Res *p;
{
	Res *n, *cl, *f;
	Lista *l;
	
	DEBE_SER(p, HashTipo);
	if ( ( f = _hash_tiene(p,"__COPY") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	n = Nuevo( HashTipo );
	cl = HashClaves( p );
	Iterar ( l, cl ) {
		Set( n, Copiar( Get( p, l->contenido ) ), l->contenido );
	}
	return n;
}

Res *
HashSumar(v,w)
	Res *v, *w;
{
	Lista *l;
	Res *n, *cl, *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__ADD") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	DEBE_SER( w, HashTipo );
	n = Nuevo( HashTipo );
	cl = HashClaves( v );
	Iterar( l, cl ) {
		Set( n, Get( v, l->contenido), l->contenido );
	}
	cl = HashClaves( w );
	Iterar( l, cl ) {
		Set( n, Get( w, l->contenido), l->contenido );
	}
	return n;
}

Res *
HashEjecutar(f,entorno,argumentos,ambito)
	Res *f, *entorno, *argumentos, *ambito;
{
	Res *fu, *z, *codigo;
	
	DEBE_SER( f, HashTipo );
	if ( ( fu = _hash_tiene(f, "__CALL") ) != NULO )
		f = fu;

	HASH_GET( f, codigo, "codigo_actual" );
	if ( codigo != NULO ) {
		z = Ejecutar( codigo, f, argumentos, ambito );
		return z;
	} else
		return FALLO;
}

/* Las que siguen no son nativas de hash.
 * Son para objetos insertos en hash que
 * pueden tener funciones propias. */

void
HashPush(p,it,idx)
	Res *p, *it, *idx;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__PUSH") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, it, final );
		Push( args, idx, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
	}
}

Res *
HashPop(p,it)
	Res *p, *it;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__POP") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, it, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

void
HashDel(p,it)
	Res *p, *it;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__DEL") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, it, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
	}
}

void
HashSetHead(p,q)
	Res *p, *q;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__SETHEAD") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, q, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
	}
}

void
HashSetTail(p,q)
	Res *p, *q;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__SETTAIL") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		Push( args, q, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
	}
}

Res *
HashGetHead(p)
	Res *p;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__GETHEAD") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashGetTail(p)
	Res *p;
{
	Res *f;

	DEBE_SER( p, HashTipo );
	if ( ( f = _hash_tiene(p,"__GETTAIL") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, p, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashRestar(v,w)
	Res *v, *w;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__SUB") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashMult(v,w)
	Res *v, *w;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__MULT") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashDivision(v,w)
	Res *v, *w;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__DIV") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashMenosUnario(v)
	Res *v;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__UMINUS") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashOr(v,w)
	Res *v, *w;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__OR") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashAnd(v,w)
	Res *v, *w;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__AND") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashXor(v,w)
	Res *v, *w;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__XOR") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		Push( args, w, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}

Res *
HashNot(v)
	Res *v;
{
	Res *f;

	DEBE_SER( v, HashTipo );
	if ( ( f = _hash_tiene(v,"__NOT") ) != NULO ) {
		Res *args, *final, *z;

		args = Nuevo( AryTipo );
		final = IntFromInt(-1);
		Push( args, v, final );
		z = Ejecutar( f, 0, args, Builtins() );
		Liberar(final);
		return z;
	}
	return NULO;
}
