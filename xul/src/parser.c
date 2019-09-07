#include "Len.h"
#include "parser.h"

yywrap()
{
}

yyerror(s)
	char *s;
{
	lostr("error de sintaxis");
}

/*
 *  La estructura de datos al compilar es un array para las funciones.
 *  Para cada función hay un hash con los identificadores, una
 *  sección código de la función, una cuenta de símbolos.
 *
 */

Res *lista_funciones;

Res *hash_actual, *const_actual, *codigo_actual, *hash_global;
Res *nombre_archivo, *cuenta_e_actual;
Res *a_etiquetas, *items, *es_modulo;

void
ParserCerear()
{
	es_modulo = Nuevo( IntTipo );
	hash_actual = Nuevo( HashTipo );		/* Símbolos */
	hash_global = Nuevo( HashTipo );		/* Símbolos */
	const_actual = Nuevo( AryTipo );		/* Constantes */
	codigo_actual = Nuevo( CodigoTipo );		/* Codigo de la funcion */
	cuenta_e_actual = Nuevo( IntTipo );		/* Cuenta de etiquetas */
	a_etiquetas = Nuevo( AryTipo ); 		/* Array de etiquetas para break y
					   		continue en bucles */
	items = Nuevo( AryTipo );			/* Array con la cuenta de items
							de listas */
}

void
ParserInicializar()
{
	lista_funciones = Nuevo( AryTipo );		/* Lista de todo lo apilado */
	ParserCerear();
}

void
ParserNuevoId( nombre )
	Res *nombre;
{
	Res *z, *n;
	
	z = Get( hash_actual, nombre );
	if ( z == NULO ) {
			Set( hash_actual, ResNuevo(0), nombre );
	}
}

Num
ParserNuevaConst( c )
	Res *c;
{
	int i;
	Res *final, *xx;
	Lista *it;

	i = 0;
	Iterar ( it, const_actual ) {
		xx = Comparar( it->contenido, c );
		if ( NumVal( xx ) == 0 ) {
			return i;
		}
		i++;
	}
	final = IntFromInt( -1 );
	Push( const_actual, c, final );
	Liberar( final );
	return NumVal( Longitud( const_actual ) ) - 1;
}

Num
ParserNuevaEtiq()
{
	return NumVal( cuenta_e_actual )++;
}

void
ParserNuevaFun()
{
	Res *final;

	final = IntFromInt( -1 );
	Push( lista_funciones, ParserEntorno(), final );
	ParserCerear();
	Liberar( final );
}

Res *
ParserFinFun()
{
	Res *z;
	Res *final;
	Res *previa;

	final = IntFromInt( -1 );
	z = ParserEntorno();
	previa = Pop( lista_funciones, final );
	HASH_GET( previa, nombre_archivo, "nombre_archivo");
	HASH_GET( previa, hash_actual, "__CURR_HASH");
	HASH_GET( previa, const_actual, "const_actual");
	HASH_GET( previa, codigo_actual, "codigo_actual");	
	HASH_GET( previa, cuenta_e_actual, "cuenta_e_actual");
	HASH_GET( previa, a_etiquetas, "a_etiquetas");
	HASH_GET( previa, es_modulo, "es_modulo");
	HASH_GET( previa, items, "items");
	Liberar( final );
	return z;
}

Res *
ParserEntorno()
{
	Res *previa;

	/* Guardo la parte previa, con hash y código corrientes
	 * en la lista de funciones */
	previa = Nuevo( HashTipo );
	/* Lo empaquetado se desempaqueta y usa al ejecutar código
	 * y también en la función lose. XXX: todavía no.
	 */
	HASH_SET( previa, nombre_archivo, "nombre_archivo");
	HASH_SET( previa, cuenta_e_actual, "cuenta_e_actual");
	HASH_SET( previa, a_etiquetas, "a_etiquetas");
	HASH_SET( previa, hash_actual, "__CURR_HASH");
	HASH_SET( previa, const_actual, "const_actual");
	HASH_SET( previa, codigo_actual, "codigo_actual");
	HASH_SET( previa, es_modulo, "es_modulo");
	HASH_SET( previa, items, "items");
	return previa;
}
