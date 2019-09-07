#include "Len.h"

int
lose(s)
	Res *s;
{
	Res *nombre_archivo;
	
/*	HASH_GET( ENTORNO, nombre_archivo, "nombre_archivo" );*/
/*	fprintf(stderr,"\nError en \"%s\" - linea %i:\n",
			StrVal( nombre_archivo ),
			CODIGO->linea );*/
	fprintf(stderr,"Error de compilación: " );
	ImprArch(stderr, s);
	fprintf(stderr,"\nCompilación abortada.\n" );
	printf("");
	
	/*
	 * Traceback (most recent call last):
	 *   File "<fff>", line n, in ?
	 * XxxError: ...
	 *
	 * syntax error at fff, line n, near ""
	 * Couldn't ...
	 *
	 * fff:n: In ... in ...: 
	 * Compilation Failed
	 */
	
	exit(1);
}

int
lostr(c)
	Cadena c;
{
	/*fprintf(stderr,"%s\n", c);*/
	lose( StrFromStr( c ) );
}
