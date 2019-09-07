#include "Len.h"
#include "parser.h"

Res *Curr_Func;

main(argc, argv)
	int argc;
	char **argv;
{
	--argc, ++argv;
	
	ObjsInit();
	ParserInicializar();
	if ( argc ) {
		yyin = fopen( argv[0], "r" );
		nombre_archivo = StrFromStr( argv[0] );
	} else {
		yyin = stdin;
		nombre_archivo = StrFromStr( "..." );
	}
	yyparse();
	printf("\n");
	
/*	Imprimir( hash_actual ); NL;
	Imprimir( const_actual ); NL;*/

	if ( argc )
		fclose( yyin );
	
/*	Res *a, *b, *n, *m, *x, *zz;
	Valor v, w;

	v.cadena = (Cadena) malloc(sizeof(10));
	strcpy( v.cadena, "Nenuco\0" );
	n = StrNuevo();
	n->valor = v;

	w.cadena = (Cadena) malloc(sizeof(10));
	strcpy( w.cadena, "Roque\0" );
	m = StrNuevo();
	m->valor = w;

	b = Nuevo(AryTipo);

	Push(b, IntFromInt(40), IntFromInt(-1));
	Push(b, IntFromInt(50), IntFromInt(-1));
	Push(b, IntFromInt(60), IntFromInt(-1));

	x = IntFromInt(45);

	a = Nuevo(HashTipo);
	Set(a, b, m);
	Set(a, x, n);
	Imprimir(a); NL;

	zz = Get(a, n);
	Imprimir(zz); NL;
	Liberar(a);

	a = Nuevo(AryTipo);
	b = Nuevo(BoolTipo);

	n = Nuevo(IntTipo);

	v.numero = -1;
	
	SET( n, v );
	
	Push(a,b,n);
	Push(a,b,n);
	Push(a,b,n);
	Push(a,b,n);
	Push(a,b,n);
	zz = Longitud(a);
	Imprimir(zz); NL;*/
}
