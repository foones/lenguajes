%{
#include "Len.h"
#include "parser.h"
%}

%union {
	Arbol *ptr;
	Res *rep;
}

%token T_END T_NEW T_PRINT T_DEL
%token T_IF T_ELSE T_WHILE T_FOR
%token T_FUN T_LAMBDA T_RETURN T_SHIFT
%token T_GLOBAL T_IN T_COPY
%token T_MODULE
%token T_AND T_OR T_XOR T_NOT
%token T_CONTINUE T_BREAK
%token T_SEMICOLON T_COLON
%token T_COMMA T_PUNTO T_REFERENCIA
%token T_PORCENTAJE
%token T_OPPAREN T_CLPAREN
%token T_OPBRACE T_CLBRACE
%token T_OPBRACK T_CLBRACK
%token T_TRY T_HANDLE T_RAISE
%token <rep> T_ID
%token <rep> T_VALUE

%type <ptr> id
%type <ptr> fun_id
%type <ptr> referencia
%type <ptr> value
%type <ptr> fun_body
%type <ptr> cualquier_lista
%type <ptr> listvalue
%type <ptr> lista_desnuda
%type <ptr> consvalue
%type <ptr> hashvalue
%type <ptr> kdlista
%type <ptr> kdlista_coma
%type <ptr> keydatum
%type <ptr> lista
%type <ptr> lista_coma
%type <ptr> prog
%type <ptr> stat
%type <ptr> statement
%type <ptr> block
%type <ptr> expr

%left T_OR T_XOR
%left T_AND
%right T_NOT
%left T_ISEQ T_ISNEQ
%left T_ISLT T_ISGT T_ISEQLT T_ISEQGT
%left T_EQ
%left T_PLUS T_MINUS
%left T_STAR T_DASH

%%

completo :	prog		{
					Res *fun, *ent;
					codigo_actual = ArbolCompilar( $1 );
					ent = ParserEntorno();
					HASH_GET( ent, fun, "codigo_actual" );
					CodigoPrograma( fun, ent, 0, Builtins() );
					/* Impresiones */
					/*
					Imprimir( const_actual ); NL;
					CodigoVisitar( ParserEntorno() );
					ArbolVisitar( $1 );
					Imprimir( hash_actual ); NL;
					*/
					/* Fin impresiones */
				}
	;
	
prog :		prog statement	{ $$ = ArbolNuevo( OP_NOP, 0, 0, $1, $2 ); } 
	|	/* vacio */	{ $$ = ArbolNuevo( OP_NOP, 0, 0, 0, 0 ); }
	;

statement :
		stat T_SEMICOLON	{ $$ = $1; }
	|	T_IF expr statement
					{
						Arbol *aa;
						Num k;
						
						k = ParserNuevaEtiq();
						aa = ArbolNuevo( OP_JMPF, 0, k, $2, 0 );
						$$ = ArbolNuevo( OP_LINENO, 0, k, aa, $3 );
						/*
						expresion ($2)
						si es falsa salta a k
						statement ($3)
						linea k:
						*/
					}
	|	T_IF expr statement T_ELSE statement
					{
						Arbol *aa, *ab, *ac;
						Num k, l;
						
						k = ParserNuevaEtiq();
						l = ParserNuevaEtiq();
						aa = ArbolNuevo( OP_JMPF, 0, k, $2, 0 );
						ab = ArbolNuevo( OP_JMP, 0, l, aa, $3 );
						ac = ArbolNuevo( OP_LINENO, 0, k, ab, 0 );
						$$ = ArbolNuevo( OP_LINENO, 0, l, ac, $5 );
						/*
						expresion ($2)
						si es falsa salta a k
						statement ($3)
						salta a l
						linea k:
						statement ($5)
						linea l:
						*/
					}
	|	T_WHILE expr
					{
						Res *xa, *final;
						Num k, l;

						k = ParserNuevaEtiq();
						l = ParserNuevaEtiq();
						
						/*Guarda etiquetas*/
						final = IntFromInt( -1 );
						xa = Nuevo(ConsTipo);
						SetHead( xa, IntFromInt( k ));
						SetTail( xa, IntFromInt( l ));
						Push( a_etiquetas, xa, final );
						Liberar( final );
		} statement {
						Res *xa, *final;
						Num k, l;
						Arbol *aa, *ab, *ac;

						/*Recupera etiquetas*/
						final = IntFromInt( -1 );
						xa = Pop( a_etiquetas, final );
						k = NumVal( GetHead( xa ) );	
						l = NumVal( GetTail( xa ) );
						
						aa = ArbolNuevo( OP_LINENO, 0, k, 0, 0 );
						ab = ArbolNuevo( OP_JMPF, 0, l, aa, $2);
						ac = ArbolNuevo( OP_JMP, 0, k, ab, $4 );
						$$ = ArbolNuevo( OP_LINENO, 0, l, ac, 0);

						/*Borra etiquetas*/
						Liberar( xa );
						Liberar( final );
						/*
						linea k:
						expresion ($2)
						si es falsa salta a l
						statement ($4)
						salta a k
						linea l:
						*/
					}
	|	T_FOR T_OPPAREN expr T_SEMICOLON expr
		T_SEMICOLON expr T_CLPAREN
					{
						Res *xa, *final;
						Num k, l;

						k = ParserNuevaEtiq();
						l = ParserNuevaEtiq();

						/*Guarda etiquetas*/
						final = IntFromInt( -1 );
						xa = Nuevo(ConsTipo);
						SetHead( xa, IntFromInt( k ));
						SetTail( xa, IntFromInt( l ));
						Push( a_etiquetas, xa, final );
						Liberar( final );
		} statement {
						Res *xa, *final;
						Num k, l;
						Arbol *aa, *ab, *ac, *ad;
						
						final = IntFromInt( -1 );
						xa = Pop( a_etiquetas, final );
						k = NumVal( GetHead( xa ) );	
						l = NumVal( GetTail( xa ) );

						aa = ArbolNuevo( OP_LINENO, 0, k, $3, 0 );
						ab = ArbolNuevo( OP_JMPF, 0, l, aa, $5 );
						ac = ArbolNuevo( OP_NOP, 0, 0, ab, $10 );
						ad = ArbolNuevo( OP_JMP, 0, k, ac, $7 );
						$$ = ArbolNuevo( OP_LINENO, 0, l, ad, 0);

						/*Borra etiquetas*/
						xa = Pop( a_etiquetas, final );
						Liberar( xa );
						Liberar( final );
						/*
						expresion ($3)
						linea k:
						expresion ($5)
						si es falsa salta a l
						statement ($9)
						expresion ($7)
						salta a k
						linea l:
						*/
					}
	|	T_FOR id T_IN expr {
						Res *xa, *final;
						Num k, l;

						k = ParserNuevaEtiq();
						l = ParserNuevaEtiq();
						
						/*Guarda etiquetas*/
						final = IntFromInt( -1 );
						xa = Nuevo(ConsTipo);
						SetHead( xa, IntFromInt( k ));
						SetTail( xa, IntFromInt( l ));
						Push( a_etiquetas, xa, final );
						Liberar( final );
		} statement {
						Res *xa, *final;
						Num k, l;
						Arbol *aa, *ab, *ac, *ad, *ae;

						/*Recupera etiquetas*/
						final = IntFromInt( -1 );
						xa = Pop( a_etiquetas, final );
						k = NumVal( GetHead( xa ) );	
						l = NumVal( GetTail( xa ) );
						
						aa = ArbolNuevo( OP_LINENO, 0, k, 0, 0 );
						ab = ArbolNuevo( OP_SHIFT, 0, 1, $4, 0 );
						ac = ArbolNuevo( OP_SET, 0, 0, $2, ab );	
						ad = ArbolNuevo( OP_JMPF, 0, l, aa, ac);
						ae = ArbolNuevo( OP_JMP, 0, k, ad, $6 );
						$$ = ArbolNuevo( OP_LINENO, 0, l, ae, 0);

						/*Borra etiquetas*/
						Liberar( xa );
						Liberar( final );
						/*
						linea k:
						set id ($2) = shift expresion ($4)
						si es falsa salta a l
						statement ($6)
						salta a k
						linea l:
						*/
					}
	|	T_FUN fun_id {
						ParserNuevaFun();
		} fun_body {
						Arbol *aa;
						Res *compilada;
						Num k;
						
						codigo_actual = ArbolCompilar( $4 );
						es_modulo = IntFromInt(0);
						compilada = ParserFinFun();
						k = ParserNuevaConst( compilada );	
						aa = ArbolNuevo( OP_NUM, 0, k, 0, 0 );
						$$ = ArbolNuevo( OP_SET, 0, 0, $2, aa );
		}
	|	T_MODULE fun_id {
						ParserNuevaFun();
		} fun_body {
						Arbol *aa, *ab;
						Res *compilada;
						Num k;
						
						codigo_actual = ArbolCompilar( $4 );
						es_modulo = IntFromInt(1);
						compilada = ParserFinFun();
						k = ParserNuevaConst( compilada );	
						aa = ArbolNuevo( OP_NUM, 0, k, 0, 0 );
						ab = ArbolNuevo( OP_EXEC, 0, 0, aa, 0 );
						$$ = ArbolNuevo( OP_SET, 0, 0, $2, ab );
		}
	|	T_MODULE fun_id T_COLON lista_desnuda {
						ParserNuevaFun();
		} fun_body {
						Arbol *aa, *ab, *ac;
						Res *compilada;
						Num k;
						
						codigo_actual = ArbolCompilar( $6 );
						es_modulo = IntFromInt(1);
						compilada = ParserFinFun();
						k = ParserNuevaConst( compilada );	
						aa = ArbolNuevo( OP_NUM, 0, k, 0, 0 );
						ab = ArbolNuevo( OP_EXEC, 0, 0, aa, 0 );
						ac = ArbolNuevo( OP_DERIV, 0, 0, ab, $4 );
						$$ = ArbolNuevo( OP_SET, 0, 0, $2, ac );
		}
	|	T_TRY statement T_HANDLE fun_id statement
				{
					Arbol *aa, *ab, *ac, *ad;
					Num k, l;

					k = ParserNuevaEtiq();
					l = ParserNuevaEtiq();
					aa = ArbolNuevo( OP_TRY, 0, k, 0, 0 );
					ab = ArbolNuevo( OP_LINENO, 0, k, aa, $2 );
					ac = ArbolNuevo( OP_HANDLE, 0, l, $4, 0 );
					ad = ArbolNuevo( OP_NOP, 0, 0, ac, $5 );
					$$ = ArbolNuevo( OP_LINENO, 0, l, ab, ad );
				
				}
	|	T_OPBRACE block T_CLBRACE	{ $$ = $2; }
	;
block :
		block statement	{ $$ = ArbolNuevo( OP_NOP, 0, 0, $1, $2 ); }
	|	/* vacio */	{ $$ = ArbolNuevo( OP_NOP, 0, 0, 0, 0); }
	;
stat :
		expr		{ $$ = $1; }
	|	T_GLOBAL lista_desnuda
				{ $$ = ArbolNuevo( OP_GLOBAL, 0, 0, $2, 0 ); }
	|	T_DEL expr	{ $$ = ArbolNuevo( OP_DEL, 0, 0, $2, 0 ); }
	|	T_PRINT		{ $$ = ArbolNuevo( OP_IMPRIMIR, 0, 0, 0, 0 ); }
	|	T_PRINT expr	{ $$ = ArbolNuevo( OP_IMPRIMIR, 0, 1, $2, 0 ); }
	|	T_RAISE expr	{ $$ = ArbolNuevo( OP_RAISE, 0, 0, $2, 0 ); }
	|	T_END		{ $$ = ArbolNuevo( OP_FIN, 0, 0, 0, 0 ); }
	|	T_CONTINUE		{
					Res *xa, *xb, *final;

					final = IntFromInt( -1 );
					xb = Get( a_etiquetas, final );
					$$ = 0;
					if ( xb != NULO ) {
						xa = GetHead( xb );
						$$ = ArbolNuevo( OP_JMP, 0, NumVal(xa), 0, 0 );
					}
					Liberar( final );
				}
	|	T_BREAK		{
					Res *xa, *xb, *final;

					final = IntFromInt( -1 );
					xb = Get( a_etiquetas, final );
					$$ = 0;
					if ( xb != NULO ) {
						xa = GetTail( xb );
						$$ = ArbolNuevo( OP_JMP, 0, NumVal(xa), 0, 0 );
					}
					Liberar( final );
				}
	|	T_RETURN expr	{
					$$ = ArbolNuevo( OP_RETURN, 0, 0, $2, 0 );
				}
	|	/* vacio */	{ $$ = ArbolNuevo( OP_NOP, 0, 0, 0, 0 ); }
	;

expr :
		id			{ $$ = $1; }
	|	value			{ $$ = $1; }
	|	listvalue		{ $$ = $1; }
	|	consvalue		{ $$ = $1; }
	|	hashvalue		{ $$ = $1; }
	|	expr T_OR expr		{ $$ = ArbolNuevo( OP_OR, 0, 0, $1, $3 ); }
	|	expr T_XOR expr		{ $$ = ArbolNuevo( OP_XOR, 0, 0, $1, $3 ); }
	|	expr T_AND expr		{ $$ = ArbolNuevo( OP_AND, 0, 0, $1, $3 ); }
	|	T_NOT expr		{ $$ = ArbolNuevo( OP_NOT, 0, 0, $2, 0 ); }
	|	expr T_ISEQ expr	{ $$ = ArbolNuevo( OP_TEST, 0, 0, $1, $3 ); }
	|	expr T_ISGT expr	{ $$ = ArbolNuevo( OP_TEST, 0, 1, $1, $3 ); }
	|	expr T_ISLT expr	{ $$ = ArbolNuevo( OP_TEST, 0, 2, $1, $3 ); }
	|	expr T_ISEQGT expr	{ $$ = ArbolNuevo( OP_TEST, 0, 3, $1, $3 ); }
	|	expr T_ISEQLT expr	{ $$ = ArbolNuevo( OP_TEST, 0, 4, $1, $3 ); }
	|	expr T_ISNEQ expr	{ $$ = ArbolNuevo( OP_TEST, 0, 5, $1, $3 ); }
	|	expr T_EQ expr		{ $$ = ArbolNuevo( OP_SET, 0, 0, $1, $3 ); }
	|	expr T_PLUS expr	{ $$ = ArbolNuevo( OP_PLUS, 0, 0, $1, $3 ); }
	|	expr T_MINUS expr	{ $$ = ArbolNuevo( OP_MINUS, 0, 0, $1, $3 ); }
	|	expr T_STAR expr	{ $$ = ArbolNuevo( OP_MULT, 0, 0, $1, $3 ); }
	|	expr T_PORCENTAJE expr	{ $$ = ArbolNuevo( OP_MOD, 0, 0, $1, $3 ); }
	|	T_MINUS expr		{ $$ = ArbolNuevo( OP_UMINUS, 0, 0, $2, 0); }
	|	expr T_DASH expr	{ $$ = ArbolNuevo( OP_DIV, 0, 0, $1, $3 ); }
	|	expr T_OPBRACK expr T_CLBRACK
					{ $$ = ArbolNuevo( OP_SUB, 0, 0, $1, $3 ); }
	|	expr T_OPBRACK expr T_COLON expr T_CLBRACK
					{
						Arbol *aa;					
						aa = ArbolNuevo( OP_NOP, 0, 0, $3, $5 );
						$$ = ArbolNuevo( OP_SLICE, 0, 0, $1, aa );
					}
	|	expr T_OPBRACK expr T_COLON T_CLBRACK
					{
						$$ = ArbolNuevo( OP_SLICE, 0, 1, $1, $3 );
					}
	|	expr T_OPBRACK T_COLON expr T_CLBRACK
					{
						$$ = ArbolNuevo( OP_SLICE, 0, 2, $1, $4 );
					}
	|	expr T_OPBRACK T_COLON T_CLBRACK
					{
						$$ = ArbolNuevo( OP_SLICE, 0, 3, $1, 0 );
					}
	|	T_OPPAREN expr T_CLPAREN
					{ $$ = $2; }
	|	expr cualquier_lista
				{
					$$ = ArbolNuevo( OP_CALL, 0, 0, $1, $2 );
				}
	|	expr referencia cualquier_lista
				{
					Arbol *aa, *ab;
					
					aa = ArbolNuevo( OP_NOP, 0, 0, $1, $2 );
					ab = ArbolNuevo( OP_PUSH, 0, 0, $3, $1 );
					$$ = ArbolNuevo( OP_CALL, 0, 0, aa, ab );
				}
	|	T_SHIFT		{	$$ = ArbolNuevo( OP_SHIFT, 0, 0, 0, 0); }
	|	T_SHIFT expr	{	$$ = ArbolNuevo( OP_SHIFT, 0, 1, $2, 0); }
	|	T_NEW expr cualquier_lista
				{	$$ = ArbolNuevo( OP_NUEVO, 0, 0, $2, $3 ); }
	|	T_COPY expr	{	$$ = ArbolNuevo( OP_COPY, 0, 0, $2, 0 ); }
	|	T_LAMBDA {
							ParserNuevaFun();
		} fun_body {
							Res *compilada;
							Num k;
							
							codigo_actual = ArbolCompilar( $3 );
							es_modulo = IntFromInt(0);
							compilada = ParserFinFun();
							k = ParserNuevaConst( compilada );	
							$$ = ArbolNuevo( OP_NUM, 0, k, 0, 0 );
		}
	;
id :
		T_ID			{
						ParserNuevoId( $1 );
						$$ = ArbolNuevo( OP_ID, StrVal($1), 0, 0, 0 );
					}
	|	expr T_PUNTO T_ID	{	$$ = ArbolNuevo( OP_NAME, StrVal($3), 0, $1, 0 ); }
	;
fun_id :
		T_ID			{
						ParserNuevoId( $1 );
						$$ = ArbolNuevo( OP_ID, StrVal($1), 0, 0, 0 );
					}
	|	fun_id T_PUNTO T_ID	{	$$ = ArbolNuevo( OP_NAME, StrVal($3), 0, $1, 0 ); }
	;
referencia :
		T_REFERENCIA T_ID	{	$$ = ArbolNuevo( OP_REF, StrVal($2), 0, 0, 0 ); }
	;
value :
		T_VALUE			{
						Num k;
						
						k = ParserNuevaConst( $1 );
						$$ = ArbolNuevo( OP_NUM, 0, k, 0, 0 );
					}
	;
fun_body :
		listvalue statement
					{
						Arbol *aa;

						aa = ArbolNuevo( OP_READARGS, 0, 0, $1, 0 );
						$$ = ArbolNuevo( OP_NOP, 0, 0, aa, $2 );
					}
	|	statement		{	$$ = $1; }
	|	T_OPPAREN expr T_CLPAREN statement
					{
						Arbol *aa, *ab;

						aa = ArbolNuevo( OP_BUILDLIST, 0, 1, $2, 0 );
						ab = ArbolNuevo( OP_READARGS, 0, 0, aa, 0 );
						$$ = ArbolNuevo( OP_NOP, 0, 0, ab, $4 );
					}
	;
	
cualquier_lista :
		listvalue		{ $$ = $1; }
	;
listvalue :
		T_OPPAREN lista_coma T_CLPAREN
					{
						Res *cant, *final;

						final = IntFromInt( -1 );
						cant = Pop( items, final );
						$$ = ArbolNuevo(OP_BUILDLIST, 0, NumVal(cant),$2,0);
						Liberar( cant );
						Liberar( final );
					}
	|	T_OPPAREN T_CLPAREN
					{
						$$ = ArbolNuevo( OP_BUILDLIST, 0, 0, 0, 0 );
					}
	;
lista_desnuda :
		lista_coma		{
						Res *cant, *final;

						final = IntFromInt( -1 );
						cant = Pop( items, final );
						$$ = ArbolNuevo(OP_BUILDLIST, 0, NumVal(cant),$1,0);
						Liberar( cant );
						Liberar( final );
					}
	;
lista_coma :
		lista			{ $$ = $1; }
	|	lista T_COMMA		{ $$ = $1; }
	;
lista :
		expr			{
						Res *final, *cant;

						final = IntFromInt( -1 );
						cant = IntFromInt( 1 );
						Push( items, cant, final );
						$$ = $1;
						Liberar( final );
					}
	|	lista T_COMMA expr	{
						Res *final, *cant;

						final = IntFromInt( -1 );
						cant = Pop( items, final );
						NumVal(cant)++;
						Push( items, cant, final );
						$$ = ArbolNuevo( OP_NOP, 0, 0, $1, $3 );
						Liberar( final );
					}
	;
consvalue :
		T_OPPAREN keydatum T_CLPAREN
					{
						$$ = $2;
					}
	;
hashvalue :
		T_OPPAREN kdlista_coma T_CLPAREN
					{
						Res *cant, *final;

						final = IntFromInt( -1 );
						cant = Pop( items, final );
						$$ = ArbolNuevo(OP_BUILDHASH, 0, NumVal(cant),$2,0);
						Liberar( cant );
						Liberar( final );
					}
kdlista_coma :
		kdlista			{ $$ = $1; }
	|	kdlista T_COMMA		{ $$ = $1; }
	;
kdlista :
		keydatum		{
						Res *final, *cant;

						final = IntFromInt( -1 );
						cant = IntFromInt( 1 );
						Push( items, cant, final );
						$$ = $1;
						Liberar( final );
					}
	|	kdlista T_COMMA keydatum {
						Res *final, *cant;

						final = IntFromInt( -1 );
						cant = Pop( items, final );
						NumVal(cant)++;
						Push( items, cant, final );
						$$ = ArbolNuevo( OP_NOP, 0, 0, $1, $3 );
						Liberar( final );
					}
	;
keydatum :
		expr T_COLON expr	{
						$$ = ArbolNuevo(OP_BUILDCONS, 0, 0, $1, $3);
					}
%%
