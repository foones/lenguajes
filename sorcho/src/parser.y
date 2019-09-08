%{
#include <stdlib.h>
#include "Sor.h"
%}
%union {
	Objeto *val;
	unsigned cant; 
	Lista *lista;
	char *cadena;
	Entorno *env;
	char modo;
	Maquina *maq;
}
%type <val> expr expr_baja guardas atom lambda_expr unlambdosa
%type <val> listas lista_de_conses cons_infijo
%type <lista> lista_generica argumentos
%type <cant> lista_tipos
%type <val> expr_donde capsula
%type <modo> wrap_mode eval_mode otro_mode
%token <val> ID LIT
%token <cadena> ARCHIVO
%token <maq> MAQUINA
%nonassoc DEF
%left OR
%right FLECHA
%right CONSINFIJO
%left SMILEY
%right YELIMS
%token BACKQUOTE
%token PLEASE CHE COMA NL LPAREN RPAREN LDEFI RDEFI
%token LBRACK RBRACK
%token PUBIC CUBIC RUBIK
%token HLADIK MAGIC PLASTIC
%token VOID LIZARD ESTAFRIO WARP
%token BGEN EGEN BDONDE EDONDE BCAPSULA ECAPSULA BUNL EUNL BMAQ EMAQ
%token BCHARLATAN ECHARLATAN BKOAN EKOAN
%token TEOF
%token GUARDA SINO

%token BCHONGOL ECHONGOL
%token SEMICOLON
%token TIF TENDIF
%type <val> chongol_expresion
%type <lista> chongol_parametros

%start sexpr
%%
sexpr : expr NL		{
			Read_Sexpr = $1;
			return 1;
			}
|	LDEFI definiciones RDEFI
			{
			Read_Sexpr = mk_nada();
			return 1;
			}
|	wrap_mode eval_mode otro_mode ID MAQUINA {
			$5->wrap_mode = $1;
			$5->eval_mode = $2;
			$5->otro_mode = $3;
			hash_set(Actual, VAL_NOMBRE($4), mk_maquina($5));
			Read_Sexpr = mk_nada();
			return 1;
			}
|	opcion		{
			Read_Sexpr = mk_nada();
			return 1;
			}
|	BCHONGOL chongol_programa ECHONGOL
			{
			Read_Sexpr = mk_nada();
			return 1;
			}
|	TEOF		{
			return 0;
			}
;

definiciones :
	/* vacio */
|	definicion NL definiciones
;

definicion :
	CHE COMA definicion_simple
|	PLEASE ID DEF decltipo
|	PLEASE DEF decltipo
|	ID argumentos DEF expr_donde
	{
	Objeto *ant;
	Lista *l;

	ant = hash_get(Actual, VAL_NOMBRE($1));
	if (ant && (ant->tipo == tipo_generica))
		l = VAL_GENERICA(ant);
	else
		l = 0;
	hash_set(Actual, VAL_NOMBRE($1),
		mk_generica(mk_par(
			mk_curry(Pila_Actuales, $2, $4), l)));
	}
;

definicion_simple:
ID argumentos DEF expr_donde
	{
	hash_set(Actual, VAL_NOMBRE($1),
		mk_curry(Pila_Actuales, $2, $4));
	}
;

expr_donde:
	guardas
|	guardas
	BDONDE
	{
	Actual = mk_hash(HASH_PRIMO);
	Pila_Actuales = mk_entorno(Actual, Pila_Actuales);
	}
	definiciones
	EDONDE
	{
	$$ = mk_dondura(Actual, $1);
	Pila_Actuales = Pila_Actuales->padre;
	Actual = Pila_Actuales->hash;
	}
;

guardas:
	expr
|	SINO DEF expr { $$ = $3; }
|	GUARDA expr DEF expr guardas
	{
	$$ = mk_aplicacion(
	mk_aplicacion(mk_aplicacion(mk_simbolo("si"), $2), $4),
	$5);
	}
;

expr:
	expr_baja
|	expr expr_baja
		{
		if ($1->tipo == tipo_constructor) {
			$$ = mk_construccion($1, $2);
		} else if ( $1->tipo == tipo_construccion ) {
			construccion_push($1, $2);
			$$ = $1;
		} else {
			$$ = mk_aplicacion($1, $2);
		}
		}
;

expr_baja :
	atom
|	listas
|	LPAREN expr RPAREN	{ $$ = $2; }
|	LPAREN lambda_expr RPAREN
			{ $$ = $2; }
|	BGEN lista_generica EGEN
			{ $$ = mk_generica($2); }
|	BUNL unlambdosa EUNL
			{ $$ = $2; }
|	cons_infijo
|	capsula
|	expr_baja SMILEY ID
	{
		Objeto *f;
		Objeto *o;

		f = reducir(Pila_Actuales, $1);
		if (f->tipo != tipo_entorno ||
		!(o = binding(VAL_ENTORNO(f), VAL_NOMBRE($3)))) {
			fail("El obrero fue aplastado por las vigas", $1);
		}

		if (o->tipo == tipo_constructor) {
			$$ = o;
		} else {
			$$ = mk_cerradura(VAL_ENTORNO(f), o);
		}
	}
|	ID YELIMS expr_baja
	{
		Objeto *f;
		Objeto *o;

		f = reducir(Pila_Actuales, $3);
		if (f->tipo != tipo_entorno ||
		!(o = binding(VAL_ENTORNO(f), VAL_NOMBRE($1)))) {
			fail("The worker has been smashed", $3);
		}

		if (o->tipo == tipo_constructor) {
			$$ = o;
		} else {
			$$ = mk_cerradura(VAL_ENTORNO(f), o);
		}
	}
|	BMAQ MAQUINA EMAQ {
			$2->wrap_mode = WRAP_TOROIDE;
			$2->eval_mode = EVAL_STRICT;
			$2->otro_mode = OTRO_CERO;
			$$ = mk_maquina($2);
			}
;

unlambdosa:
	expr_baja
|	BACKQUOTE expr_baja expr_baja 
	{
		$$ = mk_aplicacion($2, $3);
	}
;

capsula:
	BCAPSULA
	{
	Actual = mk_hash(HASH_PRIMO);
	Pila_Actuales = mk_entorno(Actual, Pila_Actuales);
	}
	definiciones
	ECAPSULA
	{
	$$ = mk_objeto_entorno(Pila_Actuales);
	Pila_Actuales = Pila_Actuales->padre;
	Actual = Pila_Actuales->hash;
	}
;

argumentos:
	/* vacio */	{ $$ = 0; }
|	expr argumentos	{ $$ = mk_par($1, $2); }
;

lambda_expr :
	expr FLECHA expr
		{ $$ = mk_cerradura(Pila_Actuales,
			mk_funcion(Pila_Actuales, $1, $3)); }
;

lista_generica :
	expr			{ $$ = mk_par($1, 0); }
|	expr OR lista_generica	{ $$ = mk_par($1, $3); }
;

atom :	ID	{
		Objeto *r;

		r = binding(Pila_Actuales, VAL_NOMBRE($1));
		if (r && (r->tipo == tipo_constructor)) {
			$$ = r;
		} else {
			$$ = $1;
		}
		}
|	LIT
;

listas :
	LBRACK lista_de_conses RBRACK { $$ = $2; }
;

lista_de_conses :
	/* vacio */	{
	
			Objeto *r;

			r = binding(Pila_Actuales, "Nil");
			if (r && (r->tipo == tipo_constructor)) {
				$$ = r;
			} else {
				$$ = mk_simbolo("Nil");
			}
			}
|	expr lista_de_conses
			{
			Objeto *l = binding(Pila_Actuales, "Cons");

			if (!l) {
				fail("Bebe las gotas de rocio al amanecer.",
					mk_simbolo("Raindrop Prelude, Chopin"));
			}

			$$ = mk_construccion(l, $1);
			construccion_push($$, $2);
			}
;

cons_infijo:
	expr_baja CONSINFIJO expr_baja
			{
			Objeto *l = binding(Pila_Actuales, "Cons");

			if (!l) {
				fail("Tenes los cordones desatados.",
					mk_simbolo("Raindrop Prelude, Chopin"));
			}

			$$ = mk_construccion(l, $1);
			construccion_push($$, $3);
			}
;

decltipo :
	ID lista_tipos
		{
		hash_set(Actual, VAL_NOMBRE($1),
			mk_constructor(VAL_NOMBRE($1), $2));
		}
|	decltipo OR decltipo
;

lista_tipos :
	/* vacio */	{ $$ = 0; }
|	lista_tipos ID	{ $$ = $1 + 1; }
;

wrap_mode :	RUBIK	{ $$ = WRAP_TOROIDE; }
|		PUBIC	{ $$ = WRAP_REFLEJAR; }
|		CUBIC	{ $$ = WRAP_DOBLAR; }
;

eval_mode :	HLADIK	{ $$ = EVAL_STRICT; }
|		MAGIC	{ $$ = EVAL_LAZY; }
|		PLASTIC	{ $$ = EVAL_STRICT; }
;

otro_mode :	VOID		{ $$ = 0; }
|		LIZARD		{ $$ = 1; }
|		ESTAFRIO	{ $$ = 0; }
|		WARP		{ $$ = 1; }
;

opcion :
	BCHARLATAN	{ Charlatan = 1; }
|	ECHARLATAN	{ Charlatan = 0; }
|	ARCHIVO		{ cargar_archivo($1, 0); }
|	BKOAN		{
			Koan = 1;
			srandom((unsigned int) Read_Sexpr);
			}
|	EKOAN		{ Koan = 0; }
;

chongol_programa:
	/* vacio */
|	chongol_stmt SEMICOLON chongol_programa
;

chongol_stmt:
	chongol_expresion	{}
;

chongol_expresion:
	atom
|	chongol_expresion LPAREN chongol_parametros RPAREN
	{
	Objeto *f = $1;
	Lista *l;

	for (l = $3; l; l = l->siguiente) {
		f = mk_aplicacion(f, l->elemento);
	}
	$$ = f;
	}
;

chongol_parametros:
	chongol_expresion
	{ $$ = mk_par($1, 0); }
|	chongol_expresion COMA chongol_parametros
	{ $$ = mk_par($1, $3); }
;

%%

yywrap()
{
}

yyerror(char *s)
{
	fail("Error de sintaxis", mk_simbolo(s));
}
