%{
#include "Fu.h"
%}
%union {
	RES *val;
}
%token T_LPAREN T_RPAREN T_DOT T_EOF
%token T_QUOTE T_BACKQUOTE T_UNQUOTE_SPLICING T_UNQUOTE
%token <val> T_ATOM T_ATOM_DOT T_EQUAL_ATOM T_ATOM_PAREN T_CADENA_INTERPOLADA
%token REGEXP_START REGEXP_END REGEXP_CLOSURE REGEXP_OR
%token <val> REGEXP_ATOM

%type <val> sexpr expr expr_list expr_dot expr_paren

%start sexpr

%%
sexpr:	expr			{ Read_Sexpr = $1; return 0; }
|		T_EOF		{ Read_Sexpr = EOF_OBJECT; return 0; }
;

expr:	T_ATOM
|	T_CADENA_INTERPOLADA
|	expr_paren expr_list T_RPAREN	{ $$ = fu_cons($1, $2); }
|	T_LPAREN expr_list T_RPAREN	{ $$ = $2; }
|	T_QUOTE expr	{ $$ = fu_make_list("xx", fu_funes_symbol("quote"), $2); }
|	T_BACKQUOTE expr{ $$ = fu_make_list("xx", fu_funes_symbol("backquote"), $2); }
|	T_UNQUOTE expr	{ $$ = fu_make_list("xx", fu_funes_symbol("unquote"), $2); }
|	T_UNQUOTE_SPLICING expr	{ $$ = fu_make_list("xx", fu_funes_symbol("unquote-splicing"), $2); }
|	expr_dot T_ATOM { $$ = fu_make_list("xx", $1,
				fu_make_list("xx", fu_funes_symbol("quote"), $2)); }
|	T_EQUAL_ATOM expr { $$ = fu_make_list("xxxx", ARGSET,
				CAR($1), CDR($1), $2); }
;

expr_paren :
	T_ATOM_PAREN 
|	expr_dot T_ATOM_PAREN
	{ $$ = fu_make_list("xx", $1,
		fu_make_list("xx", fu_funes_symbol("quote"), $2)); }
;

expr_dot :
	T_ATOM_DOT
|	expr_dot T_ATOM_DOT { $$ = fu_make_list("xx", $1,
				fu_make_list("xx", fu_funes_symbol("quote"), $2)); }
;

expr_list:	expr expr_list		{ $$ = fu_cons($1, $2); }
|		T_DOT expr		{ $$ = $2; }
|		/* vacia */		{ $$ = NIL; }
;

%%

//void yywrap() { }

int yyerror(s)
	char *s;
{
	fu_throw(fu_exception_symbol("parse-error"),
			fu_str(s));
	return 0;
}
