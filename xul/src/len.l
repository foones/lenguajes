%{
#include "Len.h"
#include "len.tab.h"
#include "parser.h"

char CadenaBuffer[ STR_MAX ];
char *CadenaPtr;
int linea = 1;
%}

%x cadena
%x cadena_cruda

%%
#.*$			{
				linea++;
				/* Se come los comentarios. */
			}
"and"			return T_AND;
"break"			return T_BREAK;
"continue"		return T_CONTINUE;
"copy"			return T_COPY;
"del"			return T_DEL;
"else"			return T_ELSE;
"end"			return T_END;
"if"			return T_IF;
"in"			return T_IN;
"for"			return T_FOR;
"fun"			return T_FUN;
"global"		return T_GLOBAL;
"handle"		return T_HANDLE;
"lambda"		return T_LAMBDA;
"module"		return T_MODULE;
"new"			return T_NEW;
"not"			return T_NOT;
"or"			return T_OR;
"print"			return T_PRINT;
"raise"			return T_RAISE;
"return"		return T_RETURN;
"shift"			return T_SHIFT;
"try"			return T_TRY;
"while"			return T_WHILE;
"xor"			return T_XOR;

[_a-zA-Z][_a-zA-Z0-9]*	{
				yylval.rep = StrFromStr( yytext );
				return T_ID;
			}
[0-9]+(\.[0-9]+)?	{
				float n;
				n = atof( yytext );
				if ( n == (int) n ) { 
					yylval.rep = IntFromInt( n );
				} else {
					yylval.rep = FloatFromFloat( n );
				}
				return T_VALUE;
			}
\"			{
				CadenaPtr = CadenaBuffer;
				BEGIN(cadena);
			}
<cadena>{
\"			{
				BEGIN(INITIAL);
				*CadenaPtr = '\0';
				yylval.rep = StrFromStr( CadenaBuffer );
				return T_VALUE;
			}
\\\"			{
				*CadenaPtr++ = '\"';
			}
\\n			{
				*CadenaPtr++ = '\n';
			}
\\t			{
				*CadenaPtr++ = '\t';
			}
\\\\			{
				*CadenaPtr++ = '\\';
			}
.			{
				char *Ptr = yytext;

				while ( *Ptr )
					*CadenaPtr++ = *Ptr++;
			}
}
\'			{
				CadenaPtr = CadenaBuffer;
				BEGIN(cadena_cruda);
			}
<cadena_cruda>{
\'			{
				BEGIN(INITIAL);
				*CadenaPtr = '\0';
				yylval.rep = StrFromStr( CadenaBuffer );
				return T_VALUE;
			}
\\\'			{
				*CadenaPtr++ = '\'';
			}
\\\\			{
				*CadenaPtr++ = '\\';
			}
.			{
				char *Ptr = yytext;

				while ( *Ptr )
					*CadenaPtr++ = *Ptr++;
			}
}

"->"			{ return T_REFERENCIA; }
"=="			{ return T_ISEQ; }
"-"			{ return T_MINUS; }
">"			{ return T_ISGT; }
"<"			{ return T_ISLT; }
">="			{ return T_ISEQGT; }
"<="			{ return T_ISEQLT; }
"!="			{ return T_ISNEQ; }
"="			{ return T_EQ; }
"+"			{ return T_PLUS; }
"*"			{ return T_STAR; }
"/"			{ return T_DASH; }
"("			{ return T_OPPAREN; }
")"			{ return T_CLPAREN; }
"{"			{ return T_OPBRACE; }
"}"			{ return T_CLBRACE; }
"["			{ return T_OPBRACK; }
"]"			{ return T_CLBRACK; }
":"			{ return T_COLON; }
";"			{ return T_SEMICOLON; }
","			{ return T_COMMA; }
"."			{ return T_PUNTO; }
"%"			{ return T_PORCENTAJE; }
\n			{
				linea++;
				/* Se come los enters. */
			}
[ \t]+			{ /* Se come los espacios. */ }
.			{ /* Nada. */ }
%%
