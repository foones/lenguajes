%{
#include <string.h>
#include "Sor.h"
#ifdef WINDOWS
#include "parser_tab.h"
#else
#include "parser.tab.h"
#endif

Maquina *Maq_Actual;
int posx, posy;
%}
%x CODIGOS
%x CADENA
%%
<<EOF>>			{ return TEOF; }
"::".*			{ /* comentario desde :: hasta nueva linea */ }
"o"			{ return OR; }
"sino"			{ return SINO; }
"che"			{ return CHE; }
"please"		{ return PLEASE; }
"pubic"			{ return PUBIC; }
"cubic"			{ return CUBIC; }
"rubik"			{ return RUBIK; }
"hladik"		{ return HLADIK; }
"magic"			{ return MAGIC; }
"plastic"		{ return PLASTIC; }
"void"			{ return VOID; }
"lizard"		{ return LIZARD; }
"estafrio"		{ return ESTAFRIO; }
"warp"			{ return WARP; }
"<gen>"			{ return BGEN; }
"</gen>"		{ return EGEN; }
"<donde>"		{ return BDONDE; }
"</donde>"		{ return EDONDE; }
"<capsula>"		{ return BCAPSULA; }
"</capsula>"		{ return ECAPSULA; }
"<unl>"			{ return BUNL; }
"</unl>"		{ return EUNL; }
"<charlatan>"		{ return BCHARLATAN; }
"</charlatan>"		{ return ECHARLATAN; }
"<zen>"			{ return BKOAN; }
"</zen>"		{ return EKOAN; }
"<maq>"			{ return BMAQ; }
"</maq>"		{ return EMAQ; }
"<chongol>"		{ return BCHONGOL; }
"</chongol>"		{ return ECHONGOL; }
"<if>"			{ return TIF; }
"</if>"			{ return TENDIF; }
"<".*".sr>"		{
			char *s;
			unsigned l;
			l = strlen(yytext);
			s = NEWQ(char, l+1);
			strncpy(s, yytext, l);
			s[l-1] = '\0';
			yylval.cadena = s+1;
			return ARCHIVO;
			}
"-"?[0-9]+		{
			yylval.val = mk_int(atoi(yytext));
			return LIT;
			}
":("			{
			yylval.val = mk_simbolo(":(");
			return ID;
			}
"//"			{ return GUARDA; }
":)"			{ return SMILEY; }
"(:"			{ return YELIMS; }
":"			{ return DEF; }
"."			{ return NL; }
";"			{ return SEMICOLON; }
","			{ return COMA; }
"`"			{ return BACKQUOTE; }
"--->"			{ return FLECHA; }
"("			{ return LPAREN; }
")"			{ return RPAREN; }
"/*"			{ return LDEFI; }
"*/"			{ return RDEFI; }
"["			{ return LBRACK; }
"]"			{ return RBRACK; }
"|"			{ return CONSINFIJO; }
"'"."'"			{
			yylval.val = mk_char((char) yytext[1]);
			return LIT;
			}
[a-zA-Z0-9!?$%&*+/=@_~^-]+		{
			char *s;
			unsigned l;
			l = strlen(yytext);
			s = NEWQ(char, l+1);
			strncpy(s, yytext, l);
			s[l] = '\0';
			yylval.val = mk_simbolo(s);
			return ID;
			}
\"			{
			BEGIN(CADENA);
			return LBRACK;
			}
<CADENA>\"		{
			BEGIN(INITIAL);
			return RBRACK;
			}
<CADENA>\n		{
			yylval.val = mk_char('\n');
			return LIT;
			}
<CADENA>"\\n"		{
			yylval.val = mk_char('\n');
			return LIT;
			}
<CADENA>"\\t"		{
			yylval.val = mk_char('\t');
			return LIT;
			}
<CADENA>"\\0"		{
			yylval.val = mk_char('\0');
			return LIT;
			}
<CADENA>.		{
			yylval.val = mk_char((char) yytext[0]);
			return LIT;
			}
"{"\n		{
		BEGIN(CODIGOS);
		Maq_Actual = NEW(Maquina);
		limpiar_maquina(Maq_Actual, &posx, &posy);
		}
<CODIGOS>^[ \t]*"}"$	{
		yylval.maq = Maq_Actual;
		BEGIN(INITIAL);
		return MAQUINA;
		}
<CODIGOS>[ _!|#$%&+*/=?.:,;'"@()~<>{}a-zA-Z0-9\[\]\\^-] {
		poner_maquina(Maq_Actual, &posx, &posy, (char) yytext[0]);
		}
<CODIGOS>.	{
		fail("Caracter invalido", mk_simbolo(yytext));
		}
<CODIGOS>\n	{
		linea_maquina(Maq_Actual, &posx, &posy);
		}
[ \t\n]		{}
.		{ fail("Caracter invalido", mk_simbolo(yytext)); }
%%

void leer_de_archivo(FILE *f)
{
	YY_BUFFER_STATE buf;

	buf = yy_create_buffer(f, 256);
	yy_switch_to_buffer(buf);
}

void cargar_archivo(char *nombre, char flag_afuera)
{
	static unsigned sp = 0;
	unsigned i;
	YY_BUFFER_STATE buf, prev;
	FILE *f;

	if (flag_afuera) sp = 0;
	++sp;
	printf(";;");
	for (i = 0; i < 2*sp; i++)
		printf(" ");
	printf("Sorbiendo %s con crema.\n", nombre);

	if ( !(f = fopen(nombre,"r")))
		fail("No se puede leer.", mk_simbolo(nombre));

	prev = YY_CURRENT_BUFFER;
	buf = yy_create_buffer(f, 256);
	yy_switch_to_buffer(buf);
	while (f) {
		if (!yyparse())
			break;
	}
	yy_switch_to_buffer(prev);
	if (fclose(f))
		fail("No se puede cerrar.", mk_simbolo(nombre));
	printf(";;");
	for (i = 0; i < 2*sp; i++)
		printf(" ");
	printf("Muy rico %s.\n", nombre);
	--sp;
}
