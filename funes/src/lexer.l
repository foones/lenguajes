%{
#include <string.h>
#include <assert.h>

#include "Fu.h"
#include "parser.tab.h"

#define MAX_READ_BUFFER	256
#define PUSHBUF(C)	if (c-buffer<MAX_READ_BUFFER-1) {\
				*c++ = C;\
			} else {\
				*c = 0;\
				c = buffer;\
				cadena = fu_str_cat(cadena,fu_str(buffer));\
				*c++ = C;\
			}
char buffer[MAX_READ_BUFFER];
char *c;
RES *cadena;
RES *Reader_Macros;
RES *Reader_Funapply;

unsigned int cant_comentarios;
char interpolada;			/* la cadena tiene $ adentro */
RES *resultado;				/* resultado de interpolar */

int yyparse();

RES *fu_mkreader(YY_BUFFER_STATE buf);
#define VAL_READER(RES)	((YY_BUFFER_STATE) VAL(RES))


%}
%option noyywrap
EXTALNUM	[0-9a-zA-Z!%&*$+/:<=>?@_~^-]
EXTALNUM2	[0-9a-zA-Z!%&*$+/<=>?@_~^-]
%x str
%x comentario
%x multicomentario
%%
<<EOF>>			{ return T_EOF; }
"#!"			{ BEGIN(comentario);
}
";"			{ BEGIN(comentario);
}
<comentario>.*\n { 
			BEGIN(INITIAL); }
"#|"			{ cant_comentarios = 0; BEGIN(multicomentario); }
<multicomentario>{
  "#|"	{ ++cant_comentarios; }
  "|#"	{
	  if (cant_comentarios == 0)
		  BEGIN(INITIAL);
	  else --cant_comentarios;
  }
  [^#|\n]+	{}
  \n+		{}
  .		{}
}
"|#"			{ fu_throw(fu_exception_symbol("read-error"),
				fu_str("|# inesperado")); }
"("			{ return T_LPAREN; }
")"			{ return T_RPAREN; }
"."			{ return T_DOT; }
"'"			{ return T_QUOTE; }
"`"			{ return T_BACKQUOTE; }
",@"			{ return T_UNQUOTE_SPLICING; }
","			{ return T_UNQUOTE; }
"#t"		{ yylval.val = T; return T_ATOM; }
"#f"		{ yylval.val = NIL; return T_ATOM; }
"#u"		{ yylval.val = UNDEF; return T_ATOM; }
"#argset"	{ yylval.val = ARGSET; return T_ATOM; }
"-"?[0-9]*"."[0-9]+ { yylval.val = fu_float(atof(yytext)); return T_ATOM; }
"-"?[0-9]+"."[0-9]* { yylval.val = fu_float(atof(yytext)); return T_ATOM; }
"-"?[0-9]+	{ yylval.val = fu_int(atoi(yytext)); return T_ATOM; }
\\nl		{ yylval.val = fu_char((int) '\n'); return T_ATOM; }
\\cr		{ yylval.val = fu_char((int) '\r'); return T_ATOM; }
\\space		{ yylval.val = fu_char((int) ' '); return T_ATOM; }
\\.		{ yylval.val = fu_char((int) yytext[1]); return T_ATOM; }
("|"|"#".)	{
	RES *reader_actual, *leido, *res;
			
	reader_actual = fu_mkreader(YY_CURRENT_BUFFER);
	if (yytext[0] == '|')
		leido = fu_read(reader_actual);
	else if (yytext[0] == '#')
		leido = fu_char(yytext[1]);
	else
		assert(FALSE);	
	res = fu_get_hash_eq(Reader_Macros, leido);
	if (res == NIL)
		res = fu_get_hash_eq(Reader_Macros, T);
	if (res == NIL)
		fu_throw(fu_exception_symbol("not-found"),
			fu_str("read -- reader macro not found"));
	yylval.val = fu_apply(CDR(res),
		fu_make_list("xx",
			leido,
			reader_actual));
	return T_ATOM;
	}
{EXTALNUM2}+"::"{EXTALNUM2}+("."|"(") {
			char *p;
			char *s;
			char ultimo;
			unsigned l1, l2;
			for (p = yytext; *p != ':'; ++p) { }
			*p = '\0';
			l1 = p - yytext;
			l2 = strlen(&p[2])-1;
			s = NEWQ(char, l2+1);
			strncpy(s,&p[2],l2);
			ultimo = p[l2 + 2];
			s[l2] = '\0';
			yylval.val = fu_symbol_in(yytext, s);
			if (ultimo == '.')
				return T_ATOM_DOT;
			else if (ultimo == '(') {
				if (Reader_Funapply != NIL)
					return T_ATOM_PAREN;
				else {
					unput('(');
					return T_ATOM;
				}
			} else {
				assert(FALSE);
			}
		}
"--"{EXTALNUM2}+"::"{EXTALNUM2}+ {
			char *p;
			char *s;
			unsigned l1, l2;
			yytext = &yytext[2];
			for (p = yytext; *p != ':'; ++p) { }
			l1 = p - yytext;
			l2 = strlen(&p[2]);
			*p = '\0';
			s = NEWQ(char, l2+1);
			strncpy(s,&p[2],l2);
			s[l2] = '\0';
			yylval.val = fu_cons(fu_symbol_in(yytext, s),
					     fu_local_symbol(s));
			return T_EQUAL_ATOM;
		}
{EXTALNUM2}+"::"{EXTALNUM2}+ {
			char *p;
			char *s;
			unsigned l1, l2;
			for (p = yytext; *p != ':'; ++p) { }
			l1 = p - yytext;
			l2 = strlen(&p[2]);
			*p = '\0';
			s = NEWQ(char, l2+1);
			strncpy(s,&p[2],l2);
			s[l2] = '\0';
			yylval.val = fu_symbol_in(yytext, s);
			return T_ATOM;
		}
{EXTALNUM}+("."|"(")	{
			char *s;
			unsigned l;
			char ultimo;
			l = strlen(yytext)-1;
			s = NEWQ(char, l+1);
			strncpy(s,yytext,l);
			s[l] = '\0';
			ultimo = yytext[l];
			yylval.val = fu_local_symbol(s);
			if (ultimo == '.')
				return T_ATOM_DOT;
			else if (ultimo == '(') {
				if (Reader_Funapply != NIL)
					return T_ATOM_PAREN;
				else {
					unput('(');
					return T_ATOM;
				}
			} else {
				assert(FALSE);
			}
		}
"--"{EXTALNUM}+	{	char *s;
			unsigned l;
			l = strlen(yytext)-1;
			s = NEWQ(char, l+1);
			strncpy(s,&yytext[2],l);
			s[l] = '\0';
			yylval.val = fu_cons(fu_keyword_argument(s),
					     fu_local_symbol(s));
			return T_EQUAL_ATOM;
		}
":"{EXTALNUM}+	{	char *s;
			unsigned l;
			l = strlen(yytext);
			s = NEWQ(char, l+1);
			strncpy(s,&yytext[1],l);
			s[l] = '\0';
			yylval.val = fu_sym(s);
			return T_ATOM;
		}
"="{EXTALNUM}+	{	char *s;
			unsigned l;
			l = strlen(yytext);
			s = NEWQ(char, l);
			strncpy(s,&yytext[1],l);
			s[l] = '\0';
			yylval.val = fu_keyword(s);
			return T_ATOM;
		}
"&"{EXTALNUM}+	{	char *s;
			unsigned l;
			l = strlen(yytext);
			s = NEWQ(char, l);
			strncpy(s,&yytext[1],l);
			s[l] = '\0';
			yylval.val = fu_exception_symbol(s);
			return T_ATOM;
		}
{EXTALNUM}+	{	char *s;
			unsigned l;
			l = strlen(yytext);
			s = NEWQ(char, l+1);
			strncpy(s,yytext,l);
			s[l] = '\0';
			yylval.val = fu_local_symbol(s);
			return T_ATOM;
		}
\"\"			{	yylval.val = fu_str("");
					return T_ATOM;
				}
\"			{	interpolada = FALSE;
				cadena = fu_str("");
				c = buffer;
				BEGIN(str); }
<str>\"			{	BEGIN(INITIAL);
					*c = '\0';
					cadena = fu_str_cat(cadena,fu_str(buffer));
					if (interpolada) {
						resultado = fu_cons(cadena, resultado);
						yylval.val = fu_cons(
							fu_funes_symbol("cat"), fu_xrev(resultado));
						return T_CADENA_INTERPOLADA;
					} else {
						yylval.val = cadena;
						return T_ATOM;
					}
				}
<str>\\\n		{ /* nada */ }
<str>\\n		{ PUSHBUF('\n'); }
<str>\\t		{ PUSHBUF('\t'); }
<str>\\r		{ PUSHBUF('\r'); }
<str>\\b		{ PUSHBUF('\b'); }
<str>\\f		{ PUSHBUF('\f'); }
<str>\\.		{ PUSHBUF(yytext[1]); }
<str>"$@"		{
					/* interpola splicing */
					RES *prev = NIL;
					RES *cadena_ant;
					RES *resultado_ant;
					RES *separador;
					char interpolada_ant;

					BEGIN(INITIAL);
					*c = '\0';
					cadena = fu_str_cat(cadena,fu_str(buffer));
					cadena_ant = cadena;
					resultado_ant = resultado;
					interpolada_ant = interpolada;
					yyparse();
					separador = Read_Sexpr;
					yyparse();
					cadena = cadena_ant;
					resultado = resultado_ant;
					interpolada = interpolada_ant;
					if (!interpolada)
						interpolada = TRUE;
					else
						prev = resultado;
					resultado = fu_cons(
						fu_make_list("xxxx",
							fu_funes_symbol("call"),
							fu_funes_symbol("join"),
							fu_make_list("xx",
								fu_funes_symbol("str"),
								separador),
							fu_make_list("xxx",
								fu_funes_symbol("map"),
								fu_funes_symbol("str"),
								Read_Sexpr)),
						fu_cons(cadena, prev));
					cadena = fu_str("");
					c = buffer;
					BEGIN(str);
				}
<str>"$"		{
					/* interpola sin splice */
					RES *prev = NIL;
					RES *cadena_ant;
					RES *resultado_ant;
					char interpolada_ant;

					BEGIN(INITIAL);
					*c = '\0';
					cadena = fu_str_cat(cadena,fu_str(buffer));
					cadena_ant = cadena;
					resultado_ant = resultado;
					interpolada_ant = interpolada;
					yyparse();
					cadena = cadena_ant;
					resultado = resultado_ant;
					interpolada = interpolada_ant;
					if (!interpolada)
						interpolada = TRUE;
					else
						prev = resultado;
					resultado = fu_cons(
						fu_cons(fu_funes_symbol("str"), fu_cons(Read_Sexpr, NIL)),
						fu_cons(cadena, prev));
					cadena = fu_str("");
					c = buffer;
					BEGIN(str);
				}
<str>[^\\\"] 		{ char *p = yytext;
			  while (*p)
			  	PUSHBUF(*p++);
			}
[ \t\n\r]*		{ /* se come los blancos */ }
.			{
	RES *reader_actual, *leido, *res;
			
	reader_actual = fu_mkreader(YY_CURRENT_BUFFER);
	leido = fu_char(yytext[0]);
	res = fu_get_hash_eq(Reader_Macros, leido);
	if (res == NIL)
		res = fu_get_hash_eq(Reader_Macros, T);
	if (res == NIL)
		fu_throw(fu_exception_symbol("read-error"),
			fu_str("read -- caracter invalido"));
	yylval.val = fu_apply(CDR(res),
			fu_make_list("xx",
				leido,
				reader_actual));
	return T_ATOM;
}
%%

void
fu_init_reader() {
	Reader_Macros = fu_make_hash(fu_int(3));
	Reader_Funapply = NIL;
}

RES *
fu_set_reader_macro(RES *name, RES *macro)
{
	fu_set_hash_eq(Reader_Macros, name, macro);
	return T;
}

RES *
fu_set_reader_funapply(RES *val)
{
	Reader_Funapply = val;
	return T;
}

RES *
fu_read(port)
	RES *port;
{
	RES *r = port;
	YY_BUFFER_STATE prev;
	unsigned char cambio = FALSE;

	if (r != UNDEF && !TIPO_P(tipo_reader, r)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("read -- no es un reader"));
	}
	if (port == UNDEF) r = fu_reader(Std_Input, FALSE);
	if ( YY_CURRENT_BUFFER != VAL_READER(r) ) {
		prev = YY_CURRENT_BUFFER;
		yy_switch_to_buffer(VAL_READER(r));
		cambio = TRUE;
	}
	yyparse();
	if (port == UNDEF) yy_delete_buffer(YY_CURRENT_BUFFER);
	if (cambio) {
		yy_switch_to_buffer(prev);
	}
	return Read_Sexpr;
}

RES *
fu_mkreader(YY_BUFFER_STATE buf)
{
	RES *p;

	p = NEW(RES);
	TIPO(p) = tipo_reader;
	VAL(p) = (void *) buf;
	return p;
}

RES *
fu_reader(puerto_entrada, start)
	RES *puerto_entrada;
	unsigned char start;
{
	RES *p;
	YY_BUFFER_STATE buf;

	buf = yy_create_buffer(VAL_PORT(puerto_entrada), YY_BUF_SIZE);

	p = fu_mkreader(buf);
	if (start)
		yy_switch_to_buffer(buf);
	return p;
}

RES *
fu_stread(cadena)
	RES *cadena;
{
	YY_BUFFER_STATE prev, curr;
	if (!STR_P(cadena)) {
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("stread -- no es una cadena"));
	}

	prev = YY_CURRENT_BUFFER;
	curr = yy_scan_string(VAL_STR(cadena));
	yy_switch_to_buffer(curr);

	yyparse();
	yy_delete_buffer(YY_CURRENT_BUFFER);
	yy_switch_to_buffer(prev);

	return Read_Sexpr;
}
