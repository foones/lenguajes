#ifndef BISON_PARSER_TAB_H
# define BISON_PARSER_TAB_H

#ifndef YYSTYPE
typedef union {
	RES *val;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	T_LPAREN	257
# define	T_RPAREN	258
# define	T_DOT	259
# define	T_EOF	260
# define	T_QUOTE	261
# define	T_BACKQUOTE	262
# define	T_UNQUOTE_SPLICING	263
# define	T_UNQUOTE	264
# define	T_ATOM	265
# define	T_ATOM_DOT	266
# define	T_EQUAL_ATOM	267
# define	T_CADENA_INTERPOLADA	268
# define	REGEXP_START	269
# define	REGEXP_END	270
# define	REGEXP_CLOSURE	271
# define	REGEXP_OR	272
# define	REGEXP_ATOM	273


extern YYSTYPE yylval;

#endif /* not BISON_PARSER_TAB_H */
