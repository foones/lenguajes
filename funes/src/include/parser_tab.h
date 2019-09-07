typedef union {
	RES *val;
} YYSTYPE;
#define	T_LPAREN	258
#define	T_RPAREN	259
#define	T_DOT	260
#define	T_EOF	261
#define	T_QUOTE	262
#define	T_BACKQUOTE	263
#define	T_UNQUOTE_SPLICING	264
#define	T_UNQUOTE	265
#define	T_ATOM	266
#define	T_ATOM_DOT	267
#define	T_EQUAL_ATOM	268
#define	T_CADENA_INTERPOLADA	269
#define	REGEXP_START	270
#define	REGEXP_END	271
#define	REGEXP_CLOSURE	272
#define	REGEXP_OR	273
#define	REGEXP_ATOM	274


extern YYSTYPE yylval;
