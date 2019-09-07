/* PROTOS */
RES *fu_str(const char *s);
RES *fu_str_cat(RES *s1, RES *s2);
RES *fu_mkstr(RES *v);
RES *fu_fmt(RES *fmt_str, RES *args);
RES *fu_num_to_str(RES *n, RES *base, RES *plus, RES *space,
				RES *special, RES *large);
RES *fu_input();

/* MACROS */
#define VAL_STR(R)	((char *) VECTOR_TABLA(VAL_VECTOR(R)))
#define STR_P(R)	(TIPO_P(tipo_vector, R) && (VECTOR_TIPO(VAL_VECTOR(R)) == vector_uniforme_char))

#define MAX_STR_BUF			64
#define MAX_READ_BUFFER		256
