/* PROTOS */
RES *fu_char(fuint i);
RES *fu_char_int(RES *v);
RES *fu_int_char(RES *v);

/* MACROS */
#define VAL_CHAR(R)	((fuint) (R) >> 4)
