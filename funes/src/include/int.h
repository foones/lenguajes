/* PROTOS */
RES *fu_int(fuint i);
RES *fu_int_sumar(RES *resto);
RES *fu_int_restar(RES *resto);
RES *fu_int_multiplicar(RES *resto);
RES *fu_int_mod(RES *v, RES *w);
RES *fu_int_remainder(RES *v, RES *w);
RES *fu_int_quotient(RES *v, RES *w);
RES *fu_int_menor(RES *resto);
RES *fu_int_menor_o_igual(RES *resto);
RES *fu_int_mayor(RES *resto);
RES *fu_int_mayor_o_igual(RES *resto);
RES *fu_int_igual(RES *resto);
RES *fu_int_elevar(RES *a, RES *b);

/* MACROS */
#define VAL_INT(R)	((fuint) (R) >> 2)
