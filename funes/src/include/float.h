/* PROTOS */
RES *fu_float(double f);
RES *fu_float_sumar(RES *resto);
RES *fu_float_restar(RES *resto);
RES *fu_float_multiplicar(RES *resto);
RES *fu_float_dividir(RES *resto);
RES *fu_float_menor(RES *resto);
RES *fu_float_mayor(RES *resto);
RES *fu_float_elevar(RES *a, RES *b);
RES *fu_float_iguales(RES *resto);

RES *fu_float_floor(RES *a);
RES *fu_float_ceil(RES *a);
RES *fu_int_to_float(RES *a);

/* MACROS */
#define VAL_FLOAT(R)	(*((double *) VAL(R)))
#define FLOAT_P(R)	(TIPO_P(tipo_float, R))

