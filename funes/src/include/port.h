extern RES *Read_Sexpr;
extern RES *Std_Input;
extern RES *Std_Output;
extern RES *Std_Error;

/* PROTOS */
void fu_init_reader();

RES *fu_set_reader_macro(RES *name, RES *macro);
RES *fu_set_reader_funapply(RES *val);

RES *fu_port(FILE *archivo);
RES *fu_reader(RES *puerto_entrada, unsigned char start);
RES *fu_read(RES *port);
RES *fu_stread(RES *cadena);
void fu_init_port();
RES *fu_open(RES *name, RES *forma);
RES *fu_close(RES *port);
RES *fu_exists(RES *name);

/* MACROS */
#define VAL_PORT(RES)	((FILE *) VAL(RES))

