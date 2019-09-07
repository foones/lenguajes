#include "Fu.h"

extern FILE *yyin;

RES *Read_Sexpr;
RES *Std_Input;
RES *Std_Output;
RES *Std_Error;

void
fu_init_port()
{
	Std_Input = fu_port(stdin);
	Std_Output = fu_port(stdout);
	Std_Error = fu_port(stderr);
}

RES *
fu_port(arch)
	FILE *arch;
{
	RES *p;
	p = NEW(RES);
	TIPO(p) = tipo_port;
	VAL(p) = (void *) arch;
	return p;
}

RES *
fu_open(name, forma)
	RES *name, *forma;
{
	if (!STR_P(name)) {
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("open -- no es un nombre de archivo"));
	}
	if (forma == UNDEF) forma = fu_str("r");
	if (!STR_P(forma)) {
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("open -- no es una forma de apertura"));
	}
	FILE *f = fopen(VAL_STR(name), VAL_STR(forma));
	if (!f)
		fu_throw(fu_exception_symbol("system-error"),
			 fu_str("open -- no se puede abrir el archivo"));
	return fu_port(f);
}

RES *
fu_close(port)
		RES *port;
{
	if (!TIPO_P(tipo_port, port))
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("close -- no es un puerto"));
	if (fclose(VAL_PORT(port)) != 0)
		fu_throw(fu_exception_symbol("system-error"),
			 fu_str("close -- no se puede cerrar el puerto"));
	return T;
}

RES *
fu_exists(name)
	RES *name;
{
	FILE *f;

	if (!STR_P(name)) {
		fu_throw(fu_exception_symbol("wrong-type"),
			 fu_str("exists -- no es un nombre de archivo"));
	}
	if ((f = fopen(VAL_STR(name), "r"))) {
		fclose(f);
		return T;
	}
	return NIL;
}

