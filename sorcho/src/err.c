#include "Sor.h"

void fail(char *errmsg, Objeto *expresion)
{
	fprintf(stderr, "Error:\n");
	fprintf(stderr, " %s\n", errmsg);
	fprintf(stderr, " Cerca de: ");
	show_objeto(stderr, expresion);
	fprintf(stderr, "\n");
	longjmp(Err_Salto, 1);
}
