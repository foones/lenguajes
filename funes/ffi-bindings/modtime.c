#include "Fu.h"
#include <string.h>
#include "modtime.h"

time_t *get_time() {
		time_t *i = NEW(time_t);
		time(i);
		return i;
}

char *fmtime(char *format, struct tm *t) {
		char cadena[512];
		char *copia;
		int l;

		l = strftime(cadena, 512, format, t);
		if (l == 0)
			fu_throw(fu_exception_symbol("time-too-long"),
				fu_str("fmtime -- resultado demasiado largo"));
		cadena[l] = '\0';
		copia = NEWQ(char, l + 1);
		strncpy(copia, cadena, l);
		copia[l] = '\0';
		return copia;
}

