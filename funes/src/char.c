#include "Fu.h"

/* Los char terminan en 0111 */

__inline__ RES *
fu_char(i)
	fuint i;
{
	return (RES *) ((i << 4) | 7);
}

RES *
fu_char_int(v)
	RES *v;
{
	return fu_int(VAL_CHAR(v));
}

RES *
fu_int_char(v)
	RES *v;
{
	fuint i = VAL_INT(v);

	if (i > 255) {
		fu_throw(fu_exception_symbol("out-of-range"),
					fu_str("chr -- entero fuera del rango char"));
	}
	return fu_char(VAL_INT(v));
}
