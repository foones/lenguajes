#include "Fu.h"

RES *Fu_Err_Excepcion;
RES *Fu_Err_Descripcion;
jmp_buf *Fu_Err_Salto;

void
fu_init_err()
{
		Fu_Err_Excepcion = NIL;
		Fu_Err_Descripcion = fu_str("");
}

RES *
fu_throw(excepcion, descripcion)
		RES *excepcion;
		RES *descripcion;
{
		Fu_Err_Excepcion = excepcion;
		Fu_Err_Descripcion = descripcion;
		longjmp(*Fu_Err_Salto, 1);
		return NIL;
}

RES *
fu_handle(excepcion, thunk1, thunk2)
	RES *excepcion, *thunk1, *thunk2;
{
	RES *resultado, *env_ant;
	jmp_buf *prev_err_salto;

	prev_err_salto = Fu_Err_Salto;
	env_ant = fu_env();
	Fu_Err_Salto = NEW(jmp_buf);
	if ( setjmp(*Fu_Err_Salto) ) {
		if (excepcion == T ||
				Fu_Err_Excepcion == excepcion ||
				(CONS_P(excepcion)?
				 fu_member(Fu_Err_Excepcion, excepcion, UNDEF) != NIL: 0)) {
			Fu_Err_Salto = prev_err_salto;
			resultado = fu_apply(thunk1,
					fu_make_list("xx", Fu_Err_Excepcion, Fu_Err_Descripcion));
			fu_with_env(env_ant);
			return resultado;
		} else {
			Fu_Err_Salto = prev_err_salto;
			fu_throw(Fu_Err_Excepcion, Fu_Err_Descripcion);
		}
	}
	resultado = fu_apply(thunk2, NIL);
	Fu_Err_Salto = prev_err_salto;
	fu_with_env(env_ant);
	return resultado;
}
