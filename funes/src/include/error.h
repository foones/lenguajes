extern jmp_buf *Fu_Err_Salto;
extern RES *Fu_Err_Excepcion;
extern RES *Fu_Err_Descripcion;

void fu_init_err();
RES *fu_throw(RES *excepcion, RES *descripcion);
RES *fu_handle(RES *excepcion, RES *thunk1, RES *thunk2);

#define RETHROW() {\
	Fu_Err_Salto = _prev_err_salto;\
	fu_with_env(_env_ant);\
	fu_throw(Fu_Err_Excepcion, Fu_Err_Descripcion);\
}

#define TRY(CODIGO, HANDLER) { \
	RES *_env_ant;\
	jmp_buf *_prev_err_salto;\
	_prev_err_salto = Fu_Err_Salto;\
	_env_ant = fu_env();\
	Fu_Err_Salto = NEW(jmp_buf);\
	if ( setjmp(*Fu_Err_Salto) ) {\
		HANDLER;\
		Fu_Err_Salto = _prev_err_salto;\
		fu_with_env(_env_ant);\
		goto _fin_try;\
	}\
	CODIGO;\
	Fu_Err_Salto = _prev_err_salto;\
	fu_with_env(_env_ant);\
_fin_try:\
}
