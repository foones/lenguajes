RES *fu_compile(RES *expr, RES *arglist, char push_value, char new_env);

RES *fu_fenv(RES *closure);
RES *fu_fenv_set(RES *closure, RES *env);

#define FU_COMPILE(EXPR) fu_compile(EXPR, NIL, 0, 0)
