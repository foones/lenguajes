/* ENV_SIZE deberia ser un primo */
#define ENV_SIZE	3

void fu_init_env();
RES *fu_def_env(RES *clave, RES *valor);
RES *fu_set_env(RES *clave, RES *valor);
RES *fu_get_env(RES *clave);
void fu_pop_env();
void fu_push_env();
RES *fu_env();
RES *fu_dir();
RES *fu_with_env();

extern RES *Current_Env;
