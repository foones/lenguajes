RES *fu_compile_env();
RES *fu_set_compile_env(RES *e);
RES *fu_compile_dir();
RES *fu_compile_value(RES *name);
RES *fu_set_compile_value(RES *name, RES *value);

extern RES *special_form_if;
extern RES *special_form_fun;
extern RES *special_form_quote;
extern RES *special_form_do;
extern RES *special_form_def;
extern RES *special_form_set;
extern RES *special_form_dyn;
extern RES *special_form_while;
extern RES *special_form_or;
extern RES* special_form_dir;
extern RES* special_form_thunk;
extern RES* special_form_call;
extern RES* special_form_set_cc;
extern RES* special_form_call_cc;
extern RES* special_form_spawn;
extern RES* special_form_sem_init;
extern RES* special_form_sem_p;
extern RES* special_form_sem_v;
extern RES* special_form_sem_val;
extern RES* special_form_sem_set;

/* para procedimientos inlined */
extern RES *special_form_not;
extern RES *special_form_eq;
extern RES *special_form_car;
extern RES *special_form_cdr;

void fu_init_special();

