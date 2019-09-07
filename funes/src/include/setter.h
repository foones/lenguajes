/* SETTERTABLE_SIZE deberia ser un primo */
#define SETTERTABLE_SIZE	3

void fu_init_setter();
RES *fu_setter(RES *accessor);
RES *fu_setter_set(RES *accessor, RES *setter);
