#include "Fu.h"

RES *Setter_Table;

RES *
fu_setter(accessor)
	RES *accessor;
{
	RES *setter;

	/* primero veo si es una secuencia, hash, etc. */
	if (CONS_P(accessor) || TIPO_P(tipo_vector, accessor)) {
		/*
		 * (fun (i v)
		 *   (set (elt i <accessor>) v))
		 */
		return fu_eval(
				fu_make_list("xxx",
						fu_funes_symbol("fun"),
						fu_make_list("xx", fu_funes_symbol("i"), fu_funes_symbol("v")),
						fu_make_list("xxx",
								fu_funes_symbol("set"),
								fu_make_list("xxx",
									fu_funes_symbol("elt"),
									fu_funes_symbol("i"),
									fu_make_list("xx",
											fu_funes_symbol("quote"),
											accessor)),
								fu_funes_symbol("v"))));
	}

	setter = fu_get_hash_eq(Setter_Table, accessor);

	if (setter == NIL)
		fu_throw(fu_exception_symbol("wrong-type"),
			fu_str("setter -- el accessor no tiene asociado un setter"));
	else
		return CDR(setter);

	return NIL;
}

__inline__ RES *
fu_setter_set(accessor, setter)
		RES *accessor, *setter;
{
	RES *r = fu_set_hash_eq(Setter_Table, accessor, setter);
	unsigned s = HASH_SIZE(Setter_Table);
	unsigned l = HASH_LEN(Setter_Table);

	if (s / l > 1)
		fu_rehash_eq(Setter_Table, fu_int(2 * l + 1));

	return r;
}

void
fu_init_setter()
{
	Setter_Table = fu_make_hash(fu_int(SETTERTABLE_SIZE));
}
