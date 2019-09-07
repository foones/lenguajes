#include "Fu.h"

void agregar_extender(STYPE *st, RES *q) {
	STYPE *st2;

	if (!TIPO_P(tipo_stype, q))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("mkstype -- se extiende un no stype"));

	st2 = VAL_STYPE(q);
	STYPE_SLOTS(st) = fu_cat(fu_make_list("xx",
		STYPE_SLOTS(st), STYPE_SLOTS(st2)));
	STYPE_LONGITUD(st) += STYPE_LONGITUD(st2);
	STYPE_EXTENDS(st) = fu_cons(q, STYPE_EXTENDS(st));
}

RES *
fu_make_stype(slots, options)
	RES *slots, *options;
{
	RES *p, *q;
	STYPE *st;

	p = NEW(RES);
	TIPO(p) = tipo_stype;
	st = NEW(STYPE);
	STYPE_SLOTS(st) = slots;
	STYPE_EXTENDS(st) = NIL;
	STYPE_LONGITUD(st) = VAL_INT(fu_length(slots));

	STYPE_LIST_UNSET(st);
	STYPE_PRINT_FUNCTION(st) = NIL;
	STYPE_GET_FUNCTION(st) = NIL;
	for (q = options; CONS_P(q); q = CDR(q)) {
		RES *a = CAR(q);
		if (a == fu_keyword("list")) {
			STYPE_LIST_SET(st);
		} else if (a == fu_keyword("vec")) {
			STYPE_LIST_UNSET(st);
		} else if (a == fu_keyword("print-function")) {
			q = CDR(q);
			if (!CONS_P(q))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("mkstype -- se esperaba la funcion de representacion"));
			STYPE_PRINT_FUNCTION(st) = CAR(q);
		} else if (a == fu_keyword("get")) {
			q = CDR(q);
			if (!CONS_P(q))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("mkstype -- se esperaba la funcion get"));
			STYPE_GET_FUNCTION(st) = CAR(q);
		} else if (a == fu_keyword("extends")) {
			RES *q1;

			q = CDR(q);
			if (!CONS_P(q))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("mkstype -- se esperaba stype a extender"));

			/* extiende a otro stype */
			q1 = CAR(q);
			if (TIPO_P(tipo_stype, q1)) {
				agregar_extender(st, q1);
			} else if (CONS_P(q1)) {
				RES *p1;

				for (p1 = fu_xrev(q1); CONS_P(p1); p1 = CDR(p1))
					agregar_extender(st, CAR(p1));
			} else {
				fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("mkstype -- se extiende un no stype"));
			}
		}
	}
	VAL(p) = (void *) st;
	return p;
}

RES *
struct_stype_in(stype, stype2)
	RES *stype, *stype2;
{
	RES *p;

	if (stype == stype2)
		return T;
	for (p = STYPE_EXTENDS(VAL_STYPE(stype2)); CONS_P(p); p = CDR(p))
		if (struct_stype_in(stype, CAR(p)) == T)
			return T;
	return NIL;
}

RES *
fu_struct_stypep(stype, stru)
	RES *stype, *stru;
{
	if (TIPO_P(tipo_struct, stru))
		return struct_stype_in(stype, STRUCT_STYPE(VAL_STRUCT(stru)));
	else
		return NIL;
}

RES *
fu_stype_super(stype)
	RES *stype;
{
	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("stype-super -- no es un stype"));

	return STYPE_EXTENDS(VAL_STYPE(stype));
}

RES *
fu_make_struct(stype)
	RES *stype;
{
	RES *p;
	STRU *s;
	STYPE *tipo;

	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("mkstruct -- el tipo dado no es un stype"));

	tipo = VAL_STYPE(stype);

	p = NEW(RES);
	TIPO(p) = tipo_struct;
	s = NEW(STRU);
	STRUCT_STYPE(s) = stype;
	if (STYPE_LISTP(tipo))
		STRUCT_DATA(s) = fu_mklist(fu_int(STYPE_LONGITUD(tipo)), UNDEF);
	else
		STRUCT_DATA(s) = fu_make_vector(fu_int(STYPE_LONGITUD(tipo)), UNDEF);

	VAL(p) = (void *) s;
	return p;
}

RES *
fu_struct_data(stru)
	RES *stru;
{
	if (!TIPO_P(tipo_struct, stru))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("struct-data -- no es una estructura"));
	return STRUCT_DATA(VAL_STRUCT(stru));
}

RES *
fu_struct_copy(stru)
	RES *stru;
{
	RES *p;
	if (!TIPO_P(tipo_struct, stru))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			fu_str("struct-copy -- no es una estructura"));
	p = fu_make_struct(STRUCT_STYPE(VAL_STRUCT(stru)));
	STRUCT_DATA(VAL_STRUCT(p)) = fu_copy_shallow(STRUCT_DATA(VAL_STRUCT(stru)));
	return p;
}

RES *
fu_stype_accessor(stype, slot)
	RES *stype, *slot;
{
	unsigned pos;
	unsigned char read_only = FALSE, tiene_tipo = FALSE;
	RES *p;
	RES *accessor;
	RES *estru = fu_funes_symbol("estructura"), *valor = fu_funes_symbol("valor");
	RES *accessor_fun;
	RES *tipo = NIL;
	RES *wrong_type_arg = fu_make_list("xx", fu_funes_symbol("quote"),
							fu_exception_symbol("wrong-type-arg"));

	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("stype-accessor -- no es un stype"));

	pos = 0;
	for ( p = STYPE_SLOTS(VAL_STYPE(stype)); CONS_P(p); p = CDR(p), pos++) {
		RES *a = CAR(p);
		if (a == slot) break;
		if (CONS_P(a)) {
			if (CAR(a) == slot) {
				RES *q;
				for (q = CDR(a); CONS_P(q); q = CDR(q)) {
					if (CAR(q) == fu_keyword("read-only")) {
						read_only = TRUE;
					} else if (CAR(q) == fu_keyword("type")) {
						q = CDR(q);
						if (!CONS_P(q))
							fu_throw(fu_exception_symbol("wrong-type-arg"),
			 					fu_str("stype-accessor -- falta especificar el tipo"));
						tiene_tipo = TRUE;
						tipo = CAR(q);
					}
				}
				break;
			}
		}
	}
	if (!CONS_P(p))
		fu_throw(fu_exception_symbol("slot-error"),
			 fu_str("stype-accessor -- la estructura no tiene ese slot"));

	/*
	** ;; ACCESSOR::
	** (fun (estructura)
	**   (check ,stype estructura)
	**   (,accessor_fun (struct-data estructura) ,pos))
	**
	** ;; SETTER:: (si no es read-only)
	** (fun (estructura valor)
	**   (check ,stype estructura)
	**   (check tipo valor)
	**   ((setter ,accessor_fun) (struct-data estructura) ,pos valor))
	*/

	if (STYPE_LISTP(VAL_STYPE(stype))) {
		accessor_fun = fu_funes_symbol("nth");
	} else {
		accessor_fun = fu_funes_symbol("vref");
	}

	accessor = fu_eval(
		fu_make_list("xxxx", fu_funes_symbol("fun"),
			fu_cons(estru, NIL),
			fu_make_list("xxxxx", fu_funes_symbol("check"), stype, estru,
					wrong_type_arg,
					fu_str("struct_getter -- se esperaba una estructura")),
			fu_make_list("xxx", accessor_fun,
					fu_make_list("xx", fu_funes_symbol("struct-data"), estru),
					fu_int(pos))));
	if (!read_only) {
		RES *checkea_tipo = NIL;

		if (tiene_tipo)
			checkea_tipo = fu_make_list("xxxxx", fu_funes_symbol("check"),
							tipo, valor, wrong_type_arg,
							fu_str("struct_setter -- el tipo no corresponde al slot"));

		fu_setter_set(accessor, fu_eval(
			fu_make_list("xxxxx", fu_funes_symbol("fun"),
				fu_make_list("xx", estru, valor),
				fu_make_list("xxxxx", fu_funes_symbol("check"), stype, estru,
						wrong_type_arg,
						fu_str("struct_setter -- se esperaba una estructura")),
				checkea_tipo,
				fu_make_list("xxxx",
					fu_make_list("xx", fu_funes_symbol("setter"),
										accessor_fun),
					fu_make_list("xx", fu_funes_symbol("struct-data"), estru),
					fu_int(pos),
					valor))));
	}

	return accessor;
}

RES *
fu_stype_constructor(stype, slot_list)
	RES *stype, *slot_list;
{
	RES *p;
	RES *lista_argumentos = fu_cons(NIL, NIL);
	RES *lista_asignaciones = fu_cons(NIL, NIL);
	RES *ult_argumento = lista_argumentos;
	RES *ult_asignacion = lista_asignaciones;
	RES *set_accessor_fun = fu_funes_symbol("setter");
	RES *estructura = fu_gensym();
	RES *sym_struct_data = fu_gensym();
	RES *struct_data = fu_make_list("xx", fu_funes_symbol("struct-data"), estructura);
	RES *slot;
	RES *ss, *vv;

	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("stype-accessor -- no es un stype"));

	if (STYPE_LISTP(VAL_STYPE(stype))) {
		set_accessor_fun = fu_make_list("xx", fu_funes_symbol("setter"), fu_funes_symbol("nth"));
	} else {
		set_accessor_fun = fu_make_list("xx", fu_funes_symbol("setter"), fu_funes_symbol("vref"));
	}

	for ( slot = slot_list; CONS_P(slot); slot = CDR(slot) ) {
		unsigned pos = 0;
		RES *defa = UNDEF;
		RES *nombre = CAR(slot);

		for ( p = STYPE_SLOTS(VAL_STYPE(stype)); CONS_P(p); p = CDR(p), pos++) {
			RES *a = CAR(p);
			if (a == nombre) break;
			if (CONS_P(a)) {
				if (CAR(a) == nombre) {
					RES *q;
					for (q = CDR(a); CONS_P(q); q = CDR(q)) {
						if (CAR(q) == fu_keyword("default")) {
							q = CDR(q);
							if (!CONS_P(q))
								fu_throw(fu_exception_symbol("wrong-type-arg"),
			 						fu_str("stype-constructor -- falta especificar el valor por defecto"));
						defa = CAR(q);
						}
					}
					break;
				}
			}
		}

		if (!CONS_P(p))
			fu_throw(fu_exception_symbol("slot-error"),
				fu_str("stype-constructor -- la estructura no tiene ese slot"));

		/* (<argset> funes-kwarg::nombre nombre defa) */
		ult_argumento = CDR(ult_argumento) = fu_cons(
						fu_make_list("xxxx", ARGSET,
								fu_keyword_argument(VAL_SYMNAME(nombre)),
								nombre,
								defa), NIL);
		/* (,set_accessor_fun) #G1 pos nombre) */
		ult_asignacion = CDR(ult_asignacion) = fu_cons(
						fu_make_list("xxxx",
								set_accessor_fun,
								sym_struct_data,
								fu_int(pos),
								nombre),
						NIL);
	}

	ss = fu_gensym();
	vv = fu_gensym();
	/*
	** CONSTRUCTOR::
	** (fun ,argumentos
	**  (def #G0 (mkstruct tipo))
	**  (def #G1 (struct-data #G0))
	**  (do ,@asignaciones)
	**  estructura)
	*/
	CAR(lista_asignaciones) = fu_funes_symbol("do");
	return fu_eval(
		fu_make_list("xxxxxxx", fu_funes_symbol("fun"),
				CDR(lista_argumentos),
				fu_make_list("xxx", fu_funes_symbol("def"), estructura,
						fu_make_list("xx", fu_funes_symbol("mkstruct"), stype)),
				fu_make_list("xxx", fu_funes_symbol("def"), sym_struct_data,
						struct_data),

				/* establezco el setter de la estructura:
				 *
				 * (set (setter X) (fun (S V) (slot-value-set X S V)))
				 */
				fu_make_list("xxx", fu_funes_symbol("set"),
						fu_make_list("xx", fu_funes_symbol("setter"), estructura),
						fu_make_list("xxx", fu_funes_symbol("fun"),
								fu_make_list("xx", ss, vv),
								fu_make_list("xxxx",
										fu_funes_symbol("slot-value-set"),
										estructura, ss, vv))),

				lista_asignaciones,
				estructura));
}

RES *
fu_struct_type(stru)
	RES *stru;
{
	if (!TIPO_P(tipo_struct, stru))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("struct-type -- no es una estructura"));

	return STRUCT_STYPE(VAL_STRUCT(stru));
}

RES *
fu_stype_slots(stype)
	RES *stype;
{
	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("stype-slots -- no es un stype"));

	return STYPE_SLOTS(VAL_STYPE(stype));
}

RES *
fu_slot_value(stru, slot)
	RES *stru, *slot;
{
	if (!TIPO_P(tipo_struct, stru))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("slot-value -- no es una estructura"));

	unsigned pos = 0;
	RES *p;
	RES *stype = STRUCT_STYPE(VAL_STRUCT(stru));

	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("slot-value -- el tipo de la estructura no es un stype"));


	for (p = STYPE_SLOTS(VAL_STYPE(stype)); CONS_P(p); p = CDR(p), ++pos)
		if (CAR(p) == slot || (CONS_P(CAR(p)) && CAR(CAR(p)) == slot)) break;

	if (!CONS_P(p))
		fu_throw(fu_exception_symbol("slot-error"),
			 fu_mkstr(fu_make_list("xxx",
				 fu_str("slot-value -- la estructura no tiene ese slot: `"),
				 slot,
				 fu_str("'"))));

	return fu_elt(fu_int(pos), STRUCT_DATA(VAL_STRUCT(stru)));
}

RES *
fu_slot_value_set(stru, slot, value)
	RES *stru, *slot, *value;
{
	if (!TIPO_P(tipo_struct, stru))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("slot-value -- no es una estructura"));

	unsigned pos = 0;
	RES *p;
	RES *stype = STRUCT_STYPE(VAL_STRUCT(stru));

	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("slot-value -- el tipo de la estructura no es un stype"));


	for (p = STYPE_SLOTS(VAL_STYPE(stype)); CONS_P(p); p = CDR(p), ++pos)
		if (CAR(p) == slot || (CONS_P(CAR(p)) && CAR(CAR(p)) == slot)) break;

	if (!CONS_P(p))
		fu_throw(fu_exception_symbol("slot-error"),
			 fu_str("slot-value-set -- la estructura no tiene ese slot"));

	if (CONS_P(CAR(p)) && fu_member(fu_keyword("read-only"), CAR(p), UNDEF))
		fu_throw(fu_exception_symbol("slot-error"),
			 fu_str("slot-value-set -- el slot es de solo lectura"));

	return fu_elt_set(fu_int(pos), STRUCT_DATA(VAL_STRUCT(stru)), value);
}

RES *
fu_make(stype, slot_values)
	RES *stype, *slot_values;
{
	RES *p;
	RES *slots_desnudos = NIL;

	if (!TIPO_P(tipo_stype, stype))
		fu_throw(fu_exception_symbol("wrong-type-arg"),
			 fu_str("make -- no es un stype"));

	for (p = STYPE_SLOTS(VAL_STYPE(stype)); CONS_P(p); p = CDR(p)) {
			if (CONS_P(CAR(p)))
				slots_desnudos = fu_cons(CAR(CAR(p)), slots_desnudos);
			else
				slots_desnudos = fu_cons(CAR(p), slots_desnudos);
	}
	slots_desnudos = fu_xrev(slots_desnudos);
	return fu_apply(fu_stype_constructor(stype, slots_desnudos),
		slot_values);
}
