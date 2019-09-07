/* STYPE */
typedef struct _stype STYPE;
struct _stype {
	RES *slots;
	RES *print_function;
	RES *get_function;
	RES *extends;
	unsigned char flags;
	unsigned longitud;
};

#define VAL_STYPE(RES)	((STYPE *) VAL(RES))
#define STYPE_SLOTS(P)	((P)->slots)
#define STYPE_LONGITUD(P)	((P)->longitud)
#define STYPE_PRINT_FUNCTION(P)	((P)->print_function)
#define STYPE_GET_FUNCTION(P)	((P)->get_function)
#define STYPE_EXTENDS(P)	((P)->extends)

#define STYPE_LISTP(P)		((P)->flags & 1)
#define STYPE_LIST_SET(P)	((P)->flags |= 1)
#define STYPE_LIST_UNSET(P)	((P)->flags &= ~1)

/* STRUCT */
typedef struct _stru STRU;
struct _stru {
	RES *stype;
	RES *data;
};

#define VAL_STRUCT(RES)	((STRU *) VAL(RES))
#define STRUCT_STYPE(P) ((P)->stype)
#define STRUCT_DATA(P) ((P)->data)

/* PROTOS */
RES *fu_make_stype(RES *slots, RES *options);
RES *fu_struct_stypep(RES *stype, RES *stru);
RES *fu_make_struct(RES *stype);
RES *fu_struct_data(RES *stru);
RES *fu_struct_copy(RES *stru);
RES *fu_stype_accessor(RES *stype, RES *slot);
RES *fu_stype_constructor(RES *stype, RES *slot_list);
RES *fu_struct_type(RES *stru);
RES *fu_stype_slots(RES *stype);
RES *fu_stype_super(RES *stype);
RES *fu_slot_value(RES *stru, RES *slot);
RES *fu_slot_value_set(RES *stru, RES *slot, RES *value);
RES *fu_make(RES *stype, RES *slot_values);
