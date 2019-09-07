typedef struct _cons CONS;
struct _cons {
	struct _res *car;
	struct _res *cdr;
};

RES *fu_cons(RES *car, RES *cdr);
RES *fu_acons(RES *a, RES *b, RES *c);
RES *fu_consp(RES *v);
RES *fu_car(RES *v);
RES *fu_cdr(RES *v);
RES *fu_car_set(RES *v, RES *w);
RES *fu_cdr_set(RES *v, RES *w);
RES *fu_list_length(RES *l);
RES *fu_list_to_vector(RES *l);
RES *fu_make_list(char *formato, ...);
RES *fu_mklist(RES *longitud, RES *inicial);
RES *fu_list(RES *args);
RES *fu_list_1(RES *args);
RES *fu_nth(RES *v, RES *idx);
RES *fu_nth_set(RES *v, RES *idx, RES *valor);
RES *fu_ntl(RES *v, RES *idx);
RES *fu_ntl_set(RES *v, RES *idx, RES *valor);

#define VAL_CONS(R)	((CONS *) ((fuint) (R) & ~3))
#define CAR(R)		(VAL_CONS(R)->car)
#define CDR(R)		(VAL_CONS(R)->cdr)

#define CAAR(R)		CAR(CAR(R))
#define CADR(R)		CAR(CDR(R))
#define CDAR(R)		CDR(CAR(R))
#define CDDR(R)		CDR(CDR(R))

#define CAAAR(R)	CAR(CAR(CAR(R)))
#define CAADR(R)	CAR(CAR(CDR(R)))
#define CADAR(R)	CAR(CDR(CAR(R)))
#define CADDR(R)	CAR(CDR(CDR(R)))
#define CDAAR(R)	CDR(CAR(CAR(R)))
#define CDADR(R)	CDR(CAR(CDR(R)))
#define CDDAR(R)	CDR(CDR(CAR(R)))
#define CDDDR(R)	CDR(CDR(CDR(R)))
