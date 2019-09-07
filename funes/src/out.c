#include "Fu.h"

/* identidad con efecto secundario de imprimir */
RES *
fu_fprint(port, v, style)
	RES *port, *v, *style;
{
	FILE *f = NULL;
	RES *p;

	if (TIPO_P(tipo_port, port))
		f = VAL_PORT(port);
	else if ( port == T )
		f = VAL_PORT(Std_Output);
	else
		fu_throw(fu_exception_symbol("wrong-type-arg"),
					fu_str("out -- no es un port"));

	if (style == UNDEF) {
		style = T;
	}

	if (NULL_P(v)) {
		fprintf(f, "()");
	} else if ( v == T ) {
		fprintf(f, "#t");
	} else if ( v == EOF_OBJECT ) {
		fprintf(f, "#<eof>");
	} else if ( v == UNDEF ) {
		fprintf(f, "#u");
	} else if ( v == ARGSET ) {
		fprintf(f, "#argset");
	} else if (INT_P(v)) {
		fprintf(f, "%lli", VAL_INT(v));
	} else if (CHAR_P(v)) {
		unsigned char c = (unsigned char)(VAL_CHAR(v) & 0xff);
		if ( c == '\n')
			fprintf(f, "\\nl");
		else if ( c == '\r')
			fprintf(f, "\\cr");
		else 
			fprintf(f, "\\%c", c);
	} else if (CONS_P(v)) {
		fprintf(f, "(");
		p = v;
		while (1) {
			fu_fprint(port, CAR(p), style);
			p = CDR(p);
			if (!CONS_P(p))
				break;
			fprintf(f, " ");
		}
		if ( p != NIL ) {
			fprintf(f, " . ");
			fu_fprint(port, p, style);
		}
		fprintf(f, ")");
	} else if (NINMEDIATO_P(v)) {
		switch ( TIPO(v) ) {
			case tipo_sym:
				if (fu_keyword_p(v) != NIL)
					fprintf(f, "=%s", VAL_SYMNAME(v));
				else if (VAL_SYMPACK(v) == NIL)
					fprintf(f, ":%s", VAL_SYMNAME(v));
				else if (VAL_SYMPACK(v) == fu_current_package())
					fprintf(f, "%s", VAL_SYMNAME(v));
				else
					fprintf(f, "%s::%s", VAL_PACKNAME(VAL_SYMPACK(v)),
										VAL_SYMNAME(v));
				break;
			case tipo_vector:
				{
					int i;
					VECTOR *w = VAL_VECTOR(v);

					switch (VECTOR_TIPO(w)) {
					case vector_hash: {
						RES *p;
						char primero = 1;

						fprintf(f, "{");
						for ( i = 0; i < VECTOR_LENGTH(w); i++) {
								
								for (p = VECTOR_ELEMENTO(w, i); CONS_P(p);
												p = CDR(p)) {
										if (!CONS_P(CAR(p)))
											fu_throw(fu_exception_symbol("wrong-type-arg"),
												fu_str("out -- hash mal formado"));

										if (primero) primero = 0;
										else fprintf(f, ", ");

										fu_fprint(port, CAR(CAR(p)), style);
										fprintf(f, ": ");
										fu_fprint(port, CDR(CAR(p)), style);
								}
						}
						fprintf(f, "}");
						break;
					}
					case vector_no_uniforme:
						fprintf(f, "#(");
						for ( i = 0; i < VECTOR_LENGTH(w); i++) {
								if ( i != 0 )
										fprintf(f, " ");
								fu_fprint(port, VECTOR_ELEMENTO(w, i), style);
						}
						fprintf(f, ")");
						break;
					case vector_uniforme_char:
						if (style == T)
							fprintf(f, "%s", VAL_STR(v));
						else
							fprintf(f, "\"%s\"", VAL_STR(v));
						break;
					}
					break;
				}
			case tipo_proc:
				if (PROC_MACRO(VAL_PROC(v))) {
					fprintf(f, "#<macro-proc %lx>", (long) v);
				} else {
					fprintf(f, "#<proc %lx>", (long) v);
				}
				break;
			case tipo_special_form:
				fprintf(f, "#<forma-especial %lx>", (long) v);
				break;
			case tipo_closure:
				/*fu_closure_print(v);*/
				if (CLOSURE_MACRO(VAL_CLOSURE(v))) {
					fprintf(f, "#<macro-clausura ");
				} else if (CLOSURE_DYN(VAL_CLOSURE(v))) {
					fprintf(f, "#<clausura-dinamica ");
				} else {
					fprintf(f, "#<clausura ");
				}
				fu_fprint(port, CLOSURE_ARGLIST(VAL_CLOSURE(v)), style);
				fprintf(f, ">");
				break;
			case tipo_port:
				fprintf(f, "#<puerto %lx>", (long) v);
				break;
			case tipo_tipo: {
				TIPO *t = VAL_TIPO(v);
				if (TIPO_BUILTIN_P(t)) {
					fprintf(f, "%s", TIPO_NOMBRE(t));
				} else {
					fu_fprint(port, TIPO_DES(t), style);
				}
				break;
			}
			case tipo_stype:
				fprintf(f, "#<stype %lx %s>", (long) v,
								STYPE_LISTP(VAL_STYPE(v)) ? "list" : "vec"
								);
				break;
			case tipo_struct: {
				RES *t = STRUCT_STYPE(VAL_STRUCT(v));
				if (TIPO_P(tipo_stype, t)) {
					RES *pf;
					pf = STYPE_PRINT_FUNCTION(VAL_STYPE(t));
					if (pf != NIL) {
						fu_fprint(port, fu_apply(pf, fu_cons(v, NIL)), T);
						break;
					}
				}
				fprintf(f, "#S(");
				fu_fprint(port, STRUCT_STYPE(VAL_STRUCT(v)), style);
				fprintf(f, " ");
				fu_fprint(port, STRUCT_DATA(VAL_STRUCT(v)), style);
				fprintf(f, ")");
				break;
			}
			case tipo_float:
				fprintf(f, "%e", VAL_FLOAT(v));
				break;
			case tipo_cont:
				fprintf(f, "#<cont %lx>", (long) v);
				break;
			case tipo_package:
				fprintf(f, "#<package %s>", VAL_PACKNAME(v));
				break;
		}
	}
	return v;
}

__inline__ RES *
fu_print(v)
		RES *v;
{
		return fu_fprint(Std_Output, v, NIL);
}

RES *
fu_pr(v, port)
		RES *v, *port;
{
		RES *val;

		if ( port == UNDEF ) port = Std_Output;
		if ( !TIPO_P(tipo_port, port) ) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("pr -- no es un port"));
		}
		val = fu_fprint(port, v, T);
		fprintf(VAL_PORT(port), "\n");
		return val;
}

RES *
fu_pr1(v, port)
		RES *v, *port;
{
		RES *val;

		if ( port == UNDEF ) port = Std_Output;
		val = fu_fprint(port, v, T);
		return val;
}

RES *
fu_wr(v, port)
		RES *v, *port;
{
		RES *val;

		if ( port == UNDEF ) port = Std_Output;
		if ( !TIPO_P(tipo_port, port) ) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("wr -- no es un port"));
		}
		val = fu_fprint(port, v, NIL);
		fprintf(VAL_PORT(port), "\n");
		return val;
}

RES *
fu_wr1(v, port)
		RES *v, *port;
{
		RES *val;

		if ( port == UNDEF ) port = Std_Output;
		val = fu_fprint(port, v, NIL);
		return val;
}

RES *
fu_lf(port)
		RES *port;
{
		if ( port == UNDEF ) port = Std_Output;
		if ( !TIPO_P(tipo_port, port) ) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("lf -- no es un port"));
		}
		fprintf(VAL_PORT(port), "\n");
		return NIL;
}

RES *
fu_cr(port)
		RES *port;
{
		if ( port == UNDEF ) port = Std_Output;
		if ( !TIPO_P(tipo_port, port) ) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("cr -- no es un port"));
		}
		fprintf(VAL_PORT(port), "\r");
		return NIL;
}

RES *
fu_crlf(port)
		RES *port;
{
		if ( port == UNDEF ) port = Std_Output;
		if ( !TIPO_P(tipo_port, port) ) {
			fu_throw(fu_exception_symbol("wrong-type-arg"),
				fu_str("crlf -- no es un port"));
		}
		fprintf(VAL_PORT(port), "\r\n");
		return NIL;
}
