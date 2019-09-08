#include "Sor.h"

Lista *mk_par(Objeto *elt, Lista *sig)
{
	Lista *l = NEW(Lista);

	l->elemento = elt;
	l->siguiente = sig;
	return l;
}

Objeto *mk_objeto_algebraico(Objeto *cons, Lista *elts)
{
	Objeto *o = NEW(Objeto);
	Algebraico *a = NEW(Algebraico);
	
	o->tipo = tipo_algebraico;
	VAL_ALGEBRAICO(o) = a;
	a->constructor = cons;
	a->elementos = elts;
	return o;
}

Objeto *mk_int(int i)
{
	Objeto *o = NEW(Objeto);
	
	o->tipo = tipo_entero;
	VAL_INT(o) = i;
	return o;
}

Objeto *mk_char(char c)
{
	Objeto *o = NEW(Objeto);
	
	o->tipo = tipo_char;
	VAL_CHAR(o) = c;
	return o;
}

Objeto *mk_aplicacion(Objeto *ador, Objeto *ando)
{
 	Objeto *o = NEW(Objeto);
	Aplicacion *a = NEW(Aplicacion);

	o->tipo = tipo_aplicacion;
	VAL_APLICACION(o) = a;
	a->functor = ador;
	a->arg = ando;
	return o;
}

Objeto *mk_construccion(Objeto *ador, Objeto *elt)
{
	Objeto *o = NEW(Objeto);
	Construccion *c = NEW(Construccion);
	
	o->tipo = tipo_construccion;
	VAL_CONSTRUCCION(o) = c;
	c->constructor = ador;
	c->ultimo = c->elementos = mk_par(elt, 0);
	c->longitud = 1;
	return o;
}

void construccion_push(Objeto *construccion, Objeto *elt)
{
	Construccion *c = VAL_CONSTRUCCION(construccion);

	c->ultimo = c->ultimo->siguiente = mk_par(elt, 0);
	c->longitud++;
	if (c->longitud > VAL_CONSTRUCTOR(c->constructor)->aridad)
		fail("Demasiados argumentos para el constructor.",
				c->constructor);
}

Objeto *mk_simbolo(char *nombre)
{
	Objeto *o = NEW(Objeto);

	o->tipo = tipo_simbolo;
	VAL_NOMBRE(o) = nombre;
	return o;
}

Objeto *mk_constructor(char *nom, unsigned char ari)
{
	Objeto *o = NEW(Objeto);
	Constructor *c = NEW(Constructor);

	o->tipo = tipo_constructor;
	VAL_CONSTRUCTOR(o) = c;
	c->nombre = nom;
	c->aridad = ari;
	return o;
}

Objeto *mk_nada()
{
	Objeto *nada = NEW(Objeto);

	nada->tipo = tipo_nada;
	return nada;
}

Objeto *mk_funcion(Entorno *entorno, Objeto *argumento, Objeto *cuerpo)
{
	Objeto *o = NEW(Objeto);
	Funcion *f = NEW(Funcion);

	o->tipo = tipo_funcion;
	VAL_FUNCION(o) = f;
	f->entorno = entorno;
	f->arg = argumento;
	f->cuerpo = cuerpo;
	return o;
}

Objeto *mk_cerradura(Entorno *entorno, Objeto *expresion)
{
	Objeto *o = NEW(Objeto);
	Cerradura *c = NEW(Cerradura);

	o->tipo = tipo_cerradura;
	VAL_CERRADURA(o) = c;
	c->entorno = entorno;
	c->expresion = expresion;
	return o;
}

Objeto *mk_dondura(Hash *hash, Objeto *expresion)
{
	Objeto *o = NEW(Objeto);
	Dondura *d = NEW(Dondura);

	o->tipo = tipo_dondura;
	VAL_DONDURA(o) = d;
	d->hash = hash;
	d->expresion = expresion;
	return o;
}

Objeto *mk_generica(Lista *l)
{
	Objeto *o = NEW(Objeto);

	o->tipo = tipo_generica;
	VAL_GENERICA(o) = l;
	return o;
}

Objeto *mk_objeto_entorno(Entorno *e)
{
	Objeto *o = NEW(Objeto);

	o->tipo = tipo_entorno;
	VAL_ENTORNO(o) = e;
	return o;
}

/* Dadas una lista de argumentos y una expresion
 * construye la funcion currificada asociada */
Objeto *mk_curry(Entorno *e, Lista *argumentos, Objeto *expresion)
{
	if (argumentos) {
		return
			mk_funcion(e, argumentos->elemento,
				mk_curry(e, argumentos->siguiente, expresion));
	} else
		return expresion;
}

void logo()
{
printf(
"                                                                            \n"
"____________________________________________________________________________\n"
"/////////////////// //I|                                                    \n"
"///////////////// ///|I|      The Sorcho Kang Programming Language          \n"
"///////////////_///| |I|                                                    \n"
"/////////////======| |I|      Pablo Barenbaum (C) 2004                      \n"
"|||||||||||||_____ |_|/|                                                    \n"
"=======================|____________________________________________________\n"
);
}

Hash *Actual;
Entorno *Pila_Actuales;
Objeto *Read_Sexpr;
jmp_buf Err_Salto;
char Charlatan;
char Koan;

int main(int argc, char **argv)
{
	Actual = mk_hash(HASH_PRIMO);
	Pila_Actuales = mk_entorno(Actual, 0);
	Charlatan = 0;

	leer_de_archivo(stdin);
	if (setjmp(Err_Salto)) {
		leer_de_archivo(stdin);
		goto interactivo;
	}

	if (argc == 1)
		logo();

	cargar_archivo("raindrop.sr", 1);

	if (argc >= 2) {
		/* lee de archivo */
		unsigned i;
		for (i = 1; i < argc; i++)
			cargar_archivo(argv[i], 1);
	}

interactivo:
	while (1) {
		printf("\nSorcho Kang> ");
		yyparse();
		show_objeto(stdout, reducir(mk_entorno(Actual, 0), Read_Sexpr));
	}
}
