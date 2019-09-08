#include "Sor.h"

/*Objeto *reducir_generica(Entorno *e, Objeto *g);*/
Objeto *reducir_algebraico(Entorno *e, Objeto *c);
Entorno *aparear_listas(Entorno *entorno, Entorno *ref, Lista *cargs, Lista *args);
Entorno *aparear(Entorno *entorno, Entorno *ref, Objeto *formal, Objeto *real);
Objeto *cerrar_algebraico(Entorno *e, Objeto *algebraico);
Objeto *hacer_aplicable(Entorno *e, Objeto *f);
Objeto *aplicar2(Entorno *entorno, Objeto *functor, Objeto *arg);
Objeto *aplicar_funcion(Entorno *entorno, Objeto *f, Objeto *arg);

Objeto *reducir(Entorno *entorno, Objeto *expresion)
{
	Objeto *e = expresion;

	while (1) {
	if (Charlatan) {
		show_objeto(stdout, e);
		printf("\n");
	}

	switch (e->tipo)
	{
	case tipo_aplicacion:
	case tipo_simbolo:
	case tipo_constructor:
	case tipo_construccion:
		e = evaluar(entorno, e);
		break;
	case tipo_generica: {
		Objeto *nueva;

		nueva = evaluar(entorno, e);
		if (nueva == e) {
			return e;
		} else {
			e = nueva;
			break;
		}
	}
	case tipo_cerradura: {
		Objeto *nueva;

		nueva = evaluar(entorno, e);
		if (nueva == e) {
			e = VAL_CERRADURA(e)->expresion;
			break;
		} else {
			e = nueva;
			break;
		}
	}
	case tipo_dondura: {
		Objeto *nueva;

		nueva = evaluar(entorno, e);
		if (nueva == e) {
			e = VAL_DONDURA(e)->expresion;
			break;
		} else {
			e = nueva;
			break;
		}
	}
#if 0
		if (VAL_GENERICA(e)->siguiente)
			return reducir_generica(entorno, e);
		else {
			/* Hay un solo elemento */
			e = VAL_GENERICA(e)->elemento;
			break;
		}
#endif
	case tipo_algebraico:
		return reducir_algebraico(entorno, e);
	default:
		return e;
	} 
	}
}

#if 0
Objeto *reducir_generica(Entorno *e, Objeto *g)
{
	Lista *ng = 0;
	Lista *l = 0;

	for (l = VAL_GENERICA(g); l; l = l->siguiente) {
		ng = mk_par(reducir(e, l->elemento), ng);
	}
	return mk_generica(ng);
}
#endif

Objeto *reducir_algebraico(Entorno *e, Objeto *c)
{
	Objeto *nc;
	Objeto *constructor = VAL_ALGEBRAICO(c)->constructor;
	Lista *l;

 	l = VAL_ALGEBRAICO(c)->elementos;
	if (!l)
		return c;
	nc = mk_construccion(constructor, reducir(e, l->elemento));
	for (l = l->siguiente; l; l = l->siguiente)
		construccion_push(nc, reducir(e, l->elemento));

	return mk_objeto_algebraico(constructor, VAL_CONSTRUCCION(nc)->elementos);
}

Objeto *evaluar(Entorno *entorno, Objeto *expresion)
{
	switch (expresion->tipo) {
	case tipo_simbolo: {
		Objeto *o;

		if (o = binding(entorno, VAL_NOMBRE(expresion)))
			return o;
		else
			fail("No esta bindeado", expresion);
	}
	case tipo_constructor:
		return mk_objeto_algebraico(expresion, 0);
	case tipo_construccion:
		return cerrar_algebraico(entorno,
			mk_objeto_algebraico(
				VAL_CONSTRUCCION(expresion)->constructor,
				VAL_CONSTRUCCION(expresion)->elementos));
	case tipo_aplicacion:
		return aplicar(entorno,
				VAL_APLICACION(expresion)->functor,
				mk_cerradura(entorno,
					VAL_APLICACION(expresion)->arg));
	case tipo_cerradura: {
		Entorno *e = VAL_CERRADURA(expresion)->entorno;
		Objeto *vieja = VAL_CERRADURA(expresion)->expresion;
		Objeto *nueva;
		
		nueva = evaluar(e, vieja);
		if (vieja == nueva)
			return expresion;
		else
			return mk_cerradura(e, nueva);
	}
	case tipo_dondura: {
		Entorno *e = mk_entorno(VAL_DONDURA(expresion)->hash, entorno);
		
		return mk_cerradura(e,
			evaluar(e, VAL_DONDURA(expresion)->expresion));
	}
	case tipo_generica: {
		char todos_iguales = 1;
		Lista *ng = 0;
		Lista *l;

		if (!VAL_GENERICA(expresion)->siguiente)
			return VAL_GENERICA(expresion)->elemento;
		for (l = VAL_GENERICA(expresion); l; l = l->siguiente) {
			Objeto *vieja = l->elemento;
			Objeto *nueva;

			nueva = evaluar(entorno, vieja);
			if (nueva != vieja)
				todos_iguales = 0;
			ng = mk_par(nueva, ng);
		}
		if (todos_iguales)
			return expresion;
		else
			return mk_generica(ng);
	}
	default:
		return expresion;
	}
}

Objeto *aplicar(Entorno *entorno, Objeto *functor, Objeto *arg)
{
	Objeto *ap;
	
	if (ap = aplicar2(entorno, functor, arg))
		return ap;
	else
		fail("No es aplicable o no aparean los conejos", functor);
}

Objeto *aplicar2(Entorno *entorno, Objeto *functor, Objeto *arg)
{
	Objeto *f = hacer_aplicable(entorno, functor);

	switch (f->tipo) {
	case tipo_funcion: {
		return aplicar_funcion(entorno, f, arg);
	}
	case tipo_cerradura: {
		Entorno *e = VAL_CERRADURA(f)->entorno;
		Objeto *expr = VAL_CERRADURA(f)->expresion;
		Objeto *ap;

		if (ap = aplicar2(e, expr, arg))
			return mk_cerradura(e, ap);
		else
			return 0;
	}
	case tipo_dondura: {
		Entorno *e = mk_entorno(VAL_DONDURA(f)->hash, entorno);
		Objeto *expr = VAL_DONDURA(f)->expresion;
		Objeto *ap;

		if (ap = aplicar2(e, expr, arg))
			return mk_cerradura(e, ap);
		else
			return 0;
	}
	case tipo_generica: {
		Lista *ng = 0;
		Lista *l;

		for (l = VAL_GENERICA(f); l; l = l->siguiente) {
			Objeto *f2 = l->elemento;
			Objeto *ap;

			if (ap = aplicar2(entorno, f2, arg))
				ng = mk_par(ap, ng);
		}
		if (!ng) return 0;
		return mk_generica(ng);
	}
	case tipo_maquina: {
		return ejecutar_maquina(VAL_MAQUINA(f), arg, entorno);
	}
	default:
		return 0;
	}
}

Objeto *aplicar_funcion(Entorno *entorno, Objeto *f, Objeto *arg)
{
	Entorno *e;

	if (e = aparear(entorno, entorno, VAL_FUNCION(f)->arg, arg))
		return mk_cerradura(e, VAL_FUNCION(f)->cuerpo);
	else
		return 0;
}

Objeto *hacer_aplicable(Entorno *e, Objeto *f)
{
	Objeto *nuevo;

	while ((f->tipo != tipo_generica)
		&& (f->tipo != tipo_cerradura)
		&& (f->tipo != tipo_funcion)
		&& (f->tipo != tipo_maquina)) {
		nuevo = evaluar(e, f);
		if (nuevo == f)
			return nuevo;
		f = nuevo;
	}
	return f;
}

Entorno *aparear(Entorno *entorno, Entorno *ref, Objeto *formal, Objeto *real)
{
	Objeto *r;
	Hash *h;

	if (Charlatan) {
	printf("Tratando de aparear ");
	show_objeto(stdout, formal);
	printf(" con ");
	show_objeto(stdout, real);
	printf("\n");
	}

	switch (formal->tipo) {
	case tipo_simbolo:
		if (strcmp(VAL_NOMBRE(formal), "_")) {
			/* NO es "_" */
			h = mk_hash(1);
			hash_set(h, VAL_NOMBRE(formal),
				mk_cerradura(ref, real));
			return mk_entorno(h, entorno);
		} else {
			return entorno;
		}
	case tipo_constructor:
		if (real->tipo == tipo_algebraico) {
			if (VAL_ALGEBRAICO(real)->constructor == formal) {
				return entorno;
			} else {
				return 0;
			}
		} else
			break;
	case tipo_construccion: {
		Objeto *constru = VAL_CONSTRUCCION(formal)->constructor;

		if (constru->tipo == tipo_constructor) {
			if (real->tipo == tipo_algebraico) {
				if (VAL_ALGEBRAICO(real)->constructor
						== constru) {
					Entorno *e;

					if (e = aparear_listas(entorno,
								ref,
						VAL_CONSTRUCCION(formal)->elementos,
						VAL_ALGEBRAICO(real)->elementos))
							return e;
					else
							break;
				} else {
					return 0;
				}
			} else {
				break;
			}
		} else {
			return 0;
		}
      	}
	case tipo_entero:
		if (real->tipo == tipo_entero) {
			if (VAL_INT(formal) == VAL_INT(real)) {
				return entorno;
			} else {
				return 0;
			}
		} else
			break;
	case tipo_char:
		if (real->tipo == tipo_char) {
			if (VAL_CHAR(formal) == VAL_CHAR(real)) {
				return entorno;
			} else {
				return 0;
			}
		} else
			break;
	case tipo_algebraico:
	case tipo_aplicacion:
	case tipo_funcion:
	case tipo_generica:
	case tipo_cerradura:
	case tipo_dondura:
	case tipo_entorno:
		return 0;
	default:
		break;
	}
	r = evaluar(ref, real);
	if (r->tipo == tipo_generica && !VAL_GENERICA(r)->siguiente)
		r = VAL_GENERICA(r)->elemento;
	if (r->tipo == tipo_cerradura) {
		return aparear(entorno, VAL_CERRADURA(r)->entorno,
				formal, VAL_CERRADURA(r)->expresion);
	} else if (r == real) {
		return 0;
	} else {
		return aparear(entorno, ref, formal, r);
	}
}

Entorno *aparear_listas(Entorno *entorno, Entorno *ref, Lista *cargs, Lista *args)
{
	Entorno *e = entorno;
	Lista *l1, *l2;

	for ( l1 = cargs, l2 = args; l1 && l2;
		l1 = l1->siguiente, l2 = l2->siguiente ) {
		if (!(e = aparear(e, ref, l1->elemento, l2->elemento)))
			return 0;
	}
	if (l1 || l2)
		fail("Constructor con numero equivocado de argumentos",
				cargs->elemento);
	return e;
}

Objeto *cerrar_algebraico(Entorno *e, Objeto *algebraico)
{
	Objeto *nc;
	Objeto *constructor = VAL_ALGEBRAICO(algebraico)->constructor;
	Lista *l;

 	l = VAL_ALGEBRAICO(algebraico)->elementos;
	if (!l)
		return algebraico;
	nc = mk_construccion(constructor, mk_cerradura(e, l->elemento));
	for (l = l->siguiente; l; l = l->siguiente)
		construccion_push(nc, mk_cerradura(e, l->elemento));

	return mk_objeto_algebraico(constructor, VAL_CONSTRUCCION(nc)->elementos);
}
