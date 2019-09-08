#include <stdlib.h>
#include "Sor.h"

void kshow_objeto(FILE *f, Objeto *o);
void kshow_entero(FILE *f, int i);

void show_objeto(FILE *f, Objeto *o)
{
	if (Koan) {
		kshow_objeto(f, o);
		return;
	}
	switch (o->tipo) {
	case tipo_algebraico:
		show_objeto_algebraico(f, o);
		break;
	case tipo_entero:
		show_entero(f, VAL_INT(o));
		break;
	case tipo_entorno:
		show_entorno(f, VAL_ENTORNO(o));
		break;
	case tipo_char:
		printf("'%c'", VAL_CHAR(o));
		break;
	case tipo_constructor:
		fprintf(f, VAL_CONSTRUCTOR(o)->nombre);
		break;
	case tipo_construccion:
	{
		Construccion *c = VAL_CONSTRUCCION(o);
		Lista *l;

		fprintf(f, "(");
		show_objeto(f, c->constructor);
		for (l = c->elementos; l; l = l->siguiente) {
			fprintf(f, " ");
			show_objeto(f, l->elemento);
		}
		fprintf(f, ")");
		break;
	}
	case tipo_simbolo:
		fprintf(f, VAL_NOMBRE(o));
		break;
	case tipo_nada:
		fprintf(f, "Tipo, nada.");
		break;
	case tipo_funcion:
		fprintf(f, "(");
		show_objeto(f, VAL_FUNCION(o)->arg);
		fprintf(f, " ---> ");
		show_objeto(f, VAL_FUNCION(o)->cuerpo);
		fprintf(f, ")");
		break;
	case tipo_cerradura:
		if (Charlatan) {
			fprintf(f, "<CERRADURA %lx | ", o);
		}
		show_objeto(f, VAL_CERRADURA(o)->expresion);
		if (Charlatan) {
			fprintf(f, ">");
		}
		break;
	case tipo_dondura:
		if (Charlatan) {
			fprintf(f, "<DONDURA %lx | ", o);
		}
		show_objeto(f, VAL_DONDURA(o)->expresion);
		if (Charlatan) {
			fprintf(f, ">");
		}
		break;
	case tipo_aplicacion:
		fprintf(f, "(");
		show_objeto(f, VAL_APLICACION(o)->functor);
		fprintf(f, " ");
		show_objeto(f, VAL_APLICACION(o)->arg);
		fprintf(f, ")");
		break;
	case tipo_generica:
	{
		Lista *l = VAL_GENERICA(o);

		fprintf(f, "<gen>");
		show_objeto(f, l->elemento);
		for (l = l->siguiente; l; l = l->siguiente) {
			fprintf(f, " o ");
			show_objeto(f, l->elemento);
		}
		fprintf(f, "</gen>");
		break;
	}
	case tipo_maquina:
		fprintf(f, "<maquina %lx>", o);
		break;
	default:
		break;
	}
}

void show_objeto_algebraico(FILE *f, Objeto *o)
{
	Lista *p;
	Algebraico *a = VAL_ALGEBRAICO(o);

	if (!strcmp(VAL_CONSTRUCTOR(a->constructor)->nombre, "Cons")) {
		char flag_primero = 1, flag_cadena = 0;
		Objeto *car, *cdr;

		p = a->elementos;

		if (p->elemento->tipo == tipo_char)
			flag_cadena = 1;

		if (flag_cadena)
			fprintf(f, "\"");
		else
			fprintf(f, "[");
sigue_lista:
		car = p->elemento;
		if (!p->siguiente)
			fail("Lista mal formada.", mk_simbolo("No te la puedo mostrar."));
		cdr = p->siguiente->elemento;
		if (flag_primero)
			flag_primero = 0;
		else if (!flag_cadena)
			fprintf(f, " ");
		if (flag_cadena && car->tipo == tipo_char)
			fprintf(f, "%c", VAL_CHAR(car));
		else
			show_objeto(f, car);
		if (cdr->tipo == tipo_algebraico) {
			if (!strcmp(VAL_CONSTRUCTOR(VAL_ALGEBRAICO(cdr)
				->constructor)->nombre, "Cons")) {
				p = VAL_ALGEBRAICO(cdr)->elementos;
				goto sigue_lista;
			} else if (!strcmp(VAL_CONSTRUCTOR(VAL_ALGEBRAICO(cdr)
				->constructor)->nombre, "Nil")) {
				goto fin_lista;
			} else {
				fprintf(f, " . ");
				show_objeto(f, cdr);
			}
		} else {
			fprintf(f, " . ");
			show_objeto(f, cdr);
		}
fin_lista:
		if (flag_cadena)
			fprintf(f, "\"");
		else
			fprintf(f, "]");
	} else if (!strcmp(VAL_CONSTRUCTOR(a->constructor)->nombre, "Nil")) {
		fprintf(f, "[]");
	} else {
		if (a->elementos)
			fprintf(f, "(");

		/*fprintf(f, "the ");*/

		show_objeto(f, a->constructor);
		for ( p = a->elementos; p; p = p->siguiente ) {
			fprintf(f, " ");
			show_objeto(f, p->elemento);
		}

		if (a->elementos)
			fprintf(f, ")");

	}
}

void show_entero(FILE *f, int i)
{
	if (i < 0) {
		fprintf(f, "-");
		i = -i;
	}

	if (i > 3999999)
		fprintf(f, "<%i>", i);
	else if (i == 0)
		fprintf(f, "nihil");
	else {
		int unidad = i % 10;
		int decena = (i / 10) % 10;
		int centena = (i / 100) % 10;
		int u_de_mil = (i / 1000) % 10;
		int d_de_mil = (i / 10000) % 10;
		int c_de_mil = (i / 100000) % 10;
		int u_de_millon = (i / 1000000) % 10;
		show_romano(f, u_de_millon, 'm', '-', '-');
		show_romano(f, c_de_mil, 'c', 'd', 'm');
		show_romano(f, d_de_mil, 'x', 'l', 'c');
		show_romano(f, u_de_mil, 'M', 'v', 'x');
		show_romano(f, centena, 'C', 'D', 'M');
		show_romano(f, decena, 'X', 'L', 'C');
		show_romano(f, unidad, 'I', 'V', 'X');
	}
}

void show_romano(FILE *f, int i, char c1, char c5, char c10)
{
	switch (i) {
	case 1: fprintf(f, "%c", c1);
		break;
	case 2: fprintf(f, "%c%c", c1, c1);
		break;
	case 3: fprintf(f, "%c%c%c", c1, c1, c1);
		break;
	case 4: fprintf(f, "%c%c", c1, c5);
		break;
	case 5: fprintf(f, "%c", c5);
		break;
	case 6: fprintf(f, "%c%c", c5, c1);
		break;
	case 7: fprintf(f, "%c%c%c", c5, c1, c1);
		break;
	case 8: fprintf(f, "%c%c%c%c", c5, c1, c1, c1);
		break;
	case 9: fprintf(f, "%c%c", c1, c10);
		break;
	}
}

void show_hash(FILE *f, Hash *h)
{
	unsigned key;
	char primero = 1;

	if (h == Actual) {
		fprintf(f, "<Actual>");
		return;
	}
	fprintf(f, "{");
	for (key = 0; key < h->buckets; key++) {
		ListaCV *q;

		for ( q = h->tabla[key]; q; q = q->siguiente ) {
			if (primero)
				primero = 0;
			else
				fprintf(f, ", ");
			fprintf(f, "%s : ", q->cv->clave);
			show_objeto(f, q->cv->valor);
		}
	}
	fprintf(f, "}");
}

void show_entorno(FILE *f, Entorno *entorno)
{
	Entorno *e;
	char primero = 1;

	fprintf(f, "<ENTORNO %lx | ", entorno);
	for (e = entorno; e; e = e->padre) {
		if (primero)
			primero = 0;
		else
			fprintf(f, ", ");
		show_hash(f, e->hash);
	}
	fprintf(f, ">");
}

int rnd(int cant)
{
	return (int) (random() * (float) cant / (RAND_MAX + 1.0));
}

/* Modo koan */
void kshow_objeto(FILE *f, Objeto *o)
{
	int cantidad = 32;
	int resultado;
	char *mensajes[] = {
		"<esto no es un objeto>",
		"<no es una cosa>",
		"<mira el roble en el jardin>",
		"<por que fue Boddidharma de la India a China?>",
		"<es Buda>",
		"<no es Buda>",
		"<no se puede decir con palabras>",
		"<no se puede decir sin palabras>",
		"<es mas vasto que el universo>",
		"<si has desayunado, ve a lavar tu cuenco>",

		"<si uno sabe que no existe, quien es ese \"uno\" que sabe?>",
		"<tiene la naturaleza de Buda?>",
		"<ceci n'est pas une pipe>",
		"<esta no es la descripcion de un objeto>",
		"<trueno peinado>",
		"<el calambre de yeso>",
		"<hlor u fang axaxaxas mlo>",
		"<esto no es SNOBOL>",
		"<cual es el camino hacia la iluminacion?>",
		"Error:\n Esto no es un error.\n",

		"if (hacha_de_ganto) { cortar_cabezas(); } else { cortar_cabezas(); }",
		"(try 'lisp)",
		"<el valor solicitado no corresponde a un abonado en servicio>",
		"<el valor esta ocupado; vuelva a intentar imprimirlo mas tarde>",
		"<este tampoco es el camino hacia la iluminacion>",
		"<me sigue picando el ombligo>",
		"<chuzao>",
		"<apenas el le amalaba el noema>",
		"<beware the Jabberwock>",
		"<if you continue, you're likely to be eaten by a Grue>",

		"<ralla tablas al batallar>",
		"<Presione cualquier tecla para continuar...>",
	};

	resultado = rnd(cantidad*3);
	if (resultado < cantidad) {
		fprintf(f, "%s", mensajes[resultado]);
	} else if (resultado == cantidad) {
		fprintf(f, "Error:\n");
		fprintf(f, " No se puede mostrar ");
		Koan = 0;
		show_objeto(f, o);
		Koan = 1;
		fprintf(f, "\n");
	} else {
		switch (o->tipo) {
			case tipo_algebraico:
				show_objeto_algebraico(f, o);
				break;
			case tipo_entero:
				kshow_entero(f, VAL_INT(o));
				break;
			case tipo_char:
				printf("'%c'", VAL_CHAR(o));
				break;
			case tipo_constructor:
				fprintf(f, VAL_CONSTRUCTOR(o)->nombre);
				break;
			case tipo_construccion:
				{
					Construccion *c = VAL_CONSTRUCCION(o);
					Lista *l;

					fprintf(f, "(");
					show_objeto(f, c->constructor);
					for (l = c->elementos; l; l = l->siguiente) {
						fprintf(f, " ");
						show_objeto(f, l->elemento);
					}
					fprintf(f, ")");
					break;
				}
			case tipo_simbolo:
				fprintf(f, VAL_NOMBRE(o));
				break;
			case tipo_nada:
				fprintf(f, "Tipo, nada.");
				break;
			case tipo_funcion:
				fprintf(f, "(");
				show_objeto(f, VAL_FUNCION(o)->arg);
				fprintf(f, " ---> ");
				show_objeto(f, VAL_FUNCION(o)->cuerpo);
				fprintf(f, ")");
				break;
			case tipo_cerradura:
				if (Charlatan) {
					fprintf(f, "<CERRADURA %lx | ", o);
				}
				show_objeto(f, VAL_CERRADURA(o)->expresion);
				if (Charlatan) {
					fprintf(f, ">");
				}
				break;
			case tipo_dondura:
				if (Charlatan) {
					fprintf(f, "<DONDURA %lx | ", o);
				}
				show_objeto(f, VAL_DONDURA(o)->expresion);
				if (Charlatan) {
					fprintf(f, ">");
				}
				break;
			case tipo_aplicacion:
				fprintf(f, "(");
				show_objeto(f, VAL_APLICACION(o)->functor);
				fprintf(f, " ");
				show_objeto(f, VAL_APLICACION(o)->arg);
				fprintf(f, ")");
				break;
			case tipo_generica:
				{
					Lista *l = VAL_GENERICA(o);

					fprintf(f, "<gen>");
					show_objeto(f, l->elemento);
					for (l = l->siguiente; l; l = l->siguiente) {
						fprintf(f, " o ");
						show_objeto(f, l->elemento);
					}
					fprintf(f, "</gen>");
					break;
				}
			case tipo_maquina:
				fprintf(f, "<maquina %lx>", o);
				break;
			default:
				break;
		}
	}
}

void kshow_entero(FILE *f, int i)
{
	int cantidad = 5;
	int resultado;

	resultado = rnd(cantidad);

	switch(resultado) {
	case 0: {
		char c = 'a' + rnd(26);

		fprintf(f, "<no es %c>", c);
		break;
	}
	case 1: {
		int r = i;

		while (r == i) {
			r = rnd(i*2+10)-rnd(10)+1;
		}
		fprintf(f, "<no es %i>", r);
		break;
	}
	case 2: {
		float j = (float) i;
		int r = rnd(3);

		while (j == (float) i) {
			j = i - random() * (float) 0.5 / (RAND_MAX+1.0);
		}
		switch (r) {
		case 0:
			fprintf(f, "<casi %f>", j);
			break;
		case 1:
			fprintf(f, "<mas o menos %f>", j);
			break;
		case 2:
			fprintf(f, "<cerca de %f>", j);
			break;
		}
		break;
	}
	case 3: {
		int r = rnd(9);
		char *mensajes[] = {
			"<no es una cadena>",
			"<no es una funcion>",
			"<no es un constructor>",
			"<si lo llamas numero te opones a su realidad; si no lo llamas numero ignoras los hechos>",
			"<este numero es Buda>",
			"<este numero no es Buda>",
			"<no es la lista vacia>",
			"<no soy yo>",
			"<no es tu reflejo>",
		};
		fprintf(f, "%s", mensajes[r]);
		break;
	}
	case 4: {
		int r;

		switch (i) {
		case 0:
			fprintf(f, "<oh, el famoso cero>");
			break;
		case 1:
			fprintf(f, "<phi con error acotado por 10^0>");
			break;
		case 2:
			r = rnd(2);
			if (r == 0)
				fprintf(f, "<e con error acotado por 10^0>");
			else
				fprintf(f, "<cerca del numero aureo>");
			break;
		case 3:
			r = rnd(2);
			if (r == 0)
				fprintf(f, "<casi la constante de Euler>");
			else
				fprintf(f, "<pi para fanaticos religiosos>");
			break;
		case 4:
			fprintf(f, "<pi con error acotado por 10^0>");
			break;
		case 5:
			fprintf(f, "<cantidad de dedos de pie derecho no deforme>");
			break;
		case 6:
			fprintf(f, "<cantidad de dedos de pie derecho mutante>");
			break;
		case 7:
			fprintf(f, "<un numero cualquiera>");
			break;
		case 9:
			fprintf(f, "<tres veces tres>");
			break;
		case 27:
			fprintf(f, "<tres veces tres veces tres>");
			break;
		case 8: case 16: case 32: case 64: case 128:
		case 256: case 512: case 1024: case 2048:
		case 4096: case 8192: case 16384:
			fprintf(f, "<una potencia de dos conocida>");
			break;
		case 10: case 100: case 1000: case 10000:
		case 100000:
			fprintf(f, "<una potencia de dos en binario>");
			break;
		case 12:
			fprintf(f, "<mejor dos docenas>");
			break;
		case 121:
			fprintf(f, "<once al cuadrado>");
			break;
		case 144:
			fprintf(f, "<una gruesa>");
			break;
		case 666:
			fprintf(f, "<el que tenga inteligencia calcule el telefono de la bestia, pues es un numero de hombre y su numero es 6666-6666>");
			break;
		case 1729:
			fprintf(f, "<10^3 + 9^3 -- Srinivasa in memoriam>");
			break;
		default:
			fprintf(f, "<un numero muy poco interesante>");
			break;
		}
		break;
	}
	}
}
