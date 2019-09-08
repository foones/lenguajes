#include <stdlib.h>
#include "Sor.h"

Objeto *mk_maquina(Maquina *maquina)
{
	Objeto *o = NEW(Objeto);

	o->tipo = tipo_maquina;
	VAL_MAQUINA(o) = maquina;
	return o;
}

void limpiar_maquina(Maquina *m, unsigned *posx, unsigned *posy)
{
	unsigned i, j;

	for ( i = 0; i < MAQMAXX; i++ )
		for ( j = 0; j < MAQMAXY; j++ )
			m->opcode[i][j] = ' ';
	*posx = 0;
	*posy = 0;
}

void poner_maquina(Maquina *m, unsigned *posx, unsigned *posy, char opcode)
{
	if (*posx >= MAQMAXX )
		fail("Largo de linea de la maquina excedido.",
				mk_simbolo(""));
	m->opcode[*posx][*posy] = opcode;
	*posx += 1;
}

void linea_maquina(Maquina *m, unsigned *posx, unsigned *posy)
{
	if (*posy >= MAQMAXY )
		fail("Cantidad de lineas de la maquina excedida.",
				mk_simbolo(""));
	*posy += 1;
	*posx = 0;
}

typedef struct _pila_de_pilas PilaDePilas;
struct _pila_de_pilas {
	struct _lista *pila;
	struct _pila_de_pilas *siguiente;
};

typedef struct _ip IP;
struct _ip {
	Maquina *m;
	struct _pila_de_pilas *pp;
	int x;
	int y;
	int dx;
	int dy;
	int offx;
	int offy;
	char stringmode;
};

typedef struct _lista_ip ListaIP;
struct _lista_ip {
	struct _ip *ip;
	struct _lista_ip *siguiente;
};

PilaDePilas *mk_pila_de_pilas(Lista *pila, PilaDePilas *siguiente)
{
	PilaDePilas *p = NEW(PilaDePilas);

	p->pila = pila;
	p->siguiente = siguiente;
	return p;
}

IP *mk_ip(Maquina *m, PilaDePilas *pp, int x, int y, int dx, int dy,
		int offx, int offy, char stringmode)
{
	IP *ip = NEW(IP);

	ip->m = m;
	ip->pp = pp;
	ip->x = x;
	ip->y = y;
	ip->dx = dx;
	ip->dy = dy;
	ip->stringmode = stringmode;
	return ip;
}

ListaIP *mk_lista_ip(IP *ip, ListaIP *sig)
{
	ListaIP *l = NEW(ListaIP);

	l->ip = ip;
	l->siguiente = sig;
	return l;
}

void reflejar(IP *ip)
{
	ip->dx = -ip->dx;
	ip->dy = -ip->dy;
}

void doblar_der(IP *ip)
{
	int ermedio = ip->dy;

	ip->dy = ip->dx;
	ip->dx = -ermedio;
}

void doblar_izq(IP *ip)
{
	int ermedio = ip->dy;

	ip->dy = -ip->dx;
	ip->dx = ermedio;
}

void direccion(IP *ip, int x, int y)
{
	ip->dx = x;
	ip->dy = y;
}

int adentro(int x, int y)
{
	return x >= 0 && x < MAQMAXX && y >= 0 && y < MAQMAXY;
}

void avanzar(IP *ip)
{
	ip->x += ip->dx, ip->y += ip->dy;
	switch (ip->m->wrap_mode) {
	case WRAP_TOROIDE:
		ip->x %= MAQMAXX; 
		ip->x += (ip->x < 0 ? MAQMAXX: 0);
		ip->y %= MAQMAXY; 
		ip->y += (ip->y < 0 ? MAQMAXY: 0);
		break;
	case WRAP_REFLEJAR:
		if (!adentro(ip->x, ip->y)) {
			ip->x -= ip->dx;
			ip->y -= ip->dy;
			reflejar(ip);
			avanzar(ip);
		}
		break;
	case WRAP_DOBLAR:
		if (!adentro(ip->x, ip->y)) {
			ip->x -= ip->dx;
			ip->y -= ip->dy;
			doblar_der(ip);
			avanzar(ip);
		}
		break;
	}
}

#define CAR_POS(X,Y)	ip->m->opcode[X][Y]
void poner_en_siguiente(IP *ip, char c)
{
	int x, y, dx, dy;

	dx = ip->dx;
	dy = ip->dy;
reintentar:
	x = ip->x + ip->dx;
	y = ip->y + ip->dy;
	switch (ip->m->wrap_mode) {
	case WRAP_TOROIDE:
		x %= MAQMAXX; 
		x += (x < 0 ? MAQMAXX: 0);
		y %= MAQMAXY; 
		y += (y < 0 ? MAQMAXY: 0);
		break;
	case WRAP_REFLEJAR:
		if (!adentro(x, y)) {
			ip->x -= dx;
			ip->y -= dy;
			dx = -dx;
			dy = -dy;
			goto reintentar;
		}
		break;
	case WRAP_DOBLAR:
		if (!adentro(x, y)) {
			int ermedio = dy;
			ip->x -= dx;
			ip->y -= dy;
			dy = -dx;
			dx = -ermedio;
			goto reintentar;
		}
		break;
	}
	CAR_POS(x, y) = c;
}

void push(IP *ip, Objeto *o)
{
	ip->pp->pila = mk_par(o, ip->pp->pila);
}

Objeto *pop(IP *ip)
{
	Objeto *o;

	if (ip->pp && ip->pp->pila) {
		o = ip->pp->pila->elemento;
		ip->pp->pila = ip->pp->pila->siguiente;
	} else
		o = mk_int(0);
	return o;
}

Objeto *tope(IP *ip)
{
	Objeto *o;

	if (ip->pp && ip->pp->pila) {
		o = ip->pp->pila->elemento;
	} else
		o = mk_int(0);
	return o;
}

int valor_entero(Objeto *o)
{
	if (o->tipo != tipo_entero)
		fail("Deberia ser un valor entero", o);
	return VAL_INT(o);
}

#define AVANZAR		avanzar(ip);
#define PUSH(X)		push(ip, X)
#define POP(X)		X = pop(ip)
#define INT(X)		valor_entero(X)
#define CAR_ACTUAL	CAR_POS(ip->x, ip->y)

#define EXTENSIONES 1
Objeto *interpretar(IP *ip, ListaIP *lip)
{
	char c = CAR_ACTUAL;
	Objeto *o1, *o2, *o3;

	if (ip->stringmode) {
		if (c == '"')
			ip->stringmode = 0;
		else
			PUSH(mk_int(c));
		return 0;
	}
	switch (c) {
	case '!':
		/* not */
		POP(o1);
		if (INT(o1)) {
			PUSH(mk_int(0));
		} else {
			PUSH(mk_int(1));
		}
		break;
	case '"':
		/* togglea el stringmode (todos los caracteres
		 * leidos son empujados numericamente) */
		ip->stringmode = 1;
		break;
	case '@':
		/* devuelve el valor del tope de la pila,
		 * finalizando el ip corriente */
		POP(o1);
		return o1;
	case '>':
		/* va a la derecha */
		direccion(ip, 1, 0);
		break;
	case '<':
		/* va a la izquierda */
		direccion(ip, -1, 0);
		break;
	case 'v':
		/* va abajo */
		direccion(ip, 0, 1);
		break;
	case '^':
		/* va arriba */
		direccion(ip, 0, -1);
		break;
	case '#':
		/* trampolin: avanza sin interpretar; pos = pos + delta */
		AVANZAR;
		break;
	case '$':
		/* descarta el elemento en el tope de la pila */
		POP(o1);
		break;
	case '+':
		/* suma */
		POP(o1);
		POP(o2);
		PUSH(mk_int(INT(o2) + INT(o1)));
		break;
	case '-':
		/* resta (el primer valor empujado es el minuendo
		 * y el segundo el sustraendo) */
		POP(o1);
		POP(o2);
		PUSH(mk_int(INT(o2) - INT(o1)));
		break;
	case '*':
		/* producto */
		POP(o1);
		POP(o2);
		PUSH(mk_int(INT(o2) * INT(o1)));
		break;
	case '/': {
		/* cociente entero; el primer valor empujado es el
		 * dividendo y el segundo el divisor */
		int i;

		POP(o1);
		POP(o2);
		i = INT(o1);
		if (i == 0)
			fail("Error famoso: division por cero", o2);
		PUSH(mk_int(INT(o2) / i));
		break;
	}
	case '%': {
		/* modulo; el primer valor empujado es el dividendo y
		 * el segundo el divisor */
		int i;

		POP(o1);
		POP(o2);
		i = INT(o1);
		if (i == 0)
			fail("Error famoso: division por cero", o2);
		PUSH(mk_int(INT(o2) % i));
		break;
	}
	case '\'': {
		/* lee el siguiente caracter y empuja en la pila
		 * el numero asociado */
		char c2;
		AVANZAR;
		c2 = CAR_ACTUAL;
		PUSH(mk_int(c2));
		break;
	}
	case '\\':
		/* swapea los elementos en el tope de la pila */
		POP(o1);
		POP(o2);
		PUSH(o1);
		PUSH(o2);
		break;
	case '.':
		/* imprime el valor del tope de la pila */
		POP(o1);
		show_objeto(stdout, o1);
		break;
	case ',':
		/* imprime el numero del tope de la pila como caracter */
		POP(o1);
		fprintf(stdout, "%c", INT(o1));
		break;
	case '|':
		/* si es cero, va arriba, si no abajo */
		o1 = tope(ip);
		if (o1->tipo == tipo_entero && VAL_INT(o1) == 0)
			direccion(ip, 0, -1);
		else
			direccion(ip, 0, 1);
		break;
	case '_':
		/* si es cero, va a la izquierda, si no a la derecha */
		o1 = tope(ip);
		if (o1->tipo == tipo_entero && VAL_INT(o1) == 0)
			direccion(ip, -1, 0);
		else
			direccion(ip, 1, 0);
		break;
	case '`':
		/* a b -> si a > b empuja 1; si no 0 */
		POP(o1);
		POP(o2);
		if (INT(o2) > INT(o1))
			PUSH(mk_int(1));
		else
			PUSH(mk_int(0));
		break;
	case ':':
		/* copia el elemento en el tope de la pila */
		o1 = tope(ip);
		PUSH(o1);
		break;
	case ';': {
		/* avanza hasta el siguiente ; */
		char c2 = ' ';
		do {
			AVANZAR;
			c2 = CAR_ACTUAL;
		} while (c2 != ';');
		break;
	}
	case '?':
		/* se mueve en una direccion aleatoria
		 * entre norte, sur, este u oeste */
		switch (rnd(4)) {
		case 0:
			direccion(ip, 1, 0);
			break;
		case 1:
			direccion(ip, -1, 0);
			break;
		case 2:
			direccion(ip, 0, 1);
			break;
		case 3:
			direccion(ip, 0, -1);
			break;
		}
		break;
	case '[':
		/* dobla a la izquierda */
		doblar_izq(ip);
		break;
	case ']':
		/* dobla a la derecha */
		doblar_der(ip);
		break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		/* empuja el valor numerico */
		PUSH(mk_int(c - '0'));
		break;
	case 'a': case 'b': case 'c': case 'd': case 'e':
	case 'f':
		/* empuja el valor de a-f en hexa */
		PUSH(mk_int(c - 'a' + 10));
		break;
	case 'g': {
		/* x y -> empuja en la pila el valor que hay en la
		 * posicion x y */
		int x, y;

		POP(o1);
		POP(o2);
		x = INT(o2) + ip->offx;
		y = INT(o1) + ip->offy;
		if (adentro(x, y))
			PUSH(mk_int(CAR_POS(x, y)));
		else
			PUSH(mk_int('\0'));
		break;
	}
	case 'j': {
		/* salta n lugares: pos = pos + n * delta */
		int i, j;

		POP(o1);
		j = INT(o1);
		for (i = 0; i < j; i++) {
			AVANZAR;
		}
		break;
	}
	case 'k': {
		/* repite la siguiente instruccion n veces */
		int i, j;

		POP(o1);
		j = INT(o1);
		AVANZAR;
		for (i = 0; i < j; i++) {
			interpretar(ip, lip);
		}
		break;
	}
	case 'n':
		/* limpia la pila */
		if (ip->pp)
			ip->pp->pila = 0;
		break;
	case 'p': {
		/* v x y -> pone el caracter v en la posicion x y */
		int x, y;

		POP(o1);
		POP(o2);
		POP(o3);
		x = INT(o2) + ip->offx;
		y = INT(o1) + ip->offy;
		if (adentro(x, y))
			CAR_POS(x, y) = INT(o3);
		else
			fail("NO SKELETON IN MY CLOSET", o3);
		break;
	}
	case 'q':
		/* finaliza dando codigo de error */
		POP(o1);
		exit(INT(o1));
		break;
	case 'r':
		/* delta = -delta */
		reflejar(ip);
		break;
	case 's':
		/* saca un caracter de la pila y lo pone
		 * en pos + delta */
		POP(o1);
		poner_en_siguiente(ip, INT(o1));
		break;
	case 't': {
		/* crea un nuevo ip igual al actual pero
		 * con delta reflejado */
		IP *nip;

		nip = mk_ip(ip->m, ip->pp, ip->x, ip->y,
				-ip->dx, -ip->dy, ip->offx, ip->offy,
				ip->stringmode);
		lip->siguiente = mk_lista_ip(nip, lip->siguiente);
		avanzar(nip);
		break;
	}
	case 'w': {
		/* a b -> si a > b ']', si b > a '[', si no 'z' */
		int a, b;

		POP(o1);
		POP(o2);
		a = INT(o2);
		b = INT(o1);
		if (a > b)
			doblar_der(ip);
		else if (b > a)
			doblar_izq(ip);
		break;
	}
	case 'x':
		/* absolute delta; dados x y, hace que ese sea el
		 * vector de movimiento */
		POP(o1);
		POP(o2);
		ip->dx = INT(o2);
		ip->dy = INT(o1);
		break;
	case 'z':
	case ' ':
		/* nop */
		break;
	case '~': {
		/* lee un caracter y lo empuja */
		char c2;

		c2 = getc(stdin);
		PUSH(mk_int(c2));
		break;
	}
	case '&': {
		/* lee un numero y lo empuja */
		int i;

		scanf("%i", &i);
		PUSH(mk_int(i));
		break;
	}
	case '{': {
		/* crea una nueva pila y mueve los n argumentos
		 * de la vieja a esta
		 * guarda el offset en la vieja */
		Lista *p1 = 0, *p2 = 0;
		Lista *q;
		int i, cant_args;

		POP(o1);
		cant_args = INT(o1);
		for (i = 0; i < cant_args; i++) {
			Objeto *arg;

			POP(arg);
			p1 = mk_par(arg, p1);
		}
		for (q = p1; q; q = q->siguiente)
			p2 = mk_par(q->elemento, p2);

		PUSH(mk_int(ip->offx));
		PUSH(mk_int(ip->offy));
		ip->offx = ip->x;
		ip->offy = ip->y;
		ip->pp = mk_pila_de_pilas(p2, ip->pp);
		break;
	}
	case '}': {
		/* saca una pila; recupera el offset guardado
		 * en la segunda
		 * mueve los n argumentos de la sacada a la segunda */
		Lista *p1 = 0;
		Lista *p2, *q;
		int i, cant_args;

		POP(o1);
		cant_args = INT(o1);
		for (i = 0; i < cant_args; i++) {
			Objeto *arg;

			POP(arg);
			p1 = mk_par(arg, p1);
		}

		if (ip->pp)
			ip->pp = ip->pp->siguiente;
		else {
			reflejar(ip);
			break;
		}
		POP(o1);
		POP(o2);
		ip->offx = INT(o2);
		ip->offy = INT(o1);
		if (ip->pp)
			p2 = ip->pp->pila;
		else
			p2 = 0;
		for (q = p1; q; q = q->siguiente)
			p2 = mk_par(q->elemento, p2);
		if (ip->pp)
			ip->pp = mk_pila_de_pilas(p2, ip->pp->siguiente);
		else
			ip->pp = mk_pila_de_pilas(p2, 0);
		break;
	}
	case 'u': {
		int i, cant_args;

		POP(o1);
		cant_args = INT(o1);

		if (!ip->pp)
			ip->pp = mk_pila_de_pilas(0, 0);
		if (cant_args > 0) {
			/* de la soss a la toss */
			Lista *p = ip->pp->pila;

			ip->pp = ip->pp->siguiente;
			for (i = 0; i < cant_args; i++) {
				POP(o1);
				p = mk_par(o1, p);
			}
			ip->pp = mk_pila_de_pilas(p, ip->pp);
		} else if (cant_args < 0) {
			/* de la toss a la soss */
			PilaDePilas *pp_original, *pp_base_nueva;
			Lista *p1;
			Lista *p2;

			pp_original = ip->pp;
			ip->pp = ip->pp->siguiente;
			if (!ip->pp)
				ip->pp = mk_pila_de_pilas(0, 0);
			p2 = ip->pp->pila;
			pp_base_nueva = ip->pp->siguiente;

			ip->pp = pp_original;
			for (i = 0; i < -cant_args; i++) {
				POP(o1);
				p2 = mk_par(o1, p2);
			}
			p1 = ip->pp->pila;
			ip->pp = mk_pila_de_pilas(p1,
					mk_pila_de_pilas(p2, pp_base_nueva));
		}
		break;
	}
#if EXTENSIONES
	case '(': {
		/* como t, pero en lugar de reflejar, dobla
		 * a la izquierda */
		IP *nip;

		nip = mk_ip(ip->m, ip->pp, ip->x, ip->y,
				ip->dy, -ip->dx, ip->offx, ip->offy,
				ip->stringmode);
		lip->siguiente = mk_lista_ip(nip, lip->siguiente);
		avanzar(nip);
		break;
	}
	case ')': {
		/* como t, pero en lugar de reflejar, dobla
		 * a la derecha */
		IP *nip;

		nip = mk_ip(ip->m, ip->pp, ip->x, ip->y,
				-ip->dy, ip->dx, ip->offx, ip->offy,
				ip->stringmode);
		lip->siguiente = mk_lista_ip(nip, lip->siguiente);
		avanzar(nip);
		break;
	}
	case 'E': {
		POP(o1);
		/* error */
		fail("Cagaste", o1);
		break;
	}
#endif
	default:
		//printf("\'%c\' %i\n", c, c);
		break;
	}
	return 0;
}

Lista *destructurar_lista(Objeto *o)
{
	Lista *l = 0;
	Lista *p;
	Algebraico *a;
	Objeto *car, *cdr;

	if (o->tipo != tipo_algebraico)
		return mk_par(o, 0);
 	a = VAL_ALGEBRAICO(o);
	if (strcmp(VAL_CONSTRUCTOR(a->constructor)->nombre, "Cons"))
		return mk_par(o, 0);
	p = a->elementos;
sigue_lista:
	car = p->elemento;
	if (!p->siguiente)
		fail("Lista mal formada.", mk_simbolo("No se puede destructurar."));
	cdr = p->siguiente->elemento;
	l = mk_par(car, l);
	if (cdr->tipo == tipo_algebraico) {
		if (!strcmp(VAL_CONSTRUCTOR(VAL_ALGEBRAICO(cdr)
			->constructor)->nombre, "Cons")) {
			p = VAL_ALGEBRAICO(cdr)->elementos;
			goto sigue_lista;
		} else if (!strcmp(VAL_CONSTRUCTOR(VAL_ALGEBRAICO(cdr)
			->constructor)->nombre, "Nil")) {
			goto fin_lista;
		} else {
			fail("Lista mal formada.", mk_simbolo("No se puede destructurar."));
		}
	} else {
		fail("Lista mal formada.", mk_simbolo("No se puede destructurar."));
	}
fin_lista:
	return l;
}

Objeto *ejecutar_maquina(Maquina *m, Objeto *arg, Entorno *env)
{
	ListaIP *lips, *i, *anterior;
	Objeto *arg2;
	Lista *resultados = 0;
	Lista *pila_inicial = 0;
 	
	if (m->eval_mode == EVAL_STRICT) {
		arg2 = reducir(env, arg);
	} else
		arg2 = arg;

	pila_inicial = destructurar_lista(arg2);

	lips = mk_lista_ip(
		mk_ip(m,
		mk_pila_de_pilas(pila_inicial, 0), 0, 0, 1, 0, 0, 0, 0),
		0);
	lips->siguiente = lips;

	for (anterior = i = lips; ; anterior = i, i = i->siguiente) {
		Objeto *r;

		r = interpretar(i->ip, i);
		if (r) {
			resultados = mk_par(r, resultados);
			if (i == i->siguiente)
				break;
			else
				anterior->siguiente = i->siguiente;
		} else {
			avanzar(i->ip);
		}
	}
	return mk_cerradura(env, mk_generica(resultados));
}

#undef CAR_ACTUAL
#undef CAR_POS
#undef AVANZAR
#undef PUSH
#undef POP
#undef INT
