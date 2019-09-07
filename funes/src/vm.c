#include "Fu.h"
#include <assert.h>

BCODE *
fu_make_bcode(opcode, opt)
	char opcode;
	int opt;
{
	BCODE *c;

	c = NEW(BCODE);
	BCODE_OPCODE(c) = opcode;
	BCODE_OPT(c) = opt;
	return c;
}

BCONS *
fu_make_bcons(code, next)
	BCODE *code;
	BCONS *next;
{
	BCONS *bc;

	bc = NEW(BCONS);
	BCONS_CODE(bc) = code;
	BCONS_NEXT(bc) = next;
	return bc;
}

BLIST *
fu_make_blist(first)
	BCONS *first;
{
	BLIST *bl;
	bl = NEW(BLIST);
	BLIST_LAST(bl) = BLIST_FIRST(bl) = first;
	return bl;
}

RES *
fu_closure_copy(clos)
		RES *clos;
{
		RES *p = NEW(RES);
		CLOSURE *d = NEW(CLOSURE), *o = VAL_CLOSURE(clos);

		TIPO(p) = tipo_closure;
		CLOSURE_CODEVEC(d) = CLOSURE_CODEVEC(o);
		CLOSURE_LENGTH(d) = CLOSURE_LENGTH(o);
		CLOSURE_CONSTANTS(d) = CLOSURE_CONSTANTS(o);
		CLOSURE_ARGLIST(d) = CLOSURE_ARGLIST(o);
		CLOSURE_FLAGS(d) = CLOSURE_FLAGS(o);
		CLOSURE_ENVPADRE(d) = CLOSURE_ENVPADRE(o);
		VAL(p) = d;
		return p;
}

#define SET_K(K, V)	fu_def_env(K, V)
#define GET_K(K)	fu_get_env(K)
/* KWARP y KEY => para los argumentos reales */
#define KWARGP(E)	(CONS_P(E) && CAR(E) == ARGSET && CONS_P(CDR(E)) && CONS_P(CDDR(E)) && CONS_P(CDDDR(E)))
#define KEY(E)		CADR(E)
/* KWARGP2 y KEY2 => para los argumentos formales */
#define KWARGP2(E)	KWARGP(E)
#define KEY2(E)		CADR(E)

#define VARIABLE(E)	CADDR(E)
#define VALUE(E)	CAR(CDDDR(E))

/* bindea los argumentos formales a los
* argumentos reales
*/
void bind_arguments(RES *formal_args, RES *args)
{
	RES *p, *f;
	for ( f = formal_args, p = args; ;  ) {
		if ( CONS_P(f) ) {
			if (CONS_P(p)) {
				/* argumento normal */
				if (KWARGP(CAR(p))) {
					/* el real es un kwarg */
					SET_K(KEY(CAR(p)), VALUE(CAR(p)));
					/* avanzo el real pero NO el formal */
					p = CDR(p);
					continue;
				} else if (KWARGP2(CAR(f))) {
					/* el formal es un kwarg y el real no */
					RES *a = CAR(f);
					RES *kv = GET_K(KEY2(a));
					if (kv == NIL)
						/* no tenia binding previo */
						SET_K(VARIABLE(a), CAR(p));
					else {
						SET_K(VARIABLE(a), CDR(kv));
						/* avanzo el formal pero NO el real */
						f = CDR(f);
						continue;
					}
				} else {
					fu_def_env(CAR(f), CAR(p));
				}
			} else {
				/* pasaron pocos parametros */
				while (TRUE) {
					if (CONS_P(f)) {
						RES *a = CAR(f);
						/* miro solamente los argumentos formales */
						if (KWARGP2(a)) {
							RES *kv = GET_K(KEY2(a));
							if (kv == NIL)
								/* no tenia binding previo */
								SET_K(VARIABLE(a), fu_eval(VALUE(a)));
							else
								SET_K(VARIABLE(a), CDR(kv));
						} else
							fu_throw(fu_exception_symbol("arity-error"),
								fu_str("interpret -- no coinciden las aridades"));
					} else if (f == NIL) {
						return;
					} else {
						fu_def_env(f, NIL);
						return;
					}
					f = CDR(f);
				}
			}
		} else if ( f == NIL ) {
			if ( p == NIL ) {
				/* termina bien */
				break;
			} else {
				/* pasaron demasiados parametros */
				fu_throw(fu_exception_symbol("arity-error"),
					fu_str("interpret -- no coinciden las aridades"));
			}
		} else {
			/* f no es un cons ni NIL */
			/* argumentos rest y termina */
			fu_def_env(f, p);
			break;
		}
		f = CDR(f);
		p = CDR(p);
	}
}
#undef KEY
#undef KEY2
#undef VALUE
#undef GET_K
#undef SET_K

#define STACK_PUSH(X) (state->stack = fu_cons((X), state->stack))
#define STACK_POP(X) {	assert(CONS_P(state->stack));\
						X = CAR(state->stack);\
						state->stack = CDR(state->stack); }
#define STACK_TOP()	(assert(CONS_P(state->stack)), CAR(state->stack))

/*
 * The current continuation is a _stack_ of continuations.
 * Each CONT has a CONT *next pointer (the caller).
 */
CONT *Current_Continuation = NULL;
RES *Continuation_Queue;				/* queue of continuations (for
										   multiple processes) */
RES **Last_Cont;						/* points to the pointer to the last
									   	   element in the queue */

RES *Sem_Table = NULL;					/* Dictionary of semaphore values */

#define VM_END			0
#define VM_CONTINUE		1
#define VM_TAIL_CALL	2
#define VM_SET_CC		3
#define VM_SPAWN		4
#define VM_NEXT_PROCESS	5
#define VM_SEM_INIT		6
#define VM_SEM_P		7
#define VM_SEM_V		8

/*
 * Interpreta el codigo de la continuacion (una vez!).
 *
 * Si termina, devuelve VM_END, dejando el resultado en state->val.
 *
 * Si invoca a otra funcion, devuelve VM_CONTINUE, indicando
 * cuales son la funcion y los argumentos en function y arguments
 * respectivamente.
 *
 * Si es una llamada a la cola, devuelve VM_TAIL_CALL, indicando
 * la funcion y argumentos en function y arguments.
 *
 * Si se continua una continuation, devuelve VM_SET_CC, indicando
 * la continuation en function y el valor en arguments.
 *
 * Si se hace spawn de una funcion, devuelve VM_SPAWN, indicando
 * la funcion en function y los argumentos en arguments.
 *
 * Si un proceso itera mas de ONCE_QUANTUM veces, se devuelve
 * el control con VM_NEXT_PROCESS.
 *
 */
#define ONCE_QUANTUM	1000
int fu_interpret_once(CONT *state)
{
	BCODE **code;		/* code */
	RES **consts;		/* table of constants */
	unsigned length;	/* length of the BCODE */

	unsigned quantum = ONCE_QUANTUM;

	code = CLOSURE_CODEVEC(state->clos);
	consts = VECTOR_TABLA(VAL_VECTOR(CLOSURE_CONSTANTS(state->clos)));
	length = CLOSURE_LENGTH(state->clos);

	/* interpreta el codigo */
	for ( ; state->pc < length; ++state->pc) {
		char op = BCODE_OPCODE(code[state->pc]);
		int opt = BCODE_OPT(code[state->pc]);

		if (--quantum == 0) {
				return VM_NEXT_PROCESS;
		}

		switch (op) {
		case NOP:
			break;
		case RET:
			return VM_END;
		case OP_CONST:
			state->val = consts[opt];
			break;
		case PUSH:
			STACK_PUSH(state->val);
			break;
		case CONST_PUSH:
			state->val = consts[opt];
			STACK_PUSH(state->val);
			break;
		case APPLY:
		{
			unsigned i;
			RES *arg, *largs;

			largs = NIL;
			for ( i = 0; i < opt; i++ ) {
				STACK_POP(arg);
				largs = fu_cons(arg, largs);
			}
			state->function = state->val;
			state->arguments = largs;
			return VM_CONTINUE;
		}
		case TAIL_APPLY:
		{
			int i;
			RES *arg, *largs;

			largs = NIL;
			for ( i = 0; i < opt; i++ ) {
				STACK_POP(arg);
				largs = fu_cons(arg, largs);
			}
			state->function = state->val;
			state->arguments = largs;
			if (TIPO_P(tipo_closure, state->val)
							&& !CLOSURE_DYN(VAL_CLOSURE(state->val))) {
				return VM_TAIL_CALL;
			} else {
				return VM_CONTINUE;
			}
		}
		case APPLY_CALL:
		{
			unsigned i;
			RES *arg, *largs;

			if (opt == 0) {
				largs = NIL;
			} else {
				STACK_POP(largs);
				for ( i = 1; i < opt; i++ ) {
					STACK_POP(arg);
					largs = fu_cons(arg, largs);
				}
			}

			state->function = state->val;
			state->arguments = largs;
			return VM_CONTINUE;
		}
		case JMP:
		{
			state->pc = opt;
			break;
		}
		case JNF:
		{
			if ( state->val != NIL )
				state->pc = opt;
			break;
		}
		case JF:
		{
			if ( state->val == NIL )
				state->pc = opt;
			break;
		}
		case DEF:
		{
			RES *ident;

			ident = consts[opt];
			STACK_POP(state->val);
			fu_def_env(ident, state->val);
			break;
		}
		case SET:
		{
			RES *ident;

			ident = consts[opt];
			STACK_POP(state->val);
			fu_set_env(ident, state->val);
			break;
		}
		case GET:
		{
			RES *ident;

			ident = consts[opt];
			state->val = fu_get_env(ident);
			if ( state->val == NIL ) {
				fu_throw(fu_exception_symbol("unbound-variable"),
					fu_str_cat(
						fu_str_cat(fu_str("get -- `"),
							fu_mkstr(fu_cons(ident, NIL))),
						fu_str("' simbolo no bindeado")));
			} else
				state->val = CDR(state->val);
			break;
		}
		case CLOSE:
		{
			/* cierra la closure en un entorno (generando una copia) */
			RES *clos2;

			clos2 = fu_closure_copy(state->val);
			CLOSURE_ENVPADRE(VAL_CLOSURE(clos2)) = fu_env();
			state->val = clos2;
			break;
		}
		case PUSH_ENV:
			fu_push_env();
			state->env = fu_env();
			break;
		case POP_ENV:
			fu_pop_env();
			state->env = fu_env();
			break;
		case ENV:
			state->val = fu_dir();
			break;
		case SETTER:
			state->val = fu_setter(state->val);
			break;
		case NOT:
			state->val = (state->val == NIL)? T : NIL;
			break;
		case EQ: {
			RES *p;

			STACK_POP(p);
			state->val = (state->val == p) ? T : NIL;
			break;
		}
		case OP_CAR:
			state->val = fu_car(state->val);
			break;
		case OP_CDR:
			state->val = fu_cdr(state->val);
			break;
		case CALL_CC:
			state->function = state->val;
			state->arguments = fu_cons(fu_current_continuation(), NIL);
			return VM_CONTINUE;
		case SET_CC:
			STACK_POP(state->function);
			STACK_POP(state->arguments);
			return VM_SET_CC;
		case SPAWN:
			STACK_POP(state->function);
			STACK_POP(state->arguments);
			return VM_SPAWN;
		case SEM_SET:
			switch (opt) {
			case SEM_init:
				STACK_POP(state->function);
				STACK_POP(state->arguments);
				return VM_SEM_INIT;
			case SEM_p:
				/* ojo:
				 *   sem-p NO saca el nombre del semaforo de la pila.
				 *
				 *   La idea es que, si no se puede hacer P,
				 *   el thread se quede trabado y no se avance
				 *   el pc. Esto sirve para que la vez siguiente
				 *   lea los mismos argumentos de la pila.
				 *   
				 *   Recien se hace POP cuando la operacion P es
				 *   exitosa.
				 */
				state->function = STACK_TOP();
				return VM_SEM_P;
			case SEM_v:	
				/* sem-v */
				STACK_POP(state->function);
				return VM_SEM_V;
			case SEM_val: {
				/* sem-val */
				RES *sem;
				RES *r;

				STACK_POP(sem);
				r = fu_get_hash_eq(Sem_Table, sem);

				if (!CONS_P(r))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("sem-val -- semaforo no inicializado"));

				state->val = CDR(r);
				break;
			}
			case SEM_set: {
				/* sem-set */
				RES *sem, *valor;
				RES *r;

				STACK_POP(sem);
				STACK_POP(valor);
				r = fu_get_hash_eq(Sem_Table, sem);

				if (!CONS_P(r))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("sem-set -- semaforo no inicializado"));
				if (!INT_P(valor))
					fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("sem-set -- el valor debe ser un entero"));

				CDR(r) = valor;
				state->val = valor;
				break;
			}
			}
			break;
		case PRINT:
			fu_print(state->val);
			printf("\n");
			break;
		}
	}
	return VM_END;
}

CONT *copy_cont(CONT *c) {
	CONT *copy_c;
	CONT *p;
	CONT **q;

	q = &copy_c;

	/* _copy_ the current continuation */
	for (p = c; p != NULL; p = p->next) {
		*q = NEW(CONT);
		(*q)->clos = p->clos;
		(*q)->pc = p->pc;
		(*q)->val = p->val;
		(*q)->stack = p->stack;
		(*q)->prev_env = p->prev_env;
		(*q)->env = p->env;
		(*q)->action = p->action;
		(*q)->function = p->function;
		(*q)->arguments = p->arguments;
		(*q)->next = NULL;
		q = &(*q)->next;
	}

	return copy_c;
}

RES *
fu_current_continuation()
{
	RES *c;

	c = NEW(RES);
	TIPO(c) = tipo_cont;
	VAL(c) = copy_cont(Current_Continuation);
	return c;
}

/* state handling */

#define QUEUE_SET_FRONT(C) {\
	assert(CONS_P(Continuation_Queue));\
	CAR(Continuation_Queue) = (RES *) (C);\
}

#define QUEUE_INIT() {\
	Continuation_Queue = fu_cons((RES *) Current_Continuation, NIL);\
	Last_Cont = &CDR(Continuation_Queue);\
}

#define QUEUE_NEXT() {\
	assert(CONS_P(Continuation_Queue));\
	*Last_Cont = Continuation_Queue;\
	Last_Cont = &CDR(*Last_Cont);\
	Continuation_Queue = CDR(Continuation_Queue);\
	Current_Continuation = (CONT *) CAR(Continuation_Queue);\
	*Last_Cont = NIL;\
}

#define QUEUE_ADD() {\
	Current_Continuation = NULL;\
	Continuation_Queue = fu_cons((RES *) Current_Continuation, Continuation_Queue);\
}

#define QUEUE_REMOVE() {\
	assert(CONS_P(Continuation_Queue));\
	Continuation_Queue = CDR(Continuation_Queue);\
	if (CONS_P(Continuation_Queue)) {\
		Current_Continuation = (CONT *) CAR(Continuation_Queue);\
	} else {\
		Last_Cont = &Continuation_Queue;\
	}\
}

/*CDR(*Last_Cont) = NIL;*/

#define PUSH_NEW_STATE(S) {\
	(S) = NEW(CONT);\
	(S)->next = Current_Continuation;\
	Current_Continuation = (S);\
	QUEUE_SET_FRONT(Current_Continuation);\
}

#define POP_STATE() {\
	Current_Continuation = Current_Continuation->next;\
	QUEUE_SET_FRONT(Current_Continuation);\
}

void init_state(closure, arguments, state, first_time)
	RES *closure, *arguments;
	CONT **state;
	char first_time;
{
	RES *formal_args;	/* formal arguments taken */

	if (first_time || (*state)->action != VM_TAIL_CALL) {
		PUSH_NEW_STATE(*state);
	}
			
	(*state)->clos = VAL_CLOSURE(closure);
	(*state)->val = NIL;
	(*state)->pc = 0;
	(*state)->prev_env = fu_env();
	(*state)->stack = NIL;
	(*state)->action = VM_CONTINUE;
	(*state)->function = NIL;
	(*state)->arguments = NIL;

	if (!CLOSURE_DYN((*state)->clos))
		fu_with_env(CLOSURE_ENVPADRE((*state)->clos));
	if (CLOSURE_NEWENV((*state)->clos))
		fu_push_env();

	(*state)->env = fu_env();

	formal_args = CLOSURE_ARGLIST((*state)->clos);
	bind_arguments(formal_args, arguments);
}

#define SET_CURRENT_CONTINUATION() {\
	state = Current_Continuation;\
	fu_with_env(state->env);\
}

/* Trampoline for fu_interpret_once
 * to be able to implement call/cc
 */
RES *
fu_interpret(c, args)
	RES *c, *args;
{
	RES *val;
	CONT *state;

	CONT *prev_current_continuation;
	RES *prev_continuation_queue;
	RES **prev_last_cont;

	unsigned cant_procesos;
	int quantum = QUANTUM;
	jmp_buf *prev_err_salto;

	if (Sem_Table == NULL)
		Sem_Table = fu_make_hash(fu_int(1));

	prev_continuation_queue = Continuation_Queue;
	prev_current_continuation = Current_Continuation;
	prev_last_cont = Last_Cont;

	/* Exception handling:
	 * if an exception is thrown during this instance
	 * of the intepreter, the previous continuation queue
	 * must be restored.
	 */
	prev_err_salto = Fu_Err_Salto;
	Fu_Err_Salto = NEW(jmp_buf);
	if ( setjmp(*Fu_Err_Salto) ) {
		Current_Continuation = prev_current_continuation;
		Continuation_Queue = prev_continuation_queue;
		Last_Cont = prev_last_cont;
		Fu_Err_Salto = prev_err_salto;

		fu_throw(Fu_Err_Excepcion, Fu_Err_Descripcion);
	}

	/* Interpreter follows */

	Current_Continuation = NULL;
	Continuation_Queue = NIL;

	QUEUE_INIT();

	cant_procesos = 1;
	init_state(c, args, &state, TRUE);

	while (TRUE) {
		assert(CONS_P(Continuation_Queue));
		assert((CONT *) CAR(Continuation_Queue) == Current_Continuation);
		assert(Current_Continuation == state);
		assert(*Last_Cont == NIL);

		if (cant_procesos > 1 && --quantum == 0) {
			/* must do the context switch */
			QUEUE_NEXT();
			SET_CURRENT_CONTINUATION();
			quantum = QUANTUM;
		}

		state->action = fu_interpret_once(state);

		switch (state->action) {
		case VM_TAIL_CALL:
			fu_with_env(state->prev_env);
			/* fall through! */
		case VM_CONTINUE: {
			++state->pc;

			while (TRUE) {
				if (TIPO_P(tipo_closure, state->function)) {
					init_state(state->function, state->arguments, &state,
									FALSE);
					break;
				} else if (TIPO_P(tipo_struct, state->function)) {
					RES *f = STYPE_GET_FUNCTION(VAL_STYPE(
								STRUCT_STYPE(VAL_STRUCT(state->function))));
					if (f != NIL) {
						/* agrego "self" como parametro y aplico */
						state->arguments = fu_cons(state->function,
													state->arguments);
						state->function = f;
						continue;
					}
					/* fall through */
				}
				/* si ninguna cosa funciono, llamo a fu_apply */
				state->val = fu_apply(state->function, state->arguments);
				break;
			}
			break;
		}
		case VM_SET_CC: {
			RES *arguments;

			if (!CONT_P(state->function))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
						fu_str("set/cont -- debe ser una continuation"));

			arguments = state->arguments;
			Current_Continuation = copy_cont(VAL_CONT(state->function));
			QUEUE_SET_FRONT(Current_Continuation);
			SET_CURRENT_CONTINUATION()
			state->val = arguments;
			STACK_PUSH(state->val);
			++state->pc;
			break;
		}
		case VM_SPAWN:
			++state->pc;
			++cant_procesos;
			QUEUE_ADD();
			init_state(state->function, state->arguments, &state, TRUE);
			quantum = QUANTUM;
			break;
		case VM_END:
			/* close */
			val = state->val;
			POP_STATE();
			if (Current_Continuation == NULL) {
				--cant_procesos;
				QUEUE_REMOVE();
				if (cant_procesos == 0) {
					fu_with_env(state->prev_env);
					goto fin_interpret;
				} else {
					SET_CURRENT_CONTINUATION();
					quantum = QUANTUM;
				}
			} else {
				fu_with_env(state->prev_env);
				state = Current_Continuation;
				state->val = val;
			}
			break;
		case VM_SEM_INIT: {
			unsigned s;
			unsigned l;

			if (!INT_P(state->arguments))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("sem-init -- debe ser un entero"));
			fu_set_hash_eq(Sem_Table, state->function, state->arguments);
			s = HASH_SIZE(Sem_Table);
			l = HASH_LEN(Sem_Table);
			if (s / l > 1)
				fu_rehash_eq(Sem_Table, fu_int(2 * l + 1));
			++state->pc;
			break;
		}
		case VM_SEM_P: {
			RES *r = fu_get_hash_eq(Sem_Table, state->function);
			int valor;

			if (!CONS_P(r))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("sem-p -- semaforo no inicializado"));

			valor = VAL_INT(CDR(r));
			if (valor > 0) {
				RES *tmp;

				fu_set_hash_eq(Sem_Table, state->function, fu_int(valor - 1));
				/* saco el nombre del semaforo de la pila
				 * y avanzo el pc */
				STACK_POP(tmp);
				++state->pc;
			} else {
				/* si no podemos hacer P, pasamos a otro proceso */
				quantum = 1;		/* force next process */
			}
			break;
		}
		case VM_SEM_V: {
			RES *r = fu_get_hash_eq(Sem_Table, state->function);
			int valor;

			if (!CONS_P(r))
				fu_throw(fu_exception_symbol("wrong-type-arg"),
							fu_str("sem-v -- semaforo no inicializado"));

			valor = VAL_INT(CDR(r));
			fu_set_hash_eq(Sem_Table, state->function, fu_int(valor + 1));
			++state->pc;
		}
		case VM_NEXT_PROCESS:
			quantum = 1;		/* force next process */
			break;	
		}
	}
fin_interpret:
	/* If everything goes ok, we must restore the previous
	 * values of the continuation queue and exception handler.
	 */
	Current_Continuation = prev_current_continuation;
	Continuation_Queue = prev_continuation_queue;
	Last_Cont = prev_last_cont;
	Fu_Err_Salto = prev_err_salto;
	return val;
}

#undef STACK_PUSH
#undef STACK_POP

void
fu_closure_print(c)
	RES *c;
{
	CLOSURE *clos = VAL_CLOSURE(c);
	/* codigo */
	BCODE **code = CLOSURE_CODEVEC(clos);
	unsigned pc;
	unsigned length = CLOSURE_LENGTH(clos);

	char *opcodes[] = {
		"NOP",
		"RET",
		"OP_CONST",
		"PUSH",
		"CONST_PUSH",
		"APPLY",
		"TAIL_APPLY",
		"JMP",
		"JNF",
		"JF",
		"DEF",
		"SET",
		"GET",
		"JMP_LABEL",
		"JNF_LABEL",
		"JF_LABEL",
		"LABEL",
		"CLOSE",
		"PUSH_ENV",
		"POP_ENV",
		"ENV",
		"SETTER",
		"NOT",
		"EQ",
		"OP_CAR",
		"OP_CDR",
		"OP_ENV_PADRE",
		"PRINT",
	};
 	/* tabla de constantes */
	printf("#<closure\n");
	printf("\targumentos: ");
	fu_print(CLOSURE_ARGLIST(clos));
	printf("\n");
	printf("\tconstantes: ");
	fu_print(CLOSURE_CONSTANTS(clos));
	printf("\n");

	for ( pc = 0; pc < length; pc++) {
		char op = BCODE_OPCODE(code[pc]);
		int opt = BCODE_OPT(code[pc]);

		printf("%i\t%s %i\n", pc, opcodes[(unsigned int) op], opt);
	}
	printf(">\n");
}

RES *
fu_eval(expr)
	RES *expr;
{
	return fu_interpret(FU_COMPILE(expr), NIL);
}

