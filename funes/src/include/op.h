enum {
	NOP = 0,
	RET,
	OP_CONST,
	PUSH,
	CONST_PUSH,
	APPLY,
	TAIL_APPLY,		/* apply para tail calls */
	APPLY_CALL,		/* apply con resto */
	JMP,
	JNF,
	JF,
	DEF,
	SET,
	GET,
	JMP_LABEL,		/* salta */
	JNF_LABEL,		/* salta si no NIL */
	JF_LABEL,		/* salta si NIL */
	LABEL,
	CLOSE,			/* cierra la closure en el entorno actual */
	PUSH_ENV,
	POP_ENV,
	ENV,
	SETTER,			/* obtiene el setter asociado a un accessor */
	NOT,
	EQ,
	OP_CAR,
	OP_CDR,
	CALL_CC,		/* call with current continuation */
	SET_CC,			/* invoke a continuation */
	SPAWN,			/* spawn a process */
	SEM_SET,		/* set a semaphore */
	PRINT,
};

enum {
	SEM_init = 0,
	SEM_p,
	SEM_v,
	SEM_val,
	SEM_set,
};

