#include "choclo.h"

Ref *ARGS = NIL;

Ref *choclo_args() {
	return ARGS;
}

#include "decl.i"
#include "code.i"
