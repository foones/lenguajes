#include <stdlib.h>
#include <stdio.h>
#include <setjmp.h>

/* principal */
#include "funes.h"

#include <gc.h>

/* objetos */
#include "list.h"
#include "hash.h"
#include "vec.h"
#include "cproc.h"
#include "sym.h"
#include "env.h"
#include "port.h"
#include "str.h"
#include "int.h"
#include "char.h"
#include "tipo.h"
#include "struct.h"
#include "float.h"

/* funciones */
#include "special.h"
#include "out.h"
#include "std.h"
#include "built.h"
#include "macro.h"
#include "setter.h"

/* vm y compilador */
#include "vm.h"
#include "op.h"
#include "comp.h"
#include "inline.h"

/* errores */
#include "error.h"
