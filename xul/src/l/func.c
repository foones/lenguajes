#include "Len.h"

Res *
FuncNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &FuncTipo;
	p->valor.func = 0;
	return p;
}

Res *
FuncEjecutar(f, entorno, argumentos, ambito)
	Res *f, *entorno, *argumentos, *ambito;
{
	DEBE_SER(f,FuncTipo);
	return f->valor.func( argumentos, ambito );
}
