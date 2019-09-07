#include "Len.h"

Res *
GlobalNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &GlobalTipo;
	return p;
}
