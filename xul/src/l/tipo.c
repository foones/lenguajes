#include "Len.h"

Res *
TipoNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &TipoTipo;
	p->valor.tipoptr = 0;
	return p;
}

Res *
TipoHacer( Res *p )
{
	Res *z;
	
	DEBE_SER(p,TipoTipo);
	z = Nuevo( *(p->valor.tipoptr) );
	return z;
}
