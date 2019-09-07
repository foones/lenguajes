#include "Len.h"

Res *
SeqSlice(p,in,fi)
	Res *p, *in, *fi;
{
	int i, pri, ult;
	Res *z, *final, *ind;

	pri = NumVal(in);
	ult = NumVal(fi);

	if ( pri < 0 ) pri = NumVal(Longitud(p)) + pri;
	if ( ult < 0 ) ult = NumVal(Longitud(p)) + ult + 1;
	final = IntFromInt(-1);
	z = Nuevo(*(p->tipo));
	
	if ( pri == ult )
		Push(z, Copiar(Get(p,in)), final);
	else
		for ( i = pri; i < ult; i++ ) {
			ind = IntFromInt(i);
			Push(z, Copiar(Get(p,ind)), final);
			Liberar(ind);
		}
	Liberar(final);
	return z;
}
