#include "Len.h"

Res *
FileNuevo(pars)
	Res *pars;
{
	Res *p;

	p = ResNuevo(0);
	p->tipo = &FileTipo;
	p->valor.archivo = 0;
	return p;
}

Res *
FileOpen(q,modo)
	Res *q, *modo;
{
	Res *p;

	p = Nuevo(FileTipo);
	DEBE_SER(q,StrTipo);
	DEBE_SER(modo,StrTipo);
	p->valor.archivo = fopen( StrVal(q), StrVal(modo) );
	return p;
}

void
FileClose(p)
	Res *p;
{
	DEBE_SER(p,FileTipo);
	fclose(p->valor.archivo);
}

Res *
FileReadLine(p)
	Res *p;
{
	Res *z;
	char buf[STR_MAX] = "\0", *pos, car;
	FILE *arch;
	
	DEBE_SER(p,FileTipo);
	arch = p->valor.archivo;
	pos = &buf[0];
	car = getc( arch );
	if ( car == EOF )
		return Nuevo(BoolTipo);
	while (1) {
		if ( car == EOF ) {
			ungetc(car, arch);
			break;
		}
		*pos = car;
		pos++;
		if ( STR_MAX == pos - &buf[0] )
			break;
		if ( car == '\n')
			break;
		car = getc( arch );
	}
	pos = '\0';
	z = StrFromStr( buf );
	return z;
}

Res *
FileEval(p)
	Res *p;
{
	Res *z;

	z = Nuevo(BoolTipo);
	DEBE_SER(p,FileTipo);
	if ( p->valor.archivo )
		z->valor.puntero = VERDADERO;
	else
		z->valor.puntero = FALSO;
	return z;
}
