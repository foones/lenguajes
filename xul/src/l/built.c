#include "Len.h"

Res *
BinVer(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *z;

	z = IntFromInt(0);
	printf("\nLen - Versión 1.07\n");
	printf("Pablo Barenbaum - 2003\n");
	return z;
}

Res *
BinOpen(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *nombre, *modo, *final;

	final = IntFromInt(-1);
	modo = Pop(argumentos, final);
	nombre = Pop(argumentos, final);
	Liberar(final);
	return FileOpen( nombre, modo );
}

Res *
BinClose(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *archivo, *final;

	final = IntFromInt(-1);
	archivo = Pop(argumentos, final);
	Liberar(final);
	FileClose( archivo );
	return NULO;
}

Res *
BinReadLine(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *archivo, *final;

	final = IntFromInt(-1);
	archivo = Pop(argumentos, final);
	Liberar(final);
	return FileReadLine( archivo );
}

Res *
BinStr(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *objeto, *final;
	
	final = IntFromInt(-1);
	objeto = Pop(argumentos, final);
	Liberar(final);
	return Cadena( objeto );
}

Res *
BinClass(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *objeto, *final, *z;

	final = IntFromInt(-1);
	objeto = Pop(argumentos, final);
	Liberar(final);
	return Get(objeto, StrFromStr("__CLASS") );
}

Res *
BinLen(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *objeto, *final, *z;

	final = IntFromInt(-1);
	objeto = Pop(argumentos, final);
	Liberar(final);
	return Longitud(objeto);
}

Res *
BinAppend(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *array, *elemento, *final, *z;

	final = IntFromInt(-1);
	elemento = Pop(argumentos, final);
	array = Pop(argumentos, final);
	Push(array,elemento,final);
	Liberar(final);
	z = Nuevo( BoolTipo );
	z->valor.puntero = VERDADERO;
	return z;
}

Res *
BinInput(argumentos,ambito)
	Res *argumentos, *ambito;
{
	char cadena[STR_MAX], *p;

	fgets( cadena, STR_MAX, stdin );
	p = &cadena[strlen(cadena) - 1];
	if ( *p == '\n' )
		*p = '\0';
	return StrFromStr(cadena);
}

Res *
Builtins()
{
	Res *bi, *sy, *list, *principio, *punt;

	list = Nuevo( AryTipo );
	bi = Nuevo( HashTipo );			/* Built-in */
	sy = Nuevo( HashTipo );			/* Dentro de sys */
	principio = IntFromInt(0);

	/* Objetos */
	punt = ResNuevo( 0 );
	punt->valor.puntero = NULO;
	HASH_SET( bi, punt, "nulo" );
	punt = Nuevo( BoolTipo );
	punt->valor.puntero = VERDADERO;
	HASH_SET( bi, punt, "verdadero" );
	punt = Nuevo( BoolTipo );
	punt->valor.puntero = FALSO;
	HASH_SET( bi, punt, "falso" );
	punt = Nuevo( IntTipo );
	punt->valor.puntero = INFINITO;
	HASH_SET( bi, punt, "infinito" );
	punt = Nuevo( TipoTipo );
	/* Tipos */
	punt->valor.tipoptr = &IntTipo;
	HASH_SET( bi, punt, "Int" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &AryTipo;
	HASH_SET( bi, punt, "Array" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &StrTipo;
	HASH_SET( bi, punt, "Str" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &HashTipo;
	HASH_SET( bi, punt, "Hash" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &ConsTipo;
	HASH_SET( bi, punt, "Cons" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &BoolTipo;
	HASH_SET( bi, punt, "Bool" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &FileTipo;
	HASH_SET( bi, punt, "File" );
	punt = Nuevo( TipoTipo );
	punt->valor.tipoptr = &FloatTipo;
	HASH_SET( bi, punt, "Float" );
	/* Excepciones */
	HASH_SET( bi, ExType, "ExType" );
	HASH_SET( bi, ExAttr, "ExAttr" );
	HASH_SET( bi, ExOper, "ExOper" );
	HASH_SET( bi, ExSub,  "ExSub" );
	HASH_SET( bi, ExFunc, "ExFunc" );
	HASH_SET( bi, ExSeq,  "ExSeq" );
	/* sys */
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinVer;
	HASH_SET( sy, punt, "ver" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinOpen;
	HASH_SET( sy, punt, "open" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinClose;
	HASH_SET( sy, punt, "close" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinReadLine;
	HASH_SET( sy, punt, "read" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinAppend;
	HASH_SET( sy, punt, "append" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinStr;
	HASH_SET( sy, punt, "str" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinInput;
	HASH_SET( sy, punt, "input" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinClass;
	HASH_SET( sy, punt, "class" );
	punt = Nuevo( FuncTipo );
	punt->valor.func = &BinLen;
	HASH_SET( sy, punt, "len" );
	/**/
	/* Pongo sys adentro de los Built-ins */
	HASH_SET( bi, sy, "sys" );

	Push( list, bi, principio );
	Liberar( principio );
	return list;
}
