#include "Len.h"

Res *Nulo;
Res *Verdadero;
Res *Falso;
Res *Infinito;
Res *Fallo;
Res *Excepciones;

Res *ExClass;
Res *ExType;
Res *ExAttr;
Res *ExOper;
Res *ExSub;
Res *ExFunc;
Res *ExSeq;

Res *
ObjsExStr(argumentos,ambito)
	Res *argumentos, *ambito;
{
	Res *objeto, *final, *z;

	final = IntFromInt(-1);
	objeto = Pop(argumentos, final);
	Liberar(final);
	z = Get( objeto, StrFromStr("cadena") );
	return ;
}

void
ObjsInit()
{
	/* Creo objetos */
	Nulo = Nuevo( UndefTipo );
	Nulo->referencias = -1;
	
	Verdadero = Nuevo( BoolTipo );
	Verdadero->referencias = -1;
	
	Falso = Nuevo( BoolTipo );
	Falso->referencias = -1;
	
	Infinito = Nuevo( IntTipo );
	Infinito->referencias = -1;
	
	Fallo = Nuevo( UndefTipo );
	Fallo->referencias = -1;

	Excepciones = Nuevo( AryTipo );
	
	ExClass = Nuevo( HashTipo );
	{
		Res *ex_imp;

		ex_imp = Nuevo( FuncTipo );
		ex_imp->valor.func = &ObjsExStr;
		HASH_SET( ExClass, ex_imp, "__STR" );
	}
	
	ExType = Nuevo( HashTipo );
	HASH_SET( ExType, ExClass, "__CLASS" );
	HASH_SET( ExType, StrFromStr("ExType: tipo inválido"), "cadena" );

	ExAttr = Nuevo( HashTipo );
	HASH_SET( ExAttr, ExClass, "__CLASS" );
	HASH_SET( ExAttr, StrFromStr("ExAttr: atributo no referenciable"), "cadena" );

	ExOper = Nuevo( HashTipo );
	HASH_SET( ExOper, ExClass, "__CLASS" );
	HASH_SET( ExOper, StrFromStr("ExOper: operador no aplicable"), "cadena" );
	
	ExSub = Nuevo( HashTipo );
	HASH_SET( ExSub, ExClass, "__CLASS" );
	HASH_SET( ExSub, StrFromStr("ExSub: imposible subindicar"), "cadena" );

	ExFunc = Nuevo( HashTipo );
	HASH_SET( ExFunc, ExClass, "__CLASS" );
	HASH_SET( ExFunc, StrFromStr("ExFunc: módulo o función inaccesible"), "cadena" );

	ExSeq = Nuevo( HashTipo );
	HASH_SET( ExSeq, ExClass, "__CLASS" );
	HASH_SET( ExSeq, StrFromStr("ExSeq: el objeto no es una secuencia"), "cadena" );

}
