#include "Len.h"

#define M_SECUENCIA	0,	/*len*/		\
			0,	/*push*/	\
			0,	/*pop*/		\
			0,	/*get*/		\
			0,	/*set*/		\
			0	/*del*/		

#define M_CONS		0,	/*gethead*/	\
			0,	/*gettail*/	\
			0,	/*sethead*/	\
			0	/*settail*/

#define M_NUMERO	0,	/*suma*/	\
			0,	/*resta*/	\
			0,	/*mult*/	\
			0,	/*division*/	\
			0,	/*unaryminus*/	\
			0	/*modulo*/

#define M_BOOL		0,	/*or*/		\
			0,	/*and*/		\
			0,	/*xor*/		\
			0	/*not*/
			

Tipo IntTipo = {
	"entero",		/*nombre*/
	IntNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	IntCadena,		/*cadena*/
	IntComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	IntEval,		/*eval*/
	0,			/*anexar*/
	IntHash,		/*hash*/
	M_SECUENCIA,
	M_CONS,
	IntSuma,		/*suma*/
	IntResta,		/*resta*/
	IntMult,		/*mult*/
	IntDivision,		/*division*/
	IntMenosUnario,		/*unaryminus*/
	0,			/*modulo*/
	M_BOOL,
};

Tipo FloatTipo = {
	"flotante",		/*nombre*/
	FloatNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	FloatCadena,		/*cadena*/
	FloatComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	FloatEval,		/*eval*/
	0,			/*anexar*/
	FloatHash,		/*hash*/
	M_SECUENCIA,
	M_CONS,
	FloatSuma,		/*suma*/
	FloatResta,		/*resta*/
	FloatMult,		/*mult*/
	FloatDivision,		/*division*/
	FloatMenosUnario,	/*unaryminus*/
	0,			/*modulo*/
	M_BOOL,
};

Tipo UndefTipo = {
	"indefinido",		/*nombre*/
	ResNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	ResCadena,		/*cadena*/
	ResComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	ResEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	M_BOOL,
};

Tipo GlobalTipo = {
	"global",		/*nombre*/
	GlobalNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	ResCadena,		/*cadena*/
	ResComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	ResEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	M_BOOL,
};

Tipo CodigoTipo = {
	"codigo",		/*nombre*/
	CodigoNuevo,		/*constructor*/
	CodigoLiberar,		/*liberar*/
	CodigoEjecutar,		/*ejecutar*/
	ResCadena,		/*cadena*/
	CodigoComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	ResEval,		/*eval*/
	CodigoAgregar,		/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	M_BOOL,
};

Tipo AryTipo = {
	"array",		/*nombre*/
	AryNuevo,		/*constructor*/
	AryLiberar,		/*liberar*/
	0,			/*ejecutar*/
	AryCadena,		/*cadena*/
	AryComparar,		/*comparar*/
	AryCopiar,		/*copiar*/
	AryEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	AryLongitud,		/*len*/
	AryPush,		/*push*/
	AryPop,			/*pop*/
	AryGet,			/*get*/
	ArySet,			/*set*/
	AryDel,			/*del*/
	M_CONS,
	ArySuma,		/*suma*/
	0,			/*resta*/
	0,			/*mult*/
	0,			/*division*/
	0,			/*unaryminus*/
	0,			/*modulo*/
	M_BOOL,
};

Tipo HashTipo = {
	"hash",			/*nombre*/
	HashNuevo,		/*constructor*/
	HashLiberar,		/*liberar*/
	HashEjecutar,		/*ejecutar*/
	HashCadena,		/*cadena*/
	HashComparar,		/*comparar*/
	HashCopiar,		/*copiar*/
	HashEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	HashLongitud,		/*len*/
	HashPush,		/*push*/
	HashPop,		/*pop*/
	HashGet,		/*get*/
	HashSet,		/*set*/
	HashDel,		/*del*/
	HashGetHead,		/*gethead*/
	HashGetTail,		/*gettail*/
	HashSetHead,		/*sethead*/
	HashSetTail,		/*settail*/
	HashSumar,		/*suma*/
	HashRestar,		/*resta*/
	HashMult,		/*mult*/
	HashDivision,		/*division*/
	HashMenosUnario,	/*unaryminus*/
	0,			/*modulo*/
	HashOr,			/*or*/		
	HashAnd,		/*and*/		
	HashXor,		/*xor*/		
	HashNot,		/*not*/
};

Tipo BoolTipo = {
	"logico",		/*nombre*/
	BoolNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	BoolCadena,		/*cadena*/
	BoolComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	BoolEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	BoolOr,			/*or*/		
	BoolAnd,		/*and*/		
	BoolXor,		/*xor*/		
	BoolNot,		/*not*/

};

Tipo ConsTipo = {
	"cons",			/*nombre*/
	ConsNuevo,		/*constructor*/
	ConsLiberar,		/*liberar*/
	0,			/*ejecutar*/
	ConsCadena,		/*cadena*/
	ResComparar,		/*comparar*/
	ConsCopiar,		/*copiar*/
	ConsEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	ConsGetHead,		/*gethead*/
	ConsGetTail,		/*gettail*/
	ConsSetHead,		/*sethead*/
	ConsSetTail,		/*settail*/
	M_NUMERO,
	M_BOOL,
};

Tipo StrTipo = {
	"cadena",		/*nombre*/
	StrNuevo,		/*constructor*/
	StrLiberar,		/*liberar*/
	0,			/*ejecutar*/
	StrCadena,		/*cadena*/
	StrComparar,		/*comparar*/
	StrCopiar,		/*copiar*/
	StrEval,		/*eval*/
	0,			/*anexar*/
	StrHash,		/*hash*/
	StrLongitud,		/*len*/
	StrPush,		/*push*/
	StrPop,			/*pop*/
	StrGet,			/*get*/
	0,			/*set*/
	StrDel,			/*del*/
	M_CONS,
	StrSuma,		/*suma*/
	0,			/*resta*/
	0,			/*mult*/
	0,			/*division*/
	0,			/*unaryminus*/
	StrModulo,		/*modulo*/
	M_BOOL,
};

Tipo TipoTipo = {
	"tipo",			/*nombre*/
	TipoNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	ResCadena,		/*cadena*/
	ResComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	ResEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	M_BOOL,
};

Tipo FuncTipo = {
	"funcion",		/*nombre*/
	FuncNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	FuncEjecutar,		/*ejecutar*/
	ResCadena,		/*cadena*/
	ResComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	ResEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	M_BOOL,
};

Tipo FileTipo = {
	"archivo",		/*nombre*/
	FileNuevo,		/*constructor*/
	ResLiberar,		/*liberar*/
	0,			/*ejecutar*/
	ResCadena,		/*cadena*/
	ResComparar,		/*comparar*/
	ResCopiar,		/*copiar*/
	FileEval,		/*eval*/
	0,			/*anexar*/
	0,			/*hash*/
	M_SECUENCIA,
	M_CONS,
	M_NUMERO,
	M_BOOL,
};
