extern Res *lista_funciones;
extern Res *hash_actual;
extern Res *hash_global;
extern Res *const_actual;
extern Res *codigo_actual;
extern Res *cuenta_e_actual;	/* Cuenta de etiquetas */
extern Res *items;		/* Items de la lista */
extern Res *nombre_archivo;
extern Res *a_etiquetas;
extern Res *es_modulo;

extern FILE *yyin;

void ParserInicializar( );
void ParserCerear();
void ParserNuevaFun();
Res * ParserFinFun();
void ParserNuevoId( Res *nombre );
Num ParserNuevaConst( Res *c );
Num ParserNuevaEtiq();
Res *ParserEntorno();
