#define MAQMAXX	80
#define MAQMAXY 24

#define WRAP_TOROIDE	0
#define WRAP_REFLEJAR	1
#define WRAP_DOBLAR	2

#define EVAL_LAZY	0
#define EVAL_STRICT	1

#define OTRO_CERO	0
#define OTRO_NIL	1

typedef struct _maquina Maquina;
struct _maquina {
	char opcode[MAQMAXX][MAQMAXY];
	char wrap_mode;
	char eval_mode;
	char otro_mode;
};

/* PROTOS */
Objeto *mk_maquina(Maquina *maquina);
void limpiar_maquina(Maquina *m, unsigned *posx, unsigned *posy);
void poner_maquina(Maquina *m, unsigned *posx, unsigned *posy, char opcode);
void linea_maquina(Maquina *m, unsigned *posx, unsigned *posy);
Objeto *ejecutar_maquina(Maquina *m, Objeto *arg, Entorno *env);
