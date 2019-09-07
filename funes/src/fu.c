#include "Fu.h"
#include <stdlib.h>
#include <string.h>

void
fu_logo()
{
printf("\n\n");
printf("     **  ********   **   **   ***   **  ********    ******   **\n");
printf("   **     |*   |*   |*   |*   |**   |*   |*   |*   |*    **    **\n");
printf("  |*      |*__      |*   |*   |***  |*   |*__       **__        |*\n");
printf("  |*      |***      |*   |*   |* ** **   |***        *****      |*\n");
printf("  |*      |*        |*   |*   |*  ****   |*              **     |*\n");
printf("   **     |*        |*___|*   |*   |**   |*____|   **___**     **\n");
printf("     **  ****        *****    **    **  ********    *****    **\n");
printf("\n");
}

int
main(argc,argv)
	int argc;
	char *argv[];
{
	int interactiva;
	RES *input = NULL, *env_ant = NULL;

	fu_init();

	if (argc == 1) {
		interactiva = 1;
		fu_logo();
		input = fu_reader(Std_Input, TRUE);
	} else {
		interactiva = 0;
	}

	if (interactiva) {
		Fu_Err_Salto = NEW(jmp_buf);
		if ( setjmp(*Fu_Err_Salto) ) {
			fprintf(stderr, "Excepcion no cachada: ");
			fu_fprint(Std_Error, Fu_Err_Excepcion, T);
			fprintf(stderr, "\n");
			fu_fprint(Std_Error, Fu_Err_Descripcion, T);
			fprintf(stderr, "\n");
			fprintf(stderr, "\n** Error al cargar archivo "
							"de inicializacion **\n\n");
			exit(1);
		}
		/* carga archivo de inicializacion */
		fu_load_init_file();
	}

	Fu_Err_Salto = NEW(jmp_buf);
	if ( setjmp(*Fu_Err_Salto) ) {
		fprintf(stderr, "Excepcion no cachada: ");
		fu_fprint(Std_Error, Fu_Err_Excepcion, T);
		fprintf(stderr, "\n");
		fu_fprint(Std_Error, Fu_Err_Descripcion, T);
		fprintf(stderr, "\n");
		fu_with_env(env_ant);
		if (!interactiva)
				exit(1);
	}

	env_ant = fu_env();

	Fu_Argv = NIL;
	if ( interactiva ) {
		/* sin argumentos => REPL */

		while (1) {
				RES *r;

				printf("%s> ", VAL_PACKNAME(fu_current_package()));
				r = fu_read(input);

				//fu_closure_print(FU_COMPILE(fu_read(input)));
				fu_print(fu_eval(r));
				printf("\n");
		}
	} else {
		/* con argumentos => lee de archivo */
		RES *arch;
		FILE *f;
		unsigned i;

		if ( !(f = fopen(argv[1],"r"))) {
			fprintf(stderr, "no se puede leer del archivo %s\n", argv[1]);
			exit(1);
		}
		arch = fu_reader(fu_port(f), TRUE);

		/* Lee los argumentos */
		for (i = 2; i < argc; i++)
			Fu_Argv = fu_cons(fu_stread(fu_str(argv[i])), Fu_Argv);
		Fu_Argv = fu_xrev(Fu_Argv);

		/* carga archivo de inicializacion */
		fu_load_init_file();

		/* carga el archivo en cuestion */
		while (f) {
			RES *sexpr;
			sexpr = fu_read(arch);
			if (sexpr == EOF_OBJECT)
				exit(0);
			else
				fu_eval(sexpr);
				//fu_closure_print(FU_COMPILE(sexpr));
		}
		if (fclose(f)) {
			fprintf(stderr, "no se puede cerrar el archivo\n");
			exit(1);
		}
	}
	return 0;
}
