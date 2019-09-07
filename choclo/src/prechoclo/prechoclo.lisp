
(defvar *fdecl*)
(defvar *fcode*)
(defvar *builtin-bindings*)

;; los simbolos se compilan todos en minuscula

(defun fmt (&rest args)
 (apply #'format nil args))

(defun singlep (x)
 (and (consp x) (null (cdr x))))

(let ((id 0))
  (defun nuevo-id ()
   "Crea un identificador unico valido como nombre en C."
    (fmt "E_~A" (incf id))))

(defun cacheada (funcion)
 "Crea una funcion cacheada."
 (let ((tabla (make-hash-table :test #'equal)))
  (lambda (&rest args)
   (multiple-value-bind (resultado hallado) (gethash args tabla)
    (if hallado
     resultado
     (setf (gethash args tabla) (apply funcion args)))))))

(defmacro cachear (funcion)
 `(setf (fdefinition ',funcion) (cacheada #',funcion)))

(defun envolver-n (izq der cosa n)
 "Envuelve la cosa n veces entre los delimitadores izq y der."
 (if (zerop n)
  cosa
  (fmt "~A~A~A" izq (envolver-n izq der cosa (1- n)) der)))

(defun var-ref (envs variable)
  "Dada una pila de entornos y una variable, devuelve un par
  (pos-entorno pos-variable) que indica la posicion del entorno en la pila,
  y la posicion de la variable en el entorno.
  Precondicion: la variable debe estar ligada en la pila de entornos."
  (let ((pos-env (position variable envs :test #'member)))
    (if pos-env
      (let ((pos-var (position variable (nth pos-env envs) :test #'equal)))
	(if pos-var
	  (list pos-env pos-var)
	  (error "No declarado (error imposible): ~A~%" variable)))
      (error "No declarado: ~A~%" variable))))

(defun funcion (id cuerpo)
  "Imprime en los archivos pertinentes, las declaraciones y el de codigo
  de una funcion con el id otorgado y el cuerpo dado."
  (format *fdecl* "~&Ref *~A _PROTO((Ref *envs));~%" id)
  (format *fcode* "~&Ref *~A(envs) Ref *envs; {~A~&}~%" id cuerpo)
  id)

(defun nueva-funcion (formato &rest args)
 "Crea una nueva funcion en C con un identificador unico, con el
 cuerpo dado mediante la cadena de formato y sus argumentos.
 Devuelve el identificador creado."
 (funcion (nuevo-id) (apply #'fmt formato args)))

(defun expresion-de-numero (numero)
  "Crea una funcion que devuelve constantemente el numero dado."
  (nueva-funcion "return mk_int(~A);" numero))
;; esta cacheada
(cachear expresion-de-numero)

(defun expresion-de-cadena (cadena)
  "Crea una funcion que devuelve constantemente la cadena dada."
  (nueva-funcion "
   		 static Ref *q = NIL;
		 if (NULLP(q)) q = mk_string(~S);
		 return q;" cadena))
;; esta cacheada
(cachear expresion-de-numero)

(defun expresion-nil ()
  "Crea una funcion que devuelve constantemente NIL."
  (nueva-funcion "return NIL;"))
;; esta cacheada
(cachear expresion-nil)

(defun envolver-variable (pos-env pos-var)
  "Devuelve la expresion en C que retorna la variable con el
  binding `pos-var' para el `pos-env'-esimo entorno
  en la pila."
  (fmt "VAL_VECTOR(CAR(~A))[~A]"
       (envolver-n "CDR(" ")" "envs" pos-env)
       pos-var))

(defun var-ref-expr (envs variable)
 "Devuelve la expresion en C que retorna la variable en la pila
 de entornos.
 Precondicion: la variable debe estar ligada en la pila de entornos."
 (apply #'envolver-variable (var-ref envs variable)))

(defun expresion-de-variable (pos-env pos-var)
  "Crea una funcion que devuelve la variable con el
  binding `pos-var' para el `pos-env'-esimo entorno
  en la pila."
  (nueva-funcion "return ~A;" (envolver-variable pos-env pos-var)))
;; esta cacheada
(cachear expresion-de-variable)

(defun pre-compilar-do (envs expresiones)
 (if (singlep expresiones)
  (pre-compilar-expresion envs (first expresiones))
  (let ((expresiones-compiladas
	  (mapcar
	    #'(lambda (x)
		(pre-compilar-expresion envs x))
	    expresiones)))
    (nueva-funcion
      "
      Ref *r = NIL;
      ~{~&r = ~A(envs);~%~}
      return r;" expresiones-compiladas))))

(defun pre-compilar-fun (envs argumentos cuerpo)
  "Compila una definicion de funcion, en la pila de entornos
  dado (closure), con la lista de argumentos y el cuerpo pasados."
  (let ((nargs (length argumentos))
	(cuerpo-compilado (pre-compilar-do
			    (cons argumentos envs)
			    cuerpo)))
    (nueva-funcion
      "return mk_closure(envs, ~A, ~A);" nargs cuerpo-compilado)))

(defun pre-compilar-apply (envs functor args)
 "Compila una aplicacion de funcion comun (que no es una forma especial)."
  (let ((functor-compilado (pre-compilar-expresion envs functor))
	(args-compilados (mapcar #'(lambda (e)
				     (pre-compilar-expresion envs e))
				 args))
	(nargs (length args))) 
    (nueva-funcion
      "
      Ref *nuevo_entorno = mk_vector(~A);
      Ref **tabla = VAL_VECTOR(nuevo_entorno);

      ~{~&*(tabla++) = ~A(envs);~%~}
      return aplicar(~A(envs), nuevo_entorno, ~A);"
      nargs args-compilados functor-compilado nargs)))

(defun es-redex-p (aplicacion)
  "Devuelve verdadero sii la aplicacion dada es un redex; es decir
  si el functor es la definicion de una funcion anonima."
  (assert (consp aplicacion))
  (and (consp (car aplicacion)) (eq (caar aplicacion) 'fun)))

(defun pre-compilar-letrec (envs args valores cuerpo)
  "Compila un let recursivo."
  (assert (= (length args) (length valores)))
  (let ((nuevo-envs (cons args envs)))
    (let ((valores-compilados
	    (mapcar #'(lambda (e)
			(pre-compilar-expresion nuevo-envs e))
		    valores))
	  (cuerpo-compilado (pre-compilar-do nuevo-envs cuerpo))
	  (nargs (length args)))
      (nueva-funcion
	"
	Ref *nuevo_entorno = mk_vector(~A);
	Ref **tabla = VAL_VECTOR(nuevo_entorno);
	Ref *pila = mk_cons(nuevo_entorno, envs);

	~{~&*(tabla++) = ~A(pila);~%~}
	return ~A(pila);"
	nargs valores-compilados cuerpo-compilado))))

(defun redex (envs aplicacion)
  "Compila un redex."
  (let ((args (second (first aplicacion)))
	(cuerpo (cddr (first aplicacion)))
	(valores (rest aplicacion)))

    (assert (= (length args) (length valores)))
    (let ((valores-compilados
	    (mapcar #'(lambda (e)
			(pre-compilar-expresion envs e))
		    valores))
	  (cuerpo-compilado (pre-compilar-do (cons args envs) cuerpo))
	  (nargs (length args)))
      (nueva-funcion
	"
	Ref *nuevo_entorno = mk_vector(~A);
	Ref **tabla = VAL_VECTOR(nuevo_entorno);

	~{~&*(tabla++) = ~A(envs);~%~}
	return ~A(mk_cons(nuevo_entorno, envs));"
	nargs valores-compilados cuerpo-compilado))))

(defun pre-compilar-set (envs variable valor)
  "Compila una asignacion."
  (let ((valor-compilado (pre-compilar-expresion envs valor)))
    (nueva-funcion
      "return ~A = ~A(envs);"
      (var-ref-expr envs variable) valor-compilado)))

(defun pre-compilar-if (envs expresiones)
  "Compila un if."
  (let ((expresiones-compiladas
	  (mapcar #'(lambda (x) (pre-compilar-expresion envs x))
		  expresiones)))
    (nueva-funcion
      (with-output-to-string (s)
	(format s "~&Ref *res;~%")
	(do ((exprs expresiones-compiladas (cddr exprs)))
	    ((or (null exprs) (singlep exprs))
	     (if (null exprs)
	       (format s "res = NIL;")
	       (format s "res = ~A(envs);" (first exprs))))
	  (format s "~&if (!NULLP(~A(envs))) res = ~A(envs); else~%"
		  (first exprs) (second exprs)))
	(format s "~&return res;~%")))))

(defun quotificar (expresion)
 (cond
   ((null expresion) "NIL")
   ((symbolp expresion) (fmt "mk_simbolo(\"~A\")"
			 (string-downcase (symbol-name expresion))))
   ((numberp expresion) (fmt "mk_int(~A)" expresion))
   ((stringp expresion) (fmt "mk_string(~S)" expresion))
   ((consp expresion) (fmt "mk_cons(~A, ~A)"
			   (quotificar (car expresion))
			   (quotificar (cdr expresion))))))

(defun pre-compilar-quote (envs expresion)
 (nueva-funcion
  "
  static Ref *q = NIL;
  if (NULLP(q)) q = ~A;
  return q;" (quotificar expresion)))
(cachear pre-compilar-quote)

(defun pre-compilar-aplicacion (envs expresion)
  "Compila una expresion, sabiendo que no es un atomo.
  La expresion es la aplicacion de una funcion a unos argumentos
  (funcion builtin o closure), o una forma especial."
  (case (first expresion)
    ((fun)
     ;; es una funcion anonima
     (pre-compilar-fun envs (second expresion) (cddr expresion)))
    ((set)
     ;; es una asignacion
     (pre-compilar-set envs (second expresion) (third expresion)))
    ((do)
     ;; es una secuencia
     (pre-compilar-do envs (rest expresion)))
    ((if)
     ;; es un if
     (pre-compilar-if envs (rest expresion)))
    ((quote)
     ;; es un quote
     (pre-compilar-quote envs (second expresion)))
    ((letrec)
     ;; es un letrec
     (pre-compilar-letrec
       envs
       (mapcar #'car (second expresion))
       (mapcar #'cadr (second expresion))
       (cddr expresion)))
    (t
      ;; es la aplicacion de una funcion comun
      (if (es-redex-p expresion)
	(redex envs expresion)
	(pre-compilar-apply envs (first expresion) (rest expresion))))))

(defun pre-compilar-expresion (envs expresion)
 "Compila la expresion en la pila de entornos dada."
 (cond
  ((consp expresion) (pre-compilar-aplicacion envs expresion))
  (t
   (cond
    ((null expresion)
     (expresion-nil))
    ((numberp expresion)
     ;; es un numero
     (expresion-de-numero expresion))
    ((symbolp expresion)
     ;; es referencia a una variable
     (apply #'expresion-de-variable (var-ref envs expresion)))
    ((stringp expresion)
     (expresion-de-cadena expresion))
    (t (error "La expresion ~A no es compilable" expresion))))))
(cachear pre-compilar-expresion)

(defun builtin-env ()
  "Devuelve la pila de entornos inicial (con funciones builtin)."
  (list (mapcar #'car *builtin-bindings*)))

(defun create-builtin-env ()
  "Crea una nueva funcion en C que construye la pila de entornos inicial
  (con funciones builtin) y devuelve su nombre."
  (nueva-funcion
    "
    Ref *e = mk_vector(~A);
    Ref **tabla = VAL_VECTOR(e);
    ~:{*(tabla++) = mk_proc(~A, ~A);~}
    return mk_cons(e, envs);"
    (length *builtin-bindings*)
    (mapcar #'cdr *builtin-bindings*)))

(defun pre-compilar (expr bindings)
  "Compila una expresion, escribiendo las funciones C en los
  archivos decl.i (declaraciones) y code.i (codigo)."
  (let ((*builtin-bindings* bindings))
    (with-open-file (*fdecl* "decl.i" :direction :output)
      (with-open-file (*fcode* "code.i" :direction :output)
	(let ((id (pre-compilar-do (builtin-env) expr)))
	  (format *fcode*
		  "

		  int main(argc, argv) int argc; char **argv; {
		  Ref **p = &ARGS;
		  int i;

		  for (i = 1; i < argc; ++i)
		  	p = &CDR(*p = mk_cons(mk_string(argv[i]), *p));

		  ~A(~A(NIL));
		  return 0;
		  }~%"
		  id (create-builtin-env)))))))

(defun compilar-let (bindings cuerpo)
  `((fun ,(mapcar #'first bindings)
	 ,@(mapcar #'compilar cuerpo))
    . ,(mapcar
	 #'(lambda (x)
	     (compilar (second x)))
	 bindings)))

(defun compilar-with (var val cuerpo)
  `((fun (,var)
	 ,@(mapcar #'compilar cuerpo))
    ,(compilar val)))

(defun compilar (expr)
  (if (consp expr)
    (case (car expr)
      ((let) (compilar-let (second expr) (cddr expr)))
      ((with) (compilar-with (second expr) (third expr) (cdddr expr)))
      (t
	(mapcar #'compilar expr)))
    expr))

(setq bindings 
      '(
	;; choclo.c
	(cons 2 "mk_cons")
	(pr 1 "choclo_pr")
	(pr1 1 "choclo_pr1")
	(out 2 "choclo_out")
	(out1 2 "choclo_out1")
	(out-string 2 "choclo_out_string")
	(die 1 "choclo_die")
	(open 2 "choclo_open")
	(close 1 "choclo_close")
	(read 1 "choclo_read")
	(stdin 0 "choclo_stdin")
	(eq 2 "choclo_eq")
	(= 2 "choclo_int_eql")
	(+ 2 "choclo_int_add")
	(- 2 "choclo_int_sub")
	(* 2 "choclo_int_mul")
	(< 2 "choclo_int_lt")
	(> 2 "choclo_int_gt")
	(<= 2 "choclo_int_le")
	(>= 2 "choclo_int_ge")
	(car 1 "choclo_car")
	(cdr 1 "choclo_cdr")
	(consp 1 "choclo_consp")
	(intp 1 "choclo_intp")
	(symbolp 1 "choclo_symbolp")
	(stringp 1 "choclo_stringp")
	(symtab 0 "choclo_symtab")
        ;; prog.c
	(args 0 "choclo_args")
	))

(defun leer-todo (arch)
  (loop for expr = (read arch nil :eof)
	while (not (eq expr :eof))
	collecting expr))

(defun compilar-archivo (nombre bindings)
 "Compila el archivo <nombre>.ch a codigo en C, y luego, utilizando
 un compilador de C externo, a codigo nativo <nombre>[.exe]"
 (with-open-file (f (fmt "~A" nombre))
  (pre-compilar (mapcar #'compilar (leer-todo f)) bindings)))

(mapc #'(lambda (arch)
	 (compilar-archivo arch bindings))
	  *args*)

