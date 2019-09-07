(let ((fdecl (open "decl.i" "w"))
      (fcode (open "code.i" "w")))

  (letrec

    (
     (null
       (fun (x) (eq x '())))

     (not
      (fun (x) (if x '() 't)))

     (assert
       (fun (expresion error)
	    (if (not expresion)
	      (die error))))

     (singlep
      (fun (x)
	   (if (consp x)
	    (if (null (cdr x))
	     't
	     '())
	    '())))

     (listp
       (fun (x)
	    (if
	      (consp x) 't
	      (null x) 't
	      '())))

     (first car)
     (rest cdr)
     (second (fun (x) (car (cdr x))))
     (third (fun (x) (car (cdr (cdr x)))))
     (caar (fun (x) (car (car x))))
     (cddr (fun (x) (cdr (cdr x))))
     (cdar (fun (x) (cdr (car x))))

     (map
      (fun (f l)
       (assert (listp l) "map: no es una lista")
       (if (null l)
	'()
	(cons (f (car l)) (map f (cdr l))))))

     (for-each
       (fun (f l)
	    (assert (listp l) "for-each: no es una lista")
	    (if (not (null l))
	      (do
		(f (car l))
		(map f (cdr l))))))

     (length
       (fun (l)
	    (assert (listp l) "length: no es una lista")
	    (if (null l)
	      0
	      (+ 1 (length (cdr l))))))

     (funcion
       (fun (id impresor-cuerpo)
	 (out1 fdecl "Ref *E_")
	 (out1 fdecl id)
	 (out fdecl " _PROTO((Ref *envs));")
	 (out1 fcode "Ref *E_")
	 (out1 fcode id)
	 (out fcode "(envs) Ref *envs; {")
	 (impresor-cuerpo fcode)
	 (out fcode "}")
	 id))

     (var-ref-en-env
      (fun (env variable pos-var)
       (assert (listp env) "var-ref-en-env: el entorno no es una lista")
       (if
	(null env)
	  '()
	(eq variable (car env))
	  pos-var
	(var-ref-en-env (cdr env) variable (+ pos-var 1)))))

     (var-ref
      (fun (envs variable pos-env)
       (assert (listp envs) "var-ref: la pila de entornos no es una lista")
       (if
	(null envs)
	  (die (cons "var-ref: variable no declarada" variable))
	(let ((p (var-ref-en-env (car envs) variable 0)))
	 (if (null p)
	  (var-ref (cdr envs) variable (+ pos-env 1))
	  (cons pos-env p))))))

     (nuevo-id
       (let ((id 0))
	 (fun ()
	      (set id (+ id 1)))))

     (nueva-funcion
       (fun (impresor-cuerpo)
	    (funcion (nuevo-id) impresor-cuerpo)))

     (expresion-nil
       (let ((fun-nil '()))
	 (fun ()
	      (if (not fun-nil)
		(set fun-nil
		     (nueva-funcion
		       (fun (stream)
			    (out stream "return NIL;"))))
		fun-nil))))

     (expresion-numero
       (fun (n)
	    (nueva-funcion
	      (fun (stream)
		   (out stream "static Ref *q = NIL;")
		   (out1 stream "if (NULLP(q)) q = mk_int(")
		   (out1 stream n)
		   (out stream ");")
		   (out stream "return q;")))))

     (expresion-cadena
       (fun (s)
	    (nueva-funcion
	      (fun (stream)
		   (out stream "static Ref *q = NIL;")
		   (out1 stream "if (NULLP(q)) q = mk_string(\"")
		   (out1 stream s)
		   (out stream "\");")
		   (out stream "return q;")))))

     (out-variable
       (fun (stream referencia)
	    (letrec ((repetir
		       (fun (cant cadena)
			    (if (not (= cant 0))
			      (do
				(out1 stream cadena)
				(repetir (- cant 1) cadena))))))

	      (out1 stream "VAL_VECTOR(CAR(")
	      (repetir (car referencia) "CDR(")
	      (out1 stream "envs")
	      (repetir (car referencia) ")")
	      (out1 stream "))[")
	      (out1 stream (cdr referencia))
	      (out1 stream "]"))))


     ;; no usadas:
     (list-eq
      (fun (cmp x y)
       (assert (listp x) "list-eq: x no es una lista")
       (assert (listp y) "list-eq: y no es una lista")
       (if
	(null x) (null y)
	(null y) '()
	(cmp (car x) (car y))
	   (list-eq cmp (cdr x) (cdr y))
	'())))
     (assoc
      (fun (lista elemento cmp)
       (assert (if (consp lista)
		(consp (car lista))
		(null lista))
	"assoc: no es una lista asociativa")
       (if
	(null lista)
	  '()
	(cmp (caar lista) elemento)
	   (car lista)
	(assoc (cdr lista) elemento cmp))))
     (acons
      (fun (clave valor alist)
       (cons (cons clave valor) alist)))

     (expresion-variable

;       (let ((expresiones '())
;	      (cmp (fun (x y) (list-eq = x y))))
;	  (fun (referencia)
;	   (assert (listp referencia)
;	    "expresion-variable: la referencia no es una lista")
;	   (let ((r (assoc expresiones referencia cmp)))
;	       (if r
;		 (cdr r)
;		 (cdar (set expresiones
;			   (acons
;			     referencia
;			     (nueva-funcion
;			       (fun (stream)
;				    (out1 stream "return ")
;				    (out-variable stream referencia)
;				    (out stream ";")))
;			     expresiones)))))))

	  (fun (referencia)
			   (nueva-funcion
				   (fun (stream)
					(out1 stream "return ")
					(out-variable stream referencia)
					(out stream ";"))))
	  )

     (pre-compilar-set
       (fun (envs variable valor)
	    (let ((valor-compilado (pre-compilar-expresion envs valor))
		  (referencia (var-ref envs variable 0)))
	      (nueva-funcion
		(fun (stream)
		     (out1 stream "return ")
		     (out-variable stream referencia)
		     (out1 stream " = E_")
		     (out1 stream valor-compilado)
		     (out stream "(envs);"))))))

     (pre-compilar-fun
       (fun (envs argumentos cuerpo)
	    (let ((nargs (length argumentos))
		  (cuerpo-compilado (pre-compilar-do
				      (cons argumentos envs)
				      cuerpo)))
	      (nueva-funcion
		(fun (stream)
		     (out1 stream "return mk_closure(envs, ")
		     (out1 stream nargs)
		     (out1 stream ", E_")
		     (out1 stream cuerpo-compilado)
		     (out stream ");"))))))

     (pre-compilar-do
      (fun (envs expresiones)
	   (if (singlep expresiones)
	     (pre-compilar-expresion envs (first expresiones))
	     (let ((expresiones-compiladas
		     (map (fun (x)
			   (pre-compilar-expresion envs x))
		       expresiones)))
	       (nueva-funcion
		(fun (stream)
		 (out stream "Ref *r = NIL;")
		 (for-each
		   (fun (x)
			(out1 stream "r = E_")
			(out1 stream x)
			(out stream "(envs);"))
		   expresiones-compiladas)
		 (out stream "return r;")))))))

     (pre-compilar-if
       (fun (envs expresiones)
	    (let ((expresiones-compiladas
		    (map (fun (x)
			  (pre-compilar-expresion envs x)) expresiones)))
	      (nueva-funcion
	       (fun (stream)
		  (out stream "Ref *res;")
		  (letrec ((loop
			    (fun (exprs)
			     (if
			       (null exprs)
			         (out stream "res = NIL;")
			       (singlep exprs)
			         (do
				   (out1 stream "res = E_")
				   (out1 stream (first exprs))
				   (out stream "(envs);"))
			       ; else
			         (do
				   (out1 stream "if (!NULLP(E_")
				   (out1 stream (first exprs))
				   (out1 stream "(envs))) res = E_")
				   (out1 stream (second exprs))
				   (out stream "(envs);")
				   (out stream "else ")
				   (loop (cddr exprs)))))))
		   (loop expresiones-compiladas))
		  (out stream "return res;"))))))

     (out-quote
       (fun (stream expresion)
	    (if
	      (null expresion)
	        (out stream "NIL")
	      (symbolp expresion)
	       (do
		 (out1 stream "mk_simbolo(\"")
		 (out1 stream expresion)
		 (out stream "\")"))
	      (intp expresion)
	       (do
		 (out1 stream "mk_int(")
		 (out1 stream expresion)
		 (out stream ")"))
	      (stringp expresion)
	       (do
		 (out1 stream "mk_string(")
		 (out-string stream expresion)
		 (out stream ")"))
	      (consp expresion)
	       (do
		 (out1 stream "mk_cons(")
		 (out-quote stream (car expresion))
		 (out1 stream " , ")
		 (out-quote stream (cdr expresion))
		 (out stream ")"))
	      ; else
	       (die "out-quote: expresion no quoteable"))))

     (pre-compilar-quote
       (fun (envs expresion)
	    (if (null expresion)
	      (expresion-nil)
	      (nueva-funcion
		(fun (stream)
		     (out stream "static Ref *q = NIL;")
		     (out1 stream "if (NULLP(q)) q = ")
		     (out-quote stream expresion)
		     (out stream ";")
		     (out stream "return q;"))))))

     (pre-compilar-letrec
       (fun (envs args valores cuerpo)
	    (assert (= (length args) (length valores))
		    "pre-compilar-letrec: no hay igual cantidad de valores/argumentos")
	    (let ((nuevo-envs (cons args envs)))
	      (let ((valores-compilados
		      (map (fun (e) (pre-compilar-expresion nuevo-envs e)) valores))
		    (cuerpo-compilado (pre-compilar-do nuevo-envs cuerpo))
		    (nargs (length args)))
		(nueva-funcion
		  (fun (stream)
		       (out1 stream "Ref *nuevo_entorno = mk_vector(")
		       (out1 stream nargs)
		       (out stream ");")
		       (out stream "Ref **tabla = VAL_VECTOR(nuevo_entorno);")
		       (out stream "Ref *pila = mk_cons(nuevo_entorno, envs);")
		       (for-each
			 (fun (x)
			      (out1 stream "*(tabla++) = E_")
			      (out1 stream x)
			      (out stream "(pila);"))
			 valores-compilados)
		       (out1 stream "return E_")
		       (out1 stream cuerpo-compilado)
		       (out stream "(pila);")))))))

     (pre-compilar-let ; era: redex
       (fun (envs args valores cuerpo)
	    (assert (= (length args) (length valores))
		    "pre-compilar-let: no hay igual cantidad de valores/argumentos")
	    (let ((valores-compilados
		    (map (fun (e) (pre-compilar-expresion envs e)) valores))
		  (cuerpo-compilado (pre-compilar-do (cons args envs) cuerpo))
		  (nargs (length args)))
	      (nueva-funcion
		(fun (stream)
		     (out1 stream "Ref *nuevo_entorno = mk_vector(")
		     (out1 stream nargs)
		     (out stream ");")
		     (out stream "Ref **tabla = VAL_VECTOR(nuevo_entorno);")
		     (for-each
		       (fun (x)
			    (out1 stream "*(tabla++) = E_")
			    (out1 stream x)
			    (out stream "(envs);"))
		       valores-compilados)
		     (out1 stream "return E_")
		     (out1 stream cuerpo-compilado)
		     (out stream "(mk_cons(nuevo_entorno, envs));"))))))

     (es-redex-p
	 (fun (aplicacion)
	  (assert (consp aplicacion) "es-redex-p: no es una aplicacion")
	  (if (consp (car aplicacion))
	     (if (eq (caar aplicacion) 'fun)
	      't
	      '())
	     '())))

     (pre-compilar-apply
       (fun (envs functor args)
	    (let ((functor-compilado (pre-compilar-expresion envs functor))
	      (args-compilados (map (fun (e)
	    			     (pre-compilar-expresion envs e))
	    			args))
	     (nargs (length args)))
	    (nueva-funcion
	      (fun (stream)
		   (out1 stream "Ref *nuevo_entorno = mk_vector(")
		   (out1 stream nargs)
		   (out stream ");")
		   (out stream "Ref **tabla = VAL_VECTOR(nuevo_entorno);")
		   (for-each
		     (fun (x)
		   	    (out1 stream "*(tabla++) = E_")
		   	    (out1 stream x)
		   	    (out stream "(envs);"))
		     args-compilados)
		   (out1 stream "return aplicar(E_")
		   (out1 stream functor-compilado)
		   (out1 stream "(envs), ")
		   (out1 stream " nuevo_entorno, ")
		   (out1 stream nargs)
		   (out stream ");"))))))

     (pre-compilar-aplicacion
      (fun (envs expresion)
       (if
	(eq (car expresion) 'fun)
	  (pre-compilar-fun envs (second expresion) (cddr expresion))
	(eq (car expresion) 'set)
          (pre-compilar-set envs (second expresion) (third expresion))
	(eq (car expresion) 'do)
          (pre-compilar-do envs (rest expresion))
	(eq (car expresion) 'if)
          (pre-compilar-if envs (rest expresion))
	(eq (car expresion) 'quote)
          (pre-compilar-quote envs (second expresion))
	(eq (car expresion) 'let)
          (pre-compilar-let envs
	   (map first (second expresion))
	   (map second (second expresion))
	   (cddr expresion))
	(eq (car expresion) 'letrec)
          (pre-compilar-letrec envs
	   (map first (second expresion))
	   (map second (second expresion))
	   (cddr expresion))
	(es-redex-p expresion)
	  (pre-compilar-let envs
	    (second (first expresion)) ; args
	    (rest expresion) ; valores
	    (cddr (first expresion))) ; cuerpo
        ; else
	  (pre-compilar-apply envs (first expresion) (rest expresion)))))

     (pre-compilar-expresion
       (fun (envs expresion)
	    (if
	      (consp expresion)
	        (pre-compilar-aplicacion envs expresion)
	      (null expresion)
	        (expresion-nil)
	      (intp expresion)
	        (expresion-numero expresion)
	      (symbolp expresion)
	        (expresion-variable (var-ref envs expresion 0))
	      (stringp expresion)
	        (expresion-cadena expresion)
              ; else
	      (die (cons
		    "pre-compilar-expresion: la expresion no es compilable "
		    expresion)))))

     (create-builtin-env
       (fun (bindings)
	    (nueva-funcion
	      (fun (stream)
		   (out1 stream "Ref *e = mk_vector(")
		   (out1 stream (length bindings))
		   (out stream ");")
		   (out stream "Ref **tabla = VAL_VECTOR(e);")
		   (for-each
		     (fun (x)
			  (out1 stream "*(tabla++) = mk_proc(")
			  (out1 stream (first x))
			  (out1 stream ", ")
			  (out1 stream (second x))
			  (out stream ");"))
		     (map cdr bindings))
		   (out stream "return mk_cons(e, envs);")))))

     (builtin-env
       (fun (bindings) (cons (map car bindings) '())))

     (pre-compilar
      (fun (exprs bindings)
	(let ((cuerpo-compilado (pre-compilar-do (builtin-env bindings) exprs))
	      (builtin-env (create-builtin-env bindings)))
	  (out fcode "int main(argc, argv) int argc; char **argv; {")
	  (out fcode "Ref **p = &ARGS;")
	  (out fcode "int i;")
	  (out fcode "for (i = 1; i < argc; ++i)")
	  (out fcode "        p = &CDR(*p = mk_cons(mk_string(argv[i]), *p));")
	  (out1 fcode "E_")
	  (out1 fcode cuerpo-compilado)
	  (out1 fcode "(E_")
	  (out1 fcode builtin-env)
	  (out fcode "(NIL));")
	  (out fcode "return 0;")
	  (out fcode "}"))))

     (pre-compilar-archivo
       (fun (arch bindings)
	    (let ((entrada (open arch "r")))
	     (let ((r (read entrada)))
	      (pre-compilar (cons r '()) bindings)
	      (close entrada)))))
  )

    (let ((bindings 
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
	      )))

      (assert (singlep (args)) "Uso: pochoclo <archivo>")
      (pre-compilar-archivo (first (args)) bindings)

      )

    (close fdecl)
    (close fcode)))
