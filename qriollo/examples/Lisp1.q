enchufar Chamuyo

una Sexpr es
  bien Átomo Texto
  bien Cons Sexpr Sexpr

OJO. Read.

el entre de Letra en Letra en Letra en Posta
  dados x y z da
    letra_a_numerito x <= letra_a_numerito y &&
    letra_a_numerito y <= letra_a_numerito z

el constituyente de Letra en Posta
  dada letra da
    entre 'a' letra 'z' ||
    entre 'A' letra 'Z' ||
    entre '0' letra '9' ||
    entre '-' letra '-'

el leer_átomo de [Letra] en ([Letra], [Letra])
  dada [] da ([], [])
  dada (x : xs)
    si constituyente x
      da mirar leer_átomo xs
           si (átomo, cola) da (x : átomo, cola)
         boludo
    si no da ([], (x : xs))

el leer_lista de [Letra] en ([Sexpr], [Letra])
  dada [] da escupir "final temprano de la entrada\n";
             espichar 1
  dada (' ' : xs)  da leer_lista xs
  dada ('\t' : xs) da leer_lista xs
  dada ('\r' : xs) da leer_lista xs
  dada ('\n' : xs) da leer_lista xs
  dada (')' : xs) da ([], xs)
  dada (x : xs)   da
    mirar leer1 (x : xs)
      si (sexpr, cola) da
        mirar leer_lista cola
          si (lista, cola2) da
            (sexpr : lista, cola2)
        boludo
    boludo

el leer1 de [Letra] en (Sexpr, [Letra])
  dada [] da escupir "final temprano de la entrada\n";
             espichar 1
  dada (' ' : xs)  da leer1 xs
  dada ('\t' : xs) da leer1 xs
  dada ('\r' : xs) da leer1 xs
  dada ('\n' : xs) da leer1 xs
  dada ('(' : xs) da
    mirar leer_lista xs
      si (lista, cola) da
        (plegard Cons nil lista, cola)
    boludo
  dada (x : xs)
    si constituyente x da
      mirar leer_átomo (x : xs)
        si (átomo, cola) da (Átomo $ Texto átomo, cola)
      boludo
    si no da
      escupir "entrada deforme\n";
      espichar 1

el leer de Texto en Sexpr
  dado (Texto xs) da
    mirar leer1 xs
      si (sexpr, _) da sexpr
    boludo

OJO. Eval.

el letras_iguales de [Letra] en [Letra] en Posta
  dados []       []       da Sí
  dados []       (_ : _)  da No
  dados (_ : _)  []       da No
  dados (x : xs) (y : ys) da
    letra_a_numerito x == letra_a_numerito y &&
    letras_iguales xs ys

el texto_igual de Texto en Texto en Posta
  dados (Texto a) (Texto b) da letras_iguales a b

el buscar de Sexpr en Sexpr en Sexpr
  dados (Cons (Cons k v) e) a
     si eq k a da v
     si no     da buscar e a
  dados _ a da a

el nil de Sexpr es Átomo "nil"

el car de Sexpr en Sexpr
  dado (Átomo a)  da nil
  dado (Cons x y) da x

el cdr de Sexpr en Sexpr
  dado (Átomo a)  da nil
  dado (Cons x y) da y

el cadr de Sexpr en Sexpr
  es car . cdr

el cddr de Sexpr en Sexpr
  es cdr . cdr

el eq de Sexpr en Sexpr en Posta
  dados (Átomo a) (Átomo b)
     da texto_igual a b
  dados _ _ da No

el evaluar de Sexpr en Sexpr en Sexpr
  dados entorno (Átomo a)
     da buscar entorno (Átomo a)
  dados entorno (Cons x xs)
     si eq x (Átomo "quote") da
       car xs
     si eq x (Átomo "begin") da
        evaluar_bloque entorno xs
     si eq x (Átomo "lambda") da
       Cons (Átomo "#procedure")
            (Cons entorno xs)
     si eq x (Átomo "if") da
        mirar eq (evaluar entorno (car xs)) nil
          si Sí da evaluar_bloque entorno (cddr xs)
          si No da evaluar entorno (cadr xs)
        boludo
     si no da
       aplicar (evaluar entorno x)
               (evaluar_lista entorno (sexpr_lista xs))

el extender de [Sexpr] en [Sexpr] en Sexpr en Sexpr
  dados []       _  entorno da entorno
  dados (p : ps) [] entorno da entorno
  dados (p : ps) (a : as) entorno da
    Cons (Cons p a) (extender ps as entorno)

el aplicar de Sexpr en [Sexpr] en Sexpr
  dados (Cons (Átomo "#procedure")
              (Cons entorno
                    (Cons parámetros cuerpo)))
        argumentos
     da
        evaluar_bloque
          (extender
            (sexpr_lista parámetros)
            argumentos
            entorno)
          cuerpo
  dados _ _ da nil

el evaluar_bloque de Sexpr en Sexpr en Sexpr
  dados entorno (Átomo a) da Átomo a
  dados entorno (Cons a (Átomo _)) da
    evaluar entorno a
  dados entorno (Cons a (Cons b xs)) da
    evaluar entorno a;
    evaluar_bloque entorno (Cons b xs)

el evaluar_lista de Sexpr en [Sexpr] en [Sexpr]
  dados entorno [] da []
  dados entorno (sexpr : sexprs) da
    evaluar entorno sexpr : evaluar_lista entorno sexprs

el sexpr_lista_1 de Sexpr en ([Sexpr], Quizá de Texto)
  dados sexpr da rec [] sexpr
  donde
    el rec
      dados xs (Átomo "nil") da (xs, Nada)
      dados xs (Átomo a)     da (xs, Este a)
      dados xs (Cons x ys)   da rec (xs ++ [x]) ys
  boludo

el sexpr_lista de Sexpr en [Sexpr]
  dados sexpr da 
    mirar sexpr_lista_1 sexpr
      si (xs, _) da xs
    boludo

OJO. Print.

el juntar
  dados sep []           da ""
  dados sep [x]          da x
  dados sep (x : y : ys) da x ++ sep ++ juntar sep (y : ys)

encarnar Mostrable para Sexpr
  el mostrar
    dado (Átomo x)    da x
    dado (Cons e1 e2) da
      mirar sexpr_lista_1 (Cons e1 e2)
        si (xs, Nada) da
          "(" ++ juntar " " (fap mostrar xs) ++ ")"
        si (xs, Este a) da
          "(" ++ juntar " " (fap mostrar xs)
              ++ " . " ++ a ++ ")"
      boludo
boludo

OJO. Read + eval.

el ev de Texto en Sexpr
  es evaluar nil . leer

