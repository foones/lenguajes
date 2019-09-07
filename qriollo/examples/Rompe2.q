
una Lista de a es
  bien Vacía
  bien : a [a]

un Quizá de a es
  bien Nada
  bien Solo a

la r0 de Ref de (Quizá de coso)  es Ref Nada
la r1 de Ref de (Quizá de Letra) es r0
la r2 de Ref de (Quizá de ())    es r0

la hacer de [()] en () dados xs da ()

el programa es
  hacer [
    PRIM.asignar r2 (Solo ()),
    mirar PRIM.desref r1
    si Nada   da ()
    si Solo x da PRIM.escupir_letra x
    boluda
  ]

