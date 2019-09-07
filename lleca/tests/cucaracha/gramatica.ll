/* Gramática del lenguaje Cucaracha */

programa
| /*EMPTY*/                       => Nil
| declaracion_de_funcion programa => Cons($1, $2)

declaracion_de_funcion
| "fun" ID parametros decl_tipo bloque => Function($2, $3, $4, $5)

decl_tipo
| /*EMPTY*/ => Unit
| ":" tipo  => $2

parametros
| "(" lista_parametros ")" => $2

lista_parametros
| /*EMPTY*/                           => Nil
| parametro lista_parametros_cont     => Cons($1, $2)

lista_parametros_cont
| /*EMPTY*/                           => Nil
| "," parametro lista_parametros_cont => Cons($2, $3)

parametro
| ID ":" tipo  => Parameter($1, $3)

tipo
| "Int"   => Int
| "Bool"  => Bool
| "Vec"   => Vec

bloque
| "{" lista_instrucciones "}"  => $2

lista_instrucciones
| /*EMPTY*/                       => Nil
| instruccion lista_instrucciones => Cons($1, $2)

instruccion
| ID instruccion_ID           => $2[ $1 ]
| "if" expresion bloque else  => StmtIf($2, $3, $4)
| "while" expresion bloque    => StmtWhile($2, $3)
| "return" expresion          => StmtReturn($2)

instruccion_ID
| ":=" expresion                   => StmtAssign(_, $2)
| "[" expresion "]" ":=" expresion => StmtVecAssign(_, $2, $5)
| "(" lista_expresiones ")"        => StmtCall(_, $2)

else
| /*EMPTY*/     => Nothing
| "else" bloque => Just($2)

lista_expresiones
| /*EMPTY*/                            => Nil
| expresion lista_expresiones_cont     => Cons($1, $2)

lista_expresiones_cont
| /*EMPTY*/                            => Nil
| "," expresion lista_expresiones_cont => Cons($2, $3)

expresion
| expresion_logica => $1

expresion_logica
| expresion_logica_atomica expresion_logica_op => $2[ $1 ]

expresion_logica_op
| /*EMPTY*/ => _
| "and" expresion_logica_atomica expresion_logica_op
  => $3[ ExprAnd(_, $2) ]
| "or" expresion_logica_atomica expresion_logica_op
  => $3[ ExprOr(_, $2) ]

expresion_logica_atomica
| expresion_relacional             => $1
| "not" expresion_logica_atomica   => ExprNot($2)

expresion_relacional
| expresion_aditiva expresion_relacional_op => $2[ $1 ]

expresion_relacional_op
| /*EMPTY*/              => _
| "<=" expresion_aditiva => ExprLe(_, $2)
| ">=" expresion_aditiva => ExprGe(_, $2)
| "<"  expresion_aditiva => ExprLt(_, $2)
| ">"  expresion_aditiva => ExprGt(_, $2)
| "==" expresion_aditiva => ExprEq(_, $2)
| "!=" expresion_aditiva => ExprNe(_, $2)

expresion_aditiva
| expresion_multiplicativa expresion_aditiva_op => $2[ $1 ]

expresion_aditiva_op
| /*EMPTY*/ => _
| "+" expresion_multiplicativa expresion_aditiva_op
   => $3[ExprAdd(_, $2)]
| "-" expresion_multiplicativa expresion_aditiva_op
   => $3[ExprSub(_, $2)]

expresion_multiplicativa
| expresion_atomica expresion_multiplicativa_op => $2[ $1 ]

expresion_multiplicativa_op
| /*EMPTY*/ => _
| "*" expresion_atomica expresion_multiplicativa_op
   => $3[ExprMul(_, $2)]

expresion_atomica
| ID expresion_atomica_ID   => $2[ $1 ]
| NUM                       => ExprConstNum($1)
| "True"                    => ExprConstBool(True)
| "False"                   => ExprConstBool(False)
| "[" lista_expresiones "]" => ExprVecMake($2)
| "#" ID                    => ExprVecLength($2)
| "(" expresion ")"         => $2

expresion_atomica_ID
| /*EMPTY*/                 => ExprVar(_)
| "[" expresion "]"         => ExprVecDeref(_, $2)
| "(" lista_expresiones ")" => ExprCall(_, $2)

