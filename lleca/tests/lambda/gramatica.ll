/* Gramática del cálculo lambda */

termino
| atomo argumentos            => $2[ $1 ]
| "\\" parametros "." termino => $2[ $4 ]

parametros
| ID parametros2 => Lam($1, $2)

parametros2
| /*EMPTY*/      => _
| ID parametros2 => Lam($1, $2)

argumentos
| /*EMPTY*/        => _
| atomo argumentos => $2[ App(_, $1) ]

atomo
| ID              => Var($1)
| "(" termino ")" => $2

