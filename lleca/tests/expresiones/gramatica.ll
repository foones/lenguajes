
/* Gramática de expresiones aritméticas
 * con operadores
 *    +
 *    -
 *    *
 *    div
 *    mod
 * y paréntesis,
 * respetando la precedencia y asociatividad. */

expresion
| termino expresion1 => $2[$1]

expresion1
| /*EMPTY*/              => _
| "+" termino expresion1 => $3[ add(_, $2) ]
| "-" termino expresion1 => $3[ sub(_, $2) ]

termino
| factor termino1 => $2[$1]

termino1
| /*EMPTY*/             => _
| "*" factor termino1   => $3[ mul(_, $2) ]
| "div" factor termino1 => $3[ div(_, $2) ]
| "mod" factor termino1 => $3[ mod(_, $2) ]

factor
| NUM               => $1
| "(" expresion ")" => $2

