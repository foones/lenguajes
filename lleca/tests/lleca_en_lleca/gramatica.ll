/* Gramática de Lleca escrita en Lleca */

grammar
| rule grammar => Cons($1, $2)
| /*EMPTY*/    => Nil

rule
| ID productions => Rule($1, $2)

productions
| /*EMPTY*/              => Nil
| production productions => Cons($1, $2)

production
| "|" expansion "=>" term => Production($2, $4)

expansion
| /*EMPTY*/        => Nil
| symbol expansion => Cons($1, $2)

symbol
| "ID"     => SymbolId
| "STRING" => SymbolString
| "NUM"    => SymbolNum
| STRING   => SymbolLiteral($1)
| ID       => SymbolNonterm($1)

term
| "_"                  => TermHole
| ID arguments         => TermNode($1, $2)
| STRING               => TermString($1)
| NUM                  => TermNum($1)
| "$" NUM substitution => TermParameter($1, $2)

arguments
| /*EMPTY*/       => Nil
| "(" arglist ")" => $2

arglist
| /*EMPTY*/     => Nil
| term arglistc => Cons($1, $2)

arglistc
| /*EMPTY*/         => Nil
| "," term arglistc => Cons($2, $3)

substitution
| /*EMPTY*/     => Nothing
| "[" term "]"  => Just($2)

