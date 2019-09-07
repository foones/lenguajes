-- vim:filetype=haskell:et:
{
module Parser where

import AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       )
import Lexer(Token(..))
}

%name parse
%tokentype { Token }
%error { parseError }

%left MUL

%token
  ID        { T_ID $$ }
  NUM       { T_NUM $$ }
  LPAREN    { T_LPAREN }
  RPAREN    { T_RPAREN }
  COMMA     { T_COMMA }
  LBRACK    { T_LBRACK }
  RBRACK    { T_RBRACK }
  LBRACE    { T_LBRACE }
  RBRACE    { T_RBRACE }
  ASSIGN    { T_ASSIGN }
  COLON     { T_COLON }
  HASH      { T_HASH }
  LE        { T_LE }
  GE        { T_GE }
  LT        { T_LT }
  GT        { T_GT }
  EQ        { T_EQ }
  NE        { T_NE }
  PLUS      { T_PLUS }
  MINUS     { T_MINUS }
  TIMES     { T_TIMES }
  BOOL      { T_BOOL }
  INT       { T_INT }
  VEC       { T_VEC }
  TRUE      { T_TRUE }
  FALSE     { T_FALSE }
  AND       { T_AND }
  ELSE      { T_ELSE }
  FUN       { T_FUN }
  IF        { T_IF }
  NOT       { T_NOT }
  OR        { T_OR }
  RETURN    { T_RETURN }
  WHILE     { T_WHILE }

%%

programa
  : {- empty -}                       { Program [] }
  | declaracion_de_funcion programa   { Program ($1 : unProgram $2) }

declaracion_de_funcion
  : FUN ID parametros bloque            { Function $2 Unit $3 $4 }
  | FUN ID parametros COLON tipo bloque { Function $2 $5 $3 $6 }

parametros
  : LPAREN lista_parametros RPAREN { $2 }

lista_parametros
  : {- empty -}                { [] }
  | lista_parametros_no_vacia  { $1 }

lista_parametros_no_vacia
  : parametro                                 { [$1] }
  | parametro COMMA lista_parametros_no_vacia { $1 : $3 }

parametro
  : ID COLON tipo  { Parameter $1 $3 }

tipo : INT  { Int }
     | BOOL { Bool }
     | VEC  { Vec }

bloque : LBRACE lista_instrucciones RBRACE { Block $2 }

lista_instrucciones
  : {- empty -}                     { [] }
  | instruccion lista_instrucciones { $1 : $2 }

instruccion
  : ID ASSIGN expresion                          { StmtAssign $1 $3 }
  | ID LBRACK expresion RBRACK ASSIGN expresion  { StmtVecAssign $1 $3 $6 }
  | IF expresion bloque                          { StmtIf $2 $3 }
  | IF expresion bloque ELSE bloque              { StmtIfElse $2 $3 $5 }
  | WHILE expresion bloque                       { StmtWhile $2 $3 }
  | RETURN expresion                             { StmtReturn $2 }
  | ID LPAREN lista_expresiones RPAREN           { StmtCall $1 $3 }

lista_expresiones
  : {- empty -}                { [] }
  | lista_expresiones_no_vacia { $1 }

lista_expresiones_no_vacia
  : expresion                                   { [$1] }
  | expresion COMMA lista_expresiones_no_vacia  { $1 : $3 }

expresion : expresion_logica { $1 }

expresion_logica
  : expresion_logica AND expresion_logica_atomica { ExprAnd $1 $3 }
  | expresion_logica OR expresion_logica_atomica  { ExprOr $1 $3 }
  | expresion_logica_atomica                      { $1 }

expresion_logica_atomica
  : NOT expresion_logica_atomica { ExprNot $2 }
  | expresion_relacional         { $1 }

expresion_relacional
  : expresion_aditiva LE expresion_aditiva  { ExprLe $1 $3 }
  | expresion_aditiva GE expresion_aditiva  { ExprGe $1 $3 }
  | expresion_aditiva LT expresion_aditiva  { ExprLt $1 $3 }
  | expresion_aditiva GT expresion_aditiva  { ExprGt $1 $3 }
  | expresion_aditiva EQ expresion_aditiva  { ExprEq $1 $3 }
  | expresion_aditiva NE expresion_aditiva  { ExprNe $1 $3 }
  | expresion_aditiva                       { $1 }

expresion_aditiva
  : expresion_aditiva PLUS expresion_multiplicativa  { ExprAdd $1 $3 }
  | expresion_aditiva MINUS expresion_multiplicativa { ExprSub $1 $3 }
  | expresion_multiplicativa                         { $1 }

expresion_multiplicativa
  : expresion_multiplicativa TIMES expresion_atomica { ExprMul $1 $3 }
  | expresion_atomica                                { $1 }

expresion_atomica
  : ID                                 { ExprVar $1 }
  | NUM                                { ExprConstNum $1 }
  | TRUE                               { ExprConstBool True }
  | FALSE                              { ExprConstBool False }
  | LBRACK lista_expresiones RBRACK    { ExprVecMake $2 }
  | HASH ID                            { ExprVecLength $2 }
  | ID LBRACK expresion RBRACK         { ExprVecDeref $1 $3 }
  | ID LPAREN lista_expresiones RPAREN { ExprCall $1 $3 }
  | LPAREN expresion RPAREN            { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
