{
module Parser(Parser, parseProgram, parseQuery) where

import Lexer
import Lang

type Parser = String -> M Program
}

%name    parseProgramTokens
%partial parseQueryTokens    Query
%tokentype { Token }
%error { parseError }
%monad { M }

%token
    colon    { TokenColon }
    fun      { TokenFun }
    id       { TokenId $$ }
    lparen   { TokenLParen }
    rparen   { TokenRParen }
    comma    { TokenComma }
    dot      { TokenDot }
    proof    { TokenProof }
    ask      { TokenAsk }

%%

Program
    : {- empty -}         { [] }
    | Stmt dot Program    { $1 : $3 }

Query
    : {- empty -}     { [] }
    | Stmt OptDot     { [$1] }
    | Exp OptDot      { [AskType $1] }

OptDot
    : {- empty -} { () }
    | dot         { () }

Stmt
    : id colon Exp            { Assume $1 $3 }
    | id colon Exp proof Exp  { Prove $1 $3 $5 }
    | id proof Exp            { Define $1 $3 }
    | ask Exp                 { AskType $2 }
    | ask ask Exp             { AskValue $3 }

Exp
    : colon VarTypeList dot Exp       { foldr (uncurry Lam) $4 $2 }
    | fun Atom Exp                    { Lam "_" $2 $3 }
    | AppExp                          { $1 }

AppExp
    : Atom                          { $1 }
    | AppExp Atom                   { App $1 $2 }

VarTypeList
    : id AppExp                      { [($1, $2)] }
    | VarTypeList comma id AppExp    { $1 ++ [($3, $4)] }

Atom
    : id                  { Var $1 }
    | lparen Exp rparen   { $2 }

{

parseError :: [Token] -> M a
parseError msg = fail "parse error"

parseProgram :: Parser
parseProgram = parseProgramTokens . tokenize

parseQuery :: Parser
parseQuery = parseQueryTokens . tokenize

}
