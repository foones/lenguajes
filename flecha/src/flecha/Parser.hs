
module Parser where

import Data.Char(ord)

import Lexer(Token(..), tokenize)
import AST(Id(..), Program(..), Definition(..), Expr(..), CaseBranch(..))

type Parser a = [Token] -> (a, [Token])

parse :: String -> Program
parse xs = fst . parseProgram . tokenize $ xs

parseProgram :: Parser Program
parseProgram toks0 = let (defs, toks1) = rec toks0
                      in (Program defs, toks1)
  where
    rec []    = ([], [])
    rec toks0 = let (def, toks1)  = parseDefinition toks0
                    (defs, toks2) = rec toks1
                 in (def : defs, toks2)

parseDefinition :: Parser Definition
parseDefinition (T_DEF : T_LOWERID x : toks0) =
  let (ids, toks1)  = parseLoweridsDefeq toks0
      (expr, toks2) = parseExpr toks1
   in (Def x (foldr ExprLambda expr ids), toks2)

parseExpr :: Parser Expr
parseExpr toks0 =
  let (expr1, toks1) = parseOuterExpr toks0 in
    case toks1 of
      T_SEMICOLON : toks2 ->
        let (expr2, toks3) = parseExpr toks2 in
          (ExprLet "_" expr1 expr2, toks3)
      _ -> (expr1, toks1)

parseOuterExpr toks@(T_IF      : _) = parseIfThenElse toks
parseOuterExpr toks@(T_CASE    : _) = parseCase toks
parseOuterExpr toks@(T_LET     : _) = parseLet toks
parseOuterExpr toks@(T_LAMBDA  : _) = parseLambda toks
parseOuterExpr toks                 = parseInnerExpr toks

parseIfThenElse :: Parser Expr
parseIfThenElse (T_IF : toks0) = rec toks0
  where
    rec toks0 =
      let (cond, T_THEN : toks1)  = parseInnerExpr toks0
          (expr1, toks2) = parseInnerExpr toks1
          (expr2, toks3) = case toks2 of
                             (T_ELIF : toks2') -> rec toks2'
                             (T_ELSE : toks2') -> parseInnerExpr toks2'
        in (exprIfThenElse cond expr1 expr2, toks3)
    exprIfThenElse e1 e2 e3 =
      ExprCase e1 [
         CaseBranch "True" [] e2,
         CaseBranch "False" [] e3
       ]

parseCase :: Parser Expr
parseCase (T_CASE : toks0) =
  let (subject, toks1)  = parseInnerExpr toks0
      (branches, toks2) = parseCaseBranches toks1
   in (ExprCase subject branches, toks2)

parseCaseBranches :: Parser [CaseBranch]
parseCaseBranches (T_PIPE : toks0) =
  let (branch, toks1)   = parseCaseBranch toks0
      (branches, toks2) = parseCaseBranches toks1
   in (branch : branches, toks2)
parseCaseBranches toks = ([], toks)

parseCaseBranch :: Parser CaseBranch
parseCaseBranch (T_UPPERID c : toks0) =
    let (ids, toks1)  = parseLoweridsArrow toks0
        (expr, toks2) = parseInnerExpr toks1
     in (CaseBranch c ids expr, toks2)

parseLet :: Parser Expr
parseLet (T_LET : T_LOWERID x : toks0) =
    let (ids, toks1)          = parseLoweridsDefeq toks0
        (expr1, T_IN : toks2) = parseInnerExpr toks1
        (expr2, toks3)        = parseOuterExpr toks2
     in (ExprLet x (foldr ExprLambda expr1 ids) expr2, toks3)

parseLambda :: Parser Expr
parseLambda (T_LAMBDA : toks0) =
    let (ids, toks1)  = parseLoweridsArrow toks0
        (expr, toks2) = parseOuterExpr toks1
     in (foldr ExprLambda expr ids, toks2)

data Associativity = AssocLeft
                   | Prefix
                   | NonAssoc

type PrecedenceTable = [(Associativity, [(Token, String)])]

precedenceTable :: PrecedenceTable
precedenceTable = [
   (AssocLeft, [(T_OR, "OR")]),
   (AssocLeft, [(T_AND, "AND")]),
   (Prefix, [(T_NOT, "NOT")]),
   (NonAssoc, [
     (T_EQ, "EQ"), (T_NE, "NE"), (T_GE, "GE"),
     (T_LE, "LE"), (T_GT, "GT"), (T_LT, "LT")
   ]),
   (AssocLeft, [(T_PLUS, "ADD"), (T_MINUS, "SUB")]),
   (AssocLeft, [(T_TIMES, "MUL")]),
   (AssocLeft, [(T_DIV, "DIV"), (T_MOD, "MOD")]),
   (Prefix, [(T_MINUS, "UMINUS")])
 ]

parseInnerExpr :: Parser Expr
parseInnerExpr toks = parseLevels precedenceTable toks

parseLevels :: PrecedenceTable -> Parser Expr
parseLevels []                                toks0 = parseApplication toks0
parseLevels table@((AssocLeft, ops) : table') toks0 =
    let (expr1, toks1)   = parseLevels table' toks0
        (opExprs, toks2) = rec toks1
     in (foldl (\ a (o, b) -> ExprApply (ExprApply o a) b) expr1 opExprs, toks2)
  where
    rec :: Parser [(Expr, Expr)]
    rec []            = ([], [])
    rec (tok : toks0) =
      case lookup tok ops of
        Nothing   -> ([], tok : toks0)
        Just opId ->
          let (expr1, toks1)   = parseLevels table' toks0
              (opExprs, toks2) = rec toks1
           in ((ExprVar opId, expr1) : opExprs, toks2)
parseLevels table@((Prefix, ops)    : table') toks0 =
  case toks0 of
    [] -> parseLevels table' toks0
    (tok : toks1) ->
      case lookup tok ops of
        Nothing   -> parseLevels table' toks0
        Just opId ->
          let (expr1, toks2) = parseLevels table toks1
           in (ExprApply (ExprVar opId) expr1, toks2)
parseLevels table@((NonAssoc, ops)  : table') toks0 =
  let (expr1, toks1) = parseLevels table' toks0 in
    case toks1 of
      [] -> (expr1, toks1)
      (tok : toks2) ->
        case lookup tok ops of
          Nothing   -> (expr1, toks1)
          Just opId ->
            let (expr2, toks3) = parseLevels table' toks2
             in (ExprApply (ExprApply (ExprVar opId) expr1) expr2, toks3)

parseApplication :: Parser Expr
parseApplication toks0 =
  let (expr, toks1) = parseAtom toks0
      (exprs, toks2) = rec toks1
   in (foldl ExprApply expr exprs, toks2)
  where
    rec :: Parser [Expr]
    rec []    = ([], [])
    rec toks0@(tok : _) =
      if isFirstAtom tok 
       then let (expr, toks1)  = parseAtom toks0
                (exprs, toks2) = rec toks1
             in (expr : exprs, toks2)
       else ([], toks0)

isFirstAtom :: Token -> Bool
isFirstAtom (T_LOWERID _) = True
isFirstAtom (T_UPPERID _) = True
isFirstAtom (T_NUMBER  _) = True
isFirstAtom (T_CHAR    _) = True
isFirstAtom (T_STRING  _) = True
isFirstAtom T_LPAREN      = True
isFirstAtom _             = False

parseLoweridsArrow :: Parser [Id]
parseLoweridsArrow (T_LOWERID x : toks) =
  let (xs, toks') = parseLoweridsArrow toks in
    (x : xs, toks')
parseLoweridsArrow (T_ARROW : toks) = ([], toks)

parseLoweridsDefeq :: Parser [Id]
parseLoweridsDefeq (T_LOWERID x : toks) =
  let (xs, toks') = parseLoweridsDefeq toks in
    (x : xs, toks')
parseLoweridsDefeq (T_DEFEQ : toks) = ([], toks)

parseAtom :: Parser Expr
parseAtom (T_LOWERID x : toks) = (ExprVar x, toks)
parseAtom (T_UPPERID x : toks) = (ExprConstructor x, toks)
parseAtom (T_NUMBER n : toks)  = (ExprNumber n, toks)
parseAtom (T_CHAR c : toks)    = (ExprChar (ord c), toks)
parseAtom (T_STRING s : toks)  = (unfoldString s, toks)
  where unfoldString ""        = ExprConstructor "Nil"
        unfoldString (x : xs)  = ExprApply
                                   (ExprApply
                                       (ExprConstructor "Cons")
                                       (ExprChar (ord x)))
                                   (unfoldString xs)
parseAtom (T_LPAREN : toks0) =
  let (e, T_RPAREN : toks1) = parseExpr toks0 in
      (e, toks1)

