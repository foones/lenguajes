module LumpenParser(program) where

import Control.Applicative

import Lumpen
import CombParser

binopList :: Parser Char Op -> Parser Char Expr -> Parser Char (Expr, [(Op, Expr)])
binopList op p = p /\ manyP (op /\ p)

leftBinop :: Parser Char Op -> Parser Char Expr -> Parser Char Expr
leftBinop op p = do
  (t, ts) <- binopList op p
  return (foldl (\ x (op, y) -> BinopE op x y) t ts)

rightBinop :: Parser Char Op -> Parser Char Expr -> Parser Char Expr
rightBinop op p = do
  (t, ts) <- binopList op p
  case ts of
    [] -> return t
    _  -> let ts' = zip (t : map snd ts) (map fst ts) in
            return (foldr (\ (x, op) y -> BinopE op x y) (snd (last ts)) ts')

unaryOp :: Parser Char Op -> Parser Char Expr -> Parser Char Expr
unaryOp op p = do
  ops <- manyP op
  exp <- p
  return (foldr UnaryE exp ops)

data OpType = LAssoc | RAssoc | Unary

optypeToCombop :: OpType -> Parser Char Op -> Parser Char Expr -> Parser Char Expr
optypeToCombop LAssoc = leftBinop
optypeToCombop RAssoc = rightBinop
optypeToCombop Unary  = unaryOp

operators :: [(OpType, [(Op, String)])]
operators = [
  (LAssoc, [(SeqOp, ";")]),
  (LAssoc, [(AssOp, ":=")]),
  (RAssoc, [(ConsOp, ":")]),
  (LAssoc, [(OrOp, "||")]),
  (LAssoc, [(AndOp, "&&")]),
  (Unary,  [(NotOp, "not")]),
  (LAssoc, [(EqOp, "=="), (NeOp, "/="), (NeOp, "!="), (LtOp, "<"), (LeOp, "<="), (GtOp, ">"), (GeOp, ">=")]),
  (LAssoc, [(AddOp, "+"), (SubOp, "-")]),
  (LAssoc, [(MulOp, "*"), (DivOp, "/"), (ModOp, "%")]),
  (RAssoc, [(PowOp, "^")]),
  (Unary,  [(NegOp, "-")]),
  (Unary,  [(NewOp, "new")])
  ]

-- Grammar
alpha    = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
digit    = "0123456789"
reserved = ["let", "rec", "in", "if", "then", "else", "true", "false", "new", "callcc"]
isIdent  = (`elem` alpha)
program  = expr //\ checkEOF
expr     = foldr comb iexpr operators
  where comb (optype, op) = optypeToCombop optype (anyop op)
        anyop = foldr1 (\./) . map (uncurry tokopr)
        tokopr op str
          | (str !! 0) `elem` alpha = const op <$> kw str
          | otherwise               = const op <$> tok str
iexpr    = app
app      = foldl1 AppE <$> many1P deref
deref    = (UnaryE DerefOp <$> (tok "!" /\\ deref))
       \./ atom
atom     = lam \./ letexp \./ letrec \./ ifte \./ boolean \./ callcc \./ var \./ number \./ unit \./ pexpr \./ list
unit     = const unitE <$> (tok "(" /\ tok ")")
pexpr    = mkexpr <$> (tok "(" /\\ expr /\ manyP (tok "," /\\ expr) //\ tok ")")
  where mkexpr (x, []) = x
        mkexpr (x, xs) = TupleE (x:xs)
list    = emptyList \./ nonEmptyList
  where emptyList = const (ListE []) <$> (tok "[" /\ tok "]")
        nonEmptyList = mklist <$> (tok "[" /\\ expr /\ manyP (tok "," /\\ expr) //\ tok "]")
        mklist (x, xs) = ListE (x:xs)
ignore   = manyPred (`elem` " \t\r\n")
ident    = do
  id <- tokof isIdent
  if id `elem` reserved
   then fail "reserved word not an identifier"
   else return id
idents   = many1P ident
number   = ConstE . IntC . read <$> tokof (`elem` digit)
var      = VarE <$> ident 
lam      = uncurry (flip (foldr LamE)) <$> (tok "\\" /\\ idents //\ tok "." /\ expr)
letexp   = mklet <$> (kw "let" /\\ idents //\ tok "=" /\ expr //\ kw "in" /\ expr)
  where mklet ((x:xs, a), b) = AppE (LamE x b) (foldr LamE a xs)
letrec   = mkletrec <$> (kw "let" /\\ kw "rec" /\\ idents //\ tok "=" /\ expr //\ kw "in" /\ expr)
  where mkletrec ((x:xs, a), b) = AppE (LamE x b) (AppE (VarE "%fix%") (LamE x (foldr LamE a xs)))
ifte     = mkifte <$> (kw "if" /\\ expr //\ kw "then" /\ expr //\ kw "else" /\ expr)
  where mkifte ((c, t), e) = IfE c t e
callcc   = CallccE <$> (kw "callcc" /\\ deref)
boolean  = (const (ConstE (BoolC True))  <$> kw "true")
       \./ (const (ConstE (BoolC False)) <$> kw "false")
tok      = wrap ignore . matchWord
tokof    = wrap ignore . many1Pred
kw x     = do
  id <- tokof isIdent
  if id == x
   then return id
   else fail ("expected " ++ show x)

