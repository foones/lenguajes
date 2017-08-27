
module FreeVars(freeVariables) where

import Data.List(union)

import AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       )

unionMap :: Eq b => (a -> [b]) -> [a] -> [b]
unionMap f xs = foldr union [] (map f xs)

class FreeVariables a where
  freeVariables :: a -> [Id]

instance FreeVariables BlockT where
  freeVariables (Block stmts) = unionMap freeVariables stmts

instance FreeVariables StmtT where
  freeVariables (StmtAssign x expr) =
    [x] `union` freeVariables expr
  freeVariables (StmtVecAssign x expr1 expr2) =
    [x] `union` freeVariables expr1 `union` freeVariables expr2
  freeVariables (StmtIf expr block) =
    freeVariables expr `union` freeVariables block
  freeVariables (StmtIfElse expr block1 block2) =
    freeVariables expr `union` freeVariables block1
                       `union` freeVariables block2
  freeVariables (StmtWhile expr block) =
    freeVariables expr `union` freeVariables block
  freeVariables (StmtReturn expr) =
    freeVariables expr
  freeVariables (StmtCall _ exprs) =
    unionMap freeVariables exprs

instance FreeVariables ExprT where
  freeVariables (ExprVar x) = [x]
  freeVariables (ExprConstNum _) = []
  freeVariables (ExprConstBool _) = []
  freeVariables (ExprVecMake exprs) = unionMap freeVariables exprs
  freeVariables (ExprVecLength x) = [x]
  freeVariables (ExprVecDeref x expr) = [x] `union` freeVariables expr
  freeVariables (ExprCall _ exprs) = unionMap freeVariables exprs
  freeVariables (ExprAnd expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprOr expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprNot expr) = freeVariables expr
  freeVariables (ExprLe expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprGe expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprLt expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprGt expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprEq expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprNe expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprAdd expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprSub expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2
  freeVariables (ExprMul expr1 expr2) =
    freeVariables expr1 `union` freeVariables expr2

