
module Syntax(Id, Expr(..), Declaration(..), freeVariables, freeVariablesM, splitArgs) where

import qualified Data.Set as S

type Id = String

data Expr = Type
          | Pi Id Expr Expr
          | Var Id
          | Lam Id (Maybe Expr) Expr
          | App Expr Expr
          | Hole String
  deriving Show

data Declaration = DeclAxiom Id Expr
                 | DeclDef Id Expr Expr
                 | DeclCheck Expr
                 | DeclRewrite [(Id, Expr)] Expr Expr
  deriving Show

freeVariables :: Expr -> S.Set Id
freeVariables Type          = S.empty
freeVariables (Pi x e1 e2)  = freeVariables e1 `S.union`
                              (freeVariables e2 S.\\ S.singleton x)
freeVariables (Var x)       = S.singleton x
freeVariables (Lam x e1 e2) = freeVariablesM e1 `S.union`
                              (freeVariables e2 S.\\ S.singleton x)
freeVariables (App e1 e2)   = freeVariables e1 `S.union` freeVariables e2

freeVariablesM :: Maybe Expr -> S.Set Id
freeVariablesM Nothing     = S.empty
freeVariablesM (Just expr) = freeVariables expr

splitArgs :: Expr -> (Expr, [Expr])
splitArgs (App fun arg) =
  let (head, args) = splitArgs fun in
    (head, args ++ [arg])
splitArgs expr = (expr, [])

