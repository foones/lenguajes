{-# LANGUAGE FlexibleInstances #-}

module Serializer(serialize) where

import AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       )

serialize :: ProgramT -> String
serialize = show . tree

data Tree = Leaf String
          | Node String [Tree]

instance Show Tree where
  show t = rec 0 t
    where
      sep n = concat . take n . repeat $ "  "
      rec n (Leaf s) = sep n ++ s ++ "\n"
      rec n (Node name children) =
        sep n ++ "(" ++ name ++ "\n" ++
        concat (map (rec (n + 1)) children) ++
        sep n ++ ")" ++ "\n"

class Treelike a where
  tree :: a -> Tree

instance Treelike String where
  tree = Leaf

instance Treelike Bool where
  tree = Leaf . show

instance Treelike Integer where
  tree = Leaf . show

instance Treelike Type where
  tree = Leaf . show

instance Treelike ProgramT where
  tree (Program functions) =
    Node "Program" (map tree functions)

instance Treelike FunctionT where
  tree (Function name typ params block) =
    Node "Function" ([tree name, tree typ] ++ map tree params ++ [tree block])

instance Treelike BlockT where
  tree (Block statements) =
    Node "Block" (map tree statements)

instance Treelike ParameterT where
  tree (Parameter name typ) =
    Node "Parameter" [tree name, tree typ]

instance Treelike StmtT where
  tree (StmtAssign id expr) =
    Node "StmtAssign" [tree id, tree expr]
  tree (StmtVecAssign id expr1 expr2) =
    Node "StmtVecAssign" [tree id, tree expr1, tree expr2]
  tree (StmtIf expr block) =
    Node "StmtIf" [tree expr, tree block]
  tree (StmtIfElse expr block1 block2) =
    Node "StmtIfElse" [tree expr, tree block1, tree block2]
  tree (StmtWhile expr block) =
    Node "StmtWhile" [tree expr, tree block]
  tree (StmtReturn expr) =
    Node "StmtReturn" [tree expr]
  tree (StmtCall id exprs) =
    Node "StmtCall" (tree id : map tree exprs)

instance Treelike ExprT where
  tree (ExprVar id) =
    Node "ExprVar" [tree id]
  tree (ExprConstNum n) =
    Node "ExprConstNum" [tree n]
  tree (ExprConstBool b) =
    Node "ExprConstBool" [tree b]
  tree (ExprVecMake exprs) =
    Node "ExprVecMake" (map tree exprs)
  tree (ExprVecLength id) =
    Node "ExprVecLength" [tree id]
  tree (ExprVecDeref id expr) =
    Node "ExprVecDeref" [tree id, tree expr]
  tree (ExprCall id exprs) =
    Node "ExprCall" (tree id : map tree exprs)
  tree (ExprAnd expr1 expr2) =
    Node "ExprAnd" [tree expr1, tree expr2]
  tree (ExprOr expr1 expr2) =
    Node "ExprOr" [tree expr1, tree expr2]
  tree (ExprNot expr) =
    Node "ExprNot" [tree expr]
  tree (ExprLe expr1 expr2) =
    Node "ExprLe" [tree expr1, tree expr2]
  tree (ExprGe expr1 expr2) =
    Node "ExprGe" [tree expr1, tree expr2]
  tree (ExprLt expr1 expr2) =
    Node "ExprLt" [tree expr1, tree expr2]
  tree (ExprGt expr1 expr2) =
    Node "ExprGt" [tree expr1, tree expr2]
  tree (ExprEq expr1 expr2) =
    Node "ExprEq" [tree expr1, tree expr2]
  tree (ExprNe expr1 expr2) =
    Node "ExprNe" [tree expr1, tree expr2]
  tree (ExprAdd expr1 expr2) =
    Node "ExprAdd" [tree expr1, tree expr2]
  tree (ExprSub expr1 expr2) =
    Node "ExprSub" [tree expr1, tree expr2]
  tree (ExprMul expr1 expr2) =
    Node "ExprMul" [tree expr1, tree expr2]

