module Lumpen(Id, Op(..), Constant(..), Expr(..), unitE) where

type Id = String

data Op = SeqOp
        | AssOp
        | ConsOp
        | OrOp
        | AndOp
        | NotOp             -- unary
        | EqOp | NeOp | LtOp | LeOp | GtOp | GeOp
        | AddOp | SubOp
        | MulOp | DivOp | ModOp
        | PowOp
        | NegOp | DerefOp   -- unary
        | NewOp             -- unary
  deriving Show

data Constant = IntC Integer
              | BoolC Bool

instance Show Constant where
  show (IntC n)      = show n
  show (BoolC True)  = "true"
  show (BoolC False) = "false"

data Expr = VarE Id
          | LamE Id Expr
          | AppE Expr Expr
          | ConstE Constant
          | CallccE Expr
          | IfE Expr Expr Expr
          | TupleE [Expr]
          | ListE [Expr]
          | BinopE Op Expr Expr
          | UnaryE Op Expr
  deriving Show

unitE :: Expr
unitE = TupleE []

