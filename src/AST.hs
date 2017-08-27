
module AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       ) where

type Id = String

data Type = Int
          | Bool
          | Vec
          | Unit
  deriving Show

data ProgramT = Program { unProgram :: [FunctionT] }
  deriving Show

data FunctionT = Function Id Type [ParameterT] BlockT
  deriving Show

data ParameterT = Parameter {
                    parameterName :: Id,
                    parameterType :: Type
                  }
  deriving Show

data BlockT = Block [StmtT]
  deriving Show

data StmtT = StmtAssign Id ExprT
           | StmtVecAssign Id ExprT ExprT
           | StmtIf ExprT BlockT
           | StmtIfElse ExprT BlockT BlockT
           | StmtWhile ExprT BlockT
           | StmtReturn ExprT
           | StmtCall Id [ExprT]
  deriving Show

data ExprT = ExprVar Id
           | ExprConstNum Integer
           | ExprConstBool Bool
           | ExprVecMake [ExprT]
           | ExprVecLength Id
           | ExprVecDeref Id ExprT
           | ExprCall Id [ExprT]
           | ExprAnd ExprT ExprT
           | ExprOr ExprT ExprT
           | ExprNot ExprT
           | ExprLe ExprT ExprT
           | ExprGe ExprT ExprT
           | ExprLt ExprT ExprT
           | ExprGt ExprT ExprT
           | ExprEq ExprT ExprT
           | ExprNe ExprT ExprT
           | ExprAdd ExprT ExprT
           | ExprSub ExprT ExprT
           | ExprMul ExprT ExprT
  deriving Show

