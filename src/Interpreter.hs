module Interpreter(eval) where

import Data.Char(chr)
import Control.Monad.Trans.State.Strict(StateT, runStateT, get, put, modify)
import Control.Monad.Trans.Class(lift)
import qualified Data.Map as Map(
    Map, empty, member, lookup, insert, findWithDefault, fromList, keys
  )

import AST(
         Id, Type(..), ProgramT(..), FunctionT(..), ParameterT(..),
         BlockT(..), StmtT(..), ExprT(..)
       )

type Location = Integer
data Value = VInt Integer
           | VBool Bool
           | VVec Location
           | VUnit

unVInt :: Value -> Integer
unVInt (VInt x) = x
unVInt _ = error "unVInt requires an integer"

type Store = Map.Map Location [Integer]

type S = (ProgramT, Map.Map Id Value, Store)
type M = StateT S IO

eval :: ProgramT -> IO ()
eval program = do
  (VUnit, _) <- applyFunction program "main" [] (Map.fromList [(0, [])])
  return ()

functionName :: FunctionT -> Id
functionName (Function name _ _ _) = name

applyFunction :: ProgramT -> Id -> [Value] -> Store -> IO (Value, Store)
applyFunction program@(Program funcs) funcName args store =
  let [Function _ _ params body] =
        filter (\ function -> functionName function == funcName) funcs
   in
     if length params /= length args
      then error "Arity mismatch."
      else let initialState = (program,
                               Map.fromList (zip (map parameterName params) args),
                               store)
            in do
                 (value, (_, _, store)) <- runStateT (evalBlock body) initialState
                 return (value, store)

isStmtReturn :: StmtT -> Bool
isStmtReturn (StmtReturn _) = True
isStmtReturn _              = False

evalBlock :: BlockT -> M Value
evalBlock (Block stmts) =
  case reverse stmts of
    (StmtReturn expr : _) -> do
      mapM_ evalStmt (init stmts)
      evalExpr expr
    _ -> do
      mapM_ evalStmt stmts
      return $ VUnit

evalStmt :: StmtT -> M Value
evalStmt (StmtCall "putChar" [x]) = do
  VInt xV <- evalExpr x
  lift $ putChar (chr (fromInteger xV))
  return VUnit
evalStmt (StmtCall "putNum" [x]) = do
  VInt xV <- evalExpr x
  lift $ putStr (show xV)
  return VUnit
evalStmt (StmtAssign x expr) = do
  val <- evalExpr expr
  modify (\ (program, env, store) -> (program, Map.insert x val env, store))
  return val
evalStmt (StmtVecAssign x expr1 expr2) = do
  VInt index <- evalExpr expr1
  VInt val <- evalExpr expr2
  (_, env, store) <- get
  case Map.findWithDefault VUnit x env of
    VVec loc ->
        let vec = Map.findWithDefault (error "Undefined location") loc store
            vec' = take (fromIntegral index) vec ++ [val] ++ drop (fromIntegral (index + 1)) vec
         in do
              modify (\ (program, env, store) -> (program, env, Map.insert loc vec' store))
              return VUnit
    _ -> error "Vector assignment operation should be applied to vectors only"
evalStmt (StmtIf expr block) = do
  VBool b <- evalExpr expr
  if b
   then evalBlock block
   else return VUnit 
evalStmt (StmtIfElse expr block1 block2) = do
  VBool b <- evalExpr expr
  if b
   then evalBlock block1
   else evalBlock block2
evalStmt whileStmt@(StmtWhile expr (Block stmts)) = do
  VBool b <- evalExpr expr
  if b
   then evalBlock (Block (stmts ++ [whileStmt]))
   else return VUnit
evalStmt (StmtReturn _) =
  error "Should not meet a return statement other than as the last statement in the function body."
evalStmt (StmtCall f args) = do
  vals <- mapM evalExpr args
  (program, _, store) <- get
  (val, store') <- lift $ applyFunction program f vals store
  modify (\ (program, env, _) -> (program, env, store')) 
  return VUnit
--evalStmt _ = error "not implemented"

evalExpr :: ExprT -> M Value
evalExpr (ExprVar x) = do
  (_, env, _) <- get
  return $ Map.findWithDefault VUnit x env
evalExpr (ExprConstNum n) =
  return $ VInt n
evalExpr (ExprConstBool b) = do
  return $ VBool b
evalExpr (ExprCall f args) = do
  vals <- mapM evalExpr args
  (program, _, store) <- get
  (val, store') <- lift $ applyFunction program f vals store
  modify (\ (program, env, _) -> (program, env, store')) 
  return val
evalExpr (ExprAnd e1 e2) = do
  VBool b1 <- evalExpr e1
  VBool b2 <- evalExpr e2
  return $ VBool (b1 && b2)
evalExpr (ExprOr e1 e2) = do
  VBool b1 <- evalExpr e1
  VBool b2 <- evalExpr e2
  return $ VBool (b1 || b2)
evalExpr (ExprNot e) = do
  VBool b <- evalExpr e
  return $ VBool (not b)
evalExpr (ExprEq e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VBool (x1 == x2)
evalExpr (ExprLe e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VBool (x1 <= x2)
evalExpr (ExprGe e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VBool (x1 >= x2)
evalExpr (ExprLt e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VBool (x1 < x2)
evalExpr (ExprGt e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VBool (x1 > x2)
evalExpr (ExprNe e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VBool (x1 /= x2)
evalExpr (ExprAdd e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VInt (x1 + x2)
evalExpr (ExprSub e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VInt (x1 - x2)
evalExpr (ExprMul e1 e2) = do
  VInt x1 <- evalExpr e1
  VInt x2 <- evalExpr e2
  return $ VInt (x1 * x2)
evalExpr (ExprVecMake exprs) = do
  vs <- mapM evalExpr exprs
  (_, _, store) <- get
  let newLoc = maximum (Map.keys store) + 1
   in do
        modify (\ (program, env, store) ->
                  (program, env, Map.insert newLoc (map unVInt vs) store))
        return $ VVec newLoc
evalExpr (ExprVecLength x) = do
  (_, env, store) <- get
  case Map.findWithDefault VUnit x env of
    VVec loc ->
          let vec = Map.findWithDefault (error "Undefined location") loc store
           in return $ VInt (fromIntegral (length vec))
    _ -> error "Vector length operation should be applied to vectors only"
evalExpr (ExprVecDeref x expr) = do
  VInt index <- evalExpr expr
  (_, env, store) <- get
  case Map.findWithDefault VUnit x env of
    VVec loc ->
          let vec = Map.findWithDefault (error "Undefined location") loc store
           in return $ VInt (vec !! fromIntegral index)
    _ -> error "Vector deref operation should be applied to vectors only"
--evalExpr _ = error "not implemented"

