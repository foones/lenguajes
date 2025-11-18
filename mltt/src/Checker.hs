
module Checker(checkProgram) where

import System.Exit(exitFailure)

import Pprint(pprintDeclaration, pprintExpr)
import Syntax(Id, Expr(..), Declaration(..))
import Infer(inferType, checkType, normalize)

die :: String -> IO a
die msg = do
  putStrLn "*** MLTT Error ***\n"
  putStrLn msg
  exitFailure 

liftE :: Either String a -> IO a
liftE (Left msg) = die msg
liftE (Right x)  = return x

checkProgram :: [Declaration] -> IO ()
checkProgram program = rec [] program
  where
    rec :: [Declaration] -> [Declaration] -> IO ()
    rec _ [] = return ()
    rec env (decl@(DeclAxiom name typ) : decls) = do
      putStrLn . pprintDeclaration $ decl
      liftE $ checkType env typ Type
      rec (decl : env) decls
    rec env (decl@(DeclDef name typ body) : decls) = do
      putStrLn . pprintDeclaration $ decl
      liftE $ checkType env typ Type
      liftE $ checkType env body typ
      rec (decl : env) decls
    rec env (decl@(DeclCheck expr) : decls) = do
      typ <- liftE $ inferType env expr
      putStrLn . pprintDeclaration $ decl
      putStrLn ("  = " ++ pprintExpr (normalize env expr))
      putStrLn ("  : " ++ pprintExpr typ)
      rec (decl : env) decls
    rec env (decl@(DeclRewrite params lhs rhs) : decls) = do
      checkPattern env (map fst params) lhs
      env' <- checkTelescope env params
      typ <- liftE $ inferType env' lhs
      liftE $ checkType env' rhs typ
      putStrLn . pprintDeclaration $ decl
      rec (decl : env) decls

checkTelescope :: [Declaration] -> [(Id, Expr)] -> IO [Declaration]
checkTelescope env [] = return env
checkTelescope env ((x, typ) : params) = do
  liftE $ checkType env typ Type
  checkTelescope (DeclAxiom x typ : env) params

checkPattern :: [Declaration] -> [Id] -> Expr -> IO ()
checkPattern decls params expr = recFun expr
  where
    recFun (Var x) = if isAxiom decls x
                       then return ()
                       else die ("Pattern constant " ++ x ++ " should be bound by an axiom.")
    recFun (App e1 e2) = do
      recFun e1
      recArg e2
    recArg (Var x) = if isAxiom decls x || x `elem` params
                      then return ()
                      else die ("Pattern argument " ++ x ++ " should be bound.")
    recArg (App e1 e2) = do
      recFun e1
      recArg e2
    isAxiom [] _ = False
    isAxiom (DeclAxiom y _ : _) x | x == y = True
    isAxiom (DeclDef y _ _ : _) x | x == y = False
    isAxiom (_ : decls)         x          = isAxiom decls x 

