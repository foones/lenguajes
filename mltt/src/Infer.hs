module Infer(inferType, checkType, normalize) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe(isJust, fromJust)

import Pprint(pprintExpr)
import Syntax(Id, Expr(..), Declaration(..), freeVariables, freeVariablesM)

reportError :: String -> Either String a
reportError = Left

---- Alpha equivalence ----

type Renaming = M.Map Id Id

domR :: Renaming -> S.Set Id
domR r = M.keysSet r

freeVariablesR :: Renaming -> S.Set Id
freeVariablesR r = S.fromList (M.elems r)

freshVariable :: Id -> S.Set Id -> Id
freshVariable prefix forbidden =
  let candidates = prefix : [prefix ++ show n | n <- [1..]]
   in head (filter (`notElem` forbidden) candidates)

alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent e1 e2 = rec M.empty M.empty e1 e2
  where
    rec r1 r2 Type Type = True
    rec r1 r2 (Pi x s1 t1) (Pi y s2 t2) =
      recBinder r1 r2 x (Just s1) t1 y (Just s2) t2
    rec r1 r2 (Var x) (Var y) =
      M.findWithDefault x x r1 == M.findWithDefault y y r2
    rec r1 r2 (Lam x s1 t1) (Lam y s2 t2) =
      recBinder r1 r2 x s1 t1 y s2 t2
    rec r1 r2 (App s1 t1) (App s2 t2) =
      rec r1 r2 s1 s2 && rec r1 r2 t1 t2
    rec _ _ _ _ = False
    recMaybe r1 r2 (Just e1) (Just e2) = rec r1 r2 e1 e2
    recMaybe _  _  _         _ = True
    recBinder r1 r2 x s1 t1 y s2 t2 =
         recMaybe r1 r2 s1 s2
      && let forbidden = freeVariablesR r1 `S.union` freeVariablesR r2
               `S.union` (freeVariables t1 S.\\ domR r1)
               `S.union` (freeVariables t2 S.\\ domR r2)
             z = freshVariable x forbidden
             r1' = M.insert x z r1
             r2' = M.insert y z r2
          in rec r1' r2' t1 t2

---- Substitution ----

type Substitution = M.Map Id Expr

domS :: Substitution -> S.Set Id
domS sub = M.keysSet sub

freeVariablesS :: Substitution -> S.Set Id
freeVariablesS sub = S.unions (map freeVariables (M.elems sub))

substitute :: Substitution -> Expr -> Expr
substitute sub Type          = Type
substitute sub (Pi x e1 e2)  = substituteBinder sub pi x (Just e1) e2
  where
    pi x (Just e1) e2 = Pi x e1 e2
substitute sub (Var x)       = M.findWithDefault (Var x) x sub
substitute sub (Lam x e1 e2) = substituteBinder sub Lam x e1 e2
substitute sub (App e1 e2)   = App (substitute sub e1) (substitute sub e2)

substituteMaybe :: Substitution -> Maybe Expr -> Maybe Expr
substituteMaybe sub Nothing  = Nothing
substituteMaybe sub (Just e) = Just (substitute sub e)

substituteBinder :: Substitution
                 -> (Id -> Maybe Expr -> Expr -> Expr)
                 -> Id -> Maybe Expr -> Expr -> Expr
substituteBinder sub binder x e1 e2 =
  let forbidden = freeVariablesS sub
        `S.union` (freeVariables e2 S.\\ domS sub)
      z = freshVariable x forbidden
      sub' = M.insert x (Var z) sub
   in binder z (substituteMaybe sub e1) (substitute sub' e2)

---- Reduction ----

reduce1 :: [Declaration] -> Expr -> Maybe Expr
reduce1 env expr
  | isJust m = m
  where
    m = tryRewrite env expr
reduce1 env Type          = Nothing
reduce1 env (Pi x e1 e2)  = reduce1Binder env pi x (Just e1) e2
  where
    pi x (Just e1) e2 = Pi x e1 e2
reduce1 env (Var x)       = lookupDef env x
  where
    lookupDef :: [Declaration] -> Id -> Maybe Expr
    lookupDef []                         _ = Nothing
    lookupDef (DeclAxiom y _ : _) x | x == y = Nothing
    lookupDef (DeclDef y _ expr : _) x
      | x == y = Just expr
    lookupDef (_ : decls) x = lookupDef decls x
reduce1 env (Lam x e1 e2) = reduce1Binder env Lam x e1 e2
reduce1 env (App (Lam x _ body) arg) =
  Just (substitute (M.insert x arg M.empty) body)
reduce1 env (App e1 e2) =
  case reduce1 env e1 of
    Just e1' -> Just (App e1' e2)
    Nothing ->
      case reduce1 env e2 of
        Just e2' -> Just (App e1 e2')
        Nothing -> Nothing

reduce1Maybe :: [Declaration] -> Maybe Expr -> Maybe (Maybe Expr)
reduce1Maybe env Nothing = Nothing
reduce1Maybe env (Just expr) =
  case reduce1 env expr of
    Nothing    -> Nothing
    Just expr' -> Just (Just expr')

reduce1Binder :: [Declaration]
              -> (Id -> Maybe Expr -> Expr -> Expr)
              -> Id -> Maybe Expr -> Expr -> Maybe Expr
reduce1Binder env binder x me1 e2 = do
  e1 <- case me1 of
          Just e1 -> return e1
          Nothing -> return Type -- Hack
  let env' = DeclAxiom x e1 : env
  case reduce1Maybe env me1 of
    Just me1' -> Just (binder x me1' e2)
    Nothing  ->
      case reduce1 env e2 of
        Just e2' -> Just (binder x me1 e2')
        Nothing  -> Nothing

tryRewrite :: [Declaration] -> Expr -> Maybe Expr
tryRewrite [] _ = Nothing
tryRewrite (DeclRewrite params lhs rhs : _) expr
  | isJust m = Just (substitute (fromJust m) rhs)
  where
    m = tryMatch params lhs expr
tryRewrite (_ : decls) expr = tryRewrite decls expr

tryMatch :: [(Id, Expr)] -> Expr -> Expr -> Maybe Substitution
tryMatch params pattern target = rec pattern target
  where
    rec (Var x) t
      | isParam x = Just (M.insert x t M.empty)
    rec (Var x) (Var y)
      | isFree x && x == y = Just M.empty
      | isFree x && x /= y = Nothing
    rec (App p1 p2) (App t1 t2) =
      case rec p1 t1 of
        Nothing -> Nothing
        Just sub1 ->
          case rec p2 t2 of
            Nothing -> Nothing
            Just sub2 -> mergeSubstitutions sub1 sub2
    rec _ _ = Nothing
    isFree :: Id -> Bool
    isFree x = not (isParam x)
    isParam :: Id -> Bool
    isParam x = x `elem` map fst params
    mergeSubstitutions :: Substitution -> Substitution -> Maybe Substitution
    mergeSubstitutions sub1 sub2 = rec (M.toList sub1)
      where
        rec :: [(Id, Expr)] -> Maybe Substitution
        rec []               = return sub2
        rec ((x, t1) : sub1) =
          case M.lookup x sub2 of
            Nothing -> do subM <- rec sub1
                          return (M.insert x t1 subM)
            Just t2 -> if alphaEquivalent t1 t2
                        then do subM <- rec sub1
                                return (M.insert x t1 subM)
                        else Nothing

normalize :: [Declaration] -> Expr -> Expr
normalize env expr =
  case reduce1 env expr of
    Nothing    -> expr
    Just expr' -> normalize env expr'

interconvertible :: [Declaration] -> Expr -> Expr -> Bool
interconvertible env e1 e2 = alphaEquivalent (normalize env e1) (normalize env e2)

---- Type checking ----

inferType :: [Declaration] -> Expr -> Either String Expr
inferType env Type          = return Type
inferType env (Pi x e1 e2)  = do
  checkType env e1 Type
  checkType (DeclAxiom x e1 : env) e2 Type
  return Type
inferType env (Var x)       = lookupId env x
  where
    lookupId :: [Declaration] -> Id -> Either String Expr
    lookupId [] x = reportError ("Variable no ligada: " ++ x)
    lookupId (DeclAxiom y typ : decls) x
      | x == y = return typ
    lookupId (DeclDef y typ _ : decls) x
      | x == y = return typ
    lookupId (_ : decls) x = lookupId decls x
inferType env (Lam x Nothing e2) =
  reportError "No se puede inferir el tipode una abstracción sin anotaciones."
inferType env (Lam x (Just e1) e2) = do
  checkType env e1 Type
  returnType <- inferType (DeclAxiom x e1 : env) e2
  return $ Pi x e1 returnType
inferType env (App e1 e2)   = do
  funcType <- inferType env e1
  case normalize env funcType of
    Pi x paramType returnType -> do
      checkType env e2 paramType
      return $ substitute (M.insert x e2 M.empty) returnType
    _ -> reportError $ unlines [
           "En la aplicación: " ++ pprintExpr (App e1 e2)
         , "el tipo de la función es: " ++ pprintExpr funcType
         , "que no es un tipo función."
         ]

checkType :: [Declaration] -> Expr -> Expr -> Either String ()
checkType env (Lam x me1 e2) typ = do
  case normalize env typ of
    Pi y paramType returnType -> do
      case me1 of
        Nothing -> return ()
        Just e1 ->
          if interconvertible env e1 paramType
           then return ()
           else reportError $ unlines [
                  "El contexto exige que la función tenga tipo: " ++ pprintExpr typ
                , "pero el dominio es: " ++ pprintExpr e1
                ]
      checkType (DeclAxiom x paramType : env) e2
                (substitute (M.insert y (Var x) M.empty) returnType)
    _ -> reportError $ unlines [
           "El contexto exige que la función tenga tipo: " ++ pprintExpr typ
         , "pero no es un tipo función."
         ]
checkType env (Hole name) typ = do
  reportError $ unlines [
      "Agujero: " ++ pprintExpr (Hole name)
    , pprintExpr typ
    ]

checkType env expr typ = do
  typ' <- inferType env expr
  if interconvertible env typ' typ
   then return ()
   else reportError $ unlines [
          "El tipo de la expresión " ++ pprintExpr expr
        , "es              : " ++ pprintExpr typ'
        , "pero se esperaba: " ++ pprintExpr typ
        , "----------------: " ++ pprintExpr (normalize env typ')
        , "----------------: " ++ pprintExpr (normalize env typ)
        ]

