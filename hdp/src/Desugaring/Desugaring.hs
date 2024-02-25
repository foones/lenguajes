
module Desugaring.Desugaring(desugarProgram) where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe(fromJust)

import Error(Error(..), ErrorType(..))
import Position(Position(..))
import FailState(FailState, failFS, evalFS, getFS, putFS, logFS)
import qualified Calculus.Terms as C
import Syntax.Name(
         QName, unqualifiedName, suffixQName,
         primitiveType, primitiveArrow,
         primitiveTensor, primitiveWith, primitiveLinSum,
         primitiveForall, primitiveColon, primitiveBang,
         primitiveTensorElim,
         primitiveWithIntro, primitiveWithElim1, primitiveWithElim2,
         primitiveLinSumIntro1, primitiveLinSumIntro2, primitiveLinSumElim,
         primitiveBangElim
       )
import Syntax.AST(
         AnnProgram(..), Program,
         AnnDeclaration(..), Declaration,
         AnnSignature(..), Signature,
         AnnEquation(..), Equation,
         AnnConstraint(..), Constraint,
         AnnCaseBranch(..), CaseBranch,
         AnnExpr(..), Expr,
         declarationAnnotation, exprAnnotation,
         exprHeadVariable, exprHeadArguments
       )

desugarProgram :: Program -> Either Error [C.Decl]
desugarProgram p = evalFS (desugarProgramM p) initialState
  where initialState = DesugarState {
                         state_scopes      = [emptyScope]
                       , state_types       = M.empty
                       , state_definitions = M.empty
                       }

type M = FailState DesugarState
data DesugarState = DesugarState {
                      state_scopes      :: [Scope]
                    , state_types       :: M.Map QName C.Type
                    , state_definitions :: M.Map QName C.Expr
                    }

data LinearType a = LAvailable Position a
                  | LAlreadyUsed Position a

data Scope = Scope {
               scope_typeKinds   :: M.Map QName C.Kind
             , scope_uvarTypes   :: M.Map QName C.Type
             , scope_linvarTypes :: M.Map QName (LinearType C.Type)
             }

emptyScope :: Scope
emptyScope = Scope {
               scope_typeKinds   = M.empty
             , scope_uvarTypes   = M.empty
             , scope_linvarTypes = M.empty
             }

enterScope :: M ()
enterScope = do
  state <- getFS
  putFS (state { state_scopes = emptyScope : state_scopes state })

leaveScope :: M ()
leaveScope = do
  state <- getFS
  mapM_ (\ (name, ltyp) ->
            case ltyp of
              LAvailable pos _ ->
                failM pos ("Linear variable "
                              ++ unqualifiedName name
                              ++ " is not used.")
              _ -> return ())
        (M.toList (scope_linvarTypes (head (state_scopes state))))
  putFS (state { state_scopes = tail (state_scopes state) })

withScopes :: ([Scope] -> M a) -> M a
withScopes f = do
  state <- getFS
  f (state_scopes state)

getScopes :: M [Scope]
getScopes = do
  state <- getFS
  return $ state_scopes state

putScopes :: [Scope] -> M ()
putScopes scopes' = do
  state <- getFS
  putFS (state { state_scopes = scopes' })

restoringScopes :: M a -> M a
restoringScopes m = do
  scopes <- getScopes
  x <- m
  putScopes scopes
  return x

updateCurrentScope :: (Scope -> Scope) -> M ()
updateCurrentScope f = do
  state <- getFS
  putFS (state {
           state_scopes = f (head (state_scopes state))
                        : tail (state_scopes state)
         })

bindBaseTypeToKind :: QName -> C.Kind -> M ()
bindBaseTypeToKind t k =
  updateCurrentScope
    (\ scope ->
       scope {
         scope_typeKinds = M.insert t k (scope_typeKinds scope)
       })

bindBaseTypesToKinds :: Position -> [QName] -> C.Kind -> M C.Kind
bindBaseTypesToKinds pos []       kind             = return kind
bindBaseTypesToKinds pos (_ : _)  C.KType          =
  failM pos "Expected an arrow kind."
bindBaseTypesToKinds pos (t : ts) (C.KArrow k1 k2) = do
  bindBaseTypeToKind t k1
  bindBaseTypesToKinds pos ts k2

defineType :: Position -> QName -> C.Kind -> [QName] -> C.Type -> M ()
defineType pos name kind params rhs = do
  bindBaseTypeToKind name kind
  state <- getFS
  if name `M.member` state_types state
   then failM pos ("Type " ++ unqualifiedName name ++ " already defined.")
   else return ()
  putFS (state {
           state_types = M.insert name
                                  (foldr C.TLam rhs params)
                                  (state_types state)
         })

bindUVariable :: QName -> C.Type -> M ()
bindUVariable x t =
  updateCurrentScope
    (\ scope ->
       scope { scope_uvarTypes = M.insert x t (scope_uvarTypes scope) })

defineUVariable :: Position -> QName -> C.Type -> C.Expr -> M ()
defineUVariable pos name typ rhs = do
  bindUVariable name typ
  state <- getFS
  if name `M.member` state_definitions state
   then failM pos ("Variable " ++ unqualifiedName name ++ " already defined.")
   else return ()
  putFS (state {
           state_definitions = M.insert name rhs (state_definitions state)
         })

bindLinearVariable :: Position -> QName -> C.Type -> M ()
bindLinearVariable pos x t =
  updateCurrentScope
    (\ scope ->
       scope {
         scope_linvarTypes =
           M.insert x (LAvailable pos t) (scope_linvarTypes scope)
       })

lookupBaseTypeKind :: Position -> QName -> M C.Kind
lookupBaseTypeKind pos name = withScopes rec
  where
    rec :: [Scope] -> M C.Kind
    rec [] = failM pos ("Unbound type variable: " ++ unqualifiedName name)
    rec (scope : scopes) = do
      case M.lookup name (scope_typeKinds scope) of
        Just kind -> return kind
        Nothing   -> rec scopes

lookupBaseTypeDefinition :: Position -> QName -> M C.Type
lookupBaseTypeDefinition pos name = do
  state <- getFS
  case M.lookup name (state_types state) of
    Just typ -> return typ
    Nothing  -> return (C.TBase name)

data VariableLinearity = VLinear | VNonLinear

lookupVarLinearity :: Position -> QName -> M VariableLinearity
lookupVarLinearity pos name = withScopes rec
  where
    rec :: [Scope] -> M VariableLinearity
    rec [] = failM pos ("Unbound variable: " ++ unqualifiedName name)
    rec (scope : scopes) = do
      case M.lookup name (scope_uvarTypes scope) of
        Just typ -> return VNonLinear
        Nothing  ->
          case M.lookup name (scope_linvarTypes scope) of
            Just (LAvailable _ typ)   -> return VLinear
            Just (LAlreadyUsed _ typ) -> return VLinear
            Nothing  ->
              case M.lookup name (scope_typeKinds scope) of
                Just _ ->
                  failM pos ("Variable " ++ unqualifiedName name
                             ++ " is bound as a type.")
                Nothing -> rec scopes

lookupVarType :: Position -> QName -> M C.Type
lookupVarType pos name = withScopes rec
  where
    rec :: [Scope] -> M C.Type
    rec [] = failM pos ("Unbound variable: " ++ unqualifiedName name)
    rec (scope : scopes) = do
      case M.lookup name (scope_uvarTypes scope) of
        Just typ -> return typ
        Nothing  ->
          case M.lookup name (scope_linvarTypes scope) of
            Just (LAvailable _ typ)   -> return typ
            Just (LAlreadyUsed _ typ) -> return typ
            Nothing  ->
              case M.lookup name (scope_typeKinds scope) of
                Just _ ->
                  failM pos ("Variable " ++ unqualifiedName name
                             ++ " is bound as a type.")
                Nothing -> rec scopes

lookupVarDefinition :: QName -> M C.Expr
lookupVarDefinition name = do
  state <- getFS
  case M.lookup name (state_definitions state) of
    Nothing -> return $ C.EVar name
    Just e  -> return e

lookupAndConsumeVarType :: Position -> QName -> M C.Type
lookupAndConsumeVarType pos name = do
    (scopes, typ) <- withScopes rec
    putScopes scopes
    return typ
  where
    rec :: [Scope] -> M ([Scope], C.Type)
    rec [] = failM pos ("Unbound variable: " ++ unqualifiedName name)
    rec fullScopes@(scope : scopes) = do
      case M.lookup name (scope_uvarTypes scope) of
        Just typ -> return (fullScopes, typ)
        Nothing  ->
          case M.lookup name (scope_linvarTypes scope) of
            Just (LAvailable pos' typ) ->
              let scope' = scope {
                             scope_linvarTypes =
                               M.insert name
                                        (LAlreadyUsed pos' typ)
                                        (scope_linvarTypes scope)
                           }
               in return (scope' : scopes, typ)
            Just (LAlreadyUsed _ _) ->
              failM pos ("Linear variable "
                         ++ unqualifiedName name
                         ++ " is used twice.")
            Nothing  -> do
              case M.lookup name (scope_typeKinds scope) of
                Just _ ->
                  failM pos ("Variable " ++ unqualifiedName name
                             ++ " is bound as a type.")
                Nothing -> do
                  (scopes', typ) <- rec scopes
                  return (scope : scopes', typ)

--

failM :: Position -> String -> M a
failM pos msg = failFS (Error DesugaringError pos msg)

desugarProgramM :: Program -> M [C.Decl]
desugarProgramM program = rec (programDeclarations program)
  where
    rec [] = return []
    rec (TypeSignature (Signature pos0 name kind [])
         : ValueDeclaration (Equation pos1 lhs rhs)
         : rs)
      | isKind kind = do
      case exprHeadVariable lhs of
        Just name' | name == name' -> do
          kind'  <- desugarKind kind
          params <- mapM desugarVariable (fromJust (exprHeadArguments lhs))
          enterScope
          kind''   <- bindBaseTypesToKinds (exprAnnotation kind) params kind'
          (typ, _) <- desugarType (Just kind'') rhs
          leaveScope
          let d = C.DType pos0 name kind' params typ
          defineType pos0 name kind' params typ
          logFS (show d)
          ds <- rec rs
          return (d : ds)
        _ -> failM pos1 "Name of type must match its declaration."
    rec (TypeSignature (Signature pos0 name typ [])
         : ValueDeclaration (Equation pos1 lhs rhs)
         : rs) = do
      case exprHeadVariable lhs of
        Just name' | name == name' -> do
          (typ', _) <- desugarType (Just C.KType) typ
          params <- mapM desugarVariable (fromJust (exprHeadArguments lhs))
          (expr, _) <- desugarExpr (Just typ')
                                   (foldr (ELambda pos1) rhs
                                          (map (EVar pos1) params))
          let d = C.DDef pos0 name typ' expr
          defineUVariable pos0 name typ' expr
          logFS (show d)
          ds <- rec rs
          return (d : ds)
        _ -> failM pos1 "Name of value must match its declaration."
    rec (d@(TypeSignature _) : _) =
      failM (declarationAnnotation d) "Missing value declaration."
    rec (d@(ValueDeclaration _) : _) =
      failM (declarationAnnotation d) "Missing type signature."
    rec (EvalDeclaration pos expr : rs) = do
      (m, typ) <- desugarExpr Nothing expr
      logFS ("eval " ++ show m ++ " : " ++ show typ)
      m' <- evalExpr m
      logFS ("  ~~> " ++ show m')
      rec rs
    rec (d : _) = failM (declarationAnnotation d)
                        "Expected a type signature or value declaration."

isKind :: Expr -> Bool
isKind (EVar _ t) = t == primitiveType 
isKind (EApp _ (EApp _ (EVar _ x) e1) e2)
  | x == primitiveArrow = isKind e1 && isKind e2
isKind _          = False

isKeywordType :: Expr -> Bool
isKeywordType (EVar _ t) = t == primitiveType 
isKeywordType _          = False

desugarKind :: Expr -> M C.Kind
desugarKind (EVar p t) | t == primitiveType = return C.KType 
desugarKind (EApp p (EApp _ (EVar _ x) e1) e2)
  | x == primitiveArrow = do k1 <- desugarKind e1
                             k2 <- desugarKind e2
                             return $ C.KArrow k1 k2
desugarKind e = failM (exprAnnotation e) "Expected a kind."

assertKindMatch :: Position -> Maybe C.Kind -> C.Kind -> M ()
assertKindMatch _   Nothing         _    = return ()
assertKindMatch pos (Just expected) kind = do
  if expected == kind
   then return ()
   else failM pos ("Expected a type of kind " ++ show expected ++ ".\n" ++
                   "Found type of kind      " ++ show kind ++ ".")

nameApartFrom :: QName -> S.Set QName -> QName
nameApartFrom x forbidden =
  head [x' | suffix <- "" : map show [1..],
             x' <- [suffixQName x suffix],
             not (x' `S.member` forbidden)]

typeFreeVars :: C.Type -> S.Set QName
typeFreeVars (C.TBase x)       = S.singleton x
typeFreeVars (C.TLinImp t1 t2) = S.union (typeFreeVars t1) (typeFreeVars t2)
typeFreeVars (C.TTensor t1 t2) = S.union (typeFreeVars t1) (typeFreeVars t2)
typeFreeVars (C.TWith t1 t2)   = S.union (typeFreeVars t1) (typeFreeVars t2)
typeFreeVars (C.TLinSum t1 t2) = S.union (typeFreeVars t1) (typeFreeVars t2)
typeFreeVars (C.TBang t)       = typeFreeVars t
typeFreeVars (C.TForall x t)   = typeFreeVars t S.\\ S.singleton x
typeFreeVars (C.TApp t1 t2)    = S.union (typeFreeVars t1) (typeFreeVars t2)
typeFreeVars (C.TLam x t)      = typeFreeVars t S.\\ S.singleton x

substTypeType' :: M.Map QName C.Type -> C.Type -> M C.Type
substTypeType' subst t0 = rec subst t0
  where
    substFVs :: M.Map QName C.Type -> S.Set QName
    substFVs subst =
      S.union (S.fromList (M.keys subst))
              (S.unions [typeFreeVars t | (_, t) <- M.toList subst])
    rec :: M.Map QName C.Type -> C.Type -> M C.Type
    rec subst (C.TBase x) =
      case M.lookup x subst of
        Nothing -> return $ C.TBase x
        Just s  -> return s
    rec subst (C.TLinImp t1 t2) = C.TLinImp <$> rec subst t1
                                            <*> rec subst t2
    rec subst (C.TTensor t1 t2) = C.TTensor <$> rec subst t1
                                            <*> rec subst t2
    rec subst (C.TWith t1 t2)   = C.TWith   <$> rec subst t1
                                            <*> rec subst t2
    rec subst (C.TLinSum t1 t2) = C.TLinSum <$> rec subst t1
                                            <*> rec subst t2
    rec subst (C.TBang t)       = C.TBang   <$> rec subst t
    rec subst (C.TForall x t)   = recBinder C.TForall subst x t
    rec subst (C.TApp t1 t2)    = C.TApp <$> rec subst t1
                                         <*> rec subst t2
    rec subst (C.TLam x t)      = recBinder C.TLam subst x t

    recBinder :: (QName -> C.Type -> C.Type)
              -> M.Map QName C.Type -> QName -> C.Type -> M C.Type
    recBinder binder subst x t =
      let fvs = (typeFreeVars t S.\\ S.singleton x) `S.union` substFVs subst
          z   = nameApartFrom x fvs
       in binder z <$> rec (M.insert x (C.TBase z) subst) t

substTypeType :: C.Type -> QName -> C.Type -> M C.Type
substTypeType t0 x0 s0 = substTypeType' (M.fromList [(x0, s0)]) t0

unfoldType :: Position -> C.Type -> M C.Type
unfoldType pos t = rec S.empty t
  where
    rec bound (C.TBase x)       = do
      if x `S.member` bound
       then return $ C.TBase x
       else do
         t <- lookupBaseTypeDefinition pos x
         case t of
           C.TBase y | y == x -> return t
           _                -> rec bound t
    rec bound (C.TLinImp t1 t2) = C.TLinImp   <$> rec bound t1
                                              <*> rec bound t2
    rec bound (C.TTensor t1 t2) = C.TTensor   <$> rec bound t1
                                              <*> rec bound t2
    rec bound (C.TWith t1 t2)   = C.TWith     <$> rec bound t1
                                              <*> rec bound t2
    rec bound (C.TLinSum t1 t2) = C.TLinSum   <$> rec bound t1
                                              <*> rec bound t2
    rec bound (C.TBang t)       = C.TBang     <$> rec bound t
    rec bound (C.TForall x t)   = C.TForall x <$> rec (S.insert x bound) t
    rec bound (C.TApp t1 t2)    = do
      t1' <- rec bound t1
      t2' <- rec bound t2
      case t1' of
        C.TLam x t1'' -> do
          s <- substTypeType t1'' x t2'
          rec bound s
        _ -> return $ C.TApp t1' t2'
    rec bound (C.TLam x t)      = C.TLam x <$> rec (S.insert x bound) t

equalTypes :: Position -> C.Type -> C.Type -> M Bool
equalTypes pos t1 t2 = do
    t1' <- unfoldType pos t1
    t2' <- unfoldType pos t2
    return $ rec M.empty t1' M.empty t2'
  where
    rec e1 (C.TBase x) e2 (C.TBase y) =
      case (M.lookup x e1, M.lookup y e2) of
        (Nothing, Nothing) -> x == y
        (Just y', Just x') -> x == x' && y == y'
        _                  -> False
    rec e1 (C.TLinImp t1 t2) e2 (C.TLinImp s1 s2) = rec e1 t1 e2 s1
                                                 && rec e1 t2 e2 s2
    rec e1 (C.TTensor t1 t2) e2 (C.TTensor s1 s2) = rec e1 t1 e2 s1
                                                 && rec e1 t2 e2 s2
    rec e1 (C.TWith t1 t2)   e2 (C.TWith s1 s2)   = rec e1 t1 e2 s1
                                                 && rec e1 t2 e2 s2
    rec e1 (C.TLinSum t1 t2) e2 (C.TLinSum s1 s2) = rec e1 t1 e2 s1
                                                 && rec e1 t2 e2 s2
    rec e1 (C.TBang t)       e2 (C.TBang s)       = rec e1 t e2 s
    rec e1 (C.TForall x t)   e2 (C.TForall y s)   = rec (M.insert x y e1) t
                                                        (M.insert y x e2) s
    rec e1 (C.TApp t1 t2)    e2 (C.TApp s1 s2)    = rec e1 t1 e2 s1
                                                 && rec e1 t2 e2 s2
    rec e1 (C.TLam x t)      e2 (C.TLam y s)      = rec (M.insert x y e1) t
                                                        (M.insert x y e2) s
    rec _  _                 e2 _                 = False

failExpectedBut :: Position -> C.Type -> String -> M a
failExpectedBut pos expected msg =
   failM pos ("Expected a term of type " ++ show expected ++ ".\n" ++
              msg)

failExpectedButGot :: Position -> C.Type -> C.Type -> M a
failExpectedButGot pos expected got =
   failM pos ("Expected a term of type " ++ show expected ++ ".\n" ++
              "Found a term of type    " ++ show got ++ ".")

assertTypeMatch :: Position -> Maybe C.Type -> C.Type -> M ()
assertTypeMatch _   Nothing         _   = return ()
assertTypeMatch pos (Just expected) got = do
  b <- equalTypes pos expected got
  if b
   then return ()
   else failExpectedButGot pos expected got

exprFreeVars :: C.Expr -> S.Set QName
exprFreeVars e = rec e
  where
    rec (C.EVar x)                = S.singleton x
    rec (C.EHole _)               = S.empty
    rec (C.ELam x e)              = rec e S.\\ S.singleton x
    rec (C.EApp e1 e2)            = rec e1 `S.union` rec e2
    rec (C.ELamT _ e)             = rec e
    rec (C.EAppT e _)             = rec e
    rec (C.ETensorI e1 e2)        = rec e1 `S.union` rec e2
    rec (C.ETensorE e1 x y e2)    = rec e1 `S.union` (rec e2 S.\\ S.fromList [x, y])
    rec (C.EWithI e1 e2)          = rec e1 `S.union` rec e2
    rec (C.EWithE1 e1 x e2)       = rec e1 `S.union` (rec e2 S.\\ S.singleton x)
    rec (C.EWithE2 e1 x e2)       = rec e1 `S.union` (rec e2 S.\\ S.singleton x)
    rec (C.ELinSumI1 e)           = rec e
    rec (C.ELinSumI2 e)           = rec e
    rec (C.ELinSumE e1 x e2 y e3) = rec e1 `S.union` (rec e2 S.\\ S.singleton x)
                                           `S.union` (rec e3 S.\\ S.singleton y)
    rec (C.EBangI e)              = rec e
    rec (C.EBangE e1 x e2)        = rec e1 `S.union` (rec e2 S.\\ S.singleton x)

checkBaseTypeCanBeGeneralized :: Position -> QName -> S.Set QName -> M ()
checkBaseTypeCanBeGeneralized pos x vars = do
  types <- mapM (lookupVarType pos) (S.toList vars)
  if x `S.member` S.unions (map typeFreeVars types)
    then failM pos ("Type variable "
                    ++ unqualifiedName x
                    ++ " cannot be generalized.")
    else return ()

checkVariablesAreNonLinear :: Position -> S.Set QName -> M ()
checkVariablesAreNonLinear pos vars = do
  flip mapM_ (S.toList vars)
    (\ x -> do
        linearity <- lookupVarLinearity pos x
        case linearity of
          VNonLinear -> return ()
          VLinear    ->
            failM pos ("Bang introduction depends on linear variable: "
                       ++ unqualifiedName x))

assertKType :: Position -> Maybe C.Kind -> M ()
assertKType _   Nothing        = return ()
assertKType _   (Just C.KType) = return ()
assertKType pos _              = failM pos "Expression is of atomic kind."

binaryType :: (C.Type -> C.Type -> C.Type)
           -> Position -> Maybe C.Kind -> Expr -> Expr -> M (C.Type, C.Kind)
binaryType f pos kind e1 e2 = do
  assertKType pos kind
  (t1, _) <- desugarType (Just C.KType) e1
  (t2, _) <- desugarType (Just C.KType) e2
  return (f t1 t2, C.KType)

desugarType :: Maybe C.Kind -> Expr -> M (C.Type, C.Kind)
---- Primitives
-- Unary
desugarType kind (EApp p (EVar _ x) e)
  | x == primitiveBang  = do assertKType p kind
                             (t, _) <- desugarType (Just C.KType) e
                             return (C.TBang t, C.KType)
-- Forall
desugarType kind (EApp p (EVar _ x) (EApp _ (EApp _ (EVar _ y) e1) e2))
  | x == primitiveForall && y == primitiveColon = do
      assertKType p kind
      xs <- desugarVariableSequence e1
      enterScope
      mapM_ (flip bindBaseTypeToKind C.KType) xs
      (t, _) <- desugarType (Just C.KType) e2
      leaveScope
      return (foldr C.TForall t xs, C.KType)
-- Binary
desugarType kind (EApp p (EApp _ (EVar _ x) e1) e2)
  | x == primitiveArrow  = binaryType C.TLinImp p kind e1 e2
  | x == primitiveTensor = binaryType C.TTensor p kind e1 e2
  | x == primitiveWith   = binaryType C.TWith   p kind e1 e2
  | x == primitiveLinSum = binaryType C.TLinSum p kind e1 e2
---- End primitives
desugarType kind (EApp p t1 t2) = do
  (t1', k) <- desugarType Nothing t1
  case k of
    C.KType        -> failM p ("Cannot apply type " ++ show t1'
                               ++ " of base kind.")
    C.KArrow k1 k2 -> do
      assertKindMatch p kind k2
      (t2', _) <- desugarType (Just k1) t2
      return (C.TApp t1' t2', k2)
desugarType kind (EVar p x) = do
  kind' <- lookupBaseTypeKind p x
  assertKindMatch p kind kind'
  return (C.TBase x, kind')
desugarType kind e          = failM (exprAnnotation e) "Expected a type."

desugarExpr :: Maybe C.Type -> Expr -> M (C.Expr, C.Type)
---- Primitives
desugarExpr mtyp (EApp _ (EApp _ (EVar pos x) e1)
                         (EApp _ (EApp _ (EVar _ y) e2) e3))
  | x == primitiveTensorElim && y == primitiveColon = do
      -- Tensor elimination
      xs <- desugarVariableSequence e2
      case xs of
        [x1, x2] -> do
          (mguard, tguard) <- desugarExpr Nothing e1
          case tguard of
            C.TTensor t1 t2 -> do
              enterScope
              bindLinearVariable (exprAnnotation e2) x1 t1
              bindLinearVariable (exprAnnotation e2) x2 t2
              (mbody, tbody) <- desugarExpr mtyp e3
              leaveScope
              return (C.ETensorE mguard x1 x2 mbody, tbody)
            _ -> failM (exprAnnotation e1)
                       "Expression is not of tensor type."
        _ -> failM (exprAnnotation e2)
                   "Tensor elimination must bind two variables."
desugarExpr mtyp (EApp _ (EApp _ (EVar pos x) e1)
                         (EApp _ (EApp _ (EVar _ y) e2) e3))
  | x `elem` [primitiveWithElim1, primitiveWithElim2] && y == primitiveColon = do
      -- With elimination
      xs <- desugarVariableSequence e2
      case xs of
        [x_i] -> do
          (mguard, tguard) <- desugarExpr Nothing e1
          case tguard of
            C.TWith t1 t2 -> do
              let (t_i, eWithE_i) = if x == primitiveWithElim1
                                     then (t1, C.EWithE1)
                                     else (t2, C.EWithE2)
              enterScope
              bindLinearVariable (exprAnnotation e2) x_i t_i
              (mbody, tbody) <- desugarExpr mtyp e3
              leaveScope
              return (eWithE_i mguard x_i mbody, tbody)
            _ -> failM (exprAnnotation e1)
                       "Expression is not of with type."
        _ -> failM (exprAnnotation e2)
                   "With elimination must bind one variable."
desugarExpr mtyp (EApp _ (EApp _ (EApp _ (EVar pos x) e1)
                                 (EApp _ (EApp _ (EVar _ y) e21) e22))
                         (EApp _ (EApp _ (EVar _ z) e31) e32))
  | x == primitiveLinSumElim && y == primitiveColon && z == primitiveColon = do
    -- Sum elimination
    xs <- desugarVariableSequence e21
    case xs of
      [x1] -> do
        ys <- desugarVariableSequence e31
        case ys of
          [x2] -> do
            (mguard, tguard) <- desugarExpr Nothing e1
            case tguard of
              C.TLinSum t1 t2 -> do
                enterScope
                bindLinearVariable (exprAnnotation e21) x1 t1
                (mbody1, tbody1) <- desugarExpr mtyp e22
                leaveScope
                enterScope
                bindLinearVariable (exprAnnotation e31) x2 t2
                (mbody2, _)      <- desugarExpr mtyp e32
                leaveScope
                return (C.ELinSumE mguard x1 mbody1 x2 mbody2, tbody1)
              _ -> failM (exprAnnotation e1)
                         "Expression is not of sum type."
          _ -> failM (exprAnnotation e21)
                     "Sum elimination must bind one variable."
      _ -> failM (exprAnnotation e21)
                 "Sum elimination must bind one variable."
desugarExpr mtyp (EApp _ (EApp _ (EVar pos x) e1)
                         (EApp _ (EApp _ (EVar _ y) e2) e3))
  | x == primitiveBangElim && y == primitiveColon = do
      -- Bang elimination
      xs <- desugarVariableSequence e2
      case xs of
        [x1] -> do
          (mguard, tguard) <- desugarExpr Nothing e1
          case tguard of
            C.TBang t -> do
              enterScope
              bindUVariable x1 t
              (mbody, tbody) <- desugarExpr mtyp e3
              leaveScope
              return (C.EBangE mguard x1 mbody, tbody)
            _ -> failM (exprAnnotation e1)
                       "Expression is not of bang type."
        _ -> failM (exprAnnotation e2)
                   "Bang elimination must bind one variable."
desugarExpr mtyp (EApp _ (EVar pos x) e)
  | x `elem` [primitiveLinSumIntro1, primitiveLinSumIntro2] = do
    -- Sum introduction
    (t1, t2) <-
      case mtyp of
        Just (C.TLinSum t1 t2) -> return (t1, t2)
        Just expected          -> failExpectedBut pos expected
                                    "Term is a tensor introduction."
        Nothing                -> failM pos "Sum of unknown type."
    let (t_i, eLinSumI_i) = if x == primitiveLinSumIntro1
                             then (t1, C.ELinSumI1)
                             else (t2, C.ELinSumI2)
    (m_i, _) <- desugarExpr (Just t_i) e
    t1' <- unfoldType pos t1
    t2' <- unfoldType pos t2
    return (eLinSumI_i m_i, C.TLinSum t1' t2')
desugarExpr mtyp (EApp _ (EVar pos x) e)
  | x == primitiveBang = do
    -- Bang introduction
    mt <- case mtyp of
            Just (C.TBang t) -> return $ Just t
            Just expected    -> failExpectedBut pos expected
                                     "Term is a bang introduction."
            Nothing          -> return Nothing
    (m, t') <- desugarExpr mt e
    checkVariablesAreNonLinear pos (exprFreeVars m)
    return (C.EBangI m, C.TBang t')
desugarExpr mtyp (EApp _ (EApp _ (EVar pos x) e1) e2)
  | x == primitiveTensor = do
    -- Tensor introduction
    (mtyp1, mtyp2) <-
      case mtyp of
        Nothing                -> return (Nothing, Nothing)
        Just (C.TTensor t1 t2) -> return (Just t1, Just t2)
        Just expected          -> failExpectedBut pos expected
                                    "Term is a tensor introduction."
    (m1, t1) <- desugarExpr mtyp1 e1
    (m2, t2) <- desugarExpr mtyp2 e2
    return (C.ETensorI m1 m2, C.TTensor t1 t2)
  | x == primitiveWithIntro = do
    (mtyp1, mtyp2) <-
      case mtyp of
        Nothing              -> return (Nothing, Nothing)
        Just (C.TWith t1 t2) -> return (Just t1, Just t2)
        Just expected        -> failExpectedBut pos expected
                                    "Term is a with introduction."
    (m1, t1) <- restoringScopes $ desugarExpr mtyp1 e1
    (m2, t2) <- desugarExpr mtyp2 e2
    return (C.EWithI m1 m2, C.TWith t1 t2)
---- End primitives
desugarExpr mtyp (EVar p x)
  | x == primitiveTensor       = failM p "Invalid syntax for tensor introduction."
  | x == primitiveTensorElim   = failM p "Invalid syntax for tensor elimination."
  | x == primitiveWithIntro    = failM p "Invalid syntax for with introduction."
  | x == primitiveWithElim1    = failM p "Invalid syntax for with elimination."
  | x == primitiveWithElim2    = failM p "Invalid syntax for with elimination."
  | x == primitiveLinSumIntro1 = failM p "Invalid syntax for sum introduction."
  | x == primitiveLinSumIntro2 = failM p "Invalid syntax for sum introduction."
  | x == primitiveLinSumElim   = failM p "Invalid syntax for sum elimination."
  | x == primitiveBang         = failM p "Invalid syntax for bang introduction."
  | x == primitiveBangElim     = failM p "Invalid syntax for bang elimination."
  | x == primitiveColon        = failM p "Invalid syntax."
  | otherwise = do
      typ' <- lookupAndConsumeVarType p x
      assertTypeMatch p mtyp typ'
      return (C.EVar x, typ')
desugarExpr mtyp (ELambda p e1 e2) = do
  case mtyp of
    Nothing  -> failM p "Abstraction of unknown type."
    Just typ -> do
      typ' <- unfoldType p typ
      case typ' of
        C.TLinImp t1 t2 -> do
          -- Implication introduction
          x <- desugarVariable e1
          enterScope
          bindLinearVariable (exprAnnotation e1) x t1
          (m, _) <- desugarExpr (Just t2) e2
          leaveScope
          return (C.ELam x m, C.TLinImp t1 t2)
        C.TForall x t1 -> do
          -- Universal introduction
          x <- desugarVariable e1
          enterScope
          bindBaseTypeToKind x C.KType
          (m, _) <- desugarExpr (Just t1) e2
          checkBaseTypeCanBeGeneralized p x (exprFreeVars m)
          leaveScope
          return (C.ELamT x m, C.TForall x t1)
        _ -> failM p ("Abstraction cannot have type " ++ show typ)
desugarExpr mtyp (EString p holeName) = do
  case mtyp of
    Nothing  -> failM p ("Hole " ++ show holeName ++ " of unknown type.") 
    Just typ -> do
      logFS "\n"
      logFS ("!!! Found hole " ++ show holeName ++ " of type " ++ show typ ++ ".")
      logFS "\n"
      return (C.EHole holeName, typ)
desugarExpr mtyp (EApp p e1 e2) = do
  (m1, t) <- desugarExpr Nothing e1
  case t of
    C.TLinImp t1 t2 -> do
      (m2, _) <- desugarExpr (Just t1) e2
      t2' <- unfoldType p t2
      return (C.EApp m1 m2, t2')
    C.TForall x t'  -> do
      (t2, _) <- desugarType (Just C.KType) e2
      t'' <- substTypeType t' x t2
      return (C.EAppT m1 t2, t'')
    _ -> failM p ("Term of type " ++ show t ++ " cannot be applied.")
desugarExpr _ _ = error "TODO"

desugarVariable :: Expr -> M QName
desugarVariable (EVar _ x) = return x
desugarVariable e = failM (exprAnnotation e) "Expected a variable"

desugarVariableSequence :: Expr -> M [QName]
desugarVariableSequence (EVar _ x)            = return [x]
desugarVariableSequence (EApp _ e (EVar _ x)) = do
  xs <- desugarVariableSequence e
  return (xs ++ [x])
desugarVariableSequence e =
  failM (exprAnnotation e) "Expected a sequence of variables"

---- Term evaluator

exprFreeTypeVars :: C.Expr -> S.Set QName
exprFreeTypeVars e = rec e
  where
    rec :: C.Expr -> S.Set QName
    rec (C.EVar x)                  = S.empty
    rec (C.EHole s)                 = S.empty
    rec (C.ELam x e)                = rec e
    rec (C.EApp e1 e2)              = rec e1 `S.union` rec e2
    rec (C.ELamT x e)               = rec e S.\\ S.singleton x
    rec (C.EAppT e t)               = rec e `S.union` typeFreeVars t
    rec (C.ETensorI e1 e2)          = rec e1 `S.union` rec e2
    rec (C.ETensorE e1 x y e2)      = rec e1 `S.union` rec e2
    rec (C.EWithI e1 e2)            = rec e1 `S.union` rec e2
    rec (C.EWithE1 e1 x e2)         = rec e1 `S.union` rec e2
    rec (C.EWithE2 e1 x e2)         = rec e1 `S.union` rec e2
    rec (C.ELinSumI1 e)             = rec e
    rec (C.ELinSumI2 e)             = rec e
    rec (C.ELinSumE e1 x1 e2 x2 e3) = rec e1 `S.union` rec e2 `S.union` rec e3
    rec (C.EBangI e)                = rec e
    rec (C.EBangE e1 x e2)          = rec e1 `S.union` rec e2

substExprType :: C.Expr -> QName -> C.Type -> M C.Expr
substExprType e0 x0 t0 = rec (M.fromList [(x0, t0)]) e0
  where
    substFVs :: M.Map QName C.Type -> S.Set QName
    substFVs subst =
      S.union (S.fromList (M.keys subst))
              (S.unions [typeFreeVars t | (_, t) <- M.toList subst])
    rec :: M.Map QName C.Type -> C.Expr -> M C.Expr
    rec subst (C.EVar x) = return $ C.EVar x
    rec subst (C.EHole s) = return $ C.EHole s
    rec subst (C.ELam x e) = C.ELam x <$> rec subst e
    rec subst (C.EApp e1 e2) = C.EApp <$> rec subst e1 <*> rec subst e2
    rec subst (C.ELamT x e) =
      let fvs = (exprFreeTypeVars e S.\\ S.singleton x) `S.union` substFVs subst
          z   = nameApartFrom x fvs
       in C.ELamT z <$> rec (M.insert x (C.TBase z) subst) e
    rec subst (C.EAppT e t) =
      C.EAppT <$> rec subst e <*> substTypeType' subst t
    rec subst (C.ETensorI e1 e2) =
      C.ETensorI <$> rec subst e1 <*> rec subst e2
    rec subst (C.ETensorE e1 x y e2) =
      C.ETensorE <$> rec subst e1 <*> pure x <*> pure y <*> rec subst e2
    rec subst (C.EWithI e1 e2) =
      C.EWithI <$> rec subst e1 <*> rec subst e2
    rec subst (C.EWithE1 e1 x e2) =
      C.EWithE1 <$> rec subst e1 <*> pure x <*> rec subst e2
    rec subst (C.EWithE2 e1 x e2) =
      C.EWithE2 <$> rec subst e1 <*> pure x <*> rec subst e2
    rec subst (C.ELinSumI1 e) = C.ELinSumI1 <$> rec subst e
    rec subst (C.ELinSumI2 e) = C.ELinSumI2 <$> rec subst e
    rec subst (C.ELinSumE e1 x1 e2 x2 e3) =
      C.ELinSumE <$> rec subst e1
                 <*> pure x1 <*> rec subst e2
                 <*> pure x2 <*> rec subst e3
    rec subst (C.EBangI e) = C.EBangI <$> rec subst e
    rec subst (C.EBangE e1 x e2) =
      C.EBangE <$> rec subst e1 <*> pure x <*> rec subst e2

substExprExpr' :: M.Map QName C.Expr -> C.Expr -> M C.Expr
substExprExpr' subst e0 = rec subst e0
  where
    substFVs :: M.Map QName C.Expr -> S.Set QName
    substFVs subst =
      S.union (S.fromList (M.keys subst))
              (S.unions [exprFreeVars e | (_, e) <- M.toList subst])
    rec :: M.Map QName C.Expr -> C.Expr -> M C.Expr
    rec subst (C.EVar x) =
      case M.lookup x subst of
        Nothing -> return $ C.EVar x
        Just e  -> return e
    rec _   (C.EHole s)  = return $ C.EHole s
    rec subst (C.ELam x e) =
      let fvs    = (exprFreeVars e S.\\ S.singleton x) `S.union` substFVs subst
          z      = nameApartFrom x fvs
          subst' = M.insert x (C.EVar z) subst
       in C.ELam z <$> rec subst' e
    rec subst (C.EApp e1 e2) =
      C.EApp <$> rec subst e1 <*> rec subst e2
    rec subst (C.ELamT x e) =
      C.ELamT x <$> rec subst e
    rec subst (C.EAppT e t) =
      flip C.EAppT t <$> rec subst e
    rec subst (C.ETensorI e1 e2) =
      C.ETensorI <$> rec subst e1 <*> rec subst e2
    rec subst (C.ETensorE e1 x y e2) =
      let fvs    = (exprFreeVars e2 S.\\ S.fromList [x, y])
                   `S.union` substFVs subst
          z1     = nameApartFrom x fvs
          z2     = nameApartFrom y (S.insert z1 fvs)
          subst' = M.insert y (C.EVar z2) (M.insert x (C.EVar z1) subst)
       in C.ETensorE <$> rec subst e1 <*> pure z1 <*> pure z2 <*> rec subst' e2
    rec subst (C.EWithI e1 e2) =
      C.EWithI <$> rec subst e1 <*> rec subst e2
    rec subst (C.EWithE1 e1 x e2) =
      let fvs    = (exprFreeVars e2 S.\\ S.singleton x) `S.union` substFVs subst
          z      = nameApartFrom x fvs
          subst' = M.insert x (C.EVar z) subst
       in C.EWithE1 <$> rec subst e1 <*> pure z <*> rec subst' e2
    rec subst (C.EWithE2 e1 x e2) =
      let fvs    = (exprFreeVars e2 S.\\ S.singleton x) `S.union` substFVs subst
          z      = nameApartFrom x fvs
          subst' = M.insert x (C.EVar z) subst
       in C.EWithE2 <$> rec subst e1 <*> pure z <*> rec subst' e2
    rec subst (C.ELinSumI1 e') = C.ELinSumI1 <$> rec subst e'
    rec subst (C.ELinSumI2 e') = C.ELinSumI2 <$> rec subst e'
    rec subst (C.ELinSumE e1 x1 e2 x2 e3) =
      let fvs1   = (exprFreeVars e2 S.\\ S.singleton x1) `S.union` substFVs subst
          z1     = nameApartFrom x1 fvs1
          subst1 = M.insert x1 (C.EVar z1) subst
          fvs2   = (exprFreeVars e3 S.\\ S.singleton x2) `S.union` substFVs subst
          z2     = nameApartFrom x2 fvs2
          subst2 = M.insert x2 (C.EVar z2) subst
       in C.ELinSumE <$> rec subst e1
                     <*> pure z1 <*> rec subst1 e2
                     <*> pure z2 <*> rec subst2 e3
    rec subst (C.EBangI e) = C.EBangI <$> rec subst e
    rec subst (C.EBangE e1 x e2) =
      let fvs    = (exprFreeVars e2 S.\\ S.singleton x) `S.union` substFVs subst
          z      = nameApartFrom x fvs
          subst' = M.insert x (C.EVar z) subst
       in C.EBangE <$> rec subst e1 <*> pure z <*> rec subst' e2

substExprExpr :: C.Expr -> QName -> C.Expr -> M C.Expr
substExprExpr e0 x0 f0 = substExprExpr' (M.fromList [(x0, f0)]) e0

evalExpr :: C.Expr -> M C.Expr
evalExpr e = rec S.empty e
  where
    rec :: S.Set QName -> C.Expr -> M C.Expr
    rec bound (C.EVar x) = do
      if x `S.member` bound
       then return $ C.EVar x
       else do
         r <- lookupVarDefinition x 
         case r of
           C.EVar y | x == y -> return $ C.EVar y
           _                 -> evalExpr r
    rec bound (C.EHole s) =
      return $ C.EHole s
    rec bound (C.ELam x e) =
      C.ELam x <$> rec (S.insert x bound) e
    rec bound (C.EApp e1 e2) = do
      e1' <- rec bound e1
      e2' <- rec bound e2
      case e1' of
        C.ELam x e1'' -> do
          e' <- substExprExpr e1'' x e2'
          rec bound e'
        _ -> return $ C.EApp e1' e2'
    rec bound (C.ELamT x e) =
      C.ELamT x <$> rec bound e
    rec bound (C.EAppT e t) = do
      e' <- rec bound e
      case e' of
        C.ELamT x e'' -> do
          e''' <- substExprType e'' x t
          rec bound e'''
        _ -> return $ C.EAppT e' t
    rec bound (C.ETensorI e1 e2) =
      C.ETensorI <$> rec bound e1 <*> rec bound e2
    rec bound (C.ETensorE e1 x y e2) = do
      e1' <- rec bound e1
      case e1' of
        C.ETensorI a b -> do
          e2'  <- substExprExpr' (M.fromList [(x, a), (y, b)]) e2
          rec bound e2'
        _ -> C.ETensorE e1' x y <$> rec (S.fromList [x, y] `S.union` bound) e2
    rec bound (C.EWithI e1 e2) =
      C.EWithI <$> rec bound e1 <*> rec bound e2
    rec bound (C.EWithE1 e1 x e2) = do
      e1' <- rec bound e1
      case e1' of
        C.EWithI a _ -> do
          e2' <- substExprExpr e2 x a
          rec bound e2'
        _ -> C.EWithE1 e1' x <$> rec (S.insert x bound) e2
    rec bound (C.EWithE2 e1 x e2) = do
      e1' <- rec bound e1
      case e1' of
        C.EWithI _ b -> do
          e2' <- substExprExpr e2 x b
          rec bound e2'
        _ -> C.EWithE2 e1' x <$> rec (S.insert x bound) e2
    rec bound (C.ELinSumI1 e) = C.ELinSumI1 <$> rec bound e
    rec bound (C.ELinSumI2 e) = C.ELinSumI2 <$> rec bound e
    rec bound (C.ELinSumE e1 x1 e2 x2 e3) = do
      e1' <- rec bound e1
      case e1' of
        C.ELinSumI1 a -> do
          e2' <- substExprExpr e2 x1 a
          rec bound e2'
        C.ELinSumI2 a -> do
          e3' <- substExprExpr e3 x2 a
          rec bound e3'
        _ -> C.ELinSumE e1' <$> pure x1 <*> rec (S.insert x1 bound) e2
                            <*> pure x2 <*> rec (S.insert x2 bound) e3
    rec bound (C.EBangI e) = C.EBangI <$> rec bound e
    rec bound (C.EBangE e1 x e2) = do
      e1' <- rec bound e1
      case e1' of
        C.EBangI a -> do
          e2' <- substExprExpr e2 x a
          rec bound e2'
        _ -> C.EBangE e1' x <$> rec (S.insert x bound) e2

