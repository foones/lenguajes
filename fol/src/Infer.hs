module Infer(inferTypes) where

import Expr(
         Id(..), idOf,
         HypId(..), TyconId(..), TyvarId(..), PredId(..), FunId(..), VarId(..),
         Declaration(..),
         Metavar(..), firstMetavar, nextMetavar,
         Type(..), Term(..), Form(..),
         ConstructorDecl(..), Proof(..), OptionalName(..),
         HoleName(..), OptionalForm(..), ThusType(..),
         Command(..), CaseBranch(..), IndBranch(..), IndPattern(..),
         freeVars
       )

import FailState(FailState, getFS, putFS, modifyFS, failFS, evalFS)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

----

freeTypeVars :: Type -> S.Set TyvarId
freeTypeVars (TyApp _ tys) = S.unions (map freeTypeVars tys)
freeTypeVars (TyVar x)     = S.singleton x
freeTypeVars TyUnknown     = S.empty
freeTypeVars (TyMetavar _) = S.empty
freeTypeVars TyProp        = S.empty

freeMetavars :: Type -> S.Set Metavar
freeMetavars (TyApp _ tys) = S.unions (map freeMetavars tys)
freeMetavars (TyVar _)     = S.empty
freeMetavars TyUnknown     = S.empty
freeMetavars (TyMetavar m) = S.singleton m
freeMetavars TyProp        = S.empty

----

data TypeScheme = TypeScheme [TyvarId] [Type] Type

data LocalVariable = LVar VarId | LEigenVar VarId
  deriving (Eq, Ord)

instance Show LocalVariable where
  show (LVar x)      = show x
  show (LEigenVar x) = show x

type M a = FailState String InferState a
data InferState = InferState {
                    isNextFreshMetavar :: Metavar
                  , isInstantiation    :: M.Map Metavar Type
                  , isTypedSymbols     :: M.Map Id TypeScheme
                  , isEnvironment      :: [(LocalVariable, Type)]
                  , isDataConstructors :: M.Map TyconId (S.Set FunId)
                  }

registerTypedSymbol :: Id -> TypeScheme -> M ()
registerTypedSymbol f scheme = do
  typedSymbols <- isTypedSymbols <$> getFS
  case M.lookup f typedSymbols of
    Just _ ->
      failFS ("Symbol '" ++ show f ++ "' has already been registered with a type.")
    Nothing ->
      modifyFS (\ state -> state {
                 isTypedSymbols = M.insert f scheme (isTypedSymbols state)
               })

freshType :: M Type
freshType = do
  state <- getFS
  putFS (state {
           isNextFreshMetavar = nextMetavar (isNextFreshMetavar state)
         })
  return $ TyMetavar (isNextFreshMetavar state)

substituteType :: M.Map TyvarId Type -> Type -> Type
substituteType sub (TyApp c tys) = TyApp c $ map (substituteType sub) tys
substituteType sub (TyVar a)     = M.findWithDefault (TyVar a) a sub
substituteType _   TyUnknown     = TyUnknown
substituteType _   (TyMetavar n) = TyMetavar n
substituteType _   TyProp        = TyProp

getFreshTypeForSymbol :: Id -> M ([Type], Type)
getFreshTypeForSymbol f = do
  typedSymbols <- isTypedSymbols <$> getFS
  case M.lookup f typedSymbols of
    Nothing ->
      failFS ("Symbol '" ++ show f ++ "' has no registered type.")
    Just (TypeScheme ftvs argTyps resTyp) -> do
      typs <- mapM (const freshType) ftvs
      let sub = M.fromList (zip ftvs typs)
      return (map (substituteType sub) argTyps, substituteType sub resTyp)

enterScope :: LocalVariable -> Type -> M ()
enterScope x ty =
  modifyFS (\ state -> state {
             isEnvironment = (x, ty) : isEnvironment state
           })

lookupLocalVariable :: LocalVariable -> M Type
lookupLocalVariable x = do
  environment <- isEnvironment <$> getFS
  case lookup x environment of
    Nothing -> failFS ("Unbound variable: \"" ++ show x ++ "\".")
    Just ty -> return ty

leaveScope :: M ()
leaveScope =
  modifyFS (\ state -> state {
             isEnvironment = tail (isEnvironment state)
           })

---- Unification

representative :: Type -> M Type
representative (TyMetavar m) = do
  instantiation <- isInstantiation <$> getFS
  case M.lookup m instantiation of
    Nothing -> return $ TyMetavar m
    Just t  -> representative t
representative t = return t

unfold :: Type -> M Type
unfold t = do
  t' <- representative t
  case t' of
    TyApp c tys -> TyApp c <$> mapM unfold tys
    TyVar x     -> return $ TyVar x
    TyUnknown   -> return TyUnknown
    TyMetavar m -> return $ TyMetavar m
    TyProp      -> return TyProp

occursIn :: Metavar -> Type -> M Bool
occursIn m ty = do
  ty' <- unfold ty
  return $ m `S.member` freeMetavars ty' 

instantiate :: Metavar -> Type -> M ()
instantiate m ty = do
  bOccursCheck <- m `occursIn` ty
  if bOccursCheck
   then failFS "Occurs check failure"
   else do
     instantiation <- isInstantiation <$> getFS
     case M.lookup m instantiation of
       Just _  -> failFS ("Metavariable " ++ show (TyMetavar m) ++ " already instantiated.")
       Nothing ->
         modifyFS (\ state -> state {
           isInstantiation = M.insert m ty (isInstantiation state)
         })

unify :: Type -> Type -> M ()
unify t10 t20 = do
  t1 <- representative t10
  t2 <- representative t20
  case (t1, t2) of
    (TyMetavar m1, TyMetavar m2) | m1 == m2 -> return ()
    (TyMetavar m1, _) -> instantiate m1 t2
    (_, TyMetavar m2) -> instantiate m2 t1
    (TyVar x1, TyVar x2) | x1 == x2 -> return ()
    (TyApp c1 tys1, TyApp c2 tys2) | c1 == c2 && length tys1 == length tys2 ->
      mapM_ (uncurry unify) (zip tys1 tys2)
    (TyProp, TyProp) -> return () 
    _ -> do
      ut1 <- unfold t1
      ut2 <- unfold t2
      failFS (
          "Types do not unify.\n"
       ++ "Expected: " ++ show ut1 ++ "\n"
       ++ "Got:      " ++ show ut2 ++ "\n"
       )

----

inferTypes :: [Declaration] -> Either String [Declaration]
inferTypes decls = evalFS (inferTypesM decls) initialState
  where
    initialState = InferState {
                     isNextFreshMetavar = firstMetavar
                   , isInstantiation    = M.empty
                   , isTypedSymbols     = M.empty
                   , isEnvironment      = []
                   , isDataConstructors = M.empty
                   }

inferTypesM :: [Declaration] -> M [Declaration]
inferTypesM decls = mapM inferDeclaration decls

inferDeclaration :: Declaration -> M Declaration
inferDeclaration decl@(DeclData typ constructors) = do
  case typ of
    TyApp tyCon args -> do
      let constructorNames = map (\ (ConstructorDecl c _) -> c) constructors
      modifyFS (\ state -> state {
          isDataConstructors = M.insert tyCon (S.fromList constructorNames)
                                        (isDataConstructors state)
        })
    _ -> failFS "Datatype should be headed by a type constructor."
  mapM_ (\ (ConstructorDecl c argTyps) -> do
            let ftvs = S.toList . S.unions $ map freeTypeVars (typ : argTyps)
            registerTypedSymbol (idOf c) (TypeScheme ftvs argTyps typ))
        constructors
  return decl
inferDeclaration decl@(DeclProp p argTyps) = do
  let ftvs = S.toList . S.unions $ map freeTypeVars argTyps
  registerTypedSymbol (idOf p) (TypeScheme ftvs argTyps TyProp)
  return decl
inferDeclaration (DeclAxiom id form) =
  DeclAxiom id <$> inferForm form
inferDeclaration (DeclTheorem id form proof) =
  DeclTheorem id <$> inferForm form
                 <*> inferProof proof

inferTerm :: Term -> M (Term, Type)
inferTerm (TApp f ts) = do
  (argTyps, resTyp) <- getFreshTypeForSymbol (idOf f)
  (ts', argTyps')   <- unzip <$> mapM inferTerm ts
  mapM_ (uncurry unify) (zip argTyps argTyps')
  return (TApp f ts', resTyp)
inferTerm (TEigenVar x) = do
  ty <- lookupLocalVariable (LEigenVar x)
  return (TEigenVar x, ty)
inferTerm (TVar x)      = do
  ty <- lookupLocalVariable (LVar x)
  return (TVar x, ty)

inferForm :: Form -> M Form
inferForm (FPred p ts)     = do
  (argTyps, resTyp) <- getFreshTypeForSymbol (idOf p)
  (ts', argTyps')   <- unzip <$> mapM inferTerm ts
  unify resTyp TyProp
  mapM_ (uncurry unify) (zip argTyps argTyps')
  return $ FPred p ts'
inferForm FTrue            = return $ FTrue
inferForm FFalse           = return $ FFalse
inferForm (FImp a b)       = FImp <$> inferForm a <*> inferForm b
inferForm (FOr a b)        = FOr <$> inferForm a <*> inferForm b
inferForm (FAnd a b)       = FAnd <$> inferForm a <*> inferForm b
inferForm (FNot a)         = FNot <$> inferForm a
inferForm (FForall x ty a) = inferBinder "universal quantifier" FForall x ty a
inferForm (FExists x ty a) = inferBinder "existential quantifier" FExists x ty a

inferBinder :: String -> (VarId -> Type -> Form -> Form)
            -> VarId -> Type -> Form -> M Form
inferBinder quantifierName fQuantifier x ty a = do
  tyX <- freshType
  case ty of
    TyUnknown -> return ()
    _         -> unify ty tyX
  enterScope (LVar x) tyX
  a' <- inferForm a
  leaveScope
  ty' <- unfold tyX
  if null (freeMetavars ty')
   then return $ fQuantifier x ty' a'
   else failFS ("Type of " ++ quantifierName
                ++ " must be annotated as it cannot be inferred.")

inferOForm :: OptionalForm -> M OptionalForm
inferOForm OFNone            = return OFNone
inferOForm (OFHole holeName) = return $ OFHole holeName
inferOForm (OFForm form)     = OFForm <$> inferForm form

insideTypedNameScope :: [(VarId, Type)] -> M a -> M b -> M ([(VarId, Type)], a, b)
insideTypedNameScope typedNames actionAsVars actionAsEigenVars = do
  newTypes <- flip mapM typedNames
                (\ (_, ty) -> do
                  tyX <- freshType
                  case ty of
                    TyUnknown -> return ()
                    _         -> unify ty tyX
                  return tyX)
  let newTypedNames = zip (map fst typedNames) newTypes
  -- Bind as variables
  flip mapM_ newTypedNames (\ (x, tyX) -> do enterScope (LVar x) tyX)
  result1 <- actionAsVars
  mapM_ (\ _ -> leaveScope) typedNames
  -- Bind as eigenvariables
  flip mapM_ newTypedNames (\ (x, tyX) -> do enterScope (LEigenVar x) tyX)
  result2 <- actionAsEigenVars
  mapM_ (\ _ -> leaveScope) typedNames
  --
  newTypes' <- mapM unfold newTypes
  let newTypedNames' = zip (map fst typedNames) newTypes'
  return (newTypedNames', result1, result2)

inferProof :: Proof -> M Proof
inferProof ProofEmpty = return ProofEmpty
inferProof (ProofCons (CmdLet typedNames) proof) = do
  (typedNames', (), proof') <- insideTypedNameScope typedNames
                                                    (return ())
                                                    (inferProof proof)
  return (ProofCons (CmdLet typedNames') proof')
inferProof (ProofCons (CmdConsider typedNames oname form names) proof) = do
  (typedNames', form', proof') <- insideTypedNameScope typedNames
                                                       (inferForm form)
                                                        (inferProof proof)
  return $ ProofCons (CmdConsider typedNames' oname form' names) proof'
inferProof (ProofCons cmd proof) =
  ProofCons <$> inferCommand cmd
            <*> inferProof proof

inferCommand :: Command -> M Command
inferCommand (CmdSuppose oName oForm) = CmdSuppose oName <$> inferOForm oForm
inferCommand (CmdThus cmd oName oForm names) =
  flip (CmdThus cmd oName) names <$> inferOForm oForm
inferCommand (CmdHave oName form names) =
  flip (CmdHave oName) names <$> inferForm form
inferCommand (CmdLet _) = error "Impossible"
inferCommand (CmdCases oName oJustifications caseBranches) =
  CmdCases oName oJustifications <$> mapM inferCaseBranch caseBranches
inferCommand (CmdTake tm) = do
  (tm', _) <- inferTerm tm
  return $ CmdTake tm'
inferCommand (CmdConsider _ _ _ _) = error "Impossible"
inferCommand (CmdClaim oName form proof) =
  CmdClaim oName <$> inferForm form <*> inferProof proof
inferCommand (CmdInduction ty indBranches) = inferInduction ty indBranches
inferCommand (CmdShow form) = CmdShow <$> inferForm form

inferCaseBranch :: CaseBranch -> M CaseBranch
inferCaseBranch (CaseBranch oName form proof) =
  CaseBranch oName <$> inferForm form
                   <*> inferProof proof

inferInduction :: Type -> [IndBranch] -> M Command
inferInduction ty indBranches = do
     tyInd <- freshType
     case ty of
       TyUnknown -> return ()
       _         -> unify ty tyInd
     indBranches' <- mapM (inferIndBranch tyInd) indBranches
     tyInd' <- unfold tyInd
     if not (null (freeMetavars tyInd'))
      then failFS ("Type of induction must be annotated as it cannot be inferred.")
      else return ()
     let constructorsInBranches = map (\ (IndBranch (IndPattern c _) _) -> c)
                                      indBranches
     checkConstructorsComplete tyInd' constructorsInBranches
     return $ CmdInduction tyInd' indBranches'
  where
    checkConstructorsComplete :: Type -> [FunId] -> M ()
    checkConstructorsComplete (TyApp tyCon _) branchConstructors = do 
      dataConstructors <- isDataConstructors <$> getFS
      case M.lookup tyCon dataConstructors of
        Nothing -> failFS ("Unregistered type constructor " ++ show tyCon)
        Just typeConstructors -> do
          let rep = branchConstructors L.\\ L.nub branchConstructors
          if not (null rep)
           then failFS ("induction: repeated type constructors: " ++ show rep)
           else return ()
          let dif1 = S.fromList branchConstructors S.\\ typeConstructors
          if not (S.null dif1)
           then failFS ("induction: cases are not constructors of type "
                      ++ show tyCon ++ " : " ++ show dif1)
           else return ()
          let dif2 = typeConstructors S.\\ S.fromList branchConstructors
          if not (S.null dif2)
           then failFS ("induction: missing constructors in case analysis: "
                      ++ show dif2)
           else return ()
          return ()
    checkConstructorsComplete _ _ =
      failFS "Type of induction must be lead by a type constructor"

inferIndBranch :: Type -> IndBranch -> M IndBranch
inferIndBranch tyInd (IndBranch (IndPattern f args) proof) = do
  let (argNames, argTyps) = unzip args
  (paramTyps, resTyp) <- getFreshTypeForSymbol (idOf f)
  unify tyInd resTyp
  if length argNames /= length paramTyps
   then failFS ("Constructor \"" ++ show f ++ "\" in pattern "
             ++ "is given a wrong number of arguments.")
   else return ()
  mapM_ (\ (argTyp, paramTyp) ->
           case argTyp of
             TyUnknown -> return ()
             _ -> unify argTyp paramTyp)
        (zip argTyps paramTyps)
  let typedVars = zip argNames paramTyps
  flip mapM_ typedVars (\ (x, tyX) -> enterScope (LEigenVar x) tyX)
  proof' <- inferProof proof
  flip mapM_ typedVars (\ _ -> leaveScope)
  typedVars' <- mapM (\ (x, ty) -> do
                        ty' <- unfold ty
                        return (x, ty'))
                     typedVars 
  return $ IndBranch (IndPattern f typedVars') proof'

