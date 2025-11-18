
module Elab.Elab(elaborate) where

import Data.List(nub, intersect)
import qualified Data.Set as S
import qualified Data.Map as M

import FailState(
         ErrMsg, FailState, evalFS, getFS, putFS, modifyFS, failFS, logFS,
         tryOrBacktrackFS
       )

import Utils(joinS, filterSplit, indent)
import qualified Syntax.Position as Pos
import Syntax.Name(
         HoleName, QName, globalNameType, makeInternalQName
       )
import Syntax.AST(
         AProgram(..), ADeclaration(..), AParameter(..),
         AConstructorDecl(..), AExpr(..), piManyParams, makeImplicit,
         AOptionalExpr(..),
         ACaseBranch(..), APattern(..),
         Program, Declaration, Parameter, ConstructorDecl,
         Expr, OptionalExpr, CaseBranch, Pattern,
         patternHead, annotation, parameterName, patternToExpr
       )
import Core.Term(
         ConstId(..), LocalId(..), MetavarId(..), UVarId(..), UMetavarId(..),
         Universe(..), Term(..), TPiInfo(..), TPiParam(..), Context(..),
         tAppMany, isWeakHeadReducible, weakHeadReduce, whnf,
         normalize, 
         substitute, splitTermArgs,
         freeMetavars, freeLocals, freeConsts, isTLocal, localIdFromTLocal,
         simplifyUniverse, itUSucc, weakHeadUnfoldM, weakUnfoldM
       )
import qualified Elab.UniverseSolver as USolver

elaborate :: Program -> Either String Context
elaborate program = evalFS initialState (elaborateM program)
  where
    initialState =
      ElabState {
        nextFreshMetavar            = 0
      , nextFreshUVar               = 0
      , nextFreshUMetavar           = 0
      , nextFreshLocal              = 0
      , context                     = CEmpty
      , declaredConsts              = M.empty
      , pendingConstraints          = []
      , instMetavars                = M.empty
      , instUMetavars               = M.empty
      , registeredDataTypes         = M.empty
      , registeredHoles             = []
      , pendingInaccessiblePatterns = []
      }

data ConstantSort = SDataType
                  | SConstructor
                  | SFunction

instance Show ConstantSort where
  show SDataType    = "data type"
  show SConstructor = "constructor"
  show SFunction    = "function"

data ElabState =
  ElabState {
    nextFreshMetavar            :: Integer
  , nextFreshUVar               :: Integer
  , nextFreshUMetavar           :: Integer
  , nextFreshLocal              :: Integer
  , context                     :: Context
  , declaredConsts              :: M.Map ConstId (Pos.Position, ConstantSort)
  , pendingConstraints          :: [Constraint]
  , instMetavars                :: M.Map MetavarId Term
  , instUMetavars               :: M.Map UMetavarId Universe
  , registeredDataTypes         :: M.Map ConstId DataTypeInfo
  , registeredHoles             :: [HoleInfo]
  , pendingInaccessiblePatterns :: [(Term, Pattern)]
  }

data HoleInfo = HoleInfo {
                  holePos  :: Pos.Position
                , holeName :: HoleName
                , holeEnv  :: LocalEnv
                , holeTerm :: Term
                , holeType :: Term
                }

data DataTypeInfo = DataTypeInfo {
                      dataTypeSort        :: Term
                    , dataTypeFixedParams :: [TPiParam]
                    , dataTypeIndices     :: [TPiParam]
                    , dataTypeRoot        :: Term
                    }

data LocalEnv = LEmpty
              | LExtend LocalEnv LocalId Term

instance Show LocalEnv where
  show LEmpty = ""
  show (LExtend env x t) = show env
                        ++ show x ++ " : " ++ show t ++ "\n"

localEnvLookup :: LocalEnv -> LocalId -> Maybe Term
localEnvLookup LEmpty _ = Nothing
localEnvLookup (LExtend env x t) y
  | x == y    = Just t
  | otherwise = localEnvLookup env y

localEnvLookupTypeOrFail :: LocalEnv -> LocalId -> Term
localEnvLookupTypeOrFail LEmpty x = error ("Unbound local variable: " ++ show x)
localEnvLookupTypeOrFail (LExtend env x t) y
  | x == y    = t
  | otherwise = localEnvLookupTypeOrFail env y

localEnvFromList :: [(LocalId, Term)] -> LocalEnv
localEnvFromList list = foldl (\ env (x, t) -> LExtend env x t) LEmpty list

localEnvNames :: LocalEnv -> [LocalId]
localEnvNames env = nub (rec env)
  where
    rec LEmpty = []
    rec (LExtend env x _) = rec env ++ [x]

data Constraint =
    C_Unify LocalEnv Term Term Reason
  | -- t1 is of the form  ∀ (x1 : A1) ... (xn : An) -> s
    -- where s unifies with t2
    C_UnifyCodomain LocalEnv Term Term Reason
  | C_LeSorts LocalEnv Term Term Reason
  | C_LeUniverses Universe Universe Reason

instance Show Constraint where
  show (C_Unify env t1 t2 reason) =
       "Unify:\n"
    ++ "  " ++ show t1 ++ "\n"
    ++ "  " ++ show t2 ++ "\n"
    ++ "  Environment:\n" ++ indent 4 (show env) ++ "\n"
    ++ "  " ++ show reason ++ "\n"
  show (C_UnifyCodomain env t1 t2 reason) =
       "Unify codomain:\n"
    ++ "  " ++ show t1 ++ "\n"
    ++ "  must be of the form ∀ ... → ... → " ++ show t2 ++ "\n"
    ++ "  Environment: " ++ show env ++ "\n"
    ++ "  " ++ show reason ++ "\n"
  show (C_LeSorts env t1 t2 reason) =
       "Universe level of the following sort:\n"
    ++ "  " ++ show t1 ++ "\n"
    ++ "should at most the universe level of the sort:\n"
    ++ "  " ++ show t2 ++ "\n"
    ++ "  Environment: " ++ show env ++ "\n"
    ++ "  " ++ show reason ++ "\n"
  show (C_LeUniverses u1 u2 reason) =
       "Universe level:\n"
    ++ "  " ++ show u1 ++ "\n"
    ++ "should be at most:\n"
    ++ "  " ++ show u2 ++ "\n"
    ++ "  " ++ show reason ++ "\n"

logConstraintsM :: [Constraint] -> M ()
logConstraintsM constraints = do
  constraints' <- mapM unfold constraints
  mapM_ (logFS . show) constraints'

logPendingConstraints :: M ()
logPendingConstraints = do
  pc <- pendingConstraints <$> getFS
  logFS "--- Pending constraints ---"
  logConstraintsM pc
  logFS "---"

logContext :: M ()
logContext = do
  ctx <- context <$> getFS
  ctx' <- unfold ctx
  logFS . show $ ctx'

logPendingHoles :: M ()
logPendingHoles = do
    holes <- registeredHoles <$> getFS
    if null holes
     then return ()
     else flip mapM_ holes
            (\ holeInfo -> do
                 str <- showHoleInfo holeInfo
                 logFS str)
  where
    showHoleInfo :: HoleInfo -> M String
    showHoleInfo hole = do
      env  <- unfold (holeEnv hole)
      term <- unfold (holeTerm hole)
      typ  <- unfold (holeType hole)
      return (
          "! Hole " ++ show (holeName hole) ++ "\n"
       ++ "  Near " ++ show (holePos hole) ++ "\n"
       ++ indent 2 (show env) ++ "\n"
       ++ "  _____\n"
       ++ "  Term: " ++ show term ++ "\n"
       ++ "  Type: " ++ show typ ++ "\n"
       )

data Reason = Reason Pos.Position [ReasonPart]

data ReasonPart = RS String
                | RT Term

instance Show ReasonPart where
  show (RS str)  = str
  show (RT term) = show term

reasonPosition :: Reason -> Pos.Position
reasonPosition (Reason pos _) = pos

instance Show Reason where
  show (Reason pos reasonParts) =
       "Near: " ++ show pos ++ "\n"
    ++ joinS "" (map show reasonParts)

type M = FailState ElabState

elaborateM :: Program -> M Context
elaborateM (Program declarations) = do
    let declarationBlocks = splitInBlocks declarations
    mapM_ elaborateDeclarationBlock declarationBlocks
    ---- Show context
    --logContext
    ---- Show pending constraints
    constraints <- pendingConstraints <$> getFS
    if not (null constraints)
     then logPendingConstraints
     else return ()
    ---- Do all checks
    mapM_ doCheck (filter isCheck declarations)
    ---- Show pending holes
    logPendingHoles
    ----
    ctx <- context <$> getFS
    return ctx 
  where
    isCheck :: Declaration -> Bool
    isCheck (DeclCheck _ _) = True
    isCheck _               = False

splitInBlocks :: [Declaration] -> [[Declaration]]
splitInBlocks [] = [[]]
splitInBlocks (decl : decls) =
    let (block : blocks) = splitInBlocks decls in
      if not (null block) && isBody decl && isHeader (head block)
       then [decl] : block : blocks
       else (decl : block) : blocks
  where
    isHeader :: Declaration -> Bool
    isHeader (DeclDataType _ _ _ _) = True
    isHeader (DeclType _ _ _) = True
    isHeader (DeclCheck _ _) = True
    isHeader _ = False
    isBody :: Declaration -> Bool
    isBody decl = not (isHeader decl)

elaborateDeclarationBlock :: [Declaration] -> M ()
elaborateDeclarationBlock declarations = do
    declaredDataTypes <-
      concat <$> mapM preprocessDataTypeDeclaration
                      (filter isDataTypeDecl declarations)
    declaredConstructors <-
      concat <$> mapM (preprocessDataConstructorsDeclaration
                        (S.fromList declaredDataTypes))
                      (filter isDataConstructorsDecl declarations)
    declaredFunctions <-
      concat <$> mapM preprocessTypeDeclaration
                      (filter isTypeDecl declarations)
    declaredFunctionsInEquations <-
      concat <$> mapM preprocessEquation
                      (filter isEquation declarations)
    let allDeclaredConstantNames = declaredDataTypes
                                ++ declaredConstructors
                                ++ declaredFunctions
                                ++ declaredFunctionsInEquations
    -----
    solveAllPendingConstraints
    -- TODO: propagate universe constraints
    generalizeConstants declarations allDeclaredConstantNames
  where
    isDataTypeDecl :: Declaration -> Bool
    isDataTypeDecl (DeclDataType _ _ _ _) = True
    isDataTypeDecl _                      = False
    isDataConstructorsDecl :: Declaration -> Bool
    isDataConstructorsDecl (DeclDataConstructors _ _ _ _) = True
    isDataConstructorsDecl _                              = False
    isTypeDecl :: Declaration -> Bool
    isTypeDecl (DeclType _ _ _) = True
    isTypeDecl _                = False
    isEquation :: Declaration -> Bool
    isEquation (DeclEquation _ _ _) = True
    isEquation _                    = False

-------------------------------------------------------------------------------
-- Preprocessor
-------------------------------------------------------------------------------

preprocessDataTypeDeclaration :: Declaration -> M [ConstId]
preprocessDataTypeDeclaration (DeclDataType pos typeName params eTyp) = do
  (tTyp, tTypTyp) <- inferExpr LEmpty (piManyParams params eTyp)
  let (tTypParams, tTypFamily) = splitPiFixedParams params tTyp
  let (tTypIndices, tTypRoot)  = splitPiIndices tTypFamily
  registerDataType (ConstId typeName) tTypTyp tTypParams tTypIndices tTypRoot
  ensureTailSort pos LEmpty tTyp
    [RS "When checking the definition of ", RS (show typeName), RS "."]
  declareConst pos SDataType (ConstId typeName) [] tTyp
  return [ConstId typeName]
preprocessDataTypeDeclaration _ = error "(Impossible)"

preprocessDataConstructorsDeclaration ::
     S.Set ConstId  -- name of all the data types that are being
                    -- simultaneously declared
  -> Declaration    -- constructor declaration
  -> M [ConstId]
preprocessDataConstructorsDeclaration
      simTypeNames
      (DeclDataConstructors pos typeName params ctors) = do
  let cTypeName = ConstId typeName
  checkConstantDeclaredAsDataType pos cTypeName
  constNames <-
    concat <$> mapM (preprocessConstructorDeclaration simTypeNames cTypeName params)
                    ctors
  return constNames
preprocessDataConstructorsDeclaration _ _ = error "(Impossible)"

preprocessTypeDeclaration :: Declaration -> M [ConstId]
preprocessTypeDeclaration (DeclType pos funName eTyp) = do
  (tTyp, tTypTyp) <- inferExpr LEmpty eTyp
  ensureSort pos LEmpty tTypTyp
    [RS "When checking that the declared type of function ",
     RS (show funName), RS " is a type."]
  declareConst pos SFunction (ConstId funName) [] tTyp
  return [ConstId funName]
preprocessTypeDeclaration _ = error "(Impossible)"

preprocessEquation :: Declaration -> M [ConstId]
preprocessEquation (DeclEquation pos lhs rhs) = do
    funcName <- case patternHead lhs of
                  PVar _ funcName -> return funcName
                  _ -> failM (annotation lhs)
                             ("Head of pattern in LHS of equation " ++
                              "must be a function name.")
    declareFunctionIfNotExisting pos LEmpty (ConstId funcName)
    (env, tLhs, tLhsTyp) <- inferPattern LEmpty lhs
    (tRhs, tRhsTyp) <- inferExpr env rhs
    -- TODO: termination checker
    emitConstraint $
      C_Unify env tLhsTyp tRhsTyp
        (Reason pos
           ([RS "Types of LHS and RHS of equation must unify.\n",
             RS "  Type of LHS: ", RT tLhsTyp, RS "\n",
             RS "  Type of RHS: ", RT tRhsTyp]))
    declareEquation (localEnvNames env) tLhs tRhs
    return []
  where
    declareFunctionIfNotExisting :: Pos.Position -> LocalEnv -> ConstId -> M ()
    declareFunctionIfNotExisting pos env funcName = do
      state <- getFS
      case M.lookup funcName (declaredConsts state) of
        Just (pos', prevSort) ->
          case prevSort of
            SFunction -> return ()
            _ -> failM pos ("Constant " ++ show funcName
                            ++ " already declared as a " ++ show prevSort
                            ++ " (near " ++ show pos' ++ ").")
        Nothing -> do
          fTyp <- freshMetavarAt env
          modifyFS (\ state ->
            state {
              context = CExtendConst (context state) funcName [] fTyp
            , declaredConsts = M.insert funcName (pos, SFunction) (declaredConsts state)
            })
    declareEquation :: [LocalId] -> Term -> Term -> M ()
    declareEquation vars lhs rhs =
      modifyFS (\ state -> state {
          context = CExtendEquation (context state) vars lhs rhs
        })
preprocessEquation _ = error "(Impossible)"

doCheck :: Declaration -> M ()
doCheck (DeclCheck pos expr) = do
  logFS ("--- Checking expression near " ++ show pos)
  logFS ("    " ++ show expr)
  (term, typ) <- inferExpr LEmpty expr
  solveAllPendingConstraints
  ctx <- context <$> getFS
  term' <- normalize ctx <$> unfold term
  typ' <- unfold typ
  logFS ("  = " ++ show term')
  logFS ("  : " ++ show typ')
  logFS ""
doCheck _ = error "(Impossible)"

splitPiFixedParams :: [Parameter] -> Term -> ([TPiParam], Term)
splitPiFixedParams []       t = ([], t)
splitPiFixedParams (_ : ps) (TPi tpiInfo lx typ body) =
  -- TODO: check that explicit/implicit coincides with type info
  let (tParams, tRoot) = splitPiFixedParams ps body in
      (TPiParam tpiInfo lx typ : tParams, tRoot)
splitPiFixedParams _ _ = error "Impossible"

splitPiIndices :: Term -> ([TPiParam], Term)
splitPiIndices t | isWeakHeadReducible t = splitPiIndices (weakHeadReduce t)
splitPiIndices (TPi tpiInfo lx typ body) =
  let (tIndices, tRoot) = splitPiIndices body in
      (TPiParam tpiInfo lx typ : tIndices, tRoot)
splitPiIndices t = ([], t)

preprocessConstructorDeclaration ::
      S.Set ConstId    -- name of all the data types that are being
                       -- simultaneously declared
   -> ConstId          -- name of the current data type
   -> [Parameter]      -- parameters of the current data type
   -> ConstructorDecl  -- constructor declaration
   -> M [ConstId]
preprocessConstructorDeclaration simTypeNames typeName
                                 params
                                 (ConstructorDecl pos constructorName eTyp) = do
    (tTyp, tConstructorSort) <- inferExpr LEmpty
                                  (piManyParams (map makeImplicit params) eTyp)
    let (cParams, cType) = splitPiFixedParams params tTyp
    -- Check sort
    ensureSort pos LEmpty tConstructorSort
      [RS "When checking that the declared type of constructor ",
       RS (show constructorName), RS " is a type."]
    tDataSort <- getDataTypeRoot typeName
    emitConstraint $
      C_LeSorts LEmpty tConstructorSort tDataSort
        (Reason pos
          [RS "Sort of data type constructor must fit in the sort of the type.\n",
           RS "  Constructor: ",  RS (show constructorName), RS "\n",
           RS "  Sort of constructor: ", RT tConstructorSort, RS "\n",
           RS "  Data type: ", RS (show typeName), RS "\n",
           RS "  Sort of datatype: ", RT tDataSort])
    -- Check validity of the constructor
    checkConstructorValid cType cParams cType
    --
    declareConst pos SConstructor (ConstId constructorName) [] tTyp
    return [ConstId constructorName]
  where
    checkConstructorValid :: Term -> [TPiParam] -> Term -> M ()
    checkConstructorValid fullTyp cParams cType | isWeakHeadReducible cType =
      checkConstructorValid fullTyp cParams (weakHeadReduce cType)
    checkConstructorValid fullTyp cParams (TPi info x argTyp cType) = do
      checkConstructorArgumentValid cParams argTyp
      z <- freshLocalId
      checkConstructorValid fullTyp cParams (substitute cType x (TLocal z))
    checkConstructorValid fullTyp cParams cType = do
      checkMatchCurrentTypeFamily
        ("Return type of constructor must be an instance of "
         ++ "the declared type family.\n"
         ++ "  Type of constructor: " ++ show fullTyp ++ "\n"
         ++ "  Type family        : " ++ show (typeFamily cParams))
        cParams cType
    
    checkConstructorArgumentValid :: [TPiParam] -> Term -> M ()
    checkConstructorArgumentValid cParams argTyp | isWeakHeadReducible argTyp =
      checkConstructorArgumentValid cParams (weakHeadReduce argTyp)
    checkConstructorArgumentValid cParams (TPi _ x argL argR) = do
      checkNoOccurrences
        (\ typ ->
            "Type constructor " ++ show typ
         ++ " must occur strictly positively in type of data constructor.\n"
         ++ "  Data constructor: " ++ show constructorName)
        simTypeNames argL
      z <- freshLocalId
      let argR' = substitute argR x (TLocal z)
      checkConstructorArgumentValid cParams argR'
    checkConstructorArgumentValid cParams argTyp = do
      let (head, _) = splitTermArgs argTyp
      case head of
        TConst headConstructor [] | headConstructor `S.member` simTypeNames ->
          checkMatchTypeFamily
            ("Recursive occurrence of type in constructor parameter must be an instance of "
             ++ "the declared type family.\n"
             ++ "  Type of parameter: " ++ show argTyp ++ "\n"
             ++ "  Type family      : " ++ show (typeFamily cParams))
            headConstructor argTyp
        _ ->
          checkNoOccurrences
            (\ typ ->
             "Type constructor " ++ show typ ++ " can only occur" ++
             " as the result type of its constructors and their arguments.")
            simTypeNames argTyp

    typeFamily :: [TPiParam] -> Term
    typeFamily cParams = tAppMany (TConst typeName [])
                                  [TLocal x | TPiParam _ x _ <- cParams]

    checkNoOccurrences :: (ConstId -> String) -> S.Set ConstId -> Term -> M ()
    checkNoOccurrences msg constNames term = do
      let i = S.intersection constNames (freeConsts term)
      if S.null i
       then return ()
       else failM pos (msg (head (S.toList i)))

    checkMatchCurrentTypeFamily :: String -> [TPiParam] -> Term -> M ()
    checkMatchCurrentTypeFamily msg cParams term = do
      numIndices <- fromIntegral . length <$> getDataTypeIndices typeName
      let typeFamily = tAppMany (TConst typeName [])
                                [TLocal x | TPiParam _ x _ <- cParams]
      term' <- trimNApps numIndices term
      -- TODO: check that the n arguments dont have occurrences of the type!
      if term' == typeFamily
       then return ()
       else failM pos msg

    -- The given type name must be one of the elements of simTypeNames,
    -- i.e. one of the type names that are currently being simultaneously
    -- declared. It may be the current typename, but not necessarily so.
    checkMatchTypeFamily :: String -> ConstId -> Term -> M ()
    checkMatchTypeFamily msg typeName2 term = do
      numParams  <- fromIntegral . length <$> getDataTypeFixedParams typeName
      numIndices <- fromIntegral . length <$> getDataTypeIndices typeName
      term' <- trimNApps (numParams + numIndices) term
      -- TODO: check that the n arguments dont have occurrences of the type!
      if term' == TConst typeName2 []
       then return ()
       else failM pos msg

    trimNApps :: Integer -> Term -> M Term
    trimNApps n term | n == 0 = return term
    trimNApps n term | isWeakHeadReducible term =
      trimNApps n (weakHeadReduce term)
    trimNApps n (TApp term _) = trimNApps (n - 1) term
    trimNApps _ term =
      failM pos (
        "Application of data type " ++ show typeName ++ " is missing arguments."
      )

-------------------------------------------------------------------------------
-- Constraint solver
-------------------------------------------------------------------------------

solveAllPendingConstraints :: M ()
solveAllPendingConstraints = solvePendingConstraints True

solvePendingTermConstraints :: M ()
solvePendingTermConstraints = solvePendingConstraints False

solvePendingConstraints :: Bool -> M ()
solvePendingConstraints shouldSolveUniverseConstraints = do
    -- Take the pending constraints as a working set.
    -- Beware: the process may create more constraints.
    constraints <- pendingConstraints <$> getFS
    modifyFS (\ state -> state { pendingConstraints = [] })
    -- Solve constraints by unification
    remainingConstraints <- iterativelySolveConstraints constraints
    -- Solve remaining universe constraints by backtracking
    remainingConstraints' <-
      if shouldSolveUniverseConstraints
       then solveUniverseConstraints remainingConstraints
       else return remainingConstraints
    modifyFS (\ state -> state {
                pendingConstraints = remainingConstraints' ++ pendingConstraints state
             })
  where
    iterativelySolveConstraints :: [Constraint] -> M [Constraint]
    iterativelySolveConstraints constraints = do
      simplifiedConstraints <- concat <$> mapM simplifyConstraint constraints
      remainingConstraints <- concat <$> mapM trySolvePatternConstraint simplifiedConstraints
      if length remainingConstraints == length simplifiedConstraints
       then return remainingConstraints
       else iterativelySolveConstraints remainingConstraints

    simplifyConstraint :: Constraint -> M [Constraint]
    simplifyConstraint (C_Unify env t1_0 t2_0 r) = do
      t1 <- unfold t1_0
      t2 <- unfold t2_0
      simplifyUnify env t1 t2 r
    simplifyConstraint (C_UnifyCodomain env t1_0 t2_0 r) = do
      t1 <- unfold t1_0
      t2 <- unfold t2_0
      simplifyUnifyCodomain env t1 t2 r
    simplifyConstraint (C_LeSorts env t1_0 t2_0 r) = do
      t1 <- unfold t1_0
      t2 <- unfold t2_0
      simplifyLeSorts env t1 t2 r
    simplifyConstraint (C_LeUniverses u1 u2 r) = simplifyLeUniverses u1 u2 r

    simplifyUnify :: LocalEnv -> Term -> Term -> Reason -> M [Constraint]
    simplifyUnify env t1 t2 r | t1 == t2 = return []
    simplifyUnify env t1 t2 r | isWeakHeadReducible t1 =
      simplifyUnify env (weakHeadReduce t1) t2 r
    simplifyUnify env t1 t2 r | isWeakHeadReducible t2 =
      simplifyUnify env t1 (weakHeadReduce t2) r
    simplifyUnify env (TType u1) (TType u2) r =
      simplifyUnifyUniverses u1 u2 r
    simplifyUnify env (TLam x1 typ1 body1) (TLam x2 typ2 body2) r = do
      cs1 <- simplifyUnify env typ1 typ2 r
      z <- freshLocalId
      cs2 <- simplifyUnify (LExtend env z typ1)
                           (substitute body1 x1 (TLocal z))
                           (substitute body2 x2 (TLocal z))
                           r
      return (cs1 ++ cs2)
    simplifyUnify env t1@(TPi info1 x1 typ11 typ12) t2@(TPi info2 x2 typ21 typ22) r
      | info1 == info2 = do
      cs1 <- simplifyUnify env typ11 typ21 r
      z <- freshLocalId
      cs2 <- simplifyUnify (LExtend env z typ11)
                           (substitute typ12 x1 (TLocal z))
                           (substitute typ22 x2 (TLocal z))
                           r
      return (cs1 ++ cs2)
    simplifyUnify env t1 t2 r = do
      let (fun1, args1) = splitTermArgs t1
      let (fun2, args2) = splitTermArgs t2
      rigid1 <- isRigid fun1
      rigid2 <- isRigid fun2
      case (fun1, fun2) of
        (TLocal l1, TLocal l2) | l1 == l2 && length args1 == length args2
          -> concat <$> mapM (\ (a, b) -> simplifyUnify env a b r) (zip args1 args2)
        (TConst c1 _, TConst c2 _) | c1 == c2 && rigid1 && length args1 == length args2
          ->  concat <$> mapM (\ (a, b) -> simplifyUnify env a b r)
                              (zip args1 args2)
        (_, _) | rigid1 && rigid2 && clash fun1 args1 fun2 args2
          -> do constraint' <- unfold (C_Unify env t1 t2 r)
                failM (reasonPosition r)
                      [RS "Unification fails.\n",
                       RS (show constraint'), RS "\n",
                       RS "Clashing terms:\n",
                       RS "  ", RT t1, RS "\n",
                       RS "  ", RT t2]
        _ -> do
          ctx <- context <$> getFS
          -- TODO: use "depth" of definition to prioritize unfolding one
          -- or the other
          -----
          --return [C_Unify env t1 t2 r]
          -----
          case weakHeadUnfoldM ctx t1 of
            Just t1' -> simplifyUnify env t1' t2 r
            Nothing ->
              case weakHeadUnfoldM ctx t2 of
                Just t2' -> simplifyUnify env t1 t2' r
                Nothing ->
                  case weakUnfoldM ctx t1 of
                    Just t1' -> simplifyUnify env t1' t2 r
                    Nothing ->
                      case weakUnfoldM ctx t2 of
                        Just t2' -> simplifyUnify env t1 t2' r
                        Nothing -> return [C_Unify env t1 t2 r]
          {-
          -}

    isRigid :: Term -> M Bool
    isRigid (TType _)      = return True
    isRigid (TLam _ _ _)   = return True
    isRigid (TPi _  _ _ _) = return True
    isRigid (TLocal _)     = return True
    isRigid (TConst c _) = do
      dict <- declaredConsts <$> getFS
      case M.lookup c dict of
        Just (_, SDataType)    -> return True
        Just (_, SConstructor) -> return True
        _ -> return False
    isRigid _ = return False

    clash :: Term -> [Term] -> Term -> [Term] -> Bool
    clash fun1 args1 fun2 args2 = clashFun fun1 fun2
                               || length args1 /= length args2
    clashFun :: Term -> Term -> Bool
    clashFun (TLocal l1)   (TLocal l2)   = l1 /= l2
    clashFun (TConst c1 _) (TConst c2 _) = c1 /= c2
    clashFun _             _             = True
    
    simplifyUnifyCodomain :: LocalEnv -> Term -> Term -> Reason -> M [Constraint]
    simplifyUnifyCodomain env t1 t2 r | isWeakHeadReducible t1 =
      simplifyUnifyCodomain env (weakHeadReduce t1) t2 r
    simplifyUnifyCodomain env (TPi info1 x1 typ11 typ12) t2 r = do
      z <- freshLocalId
      simplifyUnifyCodomain (LExtend env z typ11)
                        (substitute typ12 x1 (TLocal z)) t2 r
    -- TODO: consider the case in which t1 is reducible
    -- by one of the definitions (equations) in the program
    simplifyUnifyCodomain env t1 t2 r = simplifyUnify env t1 t2 r

    simplifyUnifyUniverses :: Universe -> Universe -> Reason -> M [Constraint]
    simplifyUnifyUniverses u1 u2 r = do
      cs1 <- simplifyLeUniverses u1 u2 r
      cs2 <- simplifyLeUniverses u2 u1 r
      return (cs1 ++ cs2)

    simplifyLeSorts :: LocalEnv -> Term -> Term -> Reason -> M [Constraint]
    simplifyLeSorts env t1 t2 r | isWeakHeadReducible t1 =
      simplifyLeSorts env (weakHeadReduce t1) t2 r
    simplifyLeSorts env t1 t2 r | isWeakHeadReducible t2 =
      simplifyLeSorts env t1 (weakHeadReduce t2) r
    simplifyLeSorts _ (TType u1) (TType u2) r =
      simplifyLeUniverses u1 u2 r
    simplifyLeSorts env t1 t2 r = return [C_LeSorts env t1 t2 r]

    simplifyLeUniverses :: Universe -> Universe -> Reason -> M [Constraint]
    simplifyLeUniverses u1 u2 reason = return [C_LeUniverses u1 u2 reason]

    -- Solve c and return [] if the constraint c is a pattern constraint.
    -- Return [c] if the constraint c is not a pattern constraint.
    trySolvePatternConstraint :: Constraint -> M [Constraint]
    trySolvePatternConstraint constraint@(C_Unify env t1_0 t2_0 r) = do
        t1 <- unfold t1_0
        t2 <- unfold t2_0
        try t1 t2 `orIfNonEmpty` try t2 t1
      where
        try t1 t2 = do
          case checkPatternConstraint t1 t2 of
            Just (metavar, vars) -> do
              let types = map (localEnvLookupTypeOrFail env) vars
              instantiateMetavar (reasonPosition r)
                                 metavar
                                 (foldr (uncurry TLam) t2 (zip vars types))
              return []
            Nothing -> return [C_Unify env t1 t2 r]
        orIfNonEmpty :: M [a] -> M [a] -> M [a]
        orIfNonEmpty m1 m2 = do
          state0 <- getFS
          l1 <- m1
          case l1 of
            []  -> return []
            res -> do state1 <- getFS
                      putFS state0
                      l2 <- m2
                      case l2 of
                        [] -> return []
                        _  -> do putFS state1
                                 return res
    trySolvePatternConstraint constraint =
      -- TODO: deal with other kinds of pattern constraints
      return [constraint]

    checkPatternConstraint :: Term -> Term -> Maybe (MetavarId, [LocalId])
    checkPatternConstraint t1 t2 = do
      let (fun1, args1) = splitTermArgs t1
      case fun1 of
        TMetavar m1 ->
          case arePairwiseDistinctLocalIds args1 of
            Nothing   -> Nothing
            Just vars ->
              if freeLocals t2 `S.isSubsetOf` S.fromList vars
                 && not (m1 `S.member` freeMetavars t2)
               then Just (m1, vars)
               else Nothing
        _ -> Nothing

    arePairwiseDistinctLocalIds :: [Term] -> Maybe [LocalId]
    arePairwiseDistinctLocalIds terms =
      if all isTLocal terms
       then let vars = map localIdFromTLocal terms in
              if length vars == S.size (S.fromList vars)
               then Just vars
               else Nothing
       else Nothing

    solveUniverseConstraints :: [Constraint] -> M [Constraint]
    solveUniverseConstraints constraints = do
        let uConstraints = filter isCLeUniverses constraints
        uConstraints' <- concat <$> mapM buildUConstraints uConstraints
        case USolver.universeConstraintsSatisfiable uConstraints' of
          Nothing -> failM Pos.unknown "Universe constraints unsatisfiable."
          Just solution -> do
            uf <- freshUMetavarId
            mapM_ (\ (um, level) ->
                      instantiateUMetavar Pos.unknown um
                                          (itUSucc level (UMetavar uf)))
                  (M.toList solution)
        let remainingConstraints = filter (not . isCLeUniverses) constraints
        return remainingConstraints
      where
        isCLeUniverses :: Constraint -> Bool
        isCLeUniverses (C_LeUniverses _ _ _) = True
        isCLeUniverses _ = False
        buildUConstraints :: Constraint -> M [USolver.Constraint]
        buildUConstraints (C_LeUniverses u1_0 u2_0 _) = do
          u1 <- simplifyUniverse <$> unfold u1_0
          u2 <- simplifyUniverse <$> unfold u2_0
          d1 <- univAsDict u1
          d2 <- univAsDict u2
          return $
            [USolver.Constraint
                (USolver.Atom um1 k1)
                [USolver.Atom um2 k2 | (um2, k2) <- M.toList d2]
            | (um1, k1) <- M.toList d1]
        buildUConstraints _ = error "(Impossible)"
        univAsDict :: Universe -> M (M.Map UMetavarId Integer)
        univAsDict (USucc u) = do
          opts <- univAsDict u
          return $ M.fromList [(um, k + 1) | (um, k) <- M.toList opts]
        univAsDict (UMetavar um) = return $ M.fromList [(um, 0)]
        univAsDict (UMax u1 u2)  = do
          opts1 <- univAsDict u1
          opts2 <- univAsDict u2
          return $ M.unionWith max opts1 opts2
        univAsDict (UVar _) = error "(Impossible)"

generalizeConstants :: [Declaration] -> [ConstId] -> M ()
generalizeConstants declarations declaredConstantnames = do
    metavars <- S.toList . S.unions <$> mapM collectUMetavars declarations
    vars     <- mapM (const freshUVarId) metavars
    state    <- getFS
    context' <- addParametersToConstants
                  vars
                  (M.fromList [(mv, UVar v) | (mv, v) <- zip metavars vars])
                  (S.fromList declaredConstantnames)
                  (context state)
    putFS (state { context = context' })
    return ()
  where
    collectUMetavars :: Declaration -> M (S.Set UMetavarId)
    collectUMetavars (DeclDataType _ typeName _ _) = do
      (_, typ) <- getConstantType (ConstId typeName)
      unfoldedType <- unfold typ
      return $ freeUMetavars unfoldedType
    collectUMetavars (DeclDataConstructors _ _ _ ctors) = do
      S.unions <$> mapM collectUMetavarsInConstructorDecl ctors
    collectUMetavars (DeclType _ funName _) = do
      (_, typ)  <- getConstantType (ConstId funName)
      unfoldedType <- unfold typ
      return $ freeUMetavars unfoldedType
    collectUMetavars (DeclEquation _ rhs lhs) = return S.empty
    collectUMetavars (DeclCheck _ _) = return S.empty
    ---
    collectUMetavarsInConstructorDecl :: ConstructorDecl -> M (S.Set UMetavarId)
    collectUMetavarsInConstructorDecl (ConstructorDecl _ constructorName _) = do
      (_, typ)  <- getConstantType (ConstId constructorName)
      unfoldedType <- unfold typ
      return $ freeUMetavars unfoldedType
    ---
    addParametersToConstants :: [UVarId] -> M.Map UMetavarId Universe -> S.Set ConstId
                             -> Context -> M Context
    addParametersToConstants uvars usubst constants CEmpty = return CEmpty
    addParametersToConstants uvars usubst constants
          (CExtendConst ctx constName universeParams typ)
      | constName `S.member` constants =
          if null universeParams
           then do
             ctx' <- addParametersToConstants uvars usubst constants ctx
             typ' <- unfold typ
             let typ'' = substUMetavars usubst (rec uvars constants typ')
             return $ CExtendConst ctx' constName uvars typ''
           else error "(Internal error generalizing metavariables (1))"
      | otherwise =
          return $ CExtendConst ctx constName universeParams typ -- (stop)
    addParametersToConstants uvars usubst constants
          (CExtendEquation ctx lvars lhs rhs) = do
      ctx' <- addParametersToConstants uvars usubst constants ctx
      lhs' <- unfold lhs
      let lhs'' = substUMetavars usubst (rec uvars constants lhs')
      rhs' <- unfold rhs
      let rhs'' = substUMetavars usubst (rec uvars constants rhs')
      return $ CExtendEquation ctx' lvars lhs'' rhs''
    -- Note:
    --   rec uvars constants t
    -- replaces occurrences of c.[] by c.uvars,
    -- for all constants c in the given set.
    rec :: [UVarId] -> S.Set ConstId -> Term -> Term
    rec us cs (TConst c us')
      | c `S.member` cs =
          if null us'
           then TConst c (map UVar us)
           else error "(Internal error generalizing metavariables (2))"
      | otherwise = TConst c us'
    rec us cs (TLocal l)         = TLocal l
    rec us cs (TMetavar m)       = TMetavar m
    rec us cs (TLam l typ body)  = TLam l (rec us cs typ) (rec us cs body)
    rec us cs (TApp t1 t2)       = TApp (rec us cs t1) (rec us cs t2)
    rec us cs (TType univ)       = TType univ
    rec us cs (TPi info l t1 t2) = TPi info l (rec us cs t1) (rec us cs t2)

ensureSort :: Pos.Position -> LocalEnv -> Term -> [ReasonPart] -> M ()
ensureSort pos env term rps = do
  uid <- freshUMetavarId
  emitConstraint $
    C_Unify env term (TType (UMetavar uid))
      (Reason pos
         ([RS "Expression ", RT term, RS " should be a sort.\n"] ++ rps))

ensureTailSort :: Pos.Position -> LocalEnv -> Term -> [ReasonPart] -> M ()
ensureTailSort pos env term rps = do
  uid <- freshUMetavarId
  emitConstraint $
    C_UnifyCodomain env term (TType (UMetavar uid))
      (Reason pos
        ([RT term, RS " should be a family of sorts.\n"] ++ rps))

-- Given an expression e, infer its type returning a pair (t, A)
-- where t is a representation of e in the core language,
-- and A is the type of e.
inferExpr :: LocalEnv -> Expr -> M (Term, Term)
inferExpr env0 expr = inferExprFillingFreshImplicitArg env0 expr
  where
    inferExprFillingFreshImplicitArg :: LocalEnv -> Expr -> M (Term, Term)
    inferExprFillingFreshImplicitArg env expr = do
      let pos = annotation expr
      let (eFun, eArgs) = splitImplicitArgs expr
      (tFun, tFunTyp) <- recInfer env eFun
      providedArgsWithTyps <- mapM (inferExpr env) eArgs
      solvePendingTermConstraints; tFunTyp' <- unfold tFunTyp
      fullArgs <- fullArgumentList env eFun tFun tFunTyp pos
                                   tFunTyp' providedArgsWithTyps
      typedApplyMany TPiImplicit env (annotation expr) (tFun, tFunTyp) fullArgs

    splitImplicitArgs :: Expr -> (Expr, [Expr])
    splitImplicitArgs (EAppImplicit _ fun arg) =
      let (fun', args) = splitImplicitArgs fun
       in (fun', args ++ [arg])
    splitImplicitArgs expr = (expr, [])

    fullArgumentList ::
         LocalEnv
      -> Expr             -- Original function expression
      -> Term             -- Original function (for error reporting)
      -> Term             -- Original function type (for error reporting)
      -> Pos.Position     -- Position (for error reporting)
      -> Term             -- pi type of the form ∀ {x1 : A1} ... {xn : An} B
      -> [(Term, Term)]   -- List of provided arguments with types
      -> M [(Term, Term)] -- Full argument list
    fullArgumentList env eFun tFun funTyp pos t args = do
        rec env funTyp pos t args
      where
        rec env funTyp pos t args | isWeakHeadReducible t =
          rec env funTyp pos t args
        rec env funTyp pos t args = do
          ctx <- context <$> getFS
          case weakHeadUnfoldM ctx t of
            Just t' -> rec env funTyp pos t' args
            Nothing -> rec' env funTyp pos t args
        rec' env funTyp pos
                         (TPi TPiImplicit x _ t) ((arg, argTyp) : args) = do
          fullArgs <- rec env funTyp pos (substitute t x arg) args
          return ((arg, argTyp) : fullArgs)
        rec' env funTyp pos (TPi TPiImplicit x argTyp t) []
             | not (isELamImplicit eFun) = do
          arg <- freshMetavarAt env
          fullArgs <- rec env funTyp pos (substitute t x arg) []
          return ((arg, argTyp) : fullArgs)
        rec' env funTyp pos t (_ : _) = do
          failM pos ([
              RS "Function has been provided more implicit arguments than expected.\n"
            , RS "  Function: ", RT tFun, RS "\n"
            , RS "  Type: ", RT funTyp, RS "\n"
            ])
        rec' _ _ _ _ [] = return []

    isELamImplicit :: Expr -> Bool
    isELamImplicit (ELamImplicit _ _ _ _) = True
    isELamImplicit _                      = False

    recInfer :: LocalEnv -> Expr -> M (Term, Term)
    recInfer env (EWildcard pos) = do
      mTerm <- freshMetavarAt env
      mTyp  <- freshMetavarAt env
      return (mTerm, mTyp)
    recInfer env (EHole pos hname) = do
      mTerm <- freshMetavarAt env
      mTyp  <- freshMetavarAt env
      registerHole pos hname env mTerm mTyp
      return (mTerm, mTyp)
    recInfer _   (EVar pos x)
      | x == globalNameType = do
        uid <- freshUMetavarId
        return (TType (UMetavar uid), TType (USucc (UMetavar uid)))
    recInfer env (EVar pos x) = inferVariable env pos x
    recInfer env (EApp pos eFun eArg) = do
      (tFun, tFunTyp) <- inferExpr env eFun
      (tArg, tArgTyp) <- inferExpr env eArg
      typedApply TPiExplicit env pos (tFun, tFunTyp) (tArg, tArgTyp)
    recInfer env expr@(EAppImplicit _ _ _) = error "(Impossible)"
    recInfer env (ELam pos x eTyp eBody) = do
      inferLambda env TPiExplicit pos x eTyp eBody
    recInfer env (ELamImplicit pos x eTyp eBody) = do
      inferLambda env TPiImplicit pos x eTyp eBody
    recInfer env (EPi pos param eTyp) = inferPiType env param eTyp
    recInfer env _ = error "TODO: más casos"

    inferVariable :: LocalEnv -> Pos.Position -> QName -> M (Term, Term)
    inferVariable env pos x = do
        case localEnvLookup env (LocalId x) of
          Just tTyp -> return (TLocal (LocalId x), tTyp)
          Nothing -> do
            state <- getFS
            case M.lookup (ConstId x) (declaredConsts state) of 
              Just _ -> instantiateConstantWithFreshUniverses x
              Nothing -> failM pos ("Unbound variable: " ++ show x)

    inferLambda :: LocalEnv -> TPiInfo -> Pos.Position -> QName
                -> Expr -> Expr -> M (Term, Term)
    inferLambda env tpiInfo pos x eTyp eBody = do
      let lx = LocalId x
      (tTyp, tTypTyp) <- inferExpr env eTyp
      ensureSort pos env tTypTyp
        [RS "When checking that the declared type of parameter ",
         RS (show lx), RS " is a type."]
      (tBody, tBodyTyp) <- inferExpr (LExtend env lx tTyp) eBody
      -- TODO: universe level???
      return (TLam lx tTyp tBody, TPi tpiInfo lx tTyp tBodyTyp)

typedApply :: TPiInfo -> LocalEnv -> Pos.Position
           -> (Term, Term) -> (Term, Term) -> M (Term, Term)
typedApply tPiInfo env pos (tFun, tFunTyp) (tArg, tArgTyp) = do
  lx <- freshLocalId
  mResultTyp <- freshMetavarAt env
  emitConstraint
    $ C_Unify env
        tFunTyp
        (TPi tPiInfo lx tArgTyp (TApp mResultTyp (TLocal lx)))
        (Reason pos
           [RS "Expected a function of a given type.\n",
            RS "  Function: ", RT tFun, RS "\n",
            RS "  Argument: ", RT tArg, RS "\n",
            RS "  Type of function: ", RT tFunTyp, RS "\n",
            RS "  Type of argument: ", RT tArgTyp])
  return (TApp tFun tArg, TApp mResultTyp tArg)

typedApplyMany :: TPiInfo -> LocalEnv -> Pos.Position -> (Term, Term)
               -> [(Term, Term)] -> M (Term, Term)
typedApplyMany info _   _   fun []           = return fun
typedApplyMany info env pos fun (arg : args) = do
  fun' <- typedApply info env pos fun arg
  typedApplyMany info env pos fun' args

instantiateConstantWithFreshUniverses :: QName -> M (Term, Term)
instantiateConstantWithFreshUniverses x = do
  (universeParams, tTyp) <- getConstantType (ConstId x)
  metavars <- mapM (const freshUMetavarId) universeParams
  let tTyp' = substUVars
                (M.fromList [(v, UMetavar mv)
                            | (v, mv) <- zip universeParams metavars])
                tTyp
  return (TConst (ConstId x) (map UMetavar metavars), tTyp')

inferPiType :: LocalEnv -> Parameter -> Expr -> M (Term, Term)
inferPiType env param eTyp2 = do
  let (tPiInfo, x, eTyp1) =
         case param of
           ExplicitParameter _ x eTyp1 -> (TPiExplicit, x, eTyp1)
           ImplicitParameter _ x eTyp1 -> (TPiImplicit, x, eTyp1)
  (tTyp1, tKind1) <- inferExpr env eTyp1
  let env' = LExtend env (LocalId x) tTyp1
  (tTyp2, tKind2) <- inferExpr env' eTyp2
  --
  let tPiType = TPi tPiInfo (LocalId x) tTyp1 tTyp2
  uid1 <- freshUMetavarId
  uid2 <- freshUMetavarId
  emitConstraint $
    C_Unify env tKind1 (TType (UMetavar uid1))
      (Reason (annotation eTyp1) [
          RS "Level restriction for domain of pi-type.\n"
        , RS "  Pi type: ", RT tPiType, RS "\n"
        , RS "  Domain: ", RT tTyp1, RS "\n"
        , RS "  Sort of domain: ", RT tKind1
        ])
  emitConstraint $
    C_Unify env' tKind2 (TType (UMetavar uid2))
      (Reason (annotation eTyp2) [
          RS "Level restriction for codomain of pi-type.\n"
        , RS "  Pi type: ", RT tPiType, RS "\n"
        , RS "  Codomain: ", RT tTyp2, RS "\n"
        , RS "  Sort of codomain: ", RT tKind2
        ])
  return (tPiType, TType (UMax (UMetavar uid1) (UMetavar uid2)))

----

inferPattern :: LocalEnv -> Pattern -> M (LocalEnv, Term, Term)
inferPattern origEnv origPat = do
    patVars <- map LocalId <$> freeLocalVariables origEnv origPat
    outerEnv <- extendEnvWithFreshTypes origEnv patVars
    (_, term, typ) <- inferPatternFillingFreshImplicitArg outerEnv origEnv origPat
    solvePendingInaccessiblePatterns outerEnv
    return (outerEnv, term, typ)
  where
    inferPatternFillingFreshImplicitArg :: LocalEnv -> LocalEnv -> Pattern
                                        -> M (LocalEnv, Term, Term)
    inferPatternFillingFreshImplicitArg outerEnv env pat = do
      let pos = annotation pat
      let (pFun, pArgs) = splitImplicitArgs pat
      (env1, tFun, tFunTyp) <- recInferPat outerEnv env pFun
      --
      solvePendingTermConstraints ; tFunTypU <- unfold tFunTyp
      (env2, fullArgs) <- inferAndBuildArgumentList outerEnv env1 tFun tFunTyp pos tFunTypU pArgs
      (tApp, tAppTyp) <- typedApplyMany TPiImplicit env2 (annotation pat)
                                        (tFun, tFunTyp) fullArgs
      return (env2, tApp, tAppTyp)

    splitImplicitArgs :: Pattern -> (Pattern, [Pattern])
    splitImplicitArgs (PAppImplicit _ fun arg) =
      let (fun', args) = splitImplicitArgs fun
       in (fun', args ++ [arg])
    splitImplicitArgs pat = (pat, [])

    inferAndBuildArgumentList ::
         LocalEnv
      -> LocalEnv
      -> Term             -- Original function (for error reporting)
      -> Term             -- Original function type (for error reporting)
      -> Pos.Position     -- Position (for error reporting)
      -> Term             -- pi type of the form ∀ {x1 : A1} ... {xn : An} B
      -> [Pattern]        -- List of provided arguments
      -> M (LocalEnv, [(Term, Term)]) -- Full argument list
    inferAndBuildArgumentList outerEnv env fun funTyp pos t args =
        rec env fun funTyp pos t args
      where
        rec :: LocalEnv -> Term -> Term -> Pos.Position -> Term -> [Pattern]
            -> M (LocalEnv, [(Term, Term)])
        rec env fun funTyp pos t args | isWeakHeadReducible t =
          rec env fun funTyp pos t args
        rec env fun funTyp pos t args = do
          ctx <- context <$> getFS
          case weakHeadUnfoldM ctx t of
            Just t' -> rec env fun funTyp pos t' args
            Nothing -> rec' env fun funTyp pos t args
        rec' :: LocalEnv -> Term -> Term -> Pos.Position -> Term -> [Pattern]
             -> M (LocalEnv, [(Term, Term)])
        rec' env fun funTyp pos
                         (TPi TPiImplicit x _ t) (pat : args) = do
          (env1, arg, argTyp) <- inferPatternFillingFreshImplicitArg outerEnv env pat
          (env2, fullArgs) <- rec env1 fun funTyp pos (substitute t x arg) args
          return (env2, (arg, argTyp) : fullArgs)
        rec' env fun funTyp pos
                         (TPi TPiImplicit x argTyp t) [] = do
          arg <- freshMetavarAt env
          (env1, fullArgs) <- rec env fun funTyp pos (substitute t x arg) []
          return (env1, (arg, argTyp) : fullArgs)
        rec' env fun funTyp pos t (_ : _) = do
          failM pos ([
              RS "Function has been provided more implicit arguments than expected.\n"
            , RS "  Function: ", RT fun, RS "\n"
            , RS "  Type: ", RT funTyp, RS "\n"
            ])
        rec' env _ _ _ _ [] = return (env, [])

    recInferPat :: LocalEnv -> LocalEnv -> Pattern -> M (LocalEnv, Term, Term)
    recInferPat outerEnv env (PWildcard pos) = do
      x <- freshMetavarAt env
      xTyp <- freshMetavarAt env
      return (env, x, xTyp)
    recInferPat outerEnv env (PHole pos hname) = do
      x <- freshMetavarAt env
      xTyp  <- freshMetavarAt env
      registerHole pos hname env x xTyp
      return (env, x, xTyp)
    recInferPat outerEnv _   (PVar pos x)
      | x == globalNameType =
        failM pos ("\"" ++ show globalNameType ++ "\" cannot be part of a pattern.")
    recInferPat outerEnv env (PVar pos x) = inferVariablePattern outerEnv env pos x
    recInferPat outerEnv env (PApp pos pFun pArg) = do
      (env1, tFun, tFunTyp) <- inferPatternFillingFreshImplicitArg outerEnv env pFun
      (env2, tArg, tArgTyp) <- inferPatternFillingFreshImplicitArg outerEnv env1 pArg
      (tApp, tAppTyp) <- typedApply TPiExplicit env2 pos (tFun, tFunTyp)
                                                         (tArg, tArgTyp)
      return (env2, tApp, tAppTyp)
    recInferPat outerEnv env (PInaccessible _ pat) = do
      x <- freshMetavarAt env
      xTyp <- freshMetavarAt env
      modifyFS (\ state -> state {
                  pendingInaccessiblePatterns =
                    (x, pat) : pendingInaccessiblePatterns state })
      return (env, x, xTyp)
    recInferPat outerEnv env pat@(PAppImplicit _ _ _) = error "(Impossible)"

    inferVariablePattern :: LocalEnv -> LocalEnv -> Pos.Position -> QName
                         -> M (LocalEnv, Term, Term)
    inferVariablePattern outerEnv env pos x = do
      state <- getFS
      case M.lookup (ConstId x) (declaredConsts state) of 
        Just _ -> do (term, typ) <- instantiateConstantWithFreshUniverses x
                     return (env, term, typ)
        Nothing -> do
          let lx = LocalId x
          case localEnvLookup outerEnv lx of
            Just xTyp -> do
              return (LExtend env lx xTyp, TLocal lx, xTyp)
            Nothing -> do
              failM pos ("Unbound variable " ++ show lx)

    solvePendingInaccessiblePatterns :: LocalEnv -> M ()
    solvePendingInaccessiblePatterns outerEnv = do
        ps <- pendingInaccessiblePatterns <$> getFS
        modifyFS (\ state -> state { pendingInaccessiblePatterns = [] } )
        rec ps
      where
        rec :: [(Term, Pattern)] -> M ()
        rec [] = return ()
        rec ((expectedTm, pat) : ps) = do
          (iPatTm, _) <- inferExpr outerEnv (patternToExpr pat)
          emitConstraint $ C_Unify outerEnv iPatTm expectedTm
            (Reason (annotation pat)
               ([RS "Inaccessible pattern must unify with actual term.\n",
                 RS "  Inaccessible pattern: ", RT iPatTm, RS "\n",
                 RS "  Actual term: ", RT expectedTm]))
          rec ps

    freeLocalVariables :: LocalEnv -> Pattern -> M [QName]
    freeLocalVariables _   (PWildcard _)  = return []
    freeLocalVariables _   (PHole _ _)    = return []
    freeLocalVariables env (PVar _ x) = do
      state <- getFS
      case M.lookup (ConstId x) (declaredConsts state) of 
        Just _  -> return []
        Nothing -> do
          let lx = LocalId x
          case localEnvLookup env lx of
            Just _    -> return []
            Nothing   -> return [x]
    freeLocalVariables env (PApp pos p1 p2) = do
      vs1 <- freeLocalVariables env p1
      vs2 <- freeLocalVariables env p2
      let vs = nub (vs1 ++ vs2) 
      if length vs == length (vs1 ++ vs2)
       then return vs
       else failM pos (
                 "Pattern binds variables non-linearly.\n"
              ++ "Repeated variables: " ++ joinS " " (map show (intersect vs1 vs2))
            )
    freeLocalVariables env (PAppImplicit pos p1 p2) = do
      freeLocalVariables env (PApp pos p1 p2)
    freeLocalVariables _   (PInaccessible _ _) =
      return []

-------------------------------------------------------------------------------
-- Monadic operations (helpers)
-------------------------------------------------------------------------------

declareConst :: Pos.Position -> ConstantSort -> ConstId -> [UVarId] -> Term -> M ()
declareConst pos sort constName universeParams typ = do
  checkConstantNotDeclared pos constName
  modifyFS (\ state ->
      state {
        context        = CExtendConst (context state) constName
                                      universeParams
                                      typ
      , declaredConsts = M.insert constName (pos, sort) (declaredConsts state)
      })

getConstantType :: ConstId -> M ([UVarId], Term)
getConstantType constName = do
    ctx <- context <$> getFS
    rec ctx
  where
    rec CEmpty = error "(Constant not defined)"
    rec (CExtendConst ctx constName' universeParams typ)
      | constName == constName' = return (universeParams, typ)
      | otherwise               = rec ctx
    rec (CExtendEquation ctx _ _ _) = rec ctx

checkConstantNotDeclared :: Pos.Position -> ConstId -> M ()
checkConstantNotDeclared pos constName = do
  state <- getFS
  case M.lookup constName (declaredConsts state) of
    Just (pos', _) ->
      failM pos (
           "Constant " ++ show constName
        ++ " already declared (" ++ show pos' ++ ")."
      )
    Nothing -> return ()

checkConstantDeclaredAsDataType :: Pos.Position -> ConstId -> M ()
checkConstantDeclaredAsDataType pos constName = do
  state <- getFS
  case M.lookup constName (declaredConsts state) of
    Just (_, SDataType) -> return ()
    Just (pos', sort) ->
      failM pos (
           show constName
        ++ " should have been declared as a data type,"
        ++ " but it has been declared as a " ++ show sort
        ++ " (" ++ show pos' ++ ")")
    _ -> failM pos (show constName ++ " has not been declared.")

freshMetavarAt :: LocalEnv -> M Term
freshMetavarAt env = freshMetavarDependingOn (map TLocal (localEnvNames env))
  where
    freshMetavarDependingOn :: [Term] -> M Term
    freshMetavarDependingOn localIds = do
        m <- freshMetavarId
        return $ foldl TApp (TMetavar m) localIds
    freshMetavarId :: M MetavarId
    freshMetavarId = do
      id <- nextFreshMetavar <$> getFS
      modifyFS (\ state -> state { nextFreshMetavar = id + 1 })
      return $ MetavarId id

extendEnvWithFreshTypes :: LocalEnv -> [LocalId] -> M LocalEnv
extendEnvWithFreshTypes env [] = return env
extendEnvWithFreshTypes env (x : xs) = do
  xTyp <- freshMetavarAt env
  extendEnvWithFreshTypes (LExtend env x xTyp) xs

freshUMetavarId :: M UMetavarId
freshUMetavarId = do
  id <- nextFreshUMetavar <$> getFS
  modifyFS (\ state -> state { nextFreshUMetavar = id + 1 })
  return $ UMetavarId id

freshUVarId :: M UVarId
freshUVarId = do
  id <- nextFreshUVar <$> getFS
  modifyFS (\ state -> state { nextFreshUVar = id + 1 })
  return $ UVarId (show id)

freshLocalId :: M LocalId
freshLocalId = do
  id <- nextFreshLocal <$> getFS
  modifyFS (\ state -> state { nextFreshLocal = id + 1 })
  return $ LocalId (makeInternalQName "e" id)

emitConstraint :: Constraint -> M ()
emitConstraint constraint = do
  modifyFS (\ state -> state {
      pendingConstraints = constraint : pendingConstraints state
    })

instantiateMetavar :: Pos.Position -> MetavarId -> Term -> M ()
instantiateMetavar pos m t = do
  dict <- instMetavars <$> getFS
  case M.lookup m dict of
    Just _  -> failM pos ("Metavariable " ++ show m ++ " already instantiated.")
    Nothing -> modifyFS (\ state -> state {
                          instMetavars = M.insert m t dict
                        }) 

instantiateUMetavar :: Pos.Position -> UMetavarId -> Universe -> M ()
instantiateUMetavar pos um univ = do
  dict <- instUMetavars <$> getFS
  case M.lookup um dict of
    Just _  -> failM pos ("Universe metavariable " ++ show um ++ " already instantiated.")
    Nothing -> do
      modifyFS (\ state -> state {
        instUMetavars = M.insert um univ dict
      }) 

registerDataType :: ConstId -> Term -> [TPiParam] -> [TPiParam] -> Term -> M ()
registerDataType typeName tTypTyp tTypParams tTypIndices tTypRoot = do
  modifyFS (\ state -> state {
      registeredDataTypes =
        M.insert typeName
                 (DataTypeInfo tTypTyp tTypParams tTypIndices tTypRoot)
                 (registeredDataTypes state)
    })

registerHole :: Pos.Position -> HoleName -> LocalEnv -> Term -> Term -> M ()
registerHole pos hname env mTerm mTyp = do
  modifyFS (\ state -> state {
     registeredHoles = registeredHoles state
                    ++ [HoleInfo pos hname env mTerm mTyp]
   })

getDataTypeSort :: ConstId -> M Term
getDataTypeSort typeName = do
  state <- getFS
  case M.lookup typeName (registeredDataTypes state) of
    Nothing   -> error "(Data type has not been registered)"
    Just info -> return (dataTypeSort info)

getDataTypeFixedParams :: ConstId -> M [TPiParam]
getDataTypeFixedParams typeName = do
  state <- getFS
  case M.lookup typeName (registeredDataTypes state) of
    Nothing   -> error "(Data type has not been registered)"
    Just info -> return (dataTypeFixedParams info)

getDataTypeIndices :: ConstId -> M [TPiParam]
getDataTypeIndices typeName = do
  state <- getFS
  case M.lookup typeName (registeredDataTypes state) of
    Nothing   -> error "(Data type has not been registered)"
    Just info -> return (dataTypeIndices info)

getDataTypeRoot :: ConstId -> M Term
getDataTypeRoot typeName = do
  state <- getFS
  case M.lookup typeName (registeredDataTypes state) of
    Nothing   -> error "(Data type has not been registered)"
    Just info -> return (dataTypeRoot info)

failM :: MShowable msg => Pos.Position -> msg -> M a
failM pos msg = do
  msg' <- mshow msg
  failFS (
      "Elaboration error. Near: " ++ show pos ++ "\n"
   ++ msg'
   )

warnM :: MShowable msg => Pos.Position -> msg -> M ()
warnM pos msg = do
  msg' <- mshow msg
  logFS (
      "Warning. Near: " ++ show pos ++ "\n"
   ++ msg'
   )

----

class MShowable a where
  mshow :: a -> M String

instance MShowable Char where
  mshow c = return [c]

instance MShowable Term where
  mshow t = do t' <- unfold t
               return $ show t'

instance MShowable ReasonPart where
  mshow (RS str)  = mshow str
  mshow (RT term) = mshow term

instance MShowable a => MShowable [a] where
  mshow xs = joinS "" <$> mapM mshow xs

-------------------------------------------------------------------------------
-- Operations on terms
-------------------------------------------------------------------------------

class Unfold a where
  unfold :: a -> M a

instance Unfold Term where
  unfold t = whnf <$> rec t
    where
      rec (TConst c univs) = TConst c <$> mapM unfold univs
      rec (TLocal l)   = return $ TLocal l
      rec (TMetavar m) = do
        dict <- instMetavars <$> getFS
        case M.lookup m dict of
          Nothing   -> return $ TMetavar m
          Just term -> unfold term
      rec (TLam l typ body)  = TLam l <$> unfold typ <*> unfold body
      rec (TApp t1 t2)       = TApp <$> unfold t1 <*> unfold t2
      rec (TType univ)       = TType <$> unfold univ
      rec (TPi info l t1 t2) = TPi info l <$> unfold t1 <*> unfold t2

instance Unfold Universe where
  unfold (USucc u)     = USucc <$> unfold u
  unfold (UMax u1 u2)  = UMax <$> unfold u1 <*> unfold u2
  unfold (UVar x)      = return $ UVar x
  unfold (UMetavar um) = do
    dict <- instUMetavars <$> getFS
    case M.lookup um dict of
      Nothing   -> return $ UMetavar um
      Just univ -> do
        uRes <- simplifyUniverse <$> unfold univ
        modifyFS (\ state -> state {
                   instUMetavars = M.insert um uRes (instUMetavars state)
                 }) -- memoize
        return uRes

instance Unfold LocalEnv where
  unfold LEmpty = return LEmpty
  unfold (LExtend env id term) = do
    env'  <- unfold env
    term' <- unfold term
    return $ LExtend env' id term'

instance Unfold Reason where
  unfold (Reason pos parts) = Reason pos <$> mapM unfold parts

instance Unfold ReasonPart where
  unfold (RS str)  = return $ RS str
  unfold (RT term) = RT <$> unfold term

instance Unfold Constraint where
  unfold (C_Unify env t1 t2 r) =
    C_Unify <$> unfold env <*> unfold t1 <*> unfold t2 <*> unfold r
  unfold (C_UnifyCodomain env t1 t2 r) =
    C_UnifyCodomain <$> unfold env <*> unfold t1 <*> unfold t2 <*> unfold r
  unfold (C_LeSorts env t1 t2 r) =
    C_LeSorts <$> unfold env <*> unfold t1 <*> unfold t2 <*> unfold r
  unfold (C_LeUniverses u1 u2 r) =
    C_LeUniverses <$> unfold u1 <*> unfold u2 <*> unfold r

instance Unfold Context where
  unfold CEmpty = return CEmpty
  unfold (CExtendConst ctx c us t) =
    CExtendConst <$> unfold ctx <*> return c <*> return us <*> unfold t
  unfold (CExtendEquation ctx lvars lhs rhs) =
    CExtendEquation <$> unfold ctx <*> return lvars <*> unfold lhs <*> unfold rhs

----

class FreeUMetavars a where
  freeUMetavars :: a -> S.Set UMetavarId

instance FreeUMetavars Term where
  freeUMetavars (TConst _ univs)  = S.unions (map freeUMetavars univs)
  freeUMetavars (TLocal _)        = S.empty
  freeUMetavars (TMetavar _)      = S.empty
  freeUMetavars (TLam _ typ body) = freeUMetavars typ `S.union` freeUMetavars body
  freeUMetavars (TApp t1 t2)      = freeUMetavars t1 `S.union` freeUMetavars t2
  freeUMetavars (TType univ)      = freeUMetavars univ
  freeUMetavars (TPi _ _ t1 t2)   = freeUMetavars t1 `S.union` freeUMetavars t2

instance FreeUMetavars Universe where
  freeUMetavars (USucc u)     = freeUMetavars u
  freeUMetavars (UMax u1 u2)  = freeUMetavars u1 `S.union` freeUMetavars u2
  freeUMetavars (UVar _)      = S.empty
  freeUMetavars (UMetavar um) = S.singleton um

----

class SubstUMetavars a where
  substUMetavars :: M.Map UMetavarId Universe -> a -> a

instance SubstUMetavars Term where
  substUMetavars usubst (TConst c univs)   = TConst c (map (substUMetavars usubst)
                                                           univs)
  substUMetavars _      (TLocal l)         = TLocal l
  substUMetavars _      (TMetavar m)       = TMetavar m
  substUMetavars usubst (TLam x typ body)  = TLam x (substUMetavars usubst typ)
                                                    (substUMetavars usubst body)
  substUMetavars usubst (TApp t1 t2)       = TApp (substUMetavars usubst t1)
                                                  (substUMetavars usubst t2)
  substUMetavars usubst (TType univ)       = TType (substUMetavars usubst univ)
  substUMetavars usubst (TPi info x t1 t2) = TPi info x
                                               (substUMetavars usubst t1)
                                               (substUMetavars usubst t2)

instance SubstUMetavars Universe where
  substUMetavars usubst (USucc u)     = USucc (substUMetavars usubst u)
  substUMetavars usubst (UMax u1 u2)  = UMax (substUMetavars usubst u1)
                                             (substUMetavars usubst u2)
  substUMetavars _      (UVar x)      = UVar x
  substUMetavars usubst (UMetavar um) = M.findWithDefault (UMetavar um) um usubst

----

class SubstUVars a where
  substUVars :: M.Map UVarId Universe -> a -> a

instance SubstUVars Term where
  substUVars usubst (TConst c univs)   = TConst c (map (substUVars usubst)
                                                       univs)
  substUVars _      (TLocal l)         = TLocal l
  substUVars _      (TMetavar m)       = TMetavar m
  substUVars usubst (TLam x typ body)  = TLam x (substUVars usubst typ)
                                                (substUVars usubst body)
  substUVars usubst (TApp t1 t2)       = TApp (substUVars usubst t1)
                                              (substUVars usubst t2)
  substUVars usubst (TType univ)       = TType (substUVars usubst univ)
  substUVars usubst (TPi info x t1 t2) = TPi info x (substUVars usubst t1)
                                                    (substUVars usubst t2)

instance SubstUVars Universe where
  substUVars usubst (USucc u)     = USucc (substUVars usubst u)
  substUVars usubst (UMax u1 u2)  = UMax (substUVars usubst u1)
                                         (substUVars usubst u2)
  substUVars usubst (UVar x)      = M.findWithDefault (UVar x) x usubst
  substUVars _      (UMetavar um) = UMetavar um

