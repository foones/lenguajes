module Checker(check) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(nub, partition)

import FailState(FailState, failFS, getFS, putFS, logFS, execFS)
import Utils(indent, joinS)

import Expr(
         Id(..),
         HypId(..), TyconId(..), TyvarId(..), PredId(..), FunId(..), VarId(..),
         Declaration(..), Metavar(..), Type(..),
         Term(..), Form(..), fOr,
         Proof(..), OptionalName(..), HoleName(..),
         OptionalForm(..),
         ThusType(..),
         Command(..), Justification(..),
         CaseBranch(..), IndBranch(..), IndPattern(..)
       )
import qualified Expr

----

previousFormulaName :: HypId
previousFormulaName = HypId (Id "_")

unknownFormulaName :: HypId
unknownFormulaName = HypId (Id "")

data CheckError = CheckError String

instance Show CheckError where
  show (CheckError msg) = msg

data GlobalContext = GlobalContext {
                       gctxAssumptions      :: M.Map HypId Form
                     , gctxTypeSubstitution :: M.Map Metavar Type
                     }

data LocalContext  = LocalContext {
                       lctxLocalAssumptions          :: [(HypId, Form)]
                     , lctxDisabledLocalAssumptions  :: S.Set HypId
                     , lctxDisabledGlobalAssumptions :: S.Set HypId
                     }

localCtxEmpty :: LocalContext
localCtxEmpty = LocalContext [] S.empty S.empty

localCtxBindOName :: OptionalName -> Form -> LocalContext -> LocalContext
localCtxBindOName ONNone        a lctx =
  lctx { lctxLocalAssumptions = (unknownFormulaName, a) : lctxLocalAssumptions lctx }
localCtxBindOName (ONName name) a lctx =
  lctx { lctxLocalAssumptions = (name, a) : lctxLocalAssumptions lctx }

localCtxAvailableFormulae :: LocalContext -> [Form]
localCtxAvailableFormulae lctx = map snd (lctxLocalAssumptions lctx)

localCtxLookup :: LocalContext -> HypId -> Maybe Form
localCtxLookup lctx name =
  if name == previousFormulaName && not (null (lctxLocalAssumptions lctx))
   then Just (snd (head (lctxLocalAssumptions lctx)))
   else lookup name (lctxLocalAssumptions lctx)

instance Show LocalContext where
  show lctx =
    unlines (
      if null (lctxLocalAssumptions lctx)
        then []
        else ["Local assumptions:"]
          ++ map (\ (name, form) -> "  " ++ show name ++ " : " ++ show form)
                 (lctxLocalAssumptions lctx)
    )

showFormulae :: String -> [Form] -> String
showFormulae title forms =
  unlines (
    if null forms
      then []
      else [title] ++ map (\ form -> "  " ++ show form) forms)

type StateHoles = [(HoleName, GlobalContext, LocalContext, Form)]

data CheckState =
  CheckState {
    stateCurrentSection :: HypId
  , stateGlobalContext  :: GlobalContext
  , stateHoles          :: StateHoles
  }

instance Show CheckState where
  show state =
    let globalAssumptions = gctxAssumptions (stateGlobalContext state) in
      unlines (
         [""]
      ++ (if M.null globalAssumptions
           then []
           else ["Global context:"]
             ++ map (\ (name, form) -> "  " ++ show name ++ " : " ++ show form)
                    (M.toList globalAssumptions))
      )

type M = FailState CheckError CheckState

failM :: String -> M a
failM msg = do
  state <- getFS
  failFS (CheckError (unlines (
            if stateCurrentSection state == unknownFormulaName
             then []
             else ["Near: " ++ show (stateCurrentSection state)]
         ++ [msg])))

setCurrentSection :: HypId -> M ()
setCurrentSection name = do
  state <- getFS
  putFS (state { stateCurrentSection = name })

stateLookup :: HypId -> CheckState -> Maybe Form 
stateLookup name state =
  M.lookup name (gctxAssumptions (stateGlobalContext state))

stateAvailableFormulae :: CheckState -> [Form]
stateAvailableFormulae state =
  map snd (M.toList (gctxAssumptions (stateGlobalContext state)))

globalCtxLookup :: HypId -> M (Maybe Form)
globalCtxLookup name = do
  globalAssumptions <- gctxAssumptions . stateGlobalContext <$> getFS
  return $ M.lookup name globalAssumptions

bindIfHoleToFormula :: OptionalForm -> LocalContext -> Form -> M ()
bindIfHoleToFormula (OFHole holeName) lctx form = do
  state <- getFS
  let gctx = stateGlobalContext state
  putFS (state {
    stateHoles = stateHoles state ++ [(holeName, gctx, lctx, form)]
  })
bindIfHoleToFormula _ _ _ = return ()

----

check :: [Declaration] -> Either CheckError StateHoles
check decls =
    case execFS (checkDeclarations decls) initialState of
      Left msg    -> Left msg
      Right state -> Right (stateHoles state)
  where
    initialState =
      CheckState {
        stateCurrentSection = unknownFormulaName
      , stateGlobalContext  =
          GlobalContext {
            gctxAssumptions      = M.empty
          , gctxTypeSubstitution = M.empty
          }
      , stateHoles          = []
      }

----

data Goal = Goal LocalContext Form

instance Show Goal where
  show (Goal lctx a) = show a ++ "\n" ++ show lctx

declareGlobalName :: HypId -> Form -> M ()
declareGlobalName name form = do
  state <- getFS
  let globalAssumptions = gctxAssumptions (stateGlobalContext state)
  if M.member name globalAssumptions 
   then failM ("'" ++ show name ++ "' already globally declared.")
   else return ()
  putFS (state {
           stateGlobalContext =
             (stateGlobalContext state) {
               gctxAssumptions = M.insert name form globalAssumptions
             }
         })

checkDeclarations :: [Declaration] -> M ()
checkDeclarations decls = mapM_ checkDeclaration decls

checkDeclaration :: Declaration -> M ()
checkDeclaration (DeclData _ _) =
  -- TODO
  return ()
checkDeclaration (DeclProp _ _) =
  -- TODO
  return ()
checkDeclaration (DeclAxiom name form) = do
  setCurrentSection name
  declareGlobalName name form
checkDeclaration (DeclTheorem name form proof) = do
  setCurrentSection name
  checkProof proof [Goal localCtxEmpty form]
  declareGlobalName name form

showGoals :: [Goal] -> String
showGoals []    = ""
showGoals goals = unlines (["[\n"] ++ map (indent 2 . show) goals ++ ["]"])

liftBinaryFunctionToMaybe :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
liftBinaryFunctionToMaybe f Nothing  Nothing  = Nothing
liftBinaryFunctionToMaybe f (Just a) Nothing  = Just a
liftBinaryFunctionToMaybe f Nothing  (Just a) = Just a
liftBinaryFunctionToMaybe f (Just a) (Just b) = Just (f a b)

-- Given a "pattern" optional formula oform and a "target" formula tform
-- check whether the pattern occurs as a conjunct in the target.
-- If it does not, return Nothing.
-- If it does, remove all occurrences of the pattern in the target,
-- and return a pair (f, rform)
-- where f is the removed formula (i.e. the pattern)
-- and rforms is the remaining formula, which is Nothing
-- in case f is the whole of the target formula and nothing remains.
takePatternFromConjunction :: OptionalForm -> Form -> Maybe (Form, Maybe Form)
takePatternFromConjunction OFNone         tform0 = Just (tform0, Nothing)
takePatternFromConjunction (OFHole _)     tform0 = Just (tform0, Nothing)
takePatternFromConjunction (OFForm pform) tform0 = rec tform0
  where
    rAnd :: Maybe Form -> Maybe Form -> Maybe Form
    rAnd = liftBinaryFunctionToMaybe FAnd
    rec :: Form -> Maybe (Form, Maybe Form)
    rec tform
      | Expr.equalFormulae pform tform = Just (tform, Nothing)
    rec (FAnd tform1 tform2) =
      case rec tform1 of
        Nothing ->
          case rec tform2 of
            Nothing          -> Nothing
            Just (f, rform2) -> Just (f, rAnd (Just tform1) rform2)
        Just (f, rform1) ->
          case rec tform2 of
            Nothing          -> Just (f, rAnd rform1 (Just tform2))
            Just (_, rform2) -> Just (f, rAnd rform1 rform2)
    rec _ = Nothing

takePatternFromDisjunction :: OptionalForm -> Form -> Maybe (Form, Maybe Form)
takePatternFromDisjunction OFNone         tform0 = Just (tform0, Nothing)
takePatternFromDisjunction (OFHole _)     tform0 = Just (tform0, Nothing)
takePatternFromDisjunction (OFForm pform) tform0 = rec tform0
  where
    rOr :: Maybe Form -> Maybe Form -> Maybe Form
    rOr = liftBinaryFunctionToMaybe FOr
    rec :: Form -> Maybe (Form, Maybe Form)
    rec tform
      | Expr.equalFormulae pform tform = Just (tform, Nothing)
    rec (FOr tform1 tform2) =
      case rec tform1 of
        Nothing ->
          case rec tform2 of
            Nothing          -> Nothing
            Just (f, rform2) -> Just (f, rOr (Just tform1) rform2)
        Just (f, rform1) ->
          case rec tform2 of
            Nothing          -> Just (f, rOr rform1 (Just tform2))
            Just (_, rform2) -> Just (f, rOr rform1 rform2)
    rec _ = Nothing

type Nondeterministic a = [a]

-- Apply a limited form of the resolution method.
isLogicalConsequence :: Form -> [Form] -> Either String ()
isLogicalConsequence form hypotheses =
    -- Negate the formula and try to refute all the resulting clauses.
    allClausesRefutable (formsToClauses (FNot form : hypotheses))
  where
    formsToClauses :: [Form] -> [[Form]]
    formsToClauses forms = dnfList (map nnf forms)
    ---- Disjunctive normal form.
    -- All formulae that are not conjunctions or disjunctions
    -- are considered to be atomic.
    dnf :: Form -> [[Form]]
    dnf (FAnd p q) = dnfAnd (dnf p) (dnf q)
    dnf (FOr p q)  = dnfOr (dnf p) (dnf q)
    dnf p          = [[p]]
    dnfTrue :: [[Form]]
    dnfTrue = [[]]
    dnfOr :: [[Form]] -> [[Form]] -> [[Form]]
    dnfOr p q = p ++ q
    dnfAnd :: [[Form]] -> [[Form]] -> [[Form]]
    dnfAnd p q = [x ++ y | x <- p, y <- q]
    dnfList :: [Form] -> [[Form]]
    dnfList = foldr (dnfAnd . dnf) dnfTrue
    ---- Negated normal form
    nnf :: Form -> Form
    nnf f@(FPred _ _)     = f
    nnf FTrue             = FTrue 
    nnf FFalse            = FFalse
    nnf (FImp p q)        = nnf (FOr (FNot p) q)
    nnf (FOr p q)         = FOr (nnf p) (nnf q)
    nnf (FAnd p q)        = FAnd (nnf p) (nnf q)
    nnf (FForall x typ p) = FForall x typ (nnf p)
    nnf (FExists x typ p) = FExists x typ (nnf p)
    nnf (FNot p)          = nnfNeg p
    --
    nnfNeg :: Form -> Form
    nnfNeg f@(FPred _ _)     = FNot f
    nnfNeg FTrue             = FFalse 
    nnfNeg FFalse            = FTrue
    nnfNeg (FImp p q)        = nnfNeg (FOr (FNot p) q)
    nnfNeg (FOr p q)         = FAnd (nnfNeg p) (nnfNeg q)
    nnfNeg (FAnd p q)        = FOr (nnfNeg p) (nnfNeg q)
    nnfNeg (FForall x typ p) = FExists x typ (nnfNeg p)
    nnfNeg (FExists x typ p) = FForall x typ (nnfNeg p)
    nnfNeg (FNot p)          = nnf p
    ----
    allClausesRefutable :: [[Form]] -> Either String ()
    allClausesRefutable [] = return ()
    allClausesRefutable (forms : clauses) =
      case refuteClause forms of
        [] ->
          case forallInstantiations forms of
            [] -> Left (show forms) -- Clause is irrefutable: fail
            ndForms' ->
              -- Forall has been instantiated; continue refuting clauses
              atLeastOne allClausesRefutable
                         [formsToClauses forms' ++ clauses | forms' <- ndForms']
        ndSub ->
          -- Clause is refutable with sub as the mgu.
          atLeastOne allClausesRefutable
                     [Expr.substitute sub clauses | sub <- ndSub]

    atLeastOne :: (a -> Either String ()) -> [a] -> Either String ()
    atLeastOne f []       = Left ""
    atLeastOne f (x : xs) =
      case f x of
        Right () -> Right ()
        Left msg ->
          case atLeastOne f xs of
            Right () -> Right ()
            Left _   -> Left msg

    refuteClause :: [Form] -> Nondeterministic Expr.Substitution
    refuteClause forms =
           concatMap maybeToList [Expr.unify p FFalse | p <- forms]
        ++ concatMap maybeToList [Expr.unify p (nnfNeg q) | p <- forms, q <- forms]
      where
        maybeToList Nothing  = []
        maybeToList (Just x) = [x]

    forallInstantiations :: [Form] -> Nondeterministic [Form]
    forallInstantiations forms0 = rec forms0
      where
        rec []                    = []
        rec (form@(FForall x _ p) : forms) =
          let forbidden = Expr.freeVars forms0
              z = Expr.freshVariableFor forbidden x
           in do let p' = Expr.substituteVar x (TVar z) p
                 [p' : forms] ++ map (form :) (rec forms)
        rec (form : forms) = map (form :) (rec forms)

collectJustificationFormulae :: LocalContext -> [HypId] -> M [Form]
collectJustificationFormulae lctx names =
    concat <$> mapM globalLocalLookup names
  where
    globalLocalLookup name = do
      gForms <- globalLookup name
      let lForms = localLookup name
      let glForms = gForms ++ lForms
      if null glForms
       then if S.member name (lctxDisabledLocalAssumptions lctx)
             then failM (
                     "Local assumption '" ++ show name ++ "' cannot be used.\n"
                  ++ "Eigenvariable has been shadowed."
                  )
             else
               if S.member name (lctxDisabledGlobalAssumptions lctx)
                then failM (
                       "Global assumption '" ++ show name ++ "' cannot be used.\n"
                     ++ "Eigenvariable has been shadowed."
                     )
                else failM ("Justification '" ++ show name ++ "' not found.")
       else return glForms
    globalLookup name = do
      if S.member name (lctxDisabledGlobalAssumptions lctx)
       then return []
       else do
         res <- globalCtxLookup name
         case res of
           Nothing   -> return []
           Just form -> return [form]
    localLookup name =
      case localCtxLookup lctx name of
        Nothing   -> []
        Just form -> [form]

checkFormulaIsLogicalConsequenceOf :: String -> LocalContext -> Form -> [HypId] -> M ()
checkFormulaIsLogicalConsequenceOf cmdPrefix lctx form justifications = do
  state <- getFS
  justifHypotheses <- collectJustificationFormulae lctx justifications
  case isLogicalConsequence form justifHypotheses of
    Right () -> return ()
    Left msg ->
      failM (unlines [
        cmdPrefix ++ ": Hypotheses do not entail formula."
      , "Expected formula: " ++ show form
      , "Justifications: " ++ joinS ", " (map show justifications)
      , "The following conjunctive clause should be refutable but it is irrefutable:\n"
         ++ msg
      ]
      ++ show lctx
      ++ show state
      )

checkFormulaeAreEquivalent :: String -> LocalContext -> OptionalForm -> Form -> [HypId] -> M ()
checkFormulaeAreEquivalent _ _ OFNone     _ _ = return ()
checkFormulaeAreEquivalent _ _ (OFHole _) _ _ = return ()
checkFormulaeAreEquivalent cmdPrefix lctx (OFForm form1) form2 justifications = do
  let lctx1 = localCtxBindOName ONNone form1 lctx
  checkFormulaIsLogicalConsequenceOf cmdPrefix lctx1 form2 justifications
  let lctx2 = localCtxBindOName ONNone form2 lctx
  checkFormulaIsLogicalConsequenceOf cmdPrefix lctx2 form1 justifications

-- Disable global and local assumptions with occurrences of the given parameter.
-- This is used for introduction of the universal quantifier.
forbidEigenvar :: VarId -> LocalContext -> M LocalContext
forbidEigenvar x lctx = do
  globalAssumptions <- gctxAssumptions . stateGlobalContext <$> getFS
  let (enabledLocal, disabledLocal) =
        partition (\ (name, form) -> x `S.notMember` Expr.freeEigenVars form)
                  (lctxLocalAssumptions lctx)
      disabledLocalNames = S.fromList (map fst disabledLocal)
      disabledGlobal =
        filter (\ (name, form) -> x `S.member` Expr.freeEigenVars form)
                  (M.toList globalAssumptions)
      disabledGlobalNames = S.fromList (map fst disabledGlobal)
   in return $
        lctx {
          lctxLocalAssumptions = enabledLocal
        , lctxDisabledLocalAssumptions = disabledLocalNames
                               `S.union` lctxDisabledLocalAssumptions lctx
        , lctxDisabledGlobalAssumptions = disabledGlobalNames
                               `S.union` lctxDisabledGlobalAssumptions lctx
        }

forbidAllEigenvars :: [VarId] -> LocalContext -> M LocalContext
forbidAllEigenvars []       lctx = return lctx
forbidAllEigenvars (x : xs) lctx = do
  lctx' <- forbidEigenvar x lctx
  forbidAllEigenvars xs lctx'

splitGoals :: String -> [Goal] -> M (Goal, [Goal])
splitGoals msg []       = failM (unlines [
                           msg ++ ": No goals left."
                         ])
splitGoals _   (g : gs) = return (g, gs)

justificationHypotheses :: [Justification] -> [HypId]
justificationHypotheses js = map justificationHypothesis js
  where
    justificationHypothesis (JHypothesis h)     = h
    justificationHypothesis JPreviousHypothesis = previousFormulaName

checkEigenvarsDoNotOccurInThesis :: String -> [VarId] -> Form -> M ()
checkEigenvarsDoNotOccurInThesis cmdPrefix names targetForm = do
  if length (nub names) < length names
    then failM (cmdPrefix ++ ": Repeated eigenvariables.")
    else return ()
  let introducedEigenVars = S.fromList names
  let formEigenVars = Expr.freeEigenVars targetForm
  let badEigenVars = S.intersection introducedEigenVars formEigenVars
  if not (null badEigenVars)
   then failM (
             cmdPrefix ++ ": Eigenvariables "
          ++ "[" ++ joinS "," (map show (S.toList badEigenVars)) ++ "]"
          ++ " occur in thesis.")
   else return ()

checkProof :: Proof -> [Goal] -> M ()
checkProof ProofEmpty goals =
  if null goals
   then return ()
   else failM (unlines [
           "end: There are pending goals."
         , showGoals goals
         ])
checkProof (ProofCons (CmdSuppose oName oForm) proof) goals = do
  (Goal lctx gForm, goals') <- splitGoals "suppose" goals
  case gForm of
    FImp gAntecedent gSuccedent ->
      if Expr.matchFormulae oForm gAntecedent
       then do
         bindIfHoleToFormula oForm lctx gAntecedent
         let lctx' = localCtxBindOName oName gAntecedent lctx
         checkProof proof (Goal lctx' gSuccedent : goals')
       else
         failM (unlines [
           "suppose: Antecedent does not match antecedent of goal."
         , "Antecedent        : " ++ show oForm
         , "Antecedent of goal: " ++ show gAntecedent
         ])
    _ -> failM (unlines [
           "suppose: Goal is not an implication."
         , showGoals goals
         ])
checkProof (ProofCons
             (CmdThus thusType oName oForm justifications)
             proof)
           goals = do
  let justifications' = justificationHypotheses justifications
  (goal@(Goal lctx targetForm), goals') <- splitGoals (show thusType) goals
  -- Decompose the formula `form` in the goal as a tree of conjunctions,
  -- removing those subtrees that contain the user formula.
  case takePatternFromConjunction oForm targetForm of
    Nothing -> failM (unlines [
                 show thusType ++ ": "
                 ++ "Head of goal does not match with of expected formula."
               , "Expected formula: " ++ show oForm
               , "Actual formula  : " ++ show targetForm
               ])
    Just (patternForm, mRemainingForm) -> do
      bindIfHoleToFormula oForm lctx patternForm
      let patternForm' = case thusType of
                           C_Contradiction -> FFalse
                           _               -> patternForm
      case thusType of 
        C_Admit -> do
          ----
          logFS "**** Warning: admitted goal."
          logFS ("Goal: " ++ show targetForm)
          logFS (show lctx)
          ----
        _ -> checkFormulaIsLogicalConsequenceOf
               (show thusType) lctx patternForm' justifications'
      let lctx' = localCtxBindOName oName targetForm lctx
      let pending = case mRemainingForm of
                      Nothing            -> []
                      Just remainingForm -> [Goal lctx' remainingForm]
      checkProof proof (pending ++ goals')
checkProof (ProofCons
             (CmdHave oName form justifications)
             proof)
           goals = do
  let justifications' = justificationHypotheses justifications
  (Goal lctx targetForm, goals') <- splitGoals "have" goals
  checkFormulaIsLogicalConsequenceOf "have" lctx form justifications'
  let lctx' = localCtxBindOName oName form lctx
  checkProof proof (Goal lctx' targetForm : goals')
checkProof (ProofCons (CmdLet typedNames) proof) goals = do
    let names = map fst typedNames
    (Goal lctx targetForm, goals') <- splitGoals "let" goals
    checkEigenvarsDoNotOccurInThesis "let" names targetForm
    introduceForall lctx typedNames targetForm goals' proof
  where
    introduceForall :: LocalContext -> [(VarId, Type)] -> Form -> [Goal] -> Proof
                    -> M ()
    introduceForall lctx [] form goals proof =
      checkProof proof (Goal lctx form : goals)
    introduceForall lctx ((x, tyX) : xs) (FForall vx tyVX form) goals proof = do
      if tyX /= tyVX
       then failM (unlines [
               "let: "
            ++ "Variable bound by universal formula is of type "
            ++ show tyVX
            ++ " but eigenvariable is of type "
            ++ show tyX
            ])
       else return ()
      let form' = Expr.substituteVar vx (TEigenVar x) form
      lctx' <- forbidEigenvar x lctx
      introduceForall lctx' xs form' goals proof
    introduceForall _ _ form _ _ =
      failM (unlines [
        "let: Formula is not led by a universal quantifier: "
      ++ show form
      ])
checkProof (ProofCons (CmdCases oForm oJustifications caseBranches) proof) goals = do
    (Goal lctx targetForm, goals') <- splitGoals "cases" goals
    let branchesForm = fOr (map branchFormula caseBranches)
    checkFormulaeAreEquivalent "cases" lctx oForm branchesForm
                               (justificationHypotheses oJustifications)
    bindIfHoleToFormula oForm lctx branchesForm
    checkFormulaIsLogicalConsequenceOf "cases" lctx branchesForm
                               (justificationHypotheses oJustifications)
    mapM_ (checkCaseBranch lctx targetForm) caseBranches
    checkProof proof goals'
  where
    branchFormula :: CaseBranch -> Form
    branchFormula (CaseBranch _ form _) = form
    checkCaseBranch :: LocalContext -> Form -> CaseBranch -> M ()
    checkCaseBranch lctx targetForm (CaseBranch oName form proof) = do
      let lctx' = localCtxBindOName oName form lctx
      checkProof proof [Goal lctx' targetForm]
checkProof (ProofCons (CmdTake term) proof) goals = do
  (Goal lctx gForm, goals') <- splitGoals "take" goals
  case gForm of
    FExists x _ form ->
      if null (Expr.freeVars term)
       then do let form' = Expr.substituteVar x term form
               checkProof proof (Goal lctx form' : goals')
       else failM (unlines [
              "take: Term must be ground."
            , "Given term: " ++ show term
            ])
    _ -> failM (unlines [
           "take: Goal is not an existential."
         , showGoals goals
         ])
checkProof (ProofCons (CmdConsider typedNames oName form justifications) proof) goals = do
    (Goal lctx targetForm, goals') <- splitGoals "cases" goals
    let names = map fst typedNames
    let existentialForm = foldr (uncurry FExists) form typedNames
    checkFormulaIsLogicalConsequenceOf "consider" lctx existentialForm
                                       (justificationHypotheses justifications)
    let form' = foldr (\ x -> Expr.substituteVar x (TEigenVar x)) form names
    lctx' <- forbidAllEigenvars names lctx
    let lctx'' = localCtxBindOName oName form' lctx'
    checkEigenvarsDoNotOccurInThesis "consider" names targetForm
    checkProof proof (Goal lctx'' targetForm : goals')
checkProof (ProofCons (CmdClaim oName form innerProof) proof) goals = do
  (Goal lctx gForm, goals') <- splitGoals "claim" goals
  checkProof innerProof [Goal lctx form]
  let lctx' = localCtxBindOName oName form lctx
  checkProof proof (Goal lctx' gForm : goals')
checkProof (ProofCons (CmdInduction indTyp indBranches) proof) goals = do
    (Goal lctx targetForm, goals') <- splitGoals "induction" goals
    case targetForm of
      FForall x tyX innerForm -> do
        if indTyp == tyX
         then return() 
         else failM ("induction: The induction principle is of type "
                    ++ show indTyp
                    ++ " but it is used to prove a universal formula of type "
                    ++ show tyX ++ ".")
        if null indBranches
         then failM ("induction: Cannot reason inductively over an empty datatype.")
         else return ()
        mapM_ (checkIndBranch lctx x tyX innerForm) indBranches
      _ -> failM "induction: Formula is not led by a universal quantifier."
    checkProof proof goals'
  where
    checkIndBranch :: LocalContext -> VarId -> Type -> Form -> IndBranch -> M ()
    checkIndBranch lctx x tyX innerForm (IndBranch pattern proof) = do
      let IndPattern con typedNames = pattern
      let (names, typs) = unzip typedNames
      let typedNamesOfSameType = filter (\ (_, tyY) -> tyX == tyY) typedNames
      let indHypotheses =
            map (\ (y, _) -> Expr.substituteVar x (TEigenVar y) innerForm)
                typedNamesOfSameType
      let indThesis =
            Expr.substituteVar x (TApp con (map TEigenVar names)) innerForm
      let indFormula = foldr FImp indThesis indHypotheses
      lctx' <- forbidAllEigenvars names lctx
      checkEigenvarsDoNotOccurInThesis "induction" names innerForm
      checkProof proof [Goal lctx' indFormula]
checkProof (ProofCons (CmdShow form) proof) goals = do
  (goal@(Goal lctx targetForm), goals') <- splitGoals "show" goals
  -- Decompose the formula `form` in the goal as a tree of conjunctions,
  -- removing those subtrees that contain the user formula.
  case takePatternFromConjunction (OFForm form) targetForm of
    Nothing -> failM (unlines [
                 "show: Head of goal does not match with of expected formula."
               , "Expected formula: " ++ show form
               , "Actual formula  : " ++ show targetForm
               ])
    Just (patternForm, mRemainingForm) -> do
      let pending1 = [Goal lctx patternForm]
      let pending2 = case mRemainingForm of
                       Nothing            -> []
                       Just remainingForm -> [Goal lctx remainingForm]
      checkProof proof (pending1 ++ pending2 ++ goals')
checkProof (ProofCons cmd _) _ =
  error ("Unimplemented proof command: " ++ show cmd)


