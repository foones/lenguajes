module Checker(CheckResult(..), check) where

import qualified Data.Set as S
import qualified Data.Map as Map
import qualified OrderMap as M

import FailState(FailState, execFS, getFS, putFS, failFS, logFS)
import Syntax(
         HoleId(..), HypId(..), NatVarId(..), FunId(..), PropId(..), FormVarId(..),
         NatVarKind(..),
         Program(..), Declaration(..), Nat(..), Form(..), Proof(..),
         CaseBranch(..), EqChain(..), PropParam(..), PropArg(..),
         formFreeNatVars, natFreeNatVars, isNumeral, isProofHole,
         formNot, formFalse
       )
import Substitution(
         NatRenaming,
         bindPropParamsToArgs, bindFunParamsToArgs,
         substituteForm, substituteNat,
         natRenamingFreeNatVars, freshNatVarId
       )
import Pprint(
         pprintHoleId, pprintHypId, pprintDeclaration, pprintForm, pprintNat,
         pprintNatVarParameterList
       )

data CheckResult = ResultOK
  deriving Show 

type Context = M.Map HypId Form
type NatContext = M.Map NatVarId NatVarKind

data ProofHoleInfo = ProofHoleInfo {
                       phId            :: HoleId,
                       phGlobalContext :: Context,
                       phLocalContext  :: Context,
                       phNatContext    :: NatContext,
                       phForm          :: Form
                     }

pprintNatContext :: NatContext -> String
pprintNatContext nctx = pprintNatVarParameterList (map swap (M.toList nctx))
  where swap (x, y) = (y, x)

instance Show ProofHoleInfo where
  show proofHole =
    let hole = "?" ++ pprintHoleId (phId proofHole) in
      unlines (
         ["--- Hole " ++ hole ++ " ---"]
      ++ ["    {" ++ pprintNatContext (phNatContext proofHole) ++ "}"]
      ++ ["    " ++ pprintHypId hyp ++ " : " ++ pprintForm form
           | (hyp, form) <- M.toList (phLocalContext proofHole)]
      ++ [" |- " ++ hole ++ " : " ++ pprintForm (phForm proofHole)]
      )

data CheckState = CheckState {
                    sFunctions      :: M.Map FunId ([NatVarId], Nat),
                    sPropositions   :: M.Map PropId ([PropParam], Form),
                    sContext        :: Context,
                    sNatContext     :: NatContext,
                    sTheorems       :: [HypId],
                    sProofHoles     :: [ProofHoleInfo]
                  }

type M = FailState CheckState

--------------------------------------------------------------------------------
-- Normalization and equality --

normalizeForm :: Form -> M Form
normalizeForm (FormVar formVarId) = return (FormVar formVarId)
normalizeForm (FormEq nat1 nat2) = return (FormEq nat1 nat2)
normalizeForm (FormOr form1 form2) =
  FormOr <$> normalizeForm form1
         <*> normalizeForm form2
normalizeForm (FormAnd form1 form2) =
  FormAnd <$> normalizeForm form1
          <*> normalizeForm form2
normalizeForm (FormImp form1 form2) =
  FormImp <$> normalizeForm form1
          <*> normalizeForm form2
normalizeForm (FormForall kind n form) =
  FormForall kind n <$> normalizeForm form
normalizeForm (FormExists kind n form) =
  FormExists kind n <$> normalizeForm form
normalizeForm (FormProp propId args) = do
  (params, formBody) <- getPropositionM propId
  let (formSubst, natSubst) = bindPropParamsToArgs params args
  let formBody' = substituteForm formSubst natSubst formBody
  normalizeForm formBody'

reduceNat :: Nat -> M (Maybe Nat)
reduceNat (NatHole _) = return Nothing
reduceNat (NatVar _) = return Nothing
reduceNat NatZero = return Nothing
reduceNat (NatSucc nat) = do
  mNat' <- reduceNat nat
  case mNat' of
    Nothing   -> return Nothing
    Just nat' -> return (Just (NatSucc nat'))
--
reduceNat (NatAdd NatZero nat) = return (Just nat)
reduceNat (NatAdd (NatSucc nat1) nat2) =
  return (Just (NatSucc (NatAdd nat1 nat2)))
reduceNat (NatAdd nat1 nat2) = do
  mNat1' <- reduceNat nat1
  case mNat1' of
    Just nat1' -> return (Just (NatAdd nat1' nat2))
    Nothing -> do
      mNat2' <- reduceNat nat2
      case mNat2' of
        Just nat2' -> return (Just (NatAdd nat1 nat2'))
        Nothing -> return Nothing
--
reduceNat (NatMul NatZero nat) = return (Just NatZero)
reduceNat (NatMul (NatSucc nat1) nat2) =
  return (Just (NatAdd nat2 (NatMul nat1 nat2)))
reduceNat (NatMul nat1 nat2) = do
  mNat1' <- reduceNat nat1
  case mNat1' of
    Just nat1' -> return (Just (NatMul nat1' nat2))
    Nothing -> do
      mNat2' <- reduceNat nat2
      case mNat2' of
        Just nat2' -> return (Just (NatMul nat1 nat2'))
        Nothing -> return Nothing
--
reduceNat (NatFun funId args) = do
  (params, natBody) <- getFunctionM funId
  let natSubst = bindFunParamsToArgs params args
  let natBody' = substituteNat natSubst natBody
  return (Just natBody')

normalizeNat :: Nat -> M Nat
normalizeNat nat = do
  mNat' <- reduceNat nat
  case mNat' of
    Nothing   -> return nat
    Just nat' -> normalizeNat nat'

whnfForm :: Form -> M Form
whnfForm (FormProp propId args) = do
  (params, formBody) <- getPropositionM propId
  let (formSubst, natSubst) = bindPropParamsToArgs params args
  let formBody' = substituteForm formSubst natSubst formBody
  whnfForm formBody'
whnfForm form = return form

alphaEquivForm :: NatRenaming -> NatRenaming -> Form -> Form -> Bool
alphaEquivForm r1 r2 (FormVar formVarId1) (FormVar formVarId2) =
  formVarId1 == formVarId2
alphaEquivForm r1 r2 (FormEq nat11 nat12) (FormEq nat21 nat22) =
     alphaEquivNat r1 r2 nat11 nat21
  && alphaEquivNat r1 r2 nat12 nat22
alphaEquivForm r1 r2 (FormOr form11 form12) (FormOr form21 form22) =
     alphaEquivForm r1 r2 form11 form21
  && alphaEquivForm r1 r2 form12 form22
alphaEquivForm r1 r2 (FormAnd form11 form12) (FormAnd form21 form22) =
     alphaEquivForm r1 r2 form11 form21
  && alphaEquivForm r1 r2 form12 form22
alphaEquivForm r1 r2 (FormImp form11 form12) (FormImp form21 form22) =
     alphaEquivForm r1 r2 form11 form21
  && alphaEquivForm r1 r2 form12 form22
alphaEquivForm r1 r2 (FormForall k1 n1 form1) (FormForall k2 n2 form2) =
  alphaEquivFormBinder r1 r2 k1 n1 form1 k2 n2 form2
alphaEquivForm r1 r2 (FormExists k1 n1 form1) (FormExists k2 n2 form2) =
  alphaEquivFormBinder r1 r2 k1 n1 form1 k2 n2 form2
alphaEquivForm r1 r2 (FormProp p1 args1) (FormProp p2 args2) =
     p1 == p2
  && length args1 == length args2
  && and (zipWith (alphaEquivPropArg r1 r2) args1 args2)
alphaEquivForm _ _ _ _ = False

alphaEquivFormBinder :: NatRenaming -> NatRenaming
                     -> NatVarKind -> NatVarId -> Form
                     -> NatVarKind -> NatVarId -> Form
                     -> Bool
alphaEquivFormBinder r1 r2 k1 n1 form1 k2 n2 form2 =
  k1 == k2 &&
  let forbidden = S.union (natRenamingFreeNatVars r1) (natRenamingFreeNatVars r2)
      m = freshNatVarId forbidden n1
   in alphaEquivForm (Map.insert n1 m r1) (Map.insert n2 m r2) form1 form2

alphaEquivPropArg :: NatRenaming -> NatRenaming -> PropArg -> PropArg -> Bool
alphaEquivPropArg r1 r2 (PropArgNat nat1) (PropArgNat nat2) =
  alphaEquivNat r1 r2 nat1 nat2
alphaEquivPropArg r1 r2 (PropArgForm form1) (PropArgForm form2) =
  alphaEquivForm r1 r2 form1 form2
alphaEquivPropArg _ _ _ _ = False

alphaEquivNat :: NatRenaming -> NatRenaming -> Nat -> Nat -> Bool
alphaEquivNat r1 r2 (NatHole h1) (NatHole h2) = h1 == h2
alphaEquivNat r1 r2 (NatVar n1) (NatVar n2) =
  Map.findWithDefault n1 n1 r1 == Map.findWithDefault n2 n2 r2
alphaEquivNat r1 r2 NatZero NatZero = True
alphaEquivNat r1 r2 (NatSucc nat1) (NatSucc nat2) =
  alphaEquivNat r1 r2 nat1 nat2
alphaEquivNat r1 r2 (NatAdd nat11 nat12) (NatAdd nat21 nat22) =
     alphaEquivNat r1 r2 nat11 nat21
  && alphaEquivNat r1 r2 nat12 nat22
alphaEquivNat r1 r2 (NatMul nat11 nat12) (NatMul nat21 nat22) =
     alphaEquivNat r1 r2 nat11 nat21
  && alphaEquivNat r1 r2 nat12 nat22
alphaEquivNat r1 r2 (NatFun f1 args1) (NatFun f2 args2) =
     f1 == f2
  && length args1 == length args2
  && and (zipWith (alphaEquivNat r1 r2) args1 args2)
alphaEquivNat _ _ _ _ = False

checkEqualMaybeForms :: Maybe Form -> Maybe Form -> M ()
checkEqualMaybeForms (Just form1) (Just form2) = checkEqualForms form1 form2
checkEqualMaybeForms _ _ = return ()

checkEqualForms :: Form -> Form -> M ()
checkEqualForms form1 form2 = do
  form1' <- normalizeForm form1
  form2' <- normalizeForm form2
  if alphaEquivForm Map.empty Map.empty form1' form2'
   then return ()
   else failFS (unlines [
          "Formulas should be equal:"
        , "    " ++ pprintForm form1
        , "    " ++ pprintForm form2
        , "Normalized:"
        , "    " ++ pprintForm form1'
        , "    " ++ pprintForm form2'
        ])

--------------------------------------------------------------------------------

defineFunctionM :: FunId -> [NatVarId] -> Nat -> M ()
defineFunctionM funId params nat = do
  state <- getFS
  putFS (state { sFunctions = M.insert funId (params, nat) (sFunctions state)})

definePropositionM :: PropId -> [PropParam] -> Form -> M ()
definePropositionM propId params form = do
  state <- getFS
  putFS (state { sPropositions = M.insert propId (params, form) (sPropositions state)})

getPropositionM :: PropId -> M ([PropParam], Form)
getPropositionM propId = do
  state <- getFS
  return (M.findWithDefault (error "undefined proposition") propId (sPropositions state))

getFunctionM :: FunId -> M ([NatVarId], Nat)
getFunctionM funId = do
  state <- getFS
  return (M.findWithDefault (error "undefined proposition") funId (sFunctions state))

addTheoremM :: HypId -> M ()
addTheoremM hypId = do
  state <- getFS
  putFS (state { sTheorems = hypId : sTheorems state })

addHypothesisM :: HypId -> Form -> M ()
addHypothesisM hypId form = do
  if hypId == HypId "_"
   then return ()
   else do
      state <- getFS
      putFS (state { sContext = M.insert hypId form (sContext state)})

removeHypothesisM :: HypId -> M ()
removeHypothesisM hypId = do
  if hypId == HypId "_"
   then return ()
   else do
      state <- getFS
      putFS (state { sContext = M.delete hypId (sContext state)})

getHypothesisM :: HypId -> M Form
getHypothesisM hypId = do
  state <- getFS
  return (M.findWithDefault (error "undefined hypothesis") hypId (sContext state))

addNatVarM :: NatVarKind -> NatVarId -> M ()
addNatVarM natVarKind natVarId = do
  state <- getFS
  putFS (state { sNatContext = M.insert natVarId natVarKind (sNatContext state)})

removeNatVarM :: NatVarId -> M ()
removeNatVarM natVarId = do
  state <- getFS
  putFS (state { sNatContext = M.delete natVarId (sNatContext state)})

getNatVarKindM :: NatVarId -> M NatVarKind
getNatVarKindM natVarId = do
  state <- getFS
  return (M.findWithDefault (error "undefined nat-variable") natVarId (sNatContext state))

addProofHoleM :: ProofHoleInfo -> M ()
addProofHoleM proofHoleInfo = do
  state <- getFS
  putFS (state { sProofHoles = proofHoleInfo : sProofHoles state })

getLocalContextM :: M Context
getLocalContextM = do
  context <- sContext <$> getFS
  theorems <- sTheorems <$> getFS
  return (foldr M.delete context theorems)

getGlobalContextM :: M Context
getGlobalContextM = do
  context <- sContext <$> getFS
  theorems <- sTheorems <$> getFS
  return (foldr (\ thm -> M.insert thm (M.findWithDefault undefined thm context))
                M.empty
                (reverse theorems))

getNatContextM :: M NatContext
getNatContextM = sNatContext <$> getFS

check :: Program -> Either String CheckResult
check program =
    case execFS (checkProgramAndPrintHoles program) initialState of
      Left msg    -> Left msg
      Right state -> Right ResultOK
  where
    initialState = CheckState {
                     sFunctions    = M.empty
                   , sPropositions = builtinPropositions
                   , sContext      = M.empty
                   , sNatContext   = M.empty
                   , sTheorems     = []
                   , sProofHoles   = []
                   }
    builtinPropositions =
      M.fromList [
        (PropId "true", ([], FormEq NatZero NatZero))
      , (PropId "false", ([], FormEq NatZero (NatSucc NatZero)))
      , (PropId "not", (
           [PropParamForm (FormVarId "X")],
           (FormImp (FormVar (FormVarId "X")) (FormProp (PropId "false") []))))
      ]

checkProgramAndPrintHoles :: Program -> M ()
checkProgramAndPrintHoles program = do
  checkProgram program
  proofHoles <- reverse . sProofHoles <$> getFS
  let visibleProofHoles = filter (\ ph -> phId ph /= HoleId "_") proofHoles
  if null visibleProofHoles
   then do
     if null proofHoles
      then return ()
      else logFS "!!! There are invisible proof holes."
   else do
      let firstProofHole = head visibleProofHoles
      logFS (show firstProofHole)
      return ()

checkProgram :: Program -> M ()
checkProgram (Program declarations) =
  mapM_ (\ declaration -> do
             --logFS (pprintDeclaration declaration)
             checkDeclaration declaration
             --logFS ""
             )
        declarations

checkDeclaration :: Declaration -> M ()
checkDeclaration (DeclFun funId params nat) =
  defineFunctionM funId params nat
checkDeclaration (DeclProp propId params form) =
  definePropositionM propId params form
checkDeclaration (DeclTheorem name form proof) = do
  checkProof proof (Just form)
  addHypothesisM name form
  addTheoremM name
checkDeclaration (DeclEvalForm form) = do
  form' <- normalizeForm form
  logFS ("~> " ++ pprintForm form')
checkDeclaration (DeclEvalNat nat) = do
  nat' <- normalizeNat nat
  logFS ("~> " ++ pprintNat nat')

checkProof :: Proof -> Maybe Form -> M Form
checkProof (ProofHole holeId) mForm =
  case mForm of
    Nothing ->
      failFS ("Hole " ++ pprintHoleId holeId ++ " occurs at a synthesizing context.")
    Just form -> do
      localContext <- getLocalContextM
      globalContext <- getGlobalContextM
      natContext <- getNatContextM
      addProofHoleM (ProofHoleInfo {
                      phId = holeId,
                      phGlobalContext = globalContext,
                      phLocalContext = localContext,
                      phNatContext = natContext,
                      phForm = form
                    })
      return form
checkProof (ProofAx hypId) mForm = do
  formHyp <- getHypothesisM hypId
  checkEqualMaybeForms mForm (Just formHyp)
  return formHyp
checkProof (ProofLetSym n proof) (Just form) = do
  wForm <- whnfForm form
  case wForm of
    FormForall NatVarSym m formBody -> do
      let formBody' = substituteForm Map.empty (Map.fromList [(m, NatVar n)]) formBody
      addNatVarM NatVarSym n
      checkProof proof (Just formBody')
      removeNatVarM n
      return form
    _ -> failFS ("Symbolic 'let' should prove a symbolic 'forall'. Got: " ++ pprintForm form)
checkProof (ProofLetSym n proof) Nothing = do
  error "TODO: Synthesize symbolic 'let'."
checkProof (ProofLetFin n proof) (Just form) = do
  wForm <- whnfForm form
  case wForm of
    FormForall NatVarFin m formBody -> do
      let formBody' = substituteForm Map.empty (Map.fromList [(m, NatVar n)]) formBody
      addNatVarM NatVarFin n
      checkProof proof (Just formBody')
      removeNatVarM n
      return form
    _ -> failFS ("Finitistic 'let' should prove a finitistic 'forall'. Got: " ++ pprintForm form)
checkProof (ProofLetFin n proof) Nothing = do
  error "TODO: Synthesize finitistic 'let'."
checkProof (ProofSuppose hypId mFormHyp proof) (Just form) = do
  wForm <- whnfForm form
  case wForm of
    FormImp formA formB -> do
      checkEqualMaybeForms (Just formA) mFormHyp
      addHypothesisM hypId formA
      checkProof proof (Just formB)
      removeHypothesisM hypId
      return form
    _ -> failFS ("'suppose' should prove an implication. Got: " ++ pprintForm form)
checkProof (ProofSuppose hypId mFormHyp proof) Nothing = do
  error "TODO: Synthesize 'suppose'."
checkProof (ProofShow form proof) mForm = do
  checkEqualMaybeForms (Just form) mForm
  checkProof proof (Just form)
checkProof (ProofClaim hypId cutForm cutProof mainProof) mForm = do
  checkProof cutProof (Just cutForm)
  addHypothesisM hypId cutForm
  mainForm <- checkProof mainProof mForm
  removeHypothesisM hypId
  return mainForm
checkProof (ProofApplyProof proof1 proof2) mForm = do
  formFull <- checkProof proof1 Nothing
  wFormFull <- whnfForm formFull
  case wFormFull of
    FormImp formA formB -> do
      checkProof proof2 (Just formA)
      checkEqualMaybeForms mForm (Just formB)
      return formB
    _ -> failFS ("Proof with argument must prove an implication. Got: " ++ pprintForm formFull)
checkProof (ProofApplyNat proof nat) mForm = do
  formFull <- checkProof proof Nothing
  wFormFull <- whnfForm formFull
  case wFormFull of
    FormForall natVarKind n formA -> do
      checkNaturalMatchesKind natVarKind nat
      let formA' = substituteForm Map.empty (Map.fromList [(n, nat)]) formA
      checkEqualMaybeForms mForm (Just formA')
      return formA'
    _ -> failFS ("Proof with nat argument must prove a 'forall'. Got: " ++ pprintForm formFull)
checkProof (ProofIndeed eqChain) (Just form) = do
  wForm <- whnfForm form
  case wForm of
    FormEq natA natB -> do
      checkEqChainWithEndpoints eqChain (Just natA) (Just natB)
      return form
    _ -> failFS ("'indeed' must prove an equality. Got: " ++ pprintForm form)
checkProof (ProofIndeed eqChain) Nothing = do
  (natA, natB) <- checkEqChainWithEndpoints eqChain Nothing Nothing
  return (FormEq natA natB)
checkProof (ProofInduction base step) (Just form) = do
  wForm <- whnfForm form
  case wForm of
    FormForall NatVarFin n formBody -> do
      let formBase = substituteForm Map.empty (Map.fromList [(n, NatZero)]) formBody
      let formBodySucc = substituteForm Map.empty (Map.fromList [(n, NatSucc (NatVar n))]) formBody
      let formStep = FormForall NatVarFin n
                       (FormImp
                         formBody
                         formBodySucc)
      checkProof base (Just formBase)
      checkProof step (Just formStep)
    _ -> failFS ("'induction' should prove a finitistic 'forall'. Got: " ++ pprintForm form)
checkProof (ProofInduction base step) Nothing =
  error "TODO: Synthesize 'induction'."
checkProof (ProofContradiction proof) (Just form) = do
  formAbs <- checkProof proof Nothing
  checkIsContradiction "'contradiction' should receive a proof of a contradiction"
                       formAbs
  return form
checkProof (ProofContradiction proof) Nothing = do
  error "TODO: Synthesize 'contradiction'."
checkProof (ProofThen proofs) mForm = do
    matches <- matchConjunction proofs mForm
    forms <- mapM (uncurry checkProof) matches
    return (foldr1 FormAnd forms)
  where
    matchConjunction :: [Proof] -> Maybe Form -> M [(Proof, Maybe Form)]
    matchConjunction [] _ = error "impossible"
    matchConjunction [proof] mForm = return [(proof, mForm)]
    matchConjunction (proof : proofs) Nothing = do
      matches <- matchConjunction proofs Nothing
      return ((proof, Nothing) : matches)
    matchConjunction (proof : proofs) (Just form) = do
      wForm <- whnfForm form
      case wForm of
        FormAnd form1 form2 -> do
          matches <- matchConjunction proofs (Just form2)
          return ((proof, Just form1) : matches)
        _ -> failFS ("'then...also...' proves a conjunction. Got: " ++ pprintForm form)
checkProof (ProofHave hyps proofConj proofBody) mForm = do
    checkIsMaybeHarrop "'have' can only be used to prove a Harrop formula" mForm
    let hypNames = map fst hyps
    let mForms   = map snd hyps
    let mFormConjunction = collectConjunction hyps
    formConjunction <- checkProof proofConj mFormConjunction
    conjunctForms <- matchHypsConjunction hyps formConjunction
    mapM_ (\ (mForm, cForm) -> checkEqualMaybeForms mForm (Just cForm))
          (zip mForms conjunctForms)
    mapM_ (uncurry addHypothesisM) (zip hypNames conjunctForms)
    form <- checkProof proofBody mForm
    mapM_ removeHypothesisM hypNames
    return form
  where
    collectConjunction :: [(HypId, Maybe Form)] -> Maybe Form
    collectConjunction [] = Nothing
    collectConjunction [(_, mForm)] = mForm
    collectConjunction ((_, mForm1) : hyps) = do
      form1 <- mForm1
      form2 <- collectConjunction hyps
      return (FormAnd form1 form2)
    matchHypsConjunction :: [(HypId, Maybe Form)] -> Form -> M [Form]
    matchHypsConjunction []         _    =
      failFS "'have' should take at least one assumption."
    matchHypsConjunction [_] form = do
      return [form]
    matchHypsConjunction (_ : hyps) form = do
      wForm <- whnfForm form
      case wForm of
        FormAnd form1 form2 -> do
          conjunctForms <- matchHypsConjunction hyps form2
          return (form1 : conjunctForms)
        _ -> failFS ("'have' should prove a conjunction. Got: " ++ pprintForm form)
checkProof (ProofAssume hyps proof) (Just form) = do
    let hypNames = map fst hyps
    let mForms   = map snd hyps
    negatedForms <- matchHypsDisjunction hyps form
    mapM_ (\ (mForm, nForm) -> checkEqualMaybeForms mForm (Just nForm))
          (zip mForms negatedForms)
    mapM_ (uncurry addHypothesisM) (zip hypNames negatedForms)
    checkProof proof (Just formFalse)
    mapM_ removeHypothesisM hypNames
    return form
  where
    matchHypsDisjunction :: [(HypId, Maybe Form)] -> Form -> M [Form]
    matchHypsDisjunction []         _    =
      failFS "'assume' should take at least one assumption."
    matchHypsDisjunction [_] form = do
      return [formNot form]
    matchHypsDisjunction (_ : hyps) form = do
      wForm <- whnfForm form
      case wForm of
        FormOr form1 form2 -> do
          negForms <- matchHypsDisjunction hyps form2
          return (formNot form1 : negForms)
        _ -> failFS ("'assume' should prove a disjunction. Got: " ++ pprintForm form)
checkProof (ProofAssume mHyps proof) Nothing = do
  error "TODO: Synthesize 'assume."
checkProof (ProofCases proofGuard branches) (Just mainForm) = do
    checkIsMaybeHarrop "'cases can only be used to prove a Harrop formula"
                       (Just mainForm)
    formDisjunction <- checkProof proofGuard Nothing
    checkCaseBranches branches formDisjunction
  where
    checkCaseBranches :: [CaseBranch] -> Form -> M Form
    checkCaseBranches [] _ = error "impossible"
    checkCaseBranches [branch] formDisjunction =
      checkCaseBranch branch formDisjunction
    checkCaseBranches (branch : branches) formDisjunction = do
        wFormDisjunction <- whnfForm formDisjunction
        case wFormDisjunction of
          FormOr form1 form2 -> do
            checkCaseBranch branch form1
            checkCaseBranches branches form2
          _ -> failFS ("'cases' guard must be a disjunction. Got: "
                       ++ pprintForm formDisjunction)
    checkCaseBranch :: CaseBranch -> Form -> M Form
    checkCaseBranch (CaseBranch hypId mFormHyp proof) formHyp = do
      checkEqualMaybeForms mFormHyp (Just formHyp)
      addHypothesisM hypId formHyp
      form <- checkProof proof (Just mainForm)
      removeHypothesisM hypId
      return form
checkProof (ProofCases proofGuard branches) Nothing = do
  error "TODO: Synthesize 'cases'."
checkProof (ProofTake nat proof) (Just form) = do
  wForm <- whnfForm form
  case wForm of
    FormExists natVarKind n formBody -> do
      checkNaturalMatchesKind natVarKind nat
      let formBody' = substituteForm Map.empty (Map.fromList [(n, nat)]) formBody
      checkProof proof (Just formBody')
      return form
    _ -> failFS "'take' should prove an existential."
  return form
checkProof (ProofTake nat proof) Nothing = do
  error "TODO: Synthesize 'take."
checkProof (ProofConsider natParams hypId mFormHyp proofGuard proof) (Just mainForm) = do
    checkIsMaybeHarrop "'consider can only be used to prove a Harrop formula"
                       (Just mainForm)
    formExists <- checkProof proofGuard Nothing
    formGuarantee <- matchExistential natParams formExists
    mapM_ (uncurry addNatVarM) natParams
    checkEqualMaybeForms (Just formGuarantee) mFormHyp
    addHypothesisM hypId formGuarantee
    form <- checkProof proof (Just mainForm)
    removeHypothesisM hypId
    mapM_ (removeNatVarM . snd) natParams
    return form
  where
    matchExistential :: [(NatVarKind, NatVarId)] -> Form -> M Form
    matchExistential [] formEx = return formEx
    matchExistential ((kind, id) : params) formEx = do
      wForm <- whnfForm formEx
      case wForm of
        FormExists kind' id' formBody ->
          case (kind, kind') of
            _ | kind == kind' -> do
              let formBody' = substituteForm Map.empty (Map.fromList [(id', NatVar id)]) formBody
              matchExistential params formBody'
            (NatVarSym, NatVarFin) ->
              failFS "Symbolic 'consider' applied to a finitistic existential."
            (NatVarFin, NatVarSym) ->
              failFS "Finitistic 'consider' applied to a symbolic existential."
            _ -> error "impossible"
        _ -> failFS ("'consider' guard must be an existential. Got: "
                     ++ pprintForm formEx)
checkProof (ProofConsider natParams hypId mFormHyp proofGuard proof) Nothing = do
  error "TODO: Synthesize 'consider."

checkIsContradiction :: String -> Form -> M ()
checkIsContradiction msg form = do
  wForm <- whnfForm form
  case wForm of
    FormEq natA natB -> do
      natA' <- normalizeNat natA
      natB' <- normalizeNat natB
      case (natA', natB') of
        (NatZero, NatSucc _) -> return ()
        (NatSucc _, NatZero) -> return ()
        _ -> failFS (msg ++ ". Got: " ++ pprintForm form)
    _ -> failFS (msg ++ ". Got: " ++ pprintForm form)

checkIsHarrop :: String -> Form -> M ()
checkIsHarrop msg form = do
  wForm <- whnfForm form
  case wForm of
    FormEq _ _           -> checkIsContradiction msg wForm
    FormImp _ form'      -> checkIsHarrop msg form'
    FormForall _ _ form' -> checkIsHarrop msg form'
    -- Important: in this setting, the formulae or, and, exists
    -- are always Harrop, because they are just abbreviations:
    --
    --   (A | B)     = not(A) -> not(B) -> false
    --   (A & B)     = not(A -> B -> false)
    --   exists n. A = not(forall n. not(A))
    --
    FormOr _ _       -> return ()
    FormAnd _ _      -> return ()
    FormExists _ _ _ -> return ()
    _ -> failFS (msg ++ ". Got: " ++ pprintForm form)

checkIsMaybeHarrop :: String -> Maybe Form -> M Form
checkIsMaybeHarrop msg mFormAbs =
  case mFormAbs of
    Just formAbs -> do
      checkIsHarrop msg formAbs
      return formAbs
    Nothing -> return (FormEq NatZero (NatSucc NatZero))

eqChainFirst :: EqChain -> Nat
eqChainFirst (EqRefl nat)      = nat
eqChainFirst (EqTrans nat _ _) = nat

eqChainLast :: EqChain -> Nat
eqChainLast (EqRefl nat)          = nat
eqChainLast (EqTrans _ _ eqChain) = eqChainLast eqChain

checkEqChainWithEndpoints :: EqChain -> Maybe Nat -> Maybe Nat -> M (Nat, Nat)
checkEqChainWithEndpoints eqChain mNatA0 mNatB = do
    case mNatA0 of
      Nothing -> return ()
      Just natA0 -> checkStrictEqualNatOrHole natA0 (eqChainFirst eqChain)
    rec eqChain
    return (eqChainFirst eqChain, eqChainLast eqChain)
  where
    rec (EqRefl natA) =
      case mNatB of
        Nothing -> return ()
        Just natB -> checkStrictEqualNatOrHole natA natB
    rec (EqTrans natA Nothing eqChain) = do
      checkWeaktEqualNatOrHole natA (eqChainFirst eqChain)
      rec eqChain
    rec (EqTrans natA (Just proof) eqChain) = do
      -- Must synthesize the formula, because the justification
      -- may not equate directly "natA = eqChainFirst eqChain";
      -- it may affect a subterm by congruence.
      -- Moreover if the justification proves S(n) = S(m)
      -- this can be used to justify n = m or m = n.
      form <- if isProofHole proof 
               then checkProof proof (Just (FormEq natA (eqChainFirst eqChain)))
               else checkProof proof Nothing 
      wForm <- whnfForm form
      case wForm of
        FormEq lhs rhs -> do
          checkConvertUsing (Eqn lhs rhs) natA (eqChainFirst eqChain)
          rec eqChain
        _ -> failFS ("Justification in chain of equalities must be an equation. Got: " ++ pprintForm form)

data Equation = Eqn Nat Nat

instance Show Equation where
  show (Eqn nat1 nat2) = pprintNat nat1 ++ " = " ++ pprintNat nat2

checkConvertUsing :: Equation -> Nat -> Nat -> M ()
checkConvertUsing eq nat1 nat2 =
    let eqClosure = [eq'' | eq' <- closeByPredecessor eq,
                            eq'' <- closeBySymmetry eq']
        nat1Closure = [nat1' | eq'' <- eqClosure,
                               nat1' <- allReplacements eq'' nat1]
     in if elem nat2 nat1Closure
         then return ()
         else do
           failFS ("Justification does not equate " ++ pprintNat nat1 ++ " and " ++ pprintNat nat2)
  where
    closeByPredecessor :: Equation -> [Equation]
    closeByPredecessor e@(Eqn (NatSucc n) (NatSucc m)) =
      e : closeByPredecessor (Eqn n m)
    closeByPredecessor e = [e]
    closeBySymmetry :: Equation -> [Equation]
    closeBySymmetry (Eqn n m) = [Eqn n m, Eqn m n]
    allReplacements :: Equation -> Nat -> [Nat]
    allReplacements eq@(Eqn lhs rhs) nat =
         (if nat == lhs then [rhs] else [])
      ++ allInternalReplacements eq nat
    allInternalReplacements :: Equation -> Nat -> [Nat]
    allInternalReplacements eq (NatHole _) = []
    allInternalReplacements eq (NatVar _) = []
    allInternalReplacements eq NatZero = []
    allInternalReplacements eq (NatSucc n) = map NatSucc (allReplacements eq n)
    allInternalReplacements eq (NatAdd n m) =
         (map (NatAdd n) (allReplacements eq m))
      ++ (map (flip NatAdd m) (allReplacements eq n))
    allInternalReplacements eq (NatMul n m) =
         (map (NatMul n) (allReplacements eq m))
      ++ (map (flip NatMul m) (allReplacements eq n))
    allInternalReplacements eq (NatFun f ns) =
      map (NatFun f) (allInternalReplacementsList eq ns)
    allInternalReplacementsList :: Equation -> [Nat] -> [[Nat]]
    allInternalReplacementsList eq []       = [[]]
    allInternalReplacementsList eq (n : ns) =
         map (: ns) (allReplacements eq n)
      ++ map (n :) (allInternalReplacementsList eq ns)

checkWeaktEqualNatOrHole :: Nat -> Nat -> M ()
checkWeaktEqualNatOrHole (NatHole _) _ = return ()
checkWeaktEqualNatOrHole _ (NatHole _) = return ()
checkWeaktEqualNatOrHole nat1 nat2 = do
  nat1' <- normalizeNat nat1
  nat2' <- normalizeNat nat2
  checkStrictEqualNatOrHole nat1' nat2'

checkStrictEqualNatOrHole :: Nat -> Nat -> M ()
checkStrictEqualNatOrHole (NatHole _) _ = return ()
checkStrictEqualNatOrHole _ (NatHole _) = return ()
checkStrictEqualNatOrHole nat1 nat2 = do
  if nat1 == nat2  -- Check strict, syntactical, equality
   then return ()
   else failFS (unlines [
          "Natural numbers are not strictly equal:"
        , "    " ++ pprintNat nat1
        , "    " ++ pprintNat nat2
        ])

checkNaturalMatchesKind :: NatVarKind -> Nat -> M ()
checkNaturalMatchesKind NatVarSym _   = return ()
checkNaturalMatchesKind NatVarFin nat = checkFinitisticNatural nat
  where
    checkFinitisticNatural :: Nat -> M ()
    checkFinitisticNatural nat = do
        b <- test nat
        if b
         then return ()
         else failFS ("Natural number must be finitistic. Got: " ++ pprintNat nat)
      where
        test nat =
          if isNumeral nat
           then return True
           else case nat of
                  NatHole _ -> return True
                  NatVar natVarId -> do
                    natVarKind <- getNatVarKindM natVarId
                    return (natVarKind == NatVarFin)
                  _ -> return False


