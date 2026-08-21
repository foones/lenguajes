module Pprint(
         pprintHoleId, pprintHypId,
         pprintProgram, pprintDeclaration, 
         pprintPropParams, pprintPropArgs, pprintForm, pprintNat,
         pprintNatVarParameterList
       ) where

import Syntax(
         HoleId(..), HypId(..), NatVarId(..), FunId(..), PropId(..), FormVarId(..),
         NatVarKind(..),
         Program(..), Declaration(..), Nat(..), Form(..), Proof(..),
         CaseBranch(..), EqChain(..), PropParam(..), PropArg(..),
         testNumeral
       )

maxLineWidth :: Int
maxLineWidth = 60

isShort :: String -> Bool
isShort s = '\n' `notElem` s && length s <= maxLineWidth

possiblyShort :: String -> String
possiblyShort s =
  if isShort s
   then " " ++ s
   else "\n" ++ indent s

isLeft (Left _)    = True
isLeft (Right _)   = False
fromLeft (Left x)  = x
fromLeft (Right _) = undefined

indent :: String -> String
indent = joinS "\n" . map ("  " ++) . lines

joinS :: [a] -> [[a]] -> [a]
joinS sep []       = []
joinS sep [x]      = x
joinS sep (x : xs) = x ++ sep ++ joinS sep xs

pprintHoleId :: HoleId -> String
pprintHoleId (HoleId id) = id

pprintHypId :: HypId -> String
pprintHypId (HypId id) = id

pprintNatVarId :: NatVarId -> String
pprintNatVarId (NatVarId id) = id

pprintFormVarId :: FormVarId -> String
pprintFormVarId (FormVarId id) = id

pprintFunId :: FunId -> String
pprintFunId (FunId id) = id

pprintPropId :: PropId -> String
pprintPropId (PropId id) = id

pprintProgram :: Program -> String
pprintProgram (Program decls) = joinS "\n\n" (map pprintDeclaration decls)
                              ++ "\n"

pprintDeclaration :: Declaration -> String
pprintDeclaration (DeclTheorem theoremName form proof) =
  "theorem " ++ pprintHypId theoremName ++ " : " ++ pprintForm form ++ "\n"
  ++ "proof\n"
  ++ indent (pprintProof proof) 
  ++ "\nend"
pprintDeclaration (DeclFun funId params nat) =
  "fun " ++ pprintFunId funId ++ pprintParenList pprintNatVarId params ++ " := " ++ pprintNat nat
pprintDeclaration (DeclProp predId params form) =
  "prop " ++ pprintPropId predId ++ pprintPropParams params ++ " := " ++ pprintForm form
pprintDeclaration (DeclEvalForm form) =
  "eval " ++ pprintForm form
pprintDeclaration (DeclEvalNat nat) =
  "eval " ++ pprintNat nat

pprintPropParams :: [PropParam] -> String
pprintPropParams = pprintParenList pprintPropParam

pprintPropParam :: PropParam -> String
pprintPropParam (PropParamNat natVarId)   = pprintNatVarId natVarId
pprintPropParam (PropParamForm formVarId) = pprintFormVarId formVarId

pprintPropArgs :: [PropArg] -> String
pprintPropArgs = pprintParenList pprintPropArg

pprintPropArg :: PropArg -> String
pprintPropArg (PropArgNat nat)   = pprintNat nat
pprintPropArg (PropArgForm form) = pprintForm form

pprintParenList :: (a -> String) -> [a] -> String
pprintParenList pprintElem [] = ""
pprintParenList pprintElem xs = "(" ++ joinS ", " (map pprintElem xs) ++ ")"

pprintForm :: Form -> String
pprintForm = pprintFormOuter

data Binder = BinderForall | BinderExists
  deriving Eq

instance Show Binder where
  show BinderForall = "forall"
  show BinderExists = "exists"

pprintFormOuter :: Form -> String
pprintFormOuter form =
   case splitBinder form of
     (Nothing, [], _) -> pprintFormInner form
     (Just binder, params, body) ->
       show binder ++ " " ++ pprintNatVarParameterList params ++ ", "
                   ++ pprintFormOuter body
     _ -> error "impossible"
  where
    splitBinder :: Form -> (Maybe Binder, [(NatVarKind, NatVarId)], Form)
    splitBinder fullForm@(FormForall kind id form) =
      splitBinder' BinderForall kind id form
    splitBinder fullForm@(FormExists kind id form) =
      splitBinder' BinderExists  kind id form
    splitBinder fullForm = (Nothing, [], fullForm)
    splitBinder' :: Binder -> NatVarKind -> NatVarId -> Form
                 -> (Maybe Binder, [(NatVarKind, NatVarId)], Form)
    splitBinder' binder kind id form =
      let (mInnerBinder, params, body) = splitBinder form 
       in if mInnerBinder `elem` [Just binder, Nothing]
           then (Just binder, (kind, id) : params, body)
           else (Just binder, [(kind, id)], form)

pprintNatVarParameterList :: [(NatVarKind, NatVarId)] -> String
pprintNatVarParameterList params = joinS " " (map (uncurry pprintNatVarParameter) params)

pprintNatVarParameter :: NatVarKind -> NatVarId -> String
pprintNatVarParameter NatVarSym id = pprintNatVarId id
pprintNatVarParameter NatVarFin id = "#" ++ pprintNatVarId id

pprintFormInner :: Form -> String
pprintFormInner form = pprintFormLevel 1 form

pprintFormLevel :: Integer -> Form -> String
pprintFormLevel level@1 (FormImp form1 form2) =
     pprintFormLevel (level + 1) form1
  ++ " -> "
  ++ pprintFormLevel level form2
pprintFormLevel level@2 (FormOr form1 form2) =
     pprintFormLevel (level + 1) form1
  ++ " | "
  ++ pprintFormLevel level form2
pprintFormLevel level@3 (FormAnd form1 form2) =
     pprintFormLevel (level + 1) form1
  ++ " & "
  ++ pprintFormLevel level form2
pprintFormLevel level@4 form = pprintFormAtom form
pprintFormLevel level form = pprintFormLevel (level + 1) form

pprintFormAtom :: Form -> String
pprintFormAtom (FormVar formVarId) = pprintFormVarId formVarId
pprintFormAtom (FormEq nat1 nat2) = pprintNat nat1 ++ " = " ++ pprintNat nat2
pprintFormAtom (FormProp p args) = pprintPropId p ++ pprintPropArgs args
pprintFormAtom form = "(" ++ pprintForm form ++ ")"

pprintNat :: Nat -> String
pprintNat = pprintNatOuter

pprintNatOuter :: Nat -> String
pprintNatOuter (NatAdd nat1 nat2) = pprintNatInner nat1 ++ " + " ++ pprintNatOuter nat2
pprintNatOuter nat = pprintNatInner nat

pprintNatInner :: Nat -> String
pprintNatInner (NatMul nat1 nat2) = pprintNatAtom nat1 ++ " * " ++ pprintNatInner nat2
pprintNatInner nat = pprintNatAtom nat

pprintNatAtom :: Nat -> String
pprintNatAtom (NatHole id)    = "?" ++ pprintHoleId id
pprintNatAtom (NatVar id)     = pprintNatVarId id
pprintNatAtom NatZero         = "0"
pprintNatAtom n@(NatSucc n')  =
  case testNumeral n of
    Just k  -> show k
    Nothing -> "S(" ++ pprintNat n' ++ ")"
pprintNatAtom (NatFun f args) = pprintFunId f ++ pprintParenList pprintNat args
pprintNatAtom nat             = "(" ++ pprintNat nat ++ ")"

pprintProof :: Proof -> String
pprintProof = pprintProofOuter

pprintProofOuter :: Proof -> String
pprintProofOuter proof =
    case proof of
      ProofLetSym _ _         -> pprintProofLet proof
      ProofLetFin _ _         -> pprintProofLet proof
      ProofSuppose _ _ _      -> pprintProofSuppose proof
      ProofShow _ _           -> pprintProofShow proof
      ProofClaim _ _ _ _      -> pprintProofClaim proof
      ProofThen _             -> pprintProofThen proof
      ProofHave _ _ _         -> pprintProofHave proof
      ProofAssume _ _         -> pprintProofAssume proof
      ProofCases _ _          -> pprintProofCases proof
      ProofTake _ _           -> pprintProofTake proof
      ProofConsider _ _ _ _ _ -> pprintProofConsider proof
      _ -> pprintProofApply proof
  where
    pprintProofLet :: Proof -> String
    pprintProofLet proof =
      let (params, body) = splitLet proof
       in "let " ++ pprintNatVarParameterList params ++ ",\n" ++
          pprintProofOuter body
    splitLet :: Proof -> ([(NatVarKind, NatVarId)], Proof)
    splitLet (ProofLetSym id proof) =
      let (params, body) = splitLet proof 
       in ((NatVarSym, id) : params, body)
    splitLet (ProofLetFin id proof) =
      let (params, body) = splitLet proof 
       in ((NatVarFin, id) : params, body)
    splitLet proof = ([], proof)

    pprintProofSuppose :: Proof -> String
    pprintProofSuppose proof =
      let (hyps, body) = splitSuppose proof 
       in "suppose " ++ pprintHypotheses hyps ++ ",\n" ++
          pprintProofOuter body
    splitSuppose :: Proof -> ([(HypId, Maybe Form)], Proof)
    splitSuppose proof@(ProofSuppose _ Nothing _) = splitSuppose' proof
    splitSuppose (ProofSuppose id (Just form) body) = ([(id, Just form)], body)
    splitSuppose _ = error "impossible"
    splitSuppose' :: Proof -> ([(HypId, Maybe Form)], Proof)
    splitSuppose' (ProofSuppose id Nothing proof) =
      let (hyps, body) = splitSuppose' proof
       in ((id, Nothing) : hyps, body)
    splitSuppose' proof = ([], proof)

    pprintProofAssume :: Proof -> String
    pprintProofAssume (ProofAssume hyps body) =
         "assume " ++ pprintHypotheses hyps ++ ",\n"
      ++ pprintProofOuter body
    pprintProofAssume _ = error "impossible"

    pprintProofShow :: Proof -> String
    pprintProofShow (ProofShow form body) =
      "show " ++ pprintForm form ++ ",\n" ++
      pprintProofOuter body
    pprintProofShow _ = error "impossible"

    pprintProofClaim :: Proof -> String
    pprintProofClaim (ProofClaim hypName form formProof body) =
      "claim " ++ pprintHypId hypName ++ " : " ++ pprintForm form ++ "\n" ++
      "proof\n" ++
      indent (pprintProof formProof) ++
      "end\n" ++
      pprintProofOuter body
    pprintProofClaim _ = error "impossible"

    pprintProofThen :: Proof -> String
    pprintProofThen (ProofThen proofs) =
      joinS "\n" (map (uncurry pprintThenBranch)
                      (zip ("then" : repeat "also") proofs))
    pprintProofThen _ = error "impossible"

    pprintThenBranch :: String -> Proof -> String
    pprintThenBranch branch proof = possiblyShort (pprintProofAtom proof)

    pprintProofHave :: Proof -> String
    pprintProofHave (ProofHave hyps proofConj proofBody) =
        "have " ++ pprintHypotheses hyps
      ++ " by" ++ possiblyShort (pprintProofAtom proofConj)
      ++ ",\n"
      ++ indent (pprintProof proofBody)
    pprintProofHave _ = error "impossible"

    pprintProofCases :: Proof -> String
    pprintProofCases (ProofCases proofGuard branches) =
         "cases"
      ++ possiblyShort (pprintProofAtom proofGuard)
      ++ "\n"
      ++ joinS "\n" (map pprintProofCaseBranch branches)
    pprintProofCases _ = error "impossible"

    pprintProofCaseBranch :: CaseBranch -> String
    pprintProofCaseBranch (CaseBranch hypId mForm proof) =
          "case " ++ pprintHypId hypId
       ++ (case mForm of
             Nothing   -> ""
             Just form -> " : " ++ pprintForm form)
       ++ ","
       ++ possiblyShort (pprintProofAtom proof)

    pprintProofTake :: Proof -> String
    pprintProofTake (ProofTake nat proof) =
         "take " ++ pprintNat nat ++ ",\n"
      ++ pprintProof proof
    pprintProofTake _ = error "impossible"

    pprintProofConsider :: Proof -> String
    pprintProofConsider (ProofConsider natParams hypVarId mForm proof1 proof2) =
         "consider " ++ pprintNatVarParameterList natParams
      ++ " st " ++ pprintHypothesis hypVarId mForm
      ++ " by" ++ possiblyShort (pprintProofAtom proof1) ++ ",\n"
      ++ pprintProof proof2
    pprintProofConsider _ = error "impossible"

    pprintProofApply :: Proof -> String
    pprintProofApply proof =
      let (fun, args) = splitApply proof
       in joinApplication ([pprintProofAtom fun] ++ map pprintArg args)

    joinApplication :: [String] -> String
    joinApplication (sfun : sargs) =
      let singleLine = joinS " " (sfun : sargs) 
       in if null sargs ||
             (all (notElem '\n') (sfun : sargs) && length singleLine < maxLineWidth)
           then singleLine
           else joinS "\n" (sfun : map indent sargs)
    joinApplication [] = error "impossible"
    pprintArg :: Either Proof Nat -> String
    pprintArg (Left proof@(ProofIndeed _)) = "(" ++ pprintProofAtom proof ++ ")"
    pprintArg (Left proof) = pprintProofAtom proof
    pprintArg (Right nat)  = "[" ++ pprintNat nat ++ "]"
    splitApply :: Proof -> (Proof, [Either Proof Nat])
    splitApply (ProofApplyProof proof1 proof2) =
      let (fun, args) = splitApply proof1 in
        (fun, args ++ [Left proof2])
    splitApply (ProofApplyNat proof nat) =
      let (fun, args) = splitApply proof in
        (fun, args ++ [Right nat])
    splitApply proof = (proof, [])

pprintHypotheses :: [(HypId, Maybe Form)] -> String
pprintHypotheses hyps = joinS " " (map (uncurry pprintHypothesis) hyps)

pprintHypothesis :: HypId -> Maybe Form -> String
pprintHypothesis hypId Nothing     = pprintHypId hypId
pprintHypothesis hypId (Just form) = pprintHypId hypId ++ " : " ++ pprintFormAtom form

pprintProofAtom :: Proof -> String
pprintProofAtom (ProofHole holeId) = "?" ++ pprintHoleId holeId
pprintProofAtom (ProofAx hypId) = pprintHypId hypId
pprintProofAtom (ProofIndeed (EqRefl nat)) =
  "indeed " ++ pprintNat nat
pprintProofAtom (ProofIndeed (EqTrans nat1 Nothing (EqRefl nat2))) =
  "indeed " ++ pprintNat nat1 ++ " = " ++ pprintNat nat2
pprintProofAtom (ProofIndeed eqChain) =
  "indeed\n" ++ indent (pprintEqChain eqChain)
pprintProofAtom (ProofInduction base step) =
    "induction"
  ++ possiblyShort (pprintProofAtomP base)
  ++ "\n"
  ++ indent (pprintProofAtom step)
pprintProofAtom (ProofContradiction proof) =
  "contradiction" ++ possiblyShort (pprintProofAtom proof)
pprintProofAtom proof = pprintProofAtomP proof

pprintProofAtomP :: Proof -> String
pprintProofAtomP proof@(ProofHole _) = pprintProofAtom proof
pprintProofAtomP proof@(ProofAx _)   = pprintProofAtom proof
pprintProofAtomP proof               = "(" ++ pprintProof proof ++ ")"

pprintEqChain :: EqChain -> String
pprintEqChain eqChain =
    let (nat, lines) = rec eqChain in
      joinS "\n" ((space ++ pprintNat nat) : lines)
  where
    rec :: EqChain -> (Nat, [String])
    rec (EqRefl nat) = (nat, [])
    rec (EqTrans nat1 Nothing eqChain) =
      let (nat2, lines) = rec eqChain in
        (nat1,
            (equal ++ pprintNat nat2)
          : lines)
    rec (EqTrans nat1 (Just proof) eqChain) =
      let (nat2, lines) = rec eqChain in
        (nat1,
            (equal ++ pprintNat nat2)
          : ("  by " ++ pprintProofAtomP proof)
          : lines)
    equal = "= "
    space = "  "

