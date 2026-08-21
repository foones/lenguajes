module Syntax(
         HoleId(..), HypId(..), NatVarId(..), FunId(..), PropId(..), FormVarId(..),
         NatVarKind(..),
         Program(..), Declaration(..), Nat(..), Form(..), Proof(..),
         CaseBranch(..), EqChain(..), PropParam(..), PropArg(..),
         formTrue, formFalse, formNot, proofLet, proofApply,
         formFreeNatVars, natFreeNatVars, testNumeral, isNumeral, isProofHole
       ) where

import qualified Data.Set as S

newtype HoleId = HoleId { holeIdName :: String }
  deriving (Eq, Ord, Show)

newtype HypId = HypId { hypIdName :: String }
  deriving (Eq, Ord, Show)

newtype NatVarId = NatVarId { natVarIdName :: String }
  deriving (Eq, Ord, Show)

newtype FunId = FunId { funIdName :: String }
  deriving (Eq, Ord, Show)

newtype PropId = PropId { propIdName :: String }
  deriving (Eq, Ord, Show)

newtype FormVarId = FormVarId { formVarIdName :: String }
  deriving (Eq, Ord, Show)

data NatVarKind = NatVarFin
                | NatVarSym
  deriving (Eq, Show)

data Program = Program [Declaration]
  deriving Show

data Declaration = DeclTheorem HypId Form Proof
                 | DeclFun FunId [NatVarId] Nat
                 | DeclProp PropId [PropParam] Form
                 | DeclEvalForm Form
                 | DeclEvalNat Nat
  deriving Show

data Nat = NatHole HoleId
         | NatVar NatVarId
         | NatZero
         | NatSucc Nat
         | NatAdd Nat Nat
         | NatMul Nat Nat
         | NatFun FunId [Nat]
  deriving (Eq, Show)

data Form = FormVar FormVarId
          | FormEq Nat Nat
          | FormImp Form Form
          | FormForall NatVarKind NatVarId Form
          | FormProp PropId [PropArg]
          -- Extras
          | FormOr Form Form
          | FormAnd Form Form
          | FormExists NatVarKind NatVarId Form
  deriving Show

data PropParam = PropParamNat NatVarId
               | PropParamForm FormVarId 
  deriving Show

data PropArg = PropArgNat Nat
             | PropArgForm Form
  deriving Show

formTrue :: Form
formTrue = FormProp (PropId "false") []

formFalse :: Form
formFalse = FormProp (PropId "false") []

formNot :: Form -> Form
formNot form = FormProp (PropId "not") [PropArgForm form]

data Proof =
    ProofHole HoleId
  | ProofAx HypId
  | ProofSuppose HypId (Maybe Form) Proof  -- implication introduction
  | ProofApplyProof Proof Proof            -- implication elimination
  | ProofLetSym NatVarId Proof             -- forall introduction (sym)
  | ProofLetFin NatVarId Proof             -- forall introduction (fin)
  | ProofApplyNat Proof Nat                -- forall elimination
  | ProofIndeed EqChain                    -- proof of an equality
  | ProofInduction Proof Proof             -- proof by induction
  | ProofContradiction Proof               -- proof by contradiction
  -- Extras
  | ProofShow Form Proof                                -- restate the goal
  | ProofClaim HypId Form Proof Proof                   -- cut / auxiliary claim
  | ProofThen [Proof]                                   -- conjunction introduction
  | ProofHave [(HypId, Maybe Form)] Proof Proof         -- conjunction elimination
  | ProofAssume [(HypId, Maybe Form)] Proof             -- disjunction introduction
  | ProofCases Proof [CaseBranch]                       -- disjunction elimination
  | ProofTake Nat Proof                                 -- exists introduction
  | ProofConsider [(NatVarKind, NatVarId)] HypId (Maybe Form) Proof Proof
                                                        -- exists elimination (sym)
  deriving Show

data CaseBranch = CaseBranch HypId (Maybe Form) Proof
  deriving Show

proofLet :: NatVarKind -> NatVarId -> Proof -> Proof
proofLet NatVarSym = ProofLetSym
proofLet NatVarFin = ProofLetFin

proofApply :: Proof -> Either Proof Nat -> Proof
proofApply proof (Left proof') = ProofApplyProof proof proof'
proofApply proof (Right nat)   = ProofApplyNat proof nat

data EqChain = EqRefl Nat
             | EqTrans Nat (Maybe Proof) EqChain
  deriving Show

formFreeNatVars :: Form -> S.Set NatVarId
formFreeNatVars (FormVar _) = S.empty
formFreeNatVars (FormEq nat1 nat2) =
  S.union (natFreeNatVars nat1) (natFreeNatVars nat2)
formFreeNatVars (FormOr form1 form2) =
  S.union (formFreeNatVars form1) (formFreeNatVars form2)
formFreeNatVars (FormAnd form1 form2) =
  S.union (formFreeNatVars form1) (formFreeNatVars form2)
formFreeNatVars (FormImp form1 form2) =
  S.union (formFreeNatVars form1) (formFreeNatVars form2)
formFreeNatVars (FormForall _ n form) =
  formFreeNatVars form S.\\ S.singleton n
formFreeNatVars (FormProp _ args) =
  S.unions (map propArgFreeNatVars args)
formFreeNatVars (FormExists _ n form) =
  formFreeNatVars form S.\\ S.singleton n

propArgFreeNatVars :: PropArg -> S.Set NatVarId
propArgFreeNatVars (PropArgNat nat)   = natFreeNatVars nat
propArgFreeNatVars (PropArgForm form) = formFreeNatVars form

natFreeNatVars :: Nat -> S.Set NatVarId
natFreeNatVars (NatHole _) = S.empty
natFreeNatVars (NatVar n) = S.singleton n
natFreeNatVars NatZero = S.empty
natFreeNatVars (NatSucc nat) = natFreeNatVars nat
natFreeNatVars (NatAdd nat1 nat2) =
  S.union (natFreeNatVars nat1) (natFreeNatVars nat2)
natFreeNatVars (NatMul nat1 nat2) =
  S.union (natFreeNatVars nat1) (natFreeNatVars nat2)
natFreeNatVars (NatFun _ args) =
  S.unions (map natFreeNatVars args)

testNumeral :: Nat -> Maybe Integer
testNumeral NatZero       = Just 0
testNumeral (NatSucc nat) =
  case testNumeral nat of
    Nothing -> Nothing
    Just k  -> Just (k + 1)
testNumeral _ = Nothing

isNumeral :: Nat -> Bool
isNumeral NatZero       = True
isNumeral (NatSucc nat) = isNumeral nat
isNumeral _             = False

isProofHole :: Proof -> Bool
isProofHole (ProofHole _) = True
isProofHole _ = False

