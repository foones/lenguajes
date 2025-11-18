module Expr(
         Id(..), idOf,
         HypId(..), TyconId(..), TyvarId(..), PredId(..), FunId(..), VarId(..),
         Declaration(..),
         Metavar(..), firstMetavar, nextMetavar,
         Type(..), Term(..), Form(..), fOr,
         ConstructorDecl(..), DefEquation(..), Proof(..), OptionalName(..),
         HoleName(..), OptionalForm(..),
         ThusType(..),
         Command(..), Justification(..),
         CaseBranch(..), IndBranch(..), IndPattern(..),
         Substitution,
         equalFormulae, matchFormulae,
         freeVars, freeEigenVars,
         freshVariableFor, substitute, substituteVar, unify
      ) where

import qualified Data.Set as S
import qualified Data.Map as M

import FailState(FailState, getFS, putFS, modifyFS, execFS, failFS)

import Utils(joinS, indent)

joinSpaces :: [String] -> String
joinSpaces xs = joinS " " (filter (not . null) xs)

----

newtype Id = Id String
  deriving (Eq, Ord)

newtype HypId   = HypId Id   -- names of hypothesis, axioms, theorems
  deriving (Eq, Ord)

newtype TyconId = TyconId Id -- names of type constructors
  deriving (Eq, Ord)

newtype TyvarId = TyvarId Id -- names of type variables
  deriving (Eq, Ord)

newtype PredId  = PredId Id  -- names of predicate symbols
  deriving (Eq, Ord)

newtype FunId   = FunId Id   -- names of function symbols
  deriving (Eq, Ord)

newtype VarId   = VarId Id   -- names of variables and eigenvariables
  deriving (Eq, Ord)

class HasId a where
  idOf :: a -> Id

instance HasId HypId where
  idOf (HypId x) = x

instance HasId TyconId where
  idOf (TyconId x) = x

instance HasId TyvarId where
  idOf (TyvarId x) = x

instance HasId PredId where
  idOf (PredId x) = x

instance HasId FunId where
  idOf (FunId x) = x

instance HasId VarId where
  idOf (VarId x) = x

----

varIdAppendSuffix :: VarId -> String -> VarId
varIdAppendSuffix (VarId (Id prefix)) suffix = VarId (Id (prefix ++ suffix))

showArgs :: Show a => [a] -> String
showArgs [] = ""
showArgs xs = "(" ++ joinS "," (map show xs) ++ ")"

data Declaration = DeclData Type [ConstructorDecl]
                 | DeclProp PredId [Type]
                 | DeclAxiom HypId Form
                 | DeclTheorem HypId Form Proof
                 | DeclDef FunId [Type] Type [DefEquation]

data ConstructorDecl = ConstructorDecl FunId [Type]
data DefEquation = DefEquation [Term] Term

newtype Metavar = Metavar Int
  deriving (Eq, Ord)

data Type = TyApp TyconId [Type]
          | TyVar TyvarId
          | TyUnknown
          | TyMetavar Metavar
          | TyProp
  deriving Eq

firstMetavar :: Metavar
firstMetavar = Metavar 0

nextMetavar :: Metavar -> Metavar
nextMetavar (Metavar m) = Metavar (m + 1)

data Term = TApp FunId [Term]
          | TEigenVar VarId
          | TVar VarId

data Form = FPred PredId [Term]
          | FTrue
          | FFalse
          | FImp Form Form
          | FOr Form Form
          | FAnd Form Form
          | FNot Form
          | FForall VarId Type Form
          | FExists VarId Type Form

fOr :: [Form] -> Form
fOr [] = FFalse
fOr fs = foldr1 FOr fs

data OptionalName = ONNone
                  | ONName HypId

type HoleName = String
data OptionalForm = OFNone
                  | OFHole HoleName
                  | OFForm Form

data Proof = ProofEmpty
           | ProofCons Command Proof

data ThusType = C_Thus | C_Admit | C_Contradiction

data Justification = JHypothesis HypId
                   | JPreviousHypothesis

data Command =
    CmdSuppose OptionalName OptionalForm
  | CmdThus ThusType OptionalName OptionalForm [Justification]
  | CmdHave OptionalName Form [Justification]
  | CmdLet [(VarId, Type)]
  | CmdCases OptionalForm [Justification] [CaseBranch]
  | CmdTake Term
  | CmdConsider [(VarId, Type)] OptionalName Form [Justification]
  | CmdClaim OptionalName Form Proof
  | CmdInduction Type [IndBranch]
  | CmdShow Form

data CaseBranch = CaseBranch OptionalName Form Proof
data IndBranch = IndBranch IndPattern Proof
data IndPattern = IndPattern FunId [(VarId, Type)]

----

class FreeVars a where
  freeVars      :: a -> S.Set VarId
  freeEigenVars :: a -> S.Set VarId

instance FreeVars Term where
  freeVars (TApp _ ts)   = S.unions (map freeVars ts)
  freeVars (TEigenVar _) = S.empty
  freeVars (TVar x)      = S.singleton x
  --
  freeEigenVars (TApp _ ts)   = S.unions (map freeEigenVars ts)
  freeEigenVars (TEigenVar x) = S.singleton x
  freeEigenVars (TVar _)      = S.empty

instance FreeVars Form where
  freeVars (FPred _ ts)    = S.unions (map freeVars ts)
  freeVars FTrue           = S.empty
  freeVars FFalse          = S.empty
  freeVars (FImp a b)      = freeVars a `S.union` freeVars b
  freeVars (FOr a b)       = freeVars a `S.union` freeVars b
  freeVars (FAnd a b)      = freeVars a `S.union` freeVars b
  freeVars (FNot a)        = freeVars a
  freeVars (FForall x _ a) = freeVars a S.\\ S.singleton x
  freeVars (FExists x _ a) = freeVars a S.\\ S.singleton x
  --
  freeEigenVars (FPred _ ts)    = S.unions (map freeEigenVars ts)
  freeEigenVars FTrue           = S.empty
  freeEigenVars FFalse          = S.empty
  freeEigenVars (FImp a b)      = freeEigenVars a `S.union` freeEigenVars b
  freeEigenVars (FOr a b)       = freeEigenVars a `S.union` freeEigenVars b
  freeEigenVars (FAnd a b)      = freeEigenVars a `S.union` freeEigenVars b
  freeEigenVars (FNot a)        = freeEigenVars a
  freeEigenVars (FForall x _ a) = freeEigenVars a
  freeEigenVars (FExists x _ a) = freeEigenVars a

instance FreeVars a => FreeVars [a] where
  freeVars xs      = S.unions (map freeVars xs)
  freeEigenVars xs = S.unions (map freeEigenVars xs)

type Renaming = M.Map VarId VarId

equalTermsUnder :: Renaming -> Renaming -> Term -> Term -> Bool
equalTermsUnder r1 r2 (TApp f1 ts1) (TApp f2 ts2) =
     f1 == f2
  && length ts1 == length ts2
  && all (uncurry (equalTermsUnder r1 r2)) (zip ts1 ts2)
equalTermsUnder _  _  (TEigenVar x1) (TEigenVar x2) = x1 == x2
equalTermsUnder r1 r2 (TVar x1) (TVar x2) =
  M.findWithDefault x1 x1 r1 == M.findWithDefault x2 x2 r2 
equalTermsUnder _ _ _ _ = False

similarFormulaeUnder :: (Type -> Type -> Bool)
                     -> Renaming -> Renaming -> Form -> Form -> Bool
similarFormulaeUnder cmpTypes r1 r2 (FPred p1 ts1) (FPred p2 ts2) =
     p1 == p2
  && length ts1 == length ts2
  && all (uncurry (equalTermsUnder r1 r2)) (zip ts1 ts2)
similarFormulaeUnder _ _ _ FTrue  FTrue  = True
similarFormulaeUnder _ _ _ FFalse FFalse = True
similarFormulaeUnder cmpTypes r1 r2 (FImp a1 b1) (FImp a2 b2) =
     similarFormulaeUnder cmpTypes r1 r2 a1 a2
  && similarFormulaeUnder cmpTypes r1 r2 b1 b2
similarFormulaeUnder cmpTypes r1 r2 (FOr a1 b1) (FOr a2 b2) =
     similarFormulaeUnder cmpTypes r1 r2 a1 a2
  && similarFormulaeUnder cmpTypes r1 r2 b1 b2
similarFormulaeUnder cmpTypes r1 r2 (FAnd a1 b1) (FAnd a2 b2) =
     similarFormulaeUnder cmpTypes r1 r2 a1 a2
  && similarFormulaeUnder cmpTypes r1 r2 b1 b2
similarFormulaeUnder cmpTypes r1 r2 (FNot a1) (FNot a2) =
  similarFormulaeUnder cmpTypes r1 r2 a1 a2
similarFormulaeUnder cmpTypes r1 r2 (FForall x1 ot1 a1) (FForall x2 ot2 a2) =
     cmpTypes ot1 ot2
  && similarFormulaeUnderBinder cmpTypes r1 r2 x1 x2 a1 a2
similarFormulaeUnder cmpTypes r1 r2 (FExists x1 ot1 a1) (FExists x2 ot2 a2) =
     cmpTypes ot1 ot2
  && similarFormulaeUnderBinder cmpTypes r1 r2 x1 x2 a1 a2
similarFormulaeUnder _ _ _ _ _ = False

similarFormulaeUnderBinder :: (Type -> Type -> Bool)
                           -> Renaming -> Renaming -> VarId -> VarId -> Form -> Form -> Bool
similarFormulaeUnderBinder cmpTypes r1 r2 x1 x2 a1 a2 =
  let forbidden = renamingVars r1
        `S.union` renamingVars r2
        `S.union` (freeVars a1 S.\\ S.singleton x1)
        `S.union` (freeVars a2 S.\\ S.singleton x2)
      z = freshVariableFor forbidden x1
   in similarFormulaeUnder cmpTypes (M.insert x1 z r1) (M.insert x2 z r2) a1 a2

renamingVars :: Renaming -> S.Set VarId
renamingVars r = S.fromList (map fst (M.toList r))
       `S.union` S.fromList (map snd (M.toList r))

freshVariableFor :: S.Set VarId -> VarId -> VarId
freshVariableFor forbidden prefix =
  head [z | suffix <- "" : map show [1..],
            z <- [varIdAppendSuffix prefix suffix],
            not (S.member z forbidden)]

equalFormulae :: Form -> Form -> Bool
equalFormulae = similarFormulaeUnder (==) M.empty M.empty

equalTerms :: Term -> Term -> Bool
equalTerms = equalTermsUnder M.empty M.empty

matchFormulae :: OptionalForm -> Form -> Bool
matchFormulae OFNone     _ = True
matchFormulae (OFHole _) _ = True
matchFormulae (OFForm a) b = equalFormulae a b

----

instance Show Id where
  show (Id name) = name

instance Show HypId where
  show (HypId x) = show x

instance Show TyconId where
  show (TyconId x) = show x

instance Show TyvarId where
  show (TyvarId x) = show x

instance Show PredId where
  show (PredId x) = show x

instance Show FunId where
  show (FunId x) = show x

instance Show VarId where
  show (VarId x) = show x

instance Show Declaration where
  show (DeclData typ cds) =
    "data " ++ show typ
    ++ (if null cds
         then ""
         else " = " ++ joinS " | " (map show cds))
    ++ "\n"
  show (DeclProp p typs) =
    "prop " ++ show p ++ showArgs typs ++ "\n"
  show (DeclAxiom name form) =
    "axiom " ++ show name ++ " : " ++ show form ++ "\n"
  show (DeclTheorem name form proof) =
       "theorem " ++ show name ++ " : " ++ show form ++ "\n"
    ++ "proof\n"
    ++ indent 2 (show proof)
    ++ "\nend\n"
  show (DeclDef name argTyps resTyp equations) =
    unlines (
       ["def " ++ show name ++ showArgs argTyps ++ " : " ++ show resTyp]
    ++ map (showEquationFor name) equations
    )

showEquationFor :: FunId -> DefEquation -> String
showEquationFor f (DefEquation lhsArgs rhs) =
  show f ++ showArgs lhsArgs ++ " = " ++ show rhs

instance Show ConstructorDecl where
  show (ConstructorDecl c typs) = show c ++ showArgs typs

instance Show Metavar where
  show (Metavar n) = show n

instance Show Type where
  show (TyApp c tys) = show c ++ showArgs tys
  show (TyVar a)     = show a
  show TyUnknown     = "??"
  show (TyMetavar m) = "?" ++ show m
  show TyProp        = "Prop"

instance Show Term where
  show (TApp f xs)   = show f ++ showArgs xs
  show (TEigenVar x) = "@" ++ show x
  show (TVar x)      = show x

instance Show Form where
  show (FPred p xs)    = show p ++ showArgs xs
  show FTrue           = "true"
  show FFalse          = "false"
  show f@(FImp a b)    = pShow Strict f a ++ " → " ++ pShow Lax f b
  show f@(FOr a b)     = pShow Lax f a ++ " ∨ " ++ pShow Lax f b
  show f@(FAnd a b)    = pShow Lax f a ++ " ∧ " ++ pShow Lax f b
  show f@(FNot a)      = "¬" ++ pShow Lax f a
  show f@(FForall _ _ _) = let (xs, typ, body) = rec f in
                             "∀ " ++ joinS " " (map show xs)
                                  ++ " : " ++ show typ
                                  ++ ", " ++ pShow Lax f body
    where
      rec (FForall x typ b) = let (xs, typ', b') = rec b in
                                if typ == typ'
                                 then (x : xs, typ, b')
                                 else ([x], typ, b)
      rec b                = ([], TyUnknown, b)
  show f@(FExists _ _ _) = let (xs, typ, body) = rec f in
                             "∃ " ++ joinS " " (map show xs)
                                  ++ " : " ++ show typ
                                  ++ ", " ++ pShow Lax f body
    where
      rec (FExists x typ b) = let (xs, typ', b') = rec b in
                                if typ == typ'
                                 then (x : xs, typ, b')
                                 else ([x], typ, b)
      rec b             = ([], TyUnknown, b)

data Mode = Lax | Strict

pShow :: Mode -> Form -> Form -> String
pShow mode parent form =
    if compare mode (level parent) (level form)
     then "(" ++ show form ++ ")"
     else show form
  where
    compare Lax    = (<)
    compare Strict = (<=)
    level :: Form -> Integer
    level (FPred _ _)     = 0
    level FTrue           = 0
    level FFalse          = 0
    level (FImp _ _)      = 3
    level (FOr _ _)       = 2
    level (FAnd _ _)      = 1
    level (FNot _)        = 0
    level (FForall _ _ _) = 4
    level (FExists _ _ _) = 4

instance Show Proof where
  show ProofEmpty            = ""
  show (ProofCons cmd proof) = show cmd ++ "\n" ++ show proof

instance Show OptionalForm where
  show OFNone        = "_"
  show (OFHole id)   = "?" ++ id
  show (OFForm form) = show form

showOptionalOrEmptyForm :: OptionalForm -> String
showOptionalOrEmptyForm OFNone = ""
showOptionalOrEmptyForm form   = show form

instance Show OptionalName where
  show ONNone        = ""
  show (ONName name) = show name ++ ":"

instance Show ThusType where
  show C_Thus = "thus"
  show C_Admit = "admit"
  show C_Contradiction = "contradiction"

instance Show Justification where
  show (JHypothesis hypId) = show hypId
  show JPreviousHypothesis = "_"

instance Show Command where
  show (CmdSuppose oname oform) =
    joinSpaces [
      "suppose"
    , show oname
    , show oform
    ]
  show (CmdThus thusType oname oform js) =
    joinSpaces [
      show thusType
    , show oname
    , showOptionalOrEmptyForm oform
    , byJustifications js
    ]
  show (CmdHave oname form js) =
    joinSpaces [
      "have"
    , show oname
    , show form
    , byJustifications js
    ]
  show (CmdLet typedNames) = joinSpaces ["let", showTypedNames typedNames]
  show (CmdCases oform js branches) =
    joinSpaces [
       "cases"
    , showOptionalOrEmptyForm oform
    , byJustifications js
    ] ++ "\n"
    ++ unlines (map show branches)
    ++ "end"
  show (CmdTake term) =
    joinSpaces [
      "take "
    , show term
    ]
  show (CmdConsider typedNames oname form js) =
    joinSpaces [
       "consider"
    , showTypedNames typedNames
    , "st"
    , show oname
    , show form
    , byJustifications js
    ]
  show (CmdClaim oname form proof) =
    joinSpaces [
       "claim"
    , show oname
    , show form
    ] ++ "\n"
    ++ indent 2 (show proof) ++ "\n"
    ++ "end"
  show (CmdInduction typ branches) =
    joinSpaces [
       "induction"
    , showOptionalType typ
    ] ++ "\n"
    ++ unlines (map show branches)
    ++ "end"
  show (CmdShow form) =
    joinSpaces [
      "show"
    , show form
    ]

showOptionalType :: Type -> String
showOptionalType TyUnknown = ""
showOptionalType ty        = show ty

showTypedNames :: [(VarId, Type)] -> String
showTypedNames [] = ""
showTypedNames typedNames | equalTypes =
    joinSpaces (map (show . fst) typedNames ++ [":", show firstType])
  where
    firstType  = snd (head typedNames)
    equalTypes = all (\ (_, ty) -> ty == firstType) typedNames
showTypedNames typedNames =
    joinS " " (map showTypedName typedNames)
  where
    showTypedName (x, TyUnknown) = show x
    showTypedName (x, ty)        = "(" ++ show x ++ " : " ++ show ty ++ ")"

instance Show CaseBranch where
  show (CaseBranch oName form proof) =
       "case " ++ show oName ++ show form ++ "\n"
    ++ indent 2 (show proof)

instance Show IndBranch where
  show (IndBranch pattern proof) =
       "case " ++ show pattern ++ "\n"
    ++ indent 2 (show proof)

instance Show IndPattern where
  show (IndPattern constructor args) = show constructor ++ showIds args
    where
      showIds [] = ""
      showIds typedIds = "(" ++ joinS ", " (map showTypedId typedIds) ++ ")"

showTypedId :: (VarId, Type) -> String
showTypedId (x, ty) = show x ++ " : " ++ show ty

byJustifications :: [Justification] -> String
byJustifications [] = ""
byJustifications js = "by " ++ joinS "," (map show js)

----

type Substitution = M.Map VarId Term

class Substitutable a where
  substitute :: Substitution -> a -> a

substituteVar :: Substitutable a => VarId -> Term -> a -> a
substituteVar x t e = substitute (M.fromList [(x, t)]) e

instance Substitutable Term where
  substitute sub (TApp f ts)   = TApp f (map (substitute sub) ts)
  substitute sub (TEigenVar x) = TEigenVar x
  substitute sub (TVar x)      = M.findWithDefault (TVar x) x sub

instance Substitutable Form where
  substitute sub (FPred p ts)       = FPred p (map (substitute sub) ts)
  substitute sub FTrue              = FTrue
  substitute sub FFalse             = FFalse
  substitute sub (FImp a b)         = FImp (substitute sub a) (substitute sub b)
  substitute sub (FOr a b)          = FOr (substitute sub a) (substitute sub b)
  substitute sub (FAnd a b)         = FAnd (substitute sub a) (substitute sub b)
  substitute sub (FNot a)           = FNot (substitute sub a)
  substitute sub (FForall x otyp a) = substituteBinder FForall x otyp sub a
  substitute sub (FExists x otyp a) = substituteBinder FExists x otyp sub a

instance Substitutable a => Substitutable [a] where
  substitute sub xs = map (substitute sub) xs

substitutionVars :: Substitution -> S.Set VarId
substitutionVars s = S.fromList (map fst (M.toList s))
           `S.union` S.unions (map (freeVars . snd) (M.toList s))

substituteBinder :: (VarId -> Type -> Form -> Form)
                 -> VarId -> Type -> Substitution -> Form -> Form
substituteBinder binder x otyp sub a =
  let forbidden = S.union (substitutionVars sub) (freeVars a S.\\ S.singleton x)
      z = freshVariableFor forbidden x
   in binder z otyp (substitute (M.insert x (TVar z) sub) a)

---- Type unifier

data TypeConstraint = TypeConstraint Type Type

---- Term and formula unifier

type UnifyM = FailState String UnifyState
data UnifyState =
  US {
    usSubstitution    :: M.Map VarId Term
  }

instantiateVar :: VarId -> Term -> UnifyM ()
instantiateVar x t = do
  modifyFS (\ state -> state {
             usSubstitution = M.insert x t (usSubstitution state)
           })

unify :: Form -> Form -> Maybe Substitution
unify a b =
  case execFS (unifyForms S.empty M.empty M.empty a b) initialState of
    Left  _     -> Nothing
    Right state -> Just (usSubstitution state)
  where
    initialState = US {
                     usSubstitution    = M.empty
                   }

unifyForms :: S.Set VarId -> Renaming -> Renaming -> Form -> Form -> UnifyM ()
unifyForms rigid r1 r2 (FPred p1 ts1) (FPred p2 ts2)
  | p1 == p2 && length ts1 == length ts2 =
    mapM_ (uncurry (unifyTerms rigid r1 r2)) (zip ts1 ts2)
unifyForms rigid r1 r2 FTrue  FTrue  = return ()
unifyForms rigid r1 r2 FFalse FFalse = return ()
unifyForms rigid r1 r2 (FImp a1 b1) (FImp a2 b2) = do
  unifyForms rigid r1 r2 a1 a2
  unifyForms rigid r1 r2 b1 b2
unifyForms rigid r1 r2 (FOr a1 b1) (FOr a2 b2) = do
  unifyForms rigid r1 r2 a1 a2
  unifyForms rigid r1 r2 b1 b2
unifyForms rigid r1 r2 (FAnd a1 b1) (FAnd a2 b2) = do
  unifyForms rigid r1 r2 a1 a2
  unifyForms rigid r1 r2 b1 b2
unifyForms rigid r1 r2 (FNot a1) (FNot a2) =
  unifyForms rigid r1 r2 a1 a2
unifyForms rigid r1 r2 (FForall x1 otyp1 a1) (FForall x2 otyp2 a2)
  | otyp1 == otyp2 = unifyBinder rigid r1 r2 x1 x2 a1 a2
unifyForms rigid r1 r2 (FExists x1 otyp1 a1) (FExists x2 otyp2 a2)
  | otyp1 == otyp2 = unifyBinder rigid r1 r2 x1 x2 a1 a2
unifyForms rigid r1 r2 _ _ = failFS "Formulae do not unify."

unifyBinder :: S.Set VarId -> Renaming -> Renaming
            -> VarId -> VarId -> Form -> Form -> UnifyM ()
unifyBinder rigid r1 r2 x1 x2 a1 a2 = do
  sub <- usSubstitution <$> getFS
  let forbidden = substitutionVars sub
        `S.union` rigid
        `S.union` renamingVars r1
        `S.union` renamingVars r2
        `S.union` (freeVars a1 S.\\ S.singleton x1)
        `S.union` (freeVars a2 S.\\ S.singleton x2)
      z = freshVariableFor forbidden x1
   in unifyForms (S.insert z rigid) (M.insert x1 z r1) (M.insert x2 z r2) a1 a2

unifyTerms :: S.Set VarId -> Renaming -> Renaming -> Term -> Term -> UnifyM ()
unifyTerms rigid r1 r2 t1_0 t2_0 = rec t1_0 t2_0
  where
    rec t1 t2 = do
      t1' <- unfold r1 t1
      t2' <- unfold r2 t2
      case (t1', t2') of
        (TVar x, TVar y) | x == y -> return ()
        (TVar x, _) | S.notMember x (rigid `S.union` freeVars t2') ->
          instantiateVar x t2'
        (_, TVar x) | S.notMember x (rigid `S.union` freeVars t1') ->
          instantiateVar x t1'
        (TEigenVar x1, TEigenVar x2) | x1 == x2 -> return ()
        (TApp f1 ts1, TApp f2 ts2) | f1 == f2 && length ts1 == length ts2 ->
          mapM_ (uncurry rec) (zip ts1 ts2)
        _ -> failFS "Terms do not unify."
    unfold :: Renaming -> Term -> UnifyM Term
    unfold renaming (TApp f ts) =
      TApp f <$> mapM (unfold renaming) ts
    unfold renaming (TEigenVar x) =
      return $ TEigenVar x
    unfold renaming (TVar x) =
      case M.lookup x renaming of
        Just y -> return (TVar y)
        Nothing -> do
          sub <- usSubstitution <$> getFS
          case M.lookup x sub of
            Just t' -> unfold renaming t'
            Nothing -> return (TVar x)

