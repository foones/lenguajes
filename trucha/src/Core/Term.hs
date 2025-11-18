module Core.Term(
         ConstId(..), LocalId(..), MetavarId(..), UVarId(..), UMetavarId(..),
         Universe(..), Term(..), TPiInfo(..), TPiParam(..), Context(..),
         tAppMany,
         isWeakHeadReducible, weakHeadReduce, whnf, substitute, splitTermArgs,
         weakHeadUnfoldM, weakUnfoldM,
         freeMetavars, freeLocals,
         freeConsts, isTLocal, localIdFromTLocal, simplifyUniverse, itUSucc,
         normalize
       ) where

import qualified Data.Set  as S
import qualified Data.Map  as M
import qualified Data.List as L
import Data.Maybe(isNothing)

import Utils(joinS)
import Syntax.Name(QName, makeInternalQName)

displayUniverses :: Bool
displayUniverses = False

ifDisplayUniverses :: String -> String
ifDisplayUniverses str = if displayUniverses
                          then str
                          else ""

data ConstId = ConstId QName
  deriving (Eq, Ord)

data LocalId = LocalId QName
  deriving (Eq, Ord)

data MetavarId = MetavarId Integer
  deriving (Eq, Ord)

data UVarId = UVarId String
  deriving (Eq, Ord)

data UMetavarId = UMetavarId Integer
  deriving (Eq, Ord)

data Universe = USucc Universe
              | UMax Universe Universe
              | UVar UVarId
              | UMetavar UMetavarId
  deriving (Eq, Ord)

data Term = TConst ConstId [Universe]
          | TLocal LocalId
          | TMetavar MetavarId
          | TLam LocalId Term Term
          | TApp Term Term
          | TType Universe
          | TPi TPiInfo LocalId Term Term

isTLocal :: Term -> Bool
isTLocal (TLocal _) = True
isTLocal _          = False

localIdFromTLocal :: Term -> LocalId
localIdFromTLocal (TLocal l) = l
localIdFromTLocal _          = error "(Not a TLocal term)"

data TPiParam = TPiParam TPiInfo LocalId Term

instance Show TPiParam where
  show (TPiParam TPiExplicit x t) = "(" ++ show x ++ " : " ++ show t ++ ")"
  show (TPiParam TPiImplicit x t) = "{" ++ show x ++ " : " ++ show t ++ "}"

tAppMany :: Term -> [Term] -> Term
tAppMany fun args = foldl TApp fun args

data TPiInfo = TPiExplicit
             | TPiImplicit
  deriving (Eq, Show)

data Context = CEmpty
             | CExtendConst Context ConstId [UVarId] Term
             | CExtendEquation Context [LocalId] Term Term

contextEquations :: Context -> [([LocalId], Term, Term)]
contextEquations CEmpty = []
contextEquations (CExtendConst ctx _ _ _) = contextEquations ctx
contextEquations (CExtendEquation ctx bound lhs rhs) =
  (bound, lhs, rhs) : contextEquations ctx

---

instance Show ConstId where
  show (ConstId x) = show x

instance Show LocalId where
  show (LocalId x) = show x

instance Show MetavarId where
  show (MetavarId x) = "?m" ++ show x

instance Show UVarId where
  show (UVarId x) = "u." ++ x

instance Show UMetavarId where
  show (UMetavarId x) = "u?" ++ show x

instance Show Universe where
  show (USucc u)        = "usuc " ++ showUniverseParen u
  show (UMax u1 u2)     = "umax " ++ showUniverseParen u1
                                  ++ " "
                                  ++ showUniverseParen u2
  show (UVar uvar)      = show uvar
  show (UMetavar umvar) = show umvar

showUniverseParen :: Universe -> String
showUniverseParen u@(UVar _)     = show u
showUniverseParen u@(UMetavar _) = show u
showUniverseParen u              = "(" ++ show u ++ ")"

instance Show Term where
  show (TConst c univs) = show c
                       ++ ifDisplayUniverses (".{" ++ joinS ";" (map show univs) ++ "}")
  show (TLocal x)       = show x
  show (TMetavar m)     = show m
  show (TLam x typ body) =
    "λ (" ++ show x ++ " : " ++ show typ ++ ") → " ++ show body
  show t@(TApp _ _) = showApp t
  show (TType u) = "Type" ++ ifDisplayUniverses (" " ++ showUniverseParen u)
  show (TPi tPiInfo x typ1 typ2) =
       "∀ " ++ parenthesizeWithTPiInfo tPiInfo (show x ++ " : " ++ show typ1)
    ++ " → " ++ show typ2

parenthesizeWithTPiInfo :: TPiInfo -> String -> String
parenthesizeWithTPiInfo TPiExplicit x = "(" ++ x ++ ")"
parenthesizeWithTPiInfo TPiImplicit x = "{" ++ x ++ "}"

showApp :: Term -> String
showApp t =
    let (fun, args) = splitTermArgs t
     in joinS " " (map showTermParen (fun : args))

splitTermArgs :: Term -> (Term, [Term])
splitTermArgs (TApp fun arg) =
  let (fun', args) = splitTermArgs fun
   in (fun', args ++ [arg])
splitTermArgs t = (t, [])

showTermParen :: Term -> String
showTermParen t@(TConst _ _) = show t
showTermParen t@(TLocal _)   = show t
showTermParen t@(TMetavar _) = show t
showTermParen t@(TType _)
  | not displayUniverses     = show t
showTermParen t              = "(" ++ show t ++ ")"

instance Show Context where
  show CEmpty = ""
  show (CExtendConst ctx c uvars typ) =
       show ctx
    ++ show c ++ ifDisplayUniverses (".{" ++ joinS ";" (map show uvars) ++ "}")
    ++ " : " ++ show typ ++ "\n"
  show (CExtendEquation ctx lvars lhs rhs) =
       show ctx
    ++ show lhs ++ " = " ++ show rhs ++ "\n"

type Renaming = M.Map LocalId LocalId

freshLocalIdWrt :: LocalId -> S.Set LocalId -> LocalId
freshLocalIdWrt prefix forbidden =
  head [local | n <- [1..]
              , local <- (prefix : [LocalId (makeInternalQName (show prefix) n)])
              , not (local `S.member` forbidden)]

freeMetavars :: Term -> S.Set MetavarId
freeMetavars (TConst _ _)    = S.empty
freeMetavars (TLocal _)      = S.empty
freeMetavars (TMetavar m)    = S.singleton m
freeMetavars (TLam l t1 t2)  = freeMetavars t1 `S.union` freeMetavars t2
freeMetavars (TApp t1 t2)    = freeMetavars t1 `S.union` freeMetavars t2
freeMetavars (TType _)       = S.empty
freeMetavars (TPi _ l t1 t2) = freeMetavars t1 `S.union` freeMetavars t2

freeLocals :: Term -> S.Set LocalId
freeLocals (TConst _ _)    = S.empty
freeLocals (TLocal x)      = S.singleton x
freeLocals (TMetavar _)    = S.empty
freeLocals (TLam l t1 t2)  = freeLocals t1
                             `S.union` (freeLocals t2 S.\\ S.singleton l)
freeLocals (TApp t1 t2)    = freeLocals t1 `S.union` freeLocals t2
freeLocals (TType _)       = S.empty
freeLocals (TPi _ l t1 t2) = freeLocals t1
                             `S.union` (freeLocals t2 S.\\ S.singleton l)

freeConsts :: Term -> S.Set ConstId
freeConsts (TConst c _)    = S.singleton c
freeConsts (TLocal x)      = S.empty
freeConsts (TMetavar _)    = S.empty
freeConsts (TLam l t1 t2)  = freeConsts t1 `S.union` freeConsts t2
freeConsts (TApp t1 t2)    = freeConsts t1 `S.union` freeConsts t2
freeConsts (TType _)       = S.empty
freeConsts (TPi _ l t1 t2) = freeConsts t1 `S.union` freeConsts t2

alphaEqUnder :: S.Set LocalId -> Renaming -> Renaming -> Term -> Term -> Bool 
alphaEqUnder forbidden r1 r2 (TConst c1 _) (TConst c2 _) =
  c1 == c2
alphaEqUnder forbidden r1 r2 (TLocal x1) (TLocal x2) =
  M.findWithDefault x1 x1 r1 == M.findWithDefault x2 x2 r2
alphaEqUnder forbidden r1 r2 (TMetavar m1) (TMetavar m2) = m1 == m2
alphaEqUnder forbidden r1 r2 (TLam l1 t11 t12) (TLam l2 t21 t22) =
     alphaEqUnder forbidden r1 r2 t11 t21
  && alphaEqUnderBinder forbidden r1 r2 l1 l2 t12 t22
alphaEqUnder forbidden r1 r2 (TApp t11 t12) (TApp t21 t22) =
     alphaEqUnder forbidden r1 r2 t11 t21
  && alphaEqUnder forbidden r1 r2 t12 t22
alphaEqUnder forbidden r1 r2 (TType univ1) (TType univ2) =
  univ1 == univ2
alphaEqUnder forbidden r1 r2 (TPi info1 l1 t11 t12) (TPi info2 l2 t21 t22) =
     info1 == info2
  && alphaEqUnder forbidden r1 r2 t11 t21
  && alphaEqUnderBinder forbidden r1 r2 l1 l2 t12 t22
alphaEqUnder _ _ _ _ _ = False

alphaEqUnderBinder :: S.Set LocalId -> Renaming -> Renaming
                   -> LocalId -> LocalId -> Term -> Term -> Bool 
alphaEqUnderBinder forbidden r1 r2 l1 l2 t1 t2 =
  let forbidden' = S.fromList [l1, l2]
                   `S.union` S.unions (map freeLocals [t1, t2])
                   `S.union` forbidden
      lf = freshLocalIdWrt l1 forbidden'
   in alphaEqUnder (S.insert lf forbidden')
                   (M.insert l1 lf r1) (M.insert l2 lf r2)
                   t1 t2

instance Eq Term where
  (==) = alphaEqUnder S.empty M.empty M.empty

type Substitution = M.Map LocalId Term

substitute :: Term -> LocalId -> Term -> Term
substitute t x s =
  let forbidden = S.insert x (freeLocals s `S.union` freeLocals t)
   in substituteUnder forbidden (M.fromList [(x, s)]) t

substituteDict :: Substitution -> Term -> Term
substituteDict subst term =
  let forbidden = freeLocals term
                  `S.union` S.fromList (M.keys subst)
                  `S.union` S.unions (map (freeLocals . snd) (M.toList subst))
   in substituteUnder forbidden subst term

substituteUnder :: S.Set LocalId -> Substitution -> Term -> Term
substituteUnder forbidden subst (TConst c us)  = TConst c us
substituteUnder forbidden subst (TLocal x)     = M.findWithDefault (TLocal x) x subst
substituteUnder forbidden subst (TMetavar m)   = TMetavar m
substituteUnder forbidden subst (TLam x t1 t2) =
  let z = freshLocalIdWrt x forbidden in
    TLam z (substituteUnder forbidden subst t1)
           (substituteUnder (S.insert z forbidden)
                            (M.insert x (TLocal z) subst)
                            t2)
substituteUnder forbidden subst (TApp t1 t2) =
  TApp (substituteUnder forbidden subst t1)
       (substituteUnder forbidden subst t2)
substituteUnder forbidden subst (TType univ) = TType univ
substituteUnder forbidden subst (TPi info x t1 t2) =
  let z = freshLocalIdWrt x forbidden in
    TPi info z (substituteUnder forbidden subst t1)
               (substituteUnder (S.insert z forbidden)
                                (M.insert x (TLocal z) subst)
                                t2)

weakHeadReduceM :: Term -> Maybe Term
weakHeadReduceM (TConst _ _)   = Nothing
weakHeadReduceM (TLocal _)     = Nothing
weakHeadReduceM (TMetavar _)   = Nothing
weakHeadReduceM (TLam _ _ _)   = Nothing
weakHeadReduceM (TApp (TLam lx typ body) arg)
                               = Just (substitute body lx arg)
weakHeadReduceM (TApp fun arg) = case weakHeadReduceM fun of
                                   Just fun' -> Just $ TApp fun' arg
                                   Nothing   -> Nothing
weakHeadReduceM (TType _)      = Nothing
weakHeadReduceM (TPi _ _ _ _)  = Nothing
-- TODO: contemplar también principios de inducción

isWeakHeadReducible :: Term -> Bool
isWeakHeadReducible t = case weakHeadReduceM t of
                          Just _  -> True
                          Nothing -> False

weakHeadReduce :: Term -> Term
weakHeadReduce t = case weakHeadReduceM t of
                     Just t' -> t'
                     Nothing -> error "Term is not weak head reducible"

whnf :: Term -> Term
whnf t = case weakHeadReduceM t of
           Just t' -> whnf t'
           Nothing -> t

simplifyUniverse :: Universe -> Universe
simplifyUniverse u = unrep (rep u)
  where
    rep :: Universe -> M.Map (Either UVarId UMetavarId) Integer
    rep (USucc u)      = M.fromList [(e, k + 1) | (e, k) <- M.toList (rep u)]
    rep (UVar uv)      = M.fromList [(Left uv, 0)]
    rep (UMetavar umv) = M.fromList [(Right umv, 0)]
    rep (UMax u1 u2)   = M.unionWith max (rep u1) (rep u2)
    unrep :: M.Map (Either UVarId UMetavarId) Integer -> Universe
    unrep m =
      let ((e0, k0) : opts) = M.toList m
       in foldr (\ (e, k) r -> UMax (itUSucc k (unrepHead e)) r)
                (itUSucc k0 (unrepHead e0))
                opts
    unrepHead :: Either UVarId UMetavarId -> Universe
    unrepHead (Left uv)  = UVar uv
    unrepHead (Right um) = UMetavar um

itUSucc :: Integer -> Universe -> Universe
itUSucc 0 u = u
itUSucc n u = USucc (itUSucc (n - 1) u)

----

tryMatch :: [LocalId] -> Term -> Term -> Either String (M.Map LocalId Term)
tryMatch bound pattern term
  | isWeakHeadReducible term = tryMatch bound pattern (weakHeadReduce term)
tryMatch bound (TLocal x) term
  | x `elem` bound = return $ M.fromList [(x, term)]
  | otherwise      = Left ("Variable " ++ show x ++ " is not bound by the pattern.")
-- TODO: if we find a metavariable, it may be because the elaborator
-- has introduced one in place of an implicit argument
tryMatch bound pattern term =
    let (pHead, pArgs) = splitTermArgs pattern
        (tHead, tArgs) = splitTermArgs term
     in case (pHead, tHead) of
          (TConst pc _, TConst tc _)
            | pc == tc && length pArgs == length tArgs -> 
              tryMatchList bound pArgs tArgs
          _ -> Left ("Head of pattern cannot be of the form: " ++ show pHead)
  where
    tryMatchList :: [LocalId] -> [Term] -> [Term] -> Either String (M.Map LocalId Term)
    tryMatchList bound []       [] = return M.empty
    tryMatchList bound (p : ps) (t : ts) = do
      m1 <- tryMatch bound p t
      m2 <- tryMatchList bound ps ts
      -- We allow non-linear patterns to allow inaccessible patterns.
      return $ M.union m1 m2
      {-
      -- Old version that disallows non-linear patterns:
      if S.null (M.keysSet m1 `S.intersection` M.keysSet m2)
       then return $ M.union m1 m2
       else Left ("Non-linear pattern: " ++ show (p : ps))
      -}
    tryMatchList bound _ _ = error "(Impossible)"

weakHeadUnfoldM :: Context -> Term -> Maybe Term
weakHeadUnfoldM _   t | isWeakHeadReducible t = Just $ weakHeadReduce t
weakHeadUnfoldM _   (TLocal _)    = Nothing
weakHeadUnfoldM _   (TMetavar _)  = Nothing
weakHeadUnfoldM _   (TLam _ _ _)  = Nothing
weakHeadUnfoldM _   (TType _)     = Nothing
weakHeadUnfoldM _   (TPi _ _ _ _) = Nothing
weakHeadUnfoldM ctx term =
  let matches = [(tryMatch bound lhs term, rhs)
                | (bound, lhs, rhs) <- contextEquations ctx]
      successfulMatches = filter (\ (m, _) -> isRight m) matches
   in if null successfulMatches
       then case term of
              TApp term1 term2 -> do
                term1' <- weakHeadUnfoldM ctx term1
                return $ TApp term1' term2
              _ -> Nothing
       else
         let (Right subst, rhs) = head successfulMatches
          in Just $ substituteDict subst rhs
  where
    isRight (Right _) = True
    isRight (Left _)  = False

weakUnfoldM :: Context -> Term -> Maybe Term
weakUnfoldM ctx t =
  case weakHeadUnfoldM ctx t of
    Just t' -> Just t'
    Nothing ->
      let (head, ts) = splitTermArgs t
          mts = map (weakUnfoldM ctx) ts
       in if all isNothing mts
           then Nothing
           else let ts' = map (\ (t, mt) -> maybe t id mt) (zip ts mts)
                 in Just $ tAppMany head ts'

fullUnfoldM :: Context -> Term -> Maybe Term
fullUnfoldM ctx t =
    case weakUnfoldM ctx t of
      Just t' -> Just t'
      Nothing -> rec t
  where
    rec (TLam x typ body)  =
      case fullUnfoldM ctx typ of
        Just typ' -> Just $ TLam x typ' body
        Nothing   ->
          case fullUnfoldM ctx body of
            Just body' -> Just $ TLam x typ body'
            Nothing    -> Nothing
    rec (TPi info x t1 t2) =
      case fullUnfoldM ctx t1 of
        Just t1' -> Just $ TPi info x t1' t2
        Nothing  ->
          case fullUnfoldM ctx t2 of
            Just t2' -> Just $ TPi info x t1 t2'
            Nothing  -> Nothing
    rec _ = Nothing

normalize :: Context -> Term -> Term
normalize ctx t =
  case fullUnfoldM ctx t of
    Just t' -> normalize ctx t'
    Nothing -> t

