
module Elab.UniverseSolver(
         Atom(..), Constraint(..), universeConstraintsSatisfiable
       ) where

import Data.List(nub, (\\))
import qualified Data.Map as M
import qualified Data.Set as S

import Core.Term(UMetavarId)

-- This implements the algorithm proposed in
-- [1]  M. Bezem, T. Coquand. Loop-checking and the uniform word problem
--      for join-semilattices with an inflationary endomorphism.

data Delta = Fin Integer
           | Inf
  deriving Eq

instance Ord Delta where
  Fin n <= Fin m = n <= m
  Fin _ <= Inf   = True
  Inf   <= Fin _ = False
  Inf   <= Inf   = True

addDelta :: Delta -> Delta -> Delta
addDelta (Fin n) (Fin m) = Fin (n + m)
addDelta _       _       = Inf

maxDelta :: Delta -> Integer -> Delta
maxDelta (Fin n) m = Fin (max n m)
maxDelta Inf     _ = Inf

isFin :: Delta -> Bool
isFin (Fin _) = True
isFin _       = False

finVal :: Delta -> Integer
finVal (Fin n) = n
finVal _       = error "(Impossible)"

isInf :: Delta -> Bool
isInf Inf = True
isInf _   = False

data Atom = Atom UMetavarId Integer
  deriving Show

data Constraint = Constraint Atom [Atom]
  deriving Show

constraintVars :: Constraint -> [UMetavarId]
constraintVars (Constraint a bs) = nub [x | Atom x _ <- (a : bs)]

type Assignment = M.Map UMetavarId Delta

app :: Assignment -> UMetavarId -> Delta
app f x = M.findWithDefault (error "(Impossible)") x f

funcAssignment :: [UMetavarId] -> (UMetavarId -> Delta) -> Assignment
funcAssignment vars f = M.fromList [(x, f x) | x <- vars]

universeConstraintsSatisfiable :: [Constraint] -> Maybe (M.Map UMetavarId Integer)
universeConstraintsSatisfiable constraints =
    case leastExtension (varsV, constraints) f of
      Nothing -> Nothing
      Just g  -> if all (/= Inf) [app g x | x <- varsV]
                  then Just (normalize g)
                  else Nothing
  where
    atoms = [a | Constraint a _ <- constraints]
         ++ [a | Constraint _ body <- constraints, a <- body]
    varsV = nub [x | Atom x _ <- atoms]
    m = maximum [k | Constraint _ body <- constraints
                   , Atom _ k <- body] 
    f = M.fromList [(x, Fin m) | x <- varsV]
    normalize :: M.Map UMetavarId Delta -> M.Map UMetavarId Integer
    normalize g =
      if null varsV
        then M.empty
        else let m = maximum (map (finVal . snd) (M.toList g))
              in M.map ((\ v -> m - v) . finVal) g

gain :: Constraint -> Integer
gain (Constraint (Atom _ l) body) = l - minimum [k | Atom _ k <- body]

type LatticePresentation = ([UMetavarId], [Constraint])

minimumOrInf :: [Integer] -> Delta
minimumOrInf [] = Inf
minimumOrInf ds = Fin (minimum ds)

-- Lemma 3.1 --

isModelOfLatticePresentation :: Assignment -> LatticePresentation -> Bool
isModelOfLatticePresentation f (varsV, constraints) =
  all (isModelOfConstraint f varsV) constraints

isModelOfAllConstraints :: Assignment -> [UMetavarId]
                        -> [Constraint] -> Either (UMetavarId, Delta) ()
isModelOfAllConstraints f varsV [] = Right ()
isModelOfAllConstraints f varsV (Constraint (Atom y l) body : cs) =
  let varsW = [x | Atom x _ <- body, isFin (app f x)]
      k0 = minimumOrInf [finVal (app f x) - k
                        | Atom x k <- body
                        , isFin (app f x)
                        ]
   in if null varsW
       then
         (if isInf (app f y)
           then isModelOfAllConstraints f varsV cs
           else Left (y, Inf))
       else
         (if k0 < Fin 0 || addDelta (Fin l) k0 <= app f y
           then isModelOfAllConstraints f varsV cs
           else Left (y, maxDelta (app f y) (l + finVal k0)))

isModelOfConstraint :: Assignment -> [UMetavarId] -> Constraint -> Bool
isModelOfConstraint f varsV constraint =
  case isModelOfAllConstraints f varsV [constraint] of
    Left _   -> False
    Right () -> True

-- Lemma 3.3 --

extensionStep :: LatticePresentation
              -> [UMetavarId]
              -> ([UMetavarId] -> Assignment -> Maybe Assignment)
              -> Assignment -> Maybe Assignment
extensionStep (varsV, constraints) varsW induction f = do
    let maxgain = maximum [gain c | c <- constraints]
    let fW = M.fromList [(x, k) | (x, k) <- M.toList f
                                , x `elem` varsW]
    gf <- induction varsW fW
    let problemConstraints =
            [ Constraint a body
            | Constraint a@(Atom w _) body <- constraints
            , w `elem` varsW
            , (bodyX, _) <- [splitAtoms body]
            , not (null bodyX)]
    let mgf = sum [
                max 0
                    (maxgain - finVal (app gf w)
                     + maximum [finVal (app f v) | v <- varsV \\ varsW])
              | w <- varsW
              ]
    let h = funcAssignment varsV
              (\ x -> if x `elem` varsW
                       then app gf x
                       else app f x)
    if mgf == 0
     then Just h
     else case isModelOfAllConstraints h varsV problemConstraints of
            Right () -> Just h
            Left (w, wValue) ->
              let f' = funcAssignment varsV
                         (\ x -> if x `elem` varsW
                                  then if x == w
                                        then wValue
                                        else app gf x
                                  else app f x)
               in extensionStep (varsV, constraints) varsW induction f'
  where
    splitAtoms :: [Atom] -> ([Atom], [Atom])
    splitAtoms [] = ([], [])
    splitAtoms (a@(Atom v _) : atoms) =
      let (anW, aW) = splitAtoms atoms
       in if v `elem` varsW
           then (anW, a : aW)
           else (a : anW, aW)

-- Theorem 3.2 --

leastExtension :: LatticePresentation -> Assignment -> Maybe Assignment
leastExtension ([], _) f = Just f -- Base case
leastExtension (varsV, constraints0) f =
  let maxgain = maximum [gain c | c <- constraints]
      infVars = [x | x <- varsV, app f x == Inf] in
   case () of
     _ | not (null infVars)
         -> Nothing
     _ | isModelOfLatticePresentation f (varsV, constraints)
         -> Just f -- We are done
     _ | length varsWithNewAtoms == length varsV
       -> Nothing
     _ -> extensionStep (varsV, constraints) varsW
                        (\ varsZ h -> leastExtension (varsZ, constraints) h)
                        extendedAssignment
  where
    constraints :: [Constraint]
    constraints = [c | c <- constraints0
                     , S.fromList (constraintVars c) `S.isSubsetOf`
                       S.fromList varsV]
    varsWithNewAtoms :: [(UMetavarId, Atom)]
    varsWithNewAtoms = [(x, newAtom)
                       | x <- varsV
                       , c <- constraints
                       , isConstraintFor x c
                       , newAtom <- newAtomP c 
                       ]
    varsW :: [UMetavarId]
    varsW = nub (map fst varsWithNewAtoms)
    isConstraintFor :: UMetavarId -> Constraint -> Bool
    isConstraintFor x (Constraint (Atom y _) _) = x == y
    newAtomP :: Constraint -> [Atom]
    newAtomP (Constraint (Atom x l) body) =
      let k0 = minimum [
                finVal (app f z) - k
              | Atom z k <- body
              , isFin (app f z)
              ]
          newAtom = Atom x (l + k0)
          yieldsNewAtom = k0 >= 0 && Fin (l + k0) > app f x
       in if yieldsNewAtom
           then [newAtom]
           else []
    extendedAssignment :: Assignment
    extendedAssignment =
      funcAssignment varsV
        (\ x -> maximum ([app f x]
             ++ [Fin k' | (y, Atom _ k') <- varsWithNewAtoms, x == y ]))

