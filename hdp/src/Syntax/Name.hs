
module Syntax.Name(
         splitParts, allNameParts, nameArity,
         isWellFormedName, isWellFormedOperatorName,
         QName(..), readName, qualify, moduleNameFromQName, unqualifiedName,
         suffixQName,
         modulePRIM, moduleMain, arrowSymbol, colonSymbol,
         primitiveMain,
         primitiveType,
         primitiveForall, primitiveColon,
         symbolTensor, primitiveTensor,
         symbolWith, primitiveWith,
         symbolLinSum, primitiveLinSum,
         symbolBang, primitiveBang,
         primitiveArrow, primitiveUnit,  primitiveUnderscore,
         primitiveTensorElim,
         primitiveWithIntro, primitiveWithElim1, primitiveWithElim2,
         primitiveLinSumIntro1, primitiveLinSumIntro2, primitiveLinSumElim,
         primitiveBangElim
       ) where

import Lexer.Categories(isKeyword, isInteger)

splitParts :: String -> [String]
splitParts ""         = []
splitParts [x]        = [[x]]
splitParts ('_' : xs) = "_" : splitParts xs
splitParts (x : xs)   = let (p : ps) = splitParts xs in
                          if p == "_"
                           then [x] : p : ps
                           else (x : p) : ps

allNameParts :: String -> [String]
allNameParts id = id : (filter (/= "_") (splitParts id))

nameArity :: String -> Int
nameArity name = length (filter (== "_") (splitParts name))

-- A name part is well-formed if it is not a keyword nor an integer.
isWellFormedNamePart :: String -> Bool
isWellFormedNamePart s = not (isKeyword s) && not (isInteger s)

-- A name is well-formed if:
-- - It has at least one part.
-- - All of its parts are well-formed.
-- - Underscores alternate with non-underscores.
isWellFormedName :: String -> Bool
isWellFormedName s = case splitParts s of
                       []       -> False
                       (p : ps) -> isWellFormedNamePart p &&
                                   rec (p == "_") ps
  where
    rec _ []       = True
    rec b (p : ps) = isWellFormedNamePart p &&
                     b /= (p == "_") &&
                     rec (not b) ps

-- A well-formed name is moreover a well-formed operator name if
-- it has at least one underscore.
isWellFormedOperatorName :: String -> Bool
isWellFormedOperatorName s =
     isWellFormedName s
  && s /= "_" 
  && "_" `elem` splitParts s

-- Qualified name
data QName =
    Name String
  | Qualified String QName
  deriving (Eq, Ord)

instance Show QName where
  show (Name s)        = s
  show (Qualified q s) = q ++ "." ++ show s

readName :: String -> QName
readName s
  | isWellFormedName s = Name s
  | otherwise          = error "Name is not well-formed."

qualify :: QName -> String -> QName
qualify (Name id)            id' = Qualified id (readName id')
qualify (Qualified id qname) id' = Qualified id (qualify qname id')

moduleNameFromQName :: QName -> QName
moduleNameFromQName (Name id) =
  error ("Name \"" ++ id ++ "\" is unqualified.")
moduleNameFromQName (Qualified id (Name _)) = Name id
moduleNameFromQName (Qualified id qname)    =
  Qualified id (moduleNameFromQName qname)

unqualifiedName :: QName -> String
unqualifiedName (Name id)           = id
unqualifiedName (Qualified _ qname) = unqualifiedName qname

suffixQName :: QName -> String -> QName
suffixQName (Name id)            suf = Name (id ++ suf)
suffixQName (Qualified id qname) suf = Qualified id (suffixQName qname suf)

----

modulePRIM :: QName
modulePRIM = Name "PRIM"

moduleMain :: QName
moduleMain = Name "Main"

arrowSymbol :: String
arrowSymbol = "→"

colonSymbol :: String
colonSymbol = ":"

primitiveMain :: QName
primitiveMain = qualify modulePRIM "main"

primitiveType :: QName
primitiveType = qualify modulePRIM "Type"

primitiveForall :: QName
primitiveForall = qualify modulePRIM "∀_"

primitiveColon :: QName
primitiveColon = qualify modulePRIM "_:_"

symbolTensor :: String
symbolTensor = "⊗"

primitiveTensor :: QName
primitiveTensor = qualify modulePRIM ("_" ++ symbolTensor ++ "_")

symbolWith :: String
symbolWith = "&"

primitiveWith :: QName
primitiveWith = qualify modulePRIM ("_" ++ symbolWith ++ "_")

symbolLinSum :: String
symbolLinSum = "⊕"

primitiveLinSum :: QName
primitiveLinSum = qualify modulePRIM ("_" ++ symbolLinSum ++ "_")

symbolBang :: String
symbolBang = "!"

primitiveBang :: QName
primitiveBang = qualify modulePRIM (symbolBang ++ "_")

primitiveArrow :: QName
primitiveArrow = qualify modulePRIM ("_" ++ arrowSymbol ++ "_")

primitiveUnit :: QName
primitiveUnit = qualify modulePRIM "()"

primitiveUnderscore :: QName
primitiveUnderscore = qualify modulePRIM "_"

primitiveTensorElim :: QName
primitiveTensorElim = qualify modulePRIM "δ⊗"

primitiveWithIntro :: QName
primitiveWithIntro = qualify modulePRIM "_,_"

primitiveWithElim1 :: QName
primitiveWithElim1 = qualify modulePRIM "δ&₁"

primitiveWithElim2 :: QName
primitiveWithElim2 = qualify modulePRIM "δ&₂"

primitiveLinSumIntro1 :: QName
primitiveLinSumIntro1 = qualify modulePRIM "inl"

primitiveLinSumIntro2 :: QName
primitiveLinSumIntro2 = qualify modulePRIM "inr"

primitiveLinSumElim :: QName
primitiveLinSumElim = qualify modulePRIM "δ⊕"

primitiveBangElim :: QName
primitiveBangElim = qualify modulePRIM "δ!"

