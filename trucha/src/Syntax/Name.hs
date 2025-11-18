module Syntax.Name(
         ModuleName,
         rootModuleName, makeModuleName, moduleNameFromString, moduleNameParts,
         BareName, BareNamePart(..), HoleName(..),
         makeBareName, bareNameAllParts, bareNameNamedParts,
         QName, makeQName, qnameModuleName, qnameBareName, isBareQName,
         qnameNamedParts, qnameIsOperator, makeInternalQName,
         --
         globalNameType, globalNameUnderscore, arrowSymbol, arrowOperator,
         arrowFunction, timesSymbol, timesOperator, timesFunction
       ) where

import Utils(joinS, splitBy)

data ModuleName = ModuleName [String]
  deriving (Eq, Ord)

rootModuleName :: ModuleName
rootModuleName = ModuleName []

makeModuleName :: [String] -> ModuleName
makeModuleName xs = ModuleName xs

moduleNameFromString :: String -> ModuleName
moduleNameFromString str = makeModuleName (splitBy (== '.') str)

moduleNameParts :: ModuleName -> [String]
moduleNameParts (ModuleName parts) = parts

isRootModuleName :: ModuleName -> Bool
isRootModuleName = null . moduleNameParts

----

data QName = QName ModuleName BareName
  deriving (Eq, Ord)

data BareName = BareName [BareNamePart]
  deriving (Eq, Ord)

data BareNamePart = BNPlaceholder
                  | BNName String
  deriving (Eq, Ord)

data HoleName = HoleName String
  deriving (Eq, Ord)

makeQName :: ModuleName -> BareName -> QName
makeQName qs bareName = QName qs bareName

makeInternalQName :: String -> Integer -> QName
makeInternalQName prefix n =
  QName rootModuleName (makeBareName ["." ++ prefix ++ "." ++ show n])

makeBareName :: [String] -> BareName
makeBareName parts = BareName $ map (\ part -> if part == "_"
                                                then BNPlaceholder
                                                else BNName part)
                                    parts

bareNameAllParts :: BareName -> [BareNamePart]
bareNameAllParts (BareName parts) = parts

bareNameNamedParts :: BareName -> [BareName]
bareNameNamedParts (BareName parts) = rec parts
  where
    rec []                    = []
    rec (BNPlaceholder : parts) = rec parts
    rec (BNName name : parts)   = BareName [BNName name] : rec parts

qnameModuleName :: QName -> ModuleName
qnameModuleName (QName q _) = q

qnameBareName :: QName -> BareName
qnameBareName (QName _ b) = b

qnameNamedParts :: QName -> [QName]
qnameNamedParts qname =
  let namedParts = bareNameNamedParts (qnameBareName qname)
   in map (makeQName (qnameModuleName qname)) namedParts

qnameIsOperator :: QName -> Bool
qnameIsOperator qname = length (bareNameAllParts (qnameBareName qname)) > 1

isBareQName :: QName -> Bool
isBareQName (QName q _) = isRootModuleName q

instance Show ModuleName where
  show (ModuleName [])    = "(root)"
  show (ModuleName parts) = joinS "." parts

instance Show QName where
  show (QName moduleName bareName) =
    joinS "." (moduleNameParts moduleName ++ [show bareName])

instance Show BareName where
  show (BareName parts) = joinS "" (map show parts)

instance Show BareNamePart where
  show BNPlaceholder = "_"
  show (BNName x)    = x

instance Show HoleName where
  show (HoleName x) = "?" ++ x

----

globalNameType :: QName
globalNameType = makeQName rootModuleName (makeBareName ["Type"])

globalNameUnderscore :: QName
globalNameUnderscore = makeQName rootModuleName (makeBareName ["_"])

arrowSymbol :: String
arrowSymbol = "→"

arrowOperator :: QName
arrowOperator = makeQName rootModuleName (makeBareName [arrowSymbol])

arrowFunction :: QName
arrowFunction = makeQName rootModuleName (makeBareName ["_", arrowSymbol, "_"])

timesSymbol :: String
timesSymbol = "×"

timesOperator :: QName
timesOperator = makeQName rootModuleName (makeBareName [timesSymbol])

timesFunction :: QName
timesFunction = makeQName rootModuleName (makeBareName ["_", timesSymbol, "_"])


