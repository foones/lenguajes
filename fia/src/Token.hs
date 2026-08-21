module Token(
         Token(..), isTId,
         isUpper, isLower, isDigit, isAlpha, isIdent, isLowerName, isUpperName
       ) where

data Token = TEof
           | TId String
           | THole String
           | TNatConst Integer
           -- Symbols
           | TDefEq
           | TColon
           | TComma
           | THash
           | TLParen
           | TRParen
           | TLBrack
           | TRBrack
           -- Declarations
           | TTheorem
           | TFun
           | TProp
           | TEval
           | TProof
           | TEnd
           -- Proofs
           | TLet
           | TSuppose
           | TIndeed
           | TBy
           | TInduction
           | TContradiction
           | TShow
           | TClaim
           | TThen
           | TAlso
           | THave
           | TAssume
           | TCases
           | TCase
           | TTake
           | TConsider
           | TSt
           -- Formulas
           | TForall
           | TExists
           | TFormOr
           | TFormAnd
           | TFormImp
           -- Natural numbers
           | TSucc
           | TAdd
           | TMul
           | TEq
  deriving (Show, Eq)

isTId :: Token -> Bool
isTId (TId _) = True
isTId _       = False

isUpper :: Char -> Bool
isUpper c = 'A' <= c && c <= 'Z'

isLower :: Char -> Bool
isLower c = 'a' <= c && c <= 'z'

isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isIdent :: Char -> Bool
isIdent c = isAlpha c || isDigit c || c `elem` ['_', '\'']

isLowerName :: String -> Bool
isLowerName name = not (null name) && isLower (head name)

isUpperName :: String -> Bool
isUpperName name = not (null name) && isUpper (head name)

