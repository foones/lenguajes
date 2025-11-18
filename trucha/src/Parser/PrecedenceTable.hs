module Parser.PrecedenceTable(
         Associativity(..), isAssocLeft, isAssocRight,
         Precedence(..), PrecedenceTable(),
         emptyPrecedenceTable, declareOperator,
         PrecedenceLevel(..), Operator(..), OperatorPart(..),
         allPrecedenceLevels, isOperatorName, isOperatorPart
       ) where

import qualified Data.Map as M
import qualified Data.Set as S

import Syntax.Name(
         QName, BareNamePart(..), makeBareName, bareNameAllParts,
         makeQName, qnameModuleName, qnameBareName, qnameNamedParts
       )

data Precedence = Precedence Integer
  deriving (Eq, Ord)

data Associativity = NonAssoc
                   | AssocLeft
                   | AssocRight
  deriving (Eq, Ord)

isAssocLeft :: Associativity -> Bool
isAssocLeft AssocLeft = True
isAssocLeft _         = False

isAssocRight :: Associativity -> Bool
isAssocRight AssocRight = True
isAssocRight _          = False

data PrecedenceTable = PT {
                         table :: M.Map (Precedence, Associativity)
                                        (S.Set QName)
                       , knownOperatorNames :: S.Set QName
                       , knownOperatorParts :: S.Set QName
                       }

data PrecedenceLevel = PrecedenceLevel Associativity (S.Set Operator)

data Operator = Operator QName [OperatorPart]
  deriving (Eq, Ord, Show)

data OperatorPart = OprPlaceholder
                  | OprPart QName
  deriving (Eq, Ord, Show)

emptyPrecedenceTable :: PrecedenceTable
emptyPrecedenceTable = PT {
                         table = M.empty
                       , knownOperatorNames = S.empty
                       , knownOperatorParts = S.empty
                       }

declareOperator :: PrecedenceTable -> Associativity -> Precedence -> QName
                -> Either String PrecedenceTable
declareOperator pt associativity precedence qname =
  if qname `S.member` knownOperatorNames pt
   then Left ("Operator has already been declared: " ++ show qname)
   else do
     let key = (precedence, associativity)
     let ops = M.findWithDefault S.empty key (table pt)
     return $ pt {
       table = M.insert key (S.insert qname ops) (table pt)
     , knownOperatorNames = S.insert qname (knownOperatorNames pt)
     , knownOperatorParts = S.union (S.fromList (qnameNamedParts qname))
                                    (knownOperatorParts pt)
     }

allPrecedenceLevels :: PrecedenceTable -> [PrecedenceLevel]
allPrecedenceLevels pt =
    [ PrecedenceLevel associativity (S.map qnameAsOperator qnames)
    | ((_, associativity), qnames) <- M.toList (table pt)
    ]
  where
    qnameAsOperator :: QName -> Operator
    qnameAsOperator qname =
      let parts = bareNameAllParts (qnameBareName qname)
       in Operator qname
                   (map (barenamePartAsOperatorPart qname) parts)
    barenamePartAsOperatorPart :: QName -> BareNamePart -> OperatorPart
    barenamePartAsOperatorPart _     BNPlaceholder = OprPlaceholder
    barenamePartAsOperatorPart qname (BNName name)  =
      OprPart (makeQName (qnameModuleName qname)
                         (makeBareName [name]))

isOperatorName :: PrecedenceTable -> QName -> Bool
isOperatorName pt qname = qname `S.member` knownOperatorNames pt

isOperatorPart :: PrecedenceTable -> QName -> Bool
isOperatorPart pt qname = qname `S.member` knownOperatorParts pt
