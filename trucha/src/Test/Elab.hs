
module Test.Elab(tests) where

import Syntax.Name(QName, makeInternalQName)

import Parser.Lexer(tokenize, readQName)
import Parser.Parser(parse)
import Elab.Elab(elaborate)
import Test.Test(TestSuite(..))

sourceName :: String
sourceName = "(...)"

perform :: Either a b -> Either () b
perform x = case x of
              Left _ -> Left ()
              Right x -> Right x

elabString :: String -> Either () String
elabString source = do
  toks    <- perform $ tokenize sourceName source
  program <- perform $ parse toks
  ctx     <- perform $ elaborate program
  return (show ctx)

testSucceed :: String -> String -> TestSuite
testSucceed name source =
  TestCase name
           (case elabString source of
              Right _ -> Right ()
              Left _  -> Left ())
           (Right ())

testSucceedWithResult :: String -> String -> String -> TestSuite
testSucceedWithResult name source expectedContext =
  TestCase name
           (elabString source)
           (Right expectedContext)

testFail :: String -> String -> TestSuite
testFail name source =
  TestCase name
           (elabString source)
           (Left ())

tests :: TestSuite
tests = TestSuite "Elaborator" [
          testSucceed "Data type declaration"
            (unlines [ "data Nat : Type" ])
        ,
          testFail "Data type declaration: repeated declaration"
            (unlines [
               "data Nat : Type"
             , "data Nat : Type"
             ])
        ,
          testFail "Data type declaration: type of datatype must be a sort"
            (unlines [
               "data Nat  : Type where"
             , "data Bool : Nat  where"
             ])
        ,
          testSucceed "Data type declaration: two empty datatypes"
            (unlines [
               "data Nat  : Type where"
             , "data Bool : Type where"
             ])
        ,
          testFail "Data type declaration: constructor returns a different type"
            (unlines [
               "data Nat  : Type where"
             , "data Bool : Type where"
             , "  true : Nat"
             ])
        ,
          testSucceed "Data type declaration: fixed parameters"
            (unlines [
               "data List (A : Type) : Type where"
             , "  nil : List A"
             , "  cons : A → List A → List A"
             ])
        ,
          testSucceed "Data type declaration: fixed parameters"
            (unlines [
               "data Nat : Type where"
             , "data List (A : Type) : Type where"
             ])
        ,
          testFail "Data type declaration: fixed parameters must be fixed"
            (unlines [
               "data Nat : Type where"
             , "data List (A : Type) : Type where"
             , "  nil : List Nat"
             ])
        ,
          testFail "Data type declaration: strict positivity"
            (unlines [
               "data Nat : Type where"
             , "  nil : (Nat → Nat) → Nat"
             ])
        ,
          testSucceed "Data type declaration: indices are not fixed"
            (unlines [
               "data Nat : Type where"
             , "data List : (A : Type) → Type where"
             , "  nil : List Nat"
             ])
        ,
          testSucceed "Data type declaration: mutual recursion"
            (unlines [
               "data Nat : Type where"
             , "  zero : Nat "
             , "  suc : Nat → Nat"
             , "data Even : Nat → Type"
             , "data Odd : Nat → Type"
             , "data Even where"
             , "  E0 : Even zero"
             , "  ES : {n : Nat} → Odd n → Even (suc n)"
             , "data Odd where"
             , "  OS : {n : Nat} → Even n → Odd (suc n)"
             ])
        ,
          testSucceed "Data type declaration"
            (unlines [
              "data Bool : Type where"
            , "  true : Bool"
            , "  false : Bool"
            , "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "data List (A : Type) : Type where"
            , "  []   : List A"
            , "  _∷_  : A → List A → List A"
            ])
        ,
          testSucceed "Sigma types"
            (unlines [
              "data Bool : Type where"
            , "  true  : Bool"
            , "  false : Bool"
            , "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "data Even : Nat → Type where"
            , "  e0  : Even zero"
            , "  eSS : {n : Nat} → Even n → Even (suc (suc n))"
            , "data Sig (A : Type) (B : A → Type) : Type where"
            , "  pair : (a : A) → B a → Sig A B"
            , "x : Sig _ Even"
            , "x = pair {_} {_} _ (eSS {_} e0)"
            , "y : Sig _ (λ _ → Bool)"
            , "y = pair {_} {_} zero true"
            ])
        ,
          testFail "Implicit arguments (too many)"
            (unlines [
              "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "data Even : Nat → Type where"
            , "  e0  : Even zero"
            , "  eSS : {n : Nat} → Even n → Even (suc (suc n))"
            , "data Sig (A : Type) (B : A → Type) : Type where"
            , "  pair : (a : A) → B a → Sig A B"
            , "x : Sig _ Even"
            , "x = pair {_} {_} {_} _ (eSS {_} e0)"
            ])
        ,
          testSucceed "Implicit arguments"
            (unlines [
              "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "data Even : Nat → Type where"
            , "  e0  : Even zero"
            , "  eSS : {n : Nat} → Even n → Even (suc (suc n))"
            , "data Sig (A : Type) (B : A → Type) : Type where"
            , "  pair : (a : A) → B a → Sig A B"
            , "x : Sig _ Even"
            , "x = pair {_} {_} _ (eSS {_} e0)"
            ])
        ,
          testSucceed "Implicit arguments"
            (unlines [
              "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "data Even : Nat → Type where"
            , "  e0  : Even zero"
            , "  eSS : {n : Nat} → Even n → Even (suc (suc n))"
            , "data Sig (A : Type) (B : A → Type) : Type where"
            , "  pair : (a : A) → B a → Sig A B"
            , "x : Sig _ Even"
            , "x = pair {_} _ (eSS {_} e0)"
            ])
        ,
          testSucceed "Implicit arguments"
            (unlines [
              "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "data Even : Nat → Type where"
            , "  e0  : Even zero"
            , "  eSS : {n : Nat} → Even n → Even (suc (suc n))"
            , "data Sig (A : Type) (B : A → Type) : Type where"
            , "  pair : (a : A) → B a → Sig A B"
            , "x : Sig _ Even"
            , "x = pair _ (eSS e0)"
            ])
        ,
          testFail "Equation with Type in the LHS"
            (unlines [
              "data Bool : Type where"
            , "  true : Bool"
            , "  false : Bool"
            , "Type = true"
            ])
        ,
          testFail "Equation with constructor in the LHS"
            (unlines [
              "data Bool : Type where"
            , "  true : Bool"
            , "  false : Bool"
            , "true = true"
            ])
        ,
          testSucceed "Equation"
            (unlines [
              "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "uno = suc zero"
            ])
        ,
          testFail "Equations with different types"
            (unlines [
              "data Nat : Type where"
            , "  zero : Nat"
            , "  suc  : Nat → Nat"
            , "uno = suc zero"
            , "uno = suc"
            ])
        ]

