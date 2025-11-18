
module Test.Parser(tests) where

import Data.Maybe(fromJust)

import Syntax.Name(QName, makeInternalQName)
import Syntax.AST(
         AProgram(..), ADeclaration(..), AParameter(..),
         AConstructorDecl(..), AExpr(..), ACaseBranch(..), APattern(..),
         Program, Declaration, ConstructorDecl, Expr, CaseBranch,
         eraseAnnotations, exprToPattern
       )
import Parser.Lexer(tokenize, readQName)
import Parser.Parser(parse)
import Test.Test(TestSuite(..))

sourceName :: String
sourceName = "(...)"

perform :: Either a b -> Either () b
perform x = case x of
              Left _ -> Left ()
              Right x -> Right x

parseString :: String -> Either () (AProgram ())
parseString source = do
  toks <- perform $ tokenize sourceName source
  eraseAnnotations <$> (perform $ parse toks)

testSucceed :: String -> String -> AProgram () -> TestSuite
testSucceed name source expectedAST =
  TestCase name
           (parseString source)
           (Right expectedAST)

testFail :: String -> String -> TestSuite
testFail name source =
  TestCase name
           (parseString source)
           (Left ())

q :: String -> QName
q str = case readQName str of
          Right qName -> qName
          Left _      -> error "(Invalid QName)"

qi :: String -> Integer -> QName
qi = makeInternalQName

var :: String -> AExpr ()
var str = EVar () (q str)

wildcard :: AExpr ()
wildcard = EWildcard ()

app :: AExpr () -> [AExpr ()] -> AExpr ()
app fun args = foldl1 (EApp ()) (fun : args)

pvar :: String -> APattern ()
pvar str = PVar () (q str)

papp :: APattern () -> [APattern ()] -> APattern ()
papp fun args = foldl1 (PApp ()) (fun : args)

pwildcard :: APattern ()
pwildcard = PWildcard ()

eparam :: QName -> AExpr () -> AParameter ()
eparam = ExplicitParameter ()

iparam :: QName -> AExpr () -> AParameter ()
iparam = ImplicitParameter ()

declEquation :: AExpr () -> AExpr () -> ADeclaration ()
declEquation lhs rhs = DeclEquation () (fromJust (exprToPattern lhs)) rhs

declDataTypeAndConstructors :: a -> QName -> [AParameter a] -> AExpr a
                            -> [AConstructorDecl a] -> [ADeclaration a]
declDataTypeAndConstructors ann name parameters typ constructors =
  [ DeclDataType ann name parameters typ
  , DeclDataConstructors ann name parameters constructors
  ]

tests :: TestSuite
tests = TestSuite "Parser" [
          testSucceed "Empty file" "" (Program [])
        , testFail "Repeated module"
            (unlines [
              "module A where"
            , "  x : Type"
            , "module A where"
            , "  x : Type"
            ])
        , testFail "Repeated module"
            (unlines [
              "module A where"
            , "  module B where"
            , "    x : Type"
            , "module A.B where"
            , "  x : Type"
            ])
        , testFail "Type declaration must declare a bare name"
            (unlines [
              "module A where"
            , "  A.x : Type"
            ])
        , testSucceed "Imports"
            (unlines [
              "module A where"
            , "  x : Type"
            , "module B where"
            , "  y : x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "B.x")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A where"
            , "  x : Type"
            , "module B where"
            , "  import A"
            , "  y : x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "A.x")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A where"
            , "  x : Type"
            , "module B where"
            , "  import A()"
            , "  y : x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "B.x")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A() where"
            , "  x : Type"
            , "module B where"
            , "  import A"
            , "  y : x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "B.x")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A(x) where"
            , "  x : Type"
            , "module B where"
            , "  import A"
            , "  y : x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "A.x")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A where"
            , "  import A"
            , "  x : y"
            ])
            (Program [
              DeclType () (q "A.x") (var "A.y")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A where"
            , "  x : Type"
            , "module B where"
            , "  import A(x)"
            , "  y : x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "A.x")
            ])
        , testSucceed "Imports"
            (unlines [
              "module A where"
            , "  x : Type"
            , "module B where"
            , "  import A qualified"
            , "  y : A.x"
            ])
            (Program [
              DeclType () (q "A.x") (var "Type")
            , DeclType () (q "B.y") (var "A.x")
            ])
        , testFail "Imports"
            (unlines [
              "module A() where"
            , "  x : Type"
            , "module B where"
            , "  import A qualified"
            , "  y : A.x"
            ])
        , testSucceed "Imports"
            (unlines [
              "module A.B where"
            , "  x : Type"
            , "module C where"
            , "  import A.B qualified as Z"
            , "  y : Z.x"
            ])
            (Program [
              DeclType () (q "A.B.x") (var "Type")
            , DeclType () (q "C.y") (var "A.B.x")
            ])
        , testFail "Operator declaration"
            (unlines [
              "infix 10 _+_"
            , "infixr 20 _+_"
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "module A where"
            , "  infix 10 _+_"
            , "module B where"
            , "  infix 10 _+_"
            , "  import A"
            ])
            (Program [])
        , testFail "Operator declaration"
            (unlines [
              "infix 10 _+_"
            , "x = +"
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infix 1 if_then_else_"
            , "x = if a b then c d else e f"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "if_then_else_") [
                  (app (var "a") [var "b"])
                , (app (var "c") [var "d"])
                , (app (var "e") [var "f"])
                ])
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infix 1 &_"
            , "x = & a"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "&_") [var "a"])
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infix 1 _&"
            , "x = a &"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "_&") [var "a"])
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infix 1 _+_"
            , "infix 2 _*_"
            , "x = a * b + c * d"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "_+_") [
                  app (var "_*_") [var "a", var "b"]
                , app (var "_*_") [var "c", var "d"]
                ])
            ])
        , testFail "Operator declaration"
            (unlines [
              "infix 1 _+_"
            , "x = a + b + c"
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infixr 1 _+_"
            , "x = a + b + c"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "_+_") [
                  var "a"
                , app (var "_+_") [var "b", var "c"]
                ])
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infixl 1 _+_"
            , "x = a + b + c"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "_+_") [
                  app (var "_+_") [var "a", var "b"]
                , var "c"
                ])
            ])
        , testSucceed "Operator declaration"
            (unlines [
              "infixr 1 if_then_else_"
            , "x = if a then b else if c then d else e"
            ])
            (Program [
              declEquation
                (var "x")
                (app (var "if_then_else_") [
                  var "a"
                , var "b"
                , app (var "if_then_else_") [
                    var "c"
                  , var "d"
                  , var "e"
                  ]
                ])
            ])
        , testSucceed "Register head of LHS"
            (unlines [
              "module A where"
            , "  f x = x"
            , "module B where"
            , "  import A"
            , "  y = f x"
            ])
            (Program [
              declEquation (app (var "A.f") [var "A.x"]) (var "A.x")
            , declEquation (var "B.y") (app (var "A.f") [var "B.x"])
            ])
        , testSucceed "Datatype declaration"
            (unlines [
              "data A : Type where"
            , "data B : Type where"
            ])
            (Program (
               declDataTypeAndConstructors () (q "A") [] (var "Type") []
            ++ declDataTypeAndConstructors () (q "B") [] (var "Type") []
            ))
        , testSucceed "Datatype declaration"
            (unlines [
              "data A b : Type where"
            ])
            (Program $
              declDataTypeAndConstructors () (q "A") [
                eparam (q "b") wildcard
              ] (var "Type") [])
        , testSucceed "Datatype declaration"
            (unlines [
              "data A (b c : Type) : Type where"
            ])
            (Program $
              declDataTypeAndConstructors () (q "A") [
                eparam (q "b") (var "Type")
              , eparam (q "c") (var "Type")
              ] (var "Type") [])
        , testSucceed "Datatype declaration"
            (unlines [
              "data A {b} : Type where"
            ])
            (Program $
              declDataTypeAndConstructors () (q "A") [
                iparam (q "b") wildcard
              ] (var "Type") [])
        , testSucceed "Datatype declaration"
            (unlines [
              "data A {b c : Type} : Type where"
            ])
            (Program $
              declDataTypeAndConstructors () (q "A") [
                iparam (q "b") (var "Type")
              , iparam (q "c") (var "Type")
              ] (var "Type") [])
        , testSucceed "Datatype declaration"
            (unlines [
              "data A b (c : b) {d e f} {g : h} : Type where"
            ])
            (Program $
              declDataTypeAndConstructors () (q "A") [
                eparam (q "b") wildcard
              , eparam (q "c") (var "b")
              , iparam (q "d") wildcard
              , iparam (q "e") wildcard
              , iparam (q "f") wildcard
              , iparam (q "g") (var "h")
              ] (var "Type") [])
        , testSucceed "Dependent arrow"
            (unlines [
              "x1 = A → B"
            , "x2 = (a : A) → B"
            , "x3 = (_ : A) → B"
            , "x4 = (a b : A) → B"
            , "x5 = (a b : A) (c : B) → C"
            , "x6 = {a : A} → B"
            , "x7 = {a b : A} {c : B} → C"
            , "x8 = ∀ {a} → B"
            , "x9 = ∀ {a b} → B"
            , "x10 = {_ : A} → B"
            , "x11 = ∀ {_ _} → B"
            , "x12 = ∀ (a b) → C"
            , "x13 = ∀ a → B"
            , "x14 = ∀ _ → B"
            ])
            (Program [
              declEquation (var "x1") $
                EPi () (eparam (q "_") (var "A")) $
                var "B"
            , declEquation (var "x2") $
                EPi () (eparam (q "a") (var "A")) $
                var "B"
            , declEquation (var "x3") $
                EPi () (eparam (q "_") (var "A")) $
                var "B"
            , declEquation (var "x4") $
                EPi () (eparam (q "a") (var "A")) $
                EPi () (eparam (q "b") (var "A")) $
                var "B"
            , declEquation (var "x5") $
                EPi () (eparam (q "a") (var "A")) $
                EPi () (eparam (q "b") (var "A")) $
                EPi () (eparam (q "c") (var "B")) $
                var "C"
            , declEquation (var "x6") $
                EPi () (iparam (q "a") (var "A")) $
                var "B"
            , declEquation (var "x7") $
                EPi () (iparam (q "a") (var "A")) $
                EPi () (iparam (q "b") (var "A")) $
                EPi () (iparam (q "c") (var "B")) $
                var "C"
            , declEquation (var "x8") $
                EPi () (iparam (q "a") wildcard) $
                var "B"
            , declEquation (var "x9") $
                EPi () (iparam (q "a") wildcard) $
                EPi () (iparam (q "b") wildcard) $
                var "B"
            , declEquation (var "x10") $
                EPi () (iparam (q "_") (var "A")) $
                var "B"
            , declEquation (var "x11") $
                EPi () (iparam (q "_") wildcard) $
                EPi () (iparam (q "_") wildcard) $
                var "B"
            , declEquation (var "x12") $
                EPi () (eparam (q "a") wildcard) $
                EPi () (eparam (q "b") wildcard) $
                var "C"
            , declEquation (var "x13") $
                EPi () (eparam (q "a") wildcard) $
                var "B"
            , declEquation (var "x14") $
                EPi () (eparam (q "_") wildcard) $
                var "B"
            ])
        , testSucceed "Dependent sum"
            (unlines [
              "x1 = A × B"
            , "x2 = (a : A) × B"
            , "x3 = (_ : A) × B"
            , "x4 = (a b : A) × B"
            , "x5 = (a b : A) (c : B) × C"
            , "x6 = {a : A} × B"
            , "x7 = {a b : A} {c : B} × C"
            , "x8 = ∃ {a} × B"
            , "x9 = ∃ {a b} × B"
            , "x10 = {_ : A} × B"
            , "x11 = ∃ {_ _} × B"
            , "x12 = ∃ (a b) × C"
            , "x13 = ∃ a × B"
            , "x14 = ∃ _ × B"
            ])
            (Program [
              declEquation (var "x1") $
                ESig () (eparam (q "_") (var "A")) $
                var "B"
            , declEquation (var "x2") $
                ESig () (eparam (q "a") (var "A")) $
                var "B"
            , declEquation (var "x3") $
                ESig () (eparam (q "_") (var "A")) $
                var "B"
            , declEquation (var "x4") $
                ESig () (eparam (q "a") (var "A")) $
                ESig () (eparam (q "b") (var "A")) $
                var "B"
            , declEquation (var "x5") $
                ESig () (eparam (q "a") (var "A")) $
                ESig () (eparam (q "b") (var "A")) $
                ESig () (eparam (q "c") (var "B")) $
                var "C"
            , declEquation (var "x6") $
                ESig () (iparam (q "a") (var "A")) $
                var "B"
            , declEquation (var "x7") $
                ESig () (iparam (q "a") (var "A")) $
                ESig () (iparam (q "b") (var "A")) $
                ESig () (iparam (q "c") (var "B")) $
                var "C"
            , declEquation (var "x8") $
                ESig () (iparam (q "a") wildcard) $
                var "B"
            , declEquation (var "x9") $
                ESig () (iparam (q "a") wildcard) $
                ESig () (iparam (q "b") wildcard) $
                var "B"
            , declEquation (var "x10") $
                ESig () (iparam (q "_") (var "A")) $
                var "B"
            , declEquation (var "x11") $
                ESig () (iparam (q "_") wildcard) $
                ESig () (iparam (q "_") wildcard) $
                var "B"
            , declEquation (var "x12") $
                ESig () (eparam (q "a") wildcard) $
                ESig () (eparam (q "b") wildcard) $
                var "C"
            , declEquation (var "x13") $
                ESig () (eparam (q "a") wildcard) $
                var "B"
            , declEquation (var "x14") $
                ESig () (eparam (q "_") wildcard) $
                var "B"
            ])
        , testFail "Type syntax" "x1 = ∃ x → A"
        , testFail "Type syntax" "x1 = ∀ x × A"
        , testSucceed "Lambda"
            "a = λ x → x"
            (Program [
              declEquation (var "a")
                              (ELam () (q "x") wildcard (var "x"))
            ])
        , testSucceed "Lambda"
            "a = λ {x} → x"
            (Program [
              declEquation (var "a")
                              (ELamImplicit () (q "x") wildcard (var "x"))
            ])
        , testSucceed "Lambda"
            "a = λ _ → x"
            (Program [
              declEquation (var "a")
                              (ELam () (q "_") wildcard (var "x"))
            ])
        , testSucceed "Lambda"
            "a = λ (x : List Nat) → x"
            (Program [
              declEquation (var "a")
                              (ELam () (q "x") (app (var "List") [var "Nat"])
                                       (var "x"))
            ])
        , testSucceed "Lambda"
            "a = λ {x : Nat} → x"
            (Program [
              declEquation (var "a")
                              (ELamImplicit () (q "x") (var "Nat") (var "x"))
            ])
        , testSucceed "Pattern matching lambda"
            "a = λ { Nil → x ; (Cons x xs) → y }"
            (Program [
              declEquation (var "a")
                (ELam () (qi "p" 0) wildcard $
                  ECase () (EVar () (qi "p" 0)) [
                    CaseBranch () [pvar "Nil"] (var "x")
                  , CaseBranch () [papp (pvar "Cons") [pvar "x", pvar "xs"]]
                                  (var "y")
                  ])
            ])
        , testSucceed "Pattern matching lambda"
            "a = λ { }"
            (Program [
              declEquation (var "a")
                (ELam () (qi "p" 0) wildcard $
                  ECase () (EVar () (qi "p" 0)) [
                  ])
            ])
        , testSucceed "Pattern matching lambda"
            "a = λ { Nil Nil → x ; _ (Cons x xs) → y }"
            (Program [
              declEquation (var "a")
                (ELam () (qi "p" 0) wildcard $
                  ECase () (EVar () (qi "p" 0)) [
                    CaseBranch () [pvar "Nil", pvar "Nil"] (var "x")
                  , CaseBranch () [pwildcard, papp (pvar "Cons") [pvar "x", pvar "xs"]]
                                  (var "y")
                  ])
            ])
        ]

