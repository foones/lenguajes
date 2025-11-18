module Test.Lexer(tests) where

import Parser.Lexer(Token(..), TokenType(..), tokenize, readQName)
import Test.Test(TestSuite(..))

sourceName :: String
sourceName = "(...)"

perform :: Either a b -> Either () b
perform x = case x of
              Left _  -> Left ()
              Right x -> Right x

tokenizeString :: String -> Either () [TokenType]
tokenizeString source =
  map tokenType <$> (perform $ tokenize sourceName source)

testSucceed :: String -> String -> [TokenType] -> TestSuite
testSucceed name source expectedTokenTypes =
  TestCase name
           (tokenizeString source)
           (Right ([TLBrace] ++ expectedTokenTypes ++ [TRBrace]))

testFail :: String -> String -> TestSuite
testFail name source =
  TestCase name
           (tokenizeString source)
           (Left ())

q :: String -> TokenType
q str = case readQName str of
          Right qName -> TId qName
          Left _      -> error "(Invalid QName)"

tests :: TestSuite
tests = TestSuite "Lexer" [
          testSucceed "Empty file" "" []
        , testSucceed "Valid identifier" "hola" [q "hola"]
        , testSucceed "Valid identifier" "a_" [q "a_"]
        , testSucceed "Valid identifier" "a_b" [q "a_b"]
        , testSucceed "Valid identifier" "_b" [q "_b"]
        , testSucceed "Valid identifier" "_a_" [q "_a_"]
        , testSucceed "Valid identifier" "a_b_c" [q "a_b_c"]
        , testSucceed "Valid identifier" "A.B" [q "A.B"]
        , testSucceed "Valid identifier" "A.B.C.D" [q "A.B.C.D"]
        , testSucceed "Valid identifier" "A_where.B" [q "A_where.B"]
        , testFail "Reject invalid identifier" "A."
        , testFail "Reject invalid identifier" "A..B"
        , testFail "Reject invalid identifier" "A.where"
        , testFail "Reject invalid identifier" "A.where_B"
        , testFail "Reject invalid identifier" "A.B__C"
        , testSucceed "Dot" ".A" [TDot, q "A"]
        , testSucceed "Number" "42" [TInt 42]
        , testSucceed "Delimiters"
            ";{}()"
            [TSemicolon, TLBrace, TRBrace, TLParen, TRParen]
        , testSucceed "Keywords"
            ("data where {} let {} in case of {} module " ++
             "infix infixl infixr = : import _ λ ∀ ∃")
            [TData, TWhere, TLBrace, TRBrace, TLet, TLBrace, TRBrace,
             TIn, TCase, TOf, TLBrace, TRBrace, TModule, TInfix, TInfixL,
             TInfixR, TEq, TColon, TImport, TUnderscore,
             TLambda, TForall, TExists]
        , testSucceed "Layout" "where" [TWhere, TLBrace, TRBrace]
        , testSucceed "Layout" "where {}" [TWhere, TLBrace, TRBrace]
        , testSucceed "Layout"
                      (unlines [
                        "where"
                      , "  a"
                      ])
                      [TWhere, TLBrace, q "a", TRBrace]
        , testSucceed "Layout"
                      (unlines [
                        "where"
                      , "  a"
                      , "  b"
                      ])
                      [TWhere, TLBrace, q "a", TSemicolon, q "b", TRBrace]
        , testSucceed "Layout"
                      (unlines [
                        "where"
                      , "  a"
                      , "b"
                      ])
                      [TWhere, TLBrace, q "a", TRBrace, TSemicolon, q "b"]
        , testSucceed "Layout"
                      (unlines [
                        "where"
                      , "  a"
                      , "  where"
                      ])
                      [TWhere, TLBrace, q "a", TSemicolon, TWhere,
                       TLBrace, TRBrace, TRBrace]
        , testSucceed "Layout"
                      (unlines [
                        "where"
                      , "  a"
                      , "  where"
                      , "    b"
                      , "    c"
                      , "  d"
                      ])
                      [TWhere, TLBrace,
                       q "a",
                       TSemicolon, TWhere, TLBrace,
                       q "b",
                       TSemicolon, q "c",
                       TRBrace,
                       TSemicolon, q "d",
                       TRBrace]
        , testSucceed "Layout"
                      (unlines [
                        "where"
                      , "  a"
                      , "  where {"
                      , "b"
                      , "            ; c"
                      , "}"
                      , "  d"
                      ])
                      [TWhere, TLBrace,
                       q "a",
                       TSemicolon, TWhere, TLBrace,
                       q "b",
                       TSemicolon, q "c",
                       TRBrace,
                       TSemicolon, q "d",
                       TRBrace]
        , testSucceed "Layout" "let a in b"
                      [TLet, TLBrace, q "a", TRBrace, TIn, q "b"]
        , testSucceed "Layout"
                      (unlines [
                        "let a"
                      , " in b"
                      ])
                      [TLet, TLBrace, q "a", TRBrace, TIn, q "b"]
        , testSucceed "Layout"
                      (unlines [
                        "let a"
                      , " in b"
                      ])
                      [TLet, TLBrace, q "a", TRBrace, TIn, q "b"]
        , testSucceed "Layout"
                      (unlines [
                        "let a"
                      , "    b"
                      , " in c"
                      ])
                      [TLet, TLBrace, q "a", TSemicolon, q "b", TRBrace, TIn, q "c"]
        , testSucceed "Layout"
                      (unlines [
                        "let { a ; b }"
                      , " in c"
                      ])
                      [TLet, TLBrace, q "a", TSemicolon, q "b", TRBrace, TIn, q "c"]
        , testSucceed "Toplevel where"
                      (unlines [
                        "module A where"
                      , "x = y"
                      ])
                      [TModule, q "A", TWhere, TLBrace, q "x", TEq, q "y", TRBrace]
        ]

