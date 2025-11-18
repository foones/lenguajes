module Main(main) where

import System.Environment(getArgs)
import System.IO(hPutStrLn, stderr)
import System.Exit(die)

import FailState(ErrMsg)

import Parser.Lexer(tokenize)
import Parser.Parser(parse)

import Elab.Elab(elaborate)

import Test.Test(TestSuite(..), runTestSuite)
import qualified Test.Lexer
import qualified Test.Parser
import qualified Test.Elab

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t", filename] = doTokenize filename
run ["-p", filename] = doParse filename
run ["-e", filename] = doElaborate filename
run ["-T"]           = doRunAllTests
---
run [filename]       = doElaborate filename
run _                = displayUsage

doTokenize :: String -> IO ()
doTokenize filename = do
  source <- readFile filename
  tokens <- perform $ tokenize filename source
  mapM_ (putStrLn . show) tokens

doParse :: String -> IO ()
doParse filename = do
  source <- readFile filename
  tokens <- perform $ tokenize filename source
  ast    <- perform $ parse tokens
  putStrLn . show $ ast

doElaborate :: String -> IO ()
doElaborate filename = do
  source  <- readFile filename
  tokens  <- perform $ tokenize filename source
  ast     <- perform $ parse tokens
  context <- perform $ elaborate ast
  -- Note: context is shown by the elaboration process itself
  return ()

doRunAllTests :: IO ()
doRunAllTests = do
  runTestSuite $
    TestSuite "Trucha" [
      Test.Lexer.tests
    , Test.Parser.tests
    , Test.Elab.tests
    ]

perform :: Either ErrMsg a -> IO a
perform x = case x of
              Left msg     -> die msg
              Right result -> return result

displayUsage :: IO ()
displayUsage = hPutStrLn stderr msg
  where
    msg =
      unlines [
        "Usage:"
      , "  trucha -t file.tru      Tokenize"
      , "  trucha -p file.tru      Parse"
      , "  trucha -e file.tru      Elaborate"
      , "  trucha -T               Run all tests"
      ]

