
module Main where

import System.IO(hPutStrLn, stderr)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import Lexer(tokenize)
import Parser(parse)
import Pprint(pprintProgram)
import Checker(check)

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t", input] = runTokenizer input
run ["-p", input] = runParser input
run [input]       = runChecker input
run _             = usage

runTokenizer :: String -> IO ()
runTokenizer filename = do
  source <- readFile filename
  case tokenize source of
    Left errmsg  -> die errmsg
    Right tokens -> do
      mapM_ (putStrLn . show) tokens

runParser :: String -> IO ()
runParser filename = do
  source <- readFile filename
  case tokenize source of
    Left errmsg  -> die errmsg
    Right tokens -> do
      case parse tokens of
        Left errmsg   -> die errmsg
        Right program -> putStr (pprintProgram program)

runChecker :: String -> IO ()
runChecker filename = do
  source <- readFile filename
  case tokenize source of
    Left errmsg  -> die errmsg
    Right tokens -> do
      case parse tokens of
        Left errmsg   -> die errmsg
        Right program -> do
          case check program of
            Left errmsg -> die errmsg
            Right result -> do
              return ()

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  fia -t foo.fia      tokenize"
  putStrLn "  fia -p foo.fia      parse"
  putStrLn "  fia foo.fia         check"

die :: String -> IO ()
die msg = do
  hPutStrLn stderr ("---ERROR---")
  hPutStrLn stderr msg
  exitFailure

