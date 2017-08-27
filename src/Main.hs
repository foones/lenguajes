
module Main(main) where

import System.IO
import System.Environment

import Lexer(tokenize)
import Parser(parse)
import Serializer(serialize)
import Compiler(compile)
import Interpreter(eval)

main :: IO ()
main = do
  x <- getArgs
  case x of
    ["-a", filename] -> parseAndPrint filename
    ["-c", filename] -> parseAndCompile filename
    [filename]       -> parseAndEval filename
    _ -> putStr (
             "Uso: cuca <archivo>      -- interpretar\n" ++
             "     cuca -a <archivo>   -- imprimir AST\n" ++
             "     cuca -c <archivo>   -- compilar\n"
         )

parseAndPrint :: String -> IO ()
parseAndPrint filename = do
  contents <- readFile filename
  putStr (serialize . parse . tokenize $ contents)

parseAndCompile :: String -> IO ()
parseAndCompile filename = do
  contents <- readFile filename
  putStr (compile . parse . tokenize $ contents)

parseAndEval :: String -> IO ()
parseAndEval filename = do
  contents <- readFile filename
  eval . parse . tokenize $ contents

