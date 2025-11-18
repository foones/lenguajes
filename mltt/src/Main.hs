import System.Environment(getProgName, getArgs)

import Parser(tokenize, parseProgram)
import Pprint(pprintProgram)
import Checker(checkProgram)

main :: IO ()
main = do
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t", filename] = do
  source <- readFile filename
  mapM_ (putStrLn . show) . tokenize $ source
  return ()
run ["-p", filename] = do
  source <- readFile filename
  putStr . pprintProgram . parseProgram . tokenize $ source
  return ()
run [filename] = do
  source <- readFile filename
  checkProgram . parseProgram . tokenize $ source
  return ()
run _ = do
  progName <- getProgName
  putStrLn "Usage:"
  putStrLn ("  " ++ progName ++ " -t <archivo>      Tokenize")
  putStrLn ("  " ++ progName ++ " -p <archivo>      Parse")
  putStrLn ("  " ++ progName ++ " <archivo>         Typecheck")

