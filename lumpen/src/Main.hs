
import System.IO
import System.Environment
import System.Exit

import CombParser
import Lumpen
import LumpenParser
import LumpenEval

evalString :: String -> IO ()
evalString = eval . runParser program

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <file>")
  exitFailure

evalFile :: String -> IO ()
evalFile filename = do
  f <- openFile filename ReadMode
  input <- hGetContents f  
  evalString input
  hClose f

main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
   then usage
   else evalFile (args !! 0)

