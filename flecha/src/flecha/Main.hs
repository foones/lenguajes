import System.Environment
import System.IO(openFile, IOMode(..), hGetContents, hClose)

import AST(ToJSON(..))
import Parser(parse)
import Compiler(compileProgram)

main :: IO ()
main = do
  args <- getArgs
  runFlecha args

runFlecha :: [String] -> IO ()
runFlecha ["--parse", inputFile] = do
  hInputFile <- openFile inputFile ReadMode
  ast <- parse <$> hGetContents hInputFile
  putStrLn . show . toJSON $ ast
runFlecha [inputFile] = do
  hInputFile <- openFile inputFile ReadMode
  ast <- parse <$> hGetContents hInputFile
  putStrLn (compileProgram ast)
runFlecha _ = putStrLn . unlines $ usage
  where
    usage = [
      "Usage:",
      "  flecha --parse input.fl",
      "  flecha input.fl"
     ]

