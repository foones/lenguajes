module Main where

import System.Environment
import System.IO(openFile, IOMode(..), hGetContents, hClose)
import qualified Data.Map as M

import LlecaParser(parseLleca)

import Parser(nullable, first, follow, ll1table, ll1parse)

main :: IO ()
main = do
  args <- getArgs
  if length args == 2
   then
     let grammarFile = args !! 0
         inputFile   = args !! 1
      in run grammarFile inputFile
   else putStrLn ("Use: lleca grammar.ll input.txt\n" ++
                     "     lleca --debug")

run :: String -> String -> IO ()
run grammarFile inputFile = do
  hGrammar <- openFile grammarFile ReadMode
  grammar <- parseLleca <$> hGetContents hGrammar
  if inputFile == "--debug"
   then do
     let nullableDict = nullable grammar
         firstDict    = first nullableDict grammar
         followDict   = follow nullableDict firstDict grammar
      in do
        putStrLn "== Grammar =="
        putStrLn . show $ grammar

        putStrLn "== Nullable symbols =="
        printDict nullableDict

        putStrLn "== First sets =="
        printDict firstDict

        putStrLn "== Follow sets =="
        printDict followDict

        putStrLn "== LL1 table =="
        printDict (ll1table grammar)
   else do
     hInput <- openFile inputFile ReadMode
     input <- hGetContents hInput
     putStrLn . show $ ll1parse grammar input
     hClose hInput
     hClose hGrammar
  where
    printDict :: (Show k, Show v) => M.Map k v -> IO ()
    printDict = mapM_ (putStrLn . show) . M.toList

