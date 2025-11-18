module Main(main) where

import System.IO(hPutStrLn, stderr)
import System.Environment(getArgs)
import System.Exit(exitFailure)

import Utils(indent)
import Parser(tokenize, parse)
import Infer(inferTypes)
import Checker(check)

tokenizeFile :: String -> IO ()
tokenizeFile filename = do
  str <- readFile filename
  mapM_ (putStrLn . show) (tokenize str)

parseFile :: String -> IO ()
parseFile filename = do
  str <- readFile filename
  mapM_ (putStrLn . show) (parse . tokenize $ str)

inferTypesFile :: String -> IO ()
inferTypesFile filename = do
  str <- readFile filename
  let commands = parse . tokenize $ str
  case inferTypes commands of
    Left errmsg -> do
      putStrLn "---- Typechecker error"
      putStrLn errmsg
    Right commands' -> mapM_ (putStrLn . show) commands'

checkFile :: String -> IO ()
checkFile filename = do
  str <- readFile filename
  let commands = parse . tokenize $ str
  commands <-
    case inferTypes commands of
      Left errmsg -> do
        putStrLn "---- Typechecker error"
        putStrLn errmsg
        exitFailure
      Right commands' -> return commands'
  putStrLn ""
  putStrLn "---- Source"
  mapM_ (putStrLn . show) commands
  putStrLn ""
  case check commands of
    Left errmsg -> do
      putStrLn "---- Checker error"
      putStrLn (show errmsg)
    Right []    -> do
      putStrLn "---- Proof checked! :-)"
    Right holes -> do
      putStrLn "---- Pending holes:"
      flip mapM_ holes (\ (name, globalCtx, localCtx, form) -> do
          putStrLn ""
          putStrLn ("?" ++ name ++ " : " ++ show form)
          --putStrLn (indent 2 (show state))
        )

run :: [String] -> IO ()
run ["-t", filename] = tokenizeFile filename
run ["-p", filename] = parseFile filename
run ["-i", filename] = inferTypesFile filename
run [filename]       = checkFile filename
run _ = mapM_ (hPutStrLn stderr) [
          "Usage:"
        , "  fol -t file.j      Tokenize"
        , "  fol -p file.j      Parse"
        , "  fol -i file.j      Infer types"
        , "  fol file.j         Check"
        ]

main :: IO ()
main = do
  args <- getArgs
  run args

