module Main where

import System.IO(hPutStrLn, stderr, hSetBuffering,
                 stdin, stdout, BufferMode(..))
import System.Environment(getArgs)

import Control.Monad.Except

import Error(Error(..))
import Position(positionRegion)
import Lexer.Lexer(tokenize)
import Parser.Reader(readSource)
import Parser.Parser(parse, parseAndGetNamespace)
import Desugaring.Desugaring(desugarProgram)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  run args

run :: [String] -> IO ()
run ["-t", input] = runTokenizer input
run ["-r", input] = runReader input
run ["-p", input] = runParser input
run [input]       = runDesugaring input
run _             = usage

runComputationThen c k = 
  do r <- runExceptT c
     either die k r

runTokenizer :: String -> IO ()
runTokenizer filename = 
  runComputationThen
   (do source <- ExceptT (readFile filename >>= (return . Right))
       tokens <- ExceptT $ return $ tokenize filename source
       return tokens)
   (mapM_ (putStrLn . show))

runReader :: String -> IO ()
runReader filename = 
  runComputationThen
   (do tokens <- ExceptT $ readSource filename
       return tokens)
   (\ tokens -> mapM_ (putStrLn . show) tokens)

runParser :: String -> IO ()
runParser filename = 
  runComputationThen
   (do tokens  <- ExceptT $ readSource filename
       program <- ExceptT $ return $ parse tokens
       return program)
   (\ program -> putStrLn $ show program)

runDesugaring :: String -> IO ()
runDesugaring filename = 
  runComputationThen
   (do tokens  <- ExceptT $ readSource filename
       program <- ExceptT $ return $ parse tokens
       decls   <- ExceptT $ return $ desugarProgram program
       return decls)
   (\ decls -> return ())

usage :: IO ()
usage = do
  putStrLn ""
  putStrLn "        \"La encrucijada te parece abierta"
  putStrLn "         y la vigila, cuadrifronte, Jano.\""
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  hdp -t foo.hdp      tokenize"
  putStrLn "  hdp -r foo.hdp      tokenize (including dependencies)"
  putStrLn "  hdp -p foo.hdp      parse"
  putStrLn "  hdp foo.hdp         typecheck and evaluate"

die :: Error -> IO ()
die e = do
  hPutStrLn stderr ("---ERROR---")
  hPutStrLn stderr (positionRegion (errorPosition e))
  hPutStrLn stderr "---"
  hPutStrLn stderr (errorMessage e)

