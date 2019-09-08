module Main where

import System.IO
import System.Environment(getArgs)

import Lang
import Lexer
import Parser

--
--import System.Console.Readline(readline, addHistory)

---- if System.Console.Readline is not available
---- resort to poor man's version
----
readline :: String -> IO (Maybe String)
readline prompt = do
    putStr prompt
    hFlush stdout
    line <- getLine
    return $ Just line

addHistory :: String -> IO ()
addHistory _ = return ()

--

prompt :: String
prompt = "8f> "

putProgram :: Program -> IO ()
putProgram []     = return ()
putProgram (p:ps) = do
    putStr $ show p
    putStr "\n"
    putProgram ps

checkString :: Parser -> Maybe Env -> String -> IO (Maybe Env)
checkString parse mEnv str = do
    case checkProgram mEnv =<< parse str of
      OK (env, response) -> do
                              putProgram response
                              return $ Just env
      Error msg          -> do
                              putStr ("Error:\n" ++ msg ++ "\n")
                              return Nothing

checkFile :: Maybe Env -> String -> IO (Maybe Env)
checkFile mEnv filename = do
    putStr $ "\n[Loading file " ++ filename ++ "].\n"
    f <- openFile filename ReadMode
    s <- hGetContents f
    res <- checkString parseProgram mEnv s
    hClose f
    putStr "done.\n"
    return res

checkFiles :: Maybe Env -> [String] -> IO (Maybe Env)
checkFiles mEnv []       = return mEnv
checkFiles mEnv (fn:fns)
    | head fn == '-' = checkFiles mEnv fns
    | otherwise      = do
        mEnv' <- checkFile mEnv fn
        maybe (fail "aborted") (flip checkFiles fns . Just) mEnv'

toplevel :: Maybe Env -> IO ()
toplevel mEnv = do
    mStr <- readline prompt
    case mStr of
        Nothing     -> return ()
        Just "exit" -> return ()
        Just "quit" -> return ()
        Just "bye"  -> return ()
        Just str -> do
            addHistory str
            mEnv' <- checkString parseQuery mEnv str
            maybe (toplevel mEnv) (toplevel . Just) mEnv'

banner :: String
banner = " ___ _      _   _    __     _    _\n\
         \| __(_)__ _| |_| |_ / _|___| |__| |\n\
         \| _|| / _` | ' \\  _|  _/ _ \\ / _` |\n\
         \|___|_\\__, |_||_\\__|_| \\___/_\\__,_|\n\
         \      |___/\n\
         \Copyright (c) 2012 - Pablo Barenbaum <foones@gmail.com>\n"

usage :: String
usage = "Usage: eightfold [options]\n" ++
        "Options:\n" ++
        "    file.8f     read the given input file\n" ++
        "    -s          do not start toplevel interaction\n"

main :: IO ()
main = do
    argv <- getArgs

    putStr banner

    if length argv == 0 || "-h" `elem` argv || "--help" `elem` argv
     then putStr usage
     else return ()

    putStr "\n"
    res <- checkFiles Nothing argv

    if "-s" `elem` argv
     then return ()
     else toplevel res

