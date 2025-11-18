{-# LANGUAGE ExistentialQuantification #-}

module Test.Test(TestSuite(..), runTestSuite) where

import Utils(joinS)

data TestSuite =
    forall a. (Eq a, Show a) =>
    TestCase {
      name     :: String
    , obtained :: a
    , expected :: a
    }
  | TestSuite {
      name      :: String
    , testCases :: [TestSuite]
    }

----

runTestSuite :: TestSuite -> IO ()
runTestSuite suite = do
    rec 0 suite
    return ()
  where
    width :: Int
    width = 60
    rec :: Int -> TestSuite -> IO Tally
    rec indentation (TestSuite name testCases) = do
      putStrLn (margin indentation ++ "Test suite: " ++ name)
      tallys <- mapM (rec (indentation + 1)) testCases
      putStr (margin indentation ++ "End test suite: " ++ name)
      let tally = foldr addTally zeroTally tallys
      putStrLn (" (" ++ show tally ++ ")")
      return tally
    rec indentation (TestCase name obtained expected) = do
      putStr (margin indentation ++ "Test case: " ++ paddedRight width '.' name)
      if expected == obtained
       then do
         putStrLn "[OK]"
         return $ Tally 1 0
       else do
         putStrLn "[FAILED]"
         putStrLn (margin (indentation + 1) ++ "Expected: " ++ show expected)
         putStrLn (margin (indentation + 1) ++ "Obtained: " ++ show obtained)
         return $ Tally 0 1

----

margin :: Int -> String
margin n = replicate (2 * n) ' '

paddedRight :: Int -> Char -> String -> String
paddedRight width filler str =
  str ++ replicate (width - length str) filler

data Tally = Tally {
               passed :: Integer
             , failed :: Integer
             }

instance Show Tally where
  show (Tally passed failed) =
    joinS "/" $ filter (/= "") [
     if passed == 0
      then ""
      else show passed ++ " passed"
    ,
     if failed == 0
      then ""
      else show failed ++ " FAILED"
    ]

zeroTally :: Tally
zeroTally = Tally 0 0

addTally :: Tally -> Tally -> Tally
addTally (Tally p1 f1) (Tally p2 f2) = Tally (p1 + p2) (f1 + f2)

