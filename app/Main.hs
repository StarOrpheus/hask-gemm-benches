module Main where

import Lib
import System.Environment
import Control.Monad
import Control.Exception

newtype TestFailedException = TestFailedException String

instance Show TestFailedException where
  show (TestFailedException s) = s

instance Exception TestFailedException

simpleEq = Test (RegularTest "aaa" "aaa") "data equals query"

simpleYes = Test (RegularTest "aaa" "a") "simple char search - positive"

simpleNo = Test (RegularTest "aaa" "b") "simple char search - negative"

checkPrefixMove = Test (RegularTest "ababar" "abar") "naive prefix automato test"

simpleEnoent = Test GenerateEnoentTest "simple enoent run"

simpleReadErr = Test GenerateReadErrTest "simple read err test"

simpleManyArgs = Test GenerateBadArgsTest "simple many args test"

loadBibleDataset :: IO [Test]
loadBibleDataset = do
  bible <- readFile "assets/kjvbible.txt"
  return [ Test (RegularTest bible "lewd") "Bible test 1"
         , Test (RegularTest bible "") "Bible test 2"
         , Test (RegularTest bible "foobarbaz") "Bible test 3"
         , Test (RegularTest (bible ++ "foobarbaz") "foobarbaz") "Bible test 4"
         ]

main :: IO ()
main = do
  bibleDataset <- loadBibleDataset
  let tests = [ simpleEq
              , simpleYes
              , checkPrefixMove
              , simpleNo
              , simpleEnoent
              , simpleReadErr
              , simpleManyArgs
              ]
            ++ bibleDataset
  args <- getArgs
  if length args /= 1 then do
    putStrLn "Path to student solution expected as the only one argument"
  else do
    let solution = head args
    results <- runParTests solution tests
    forM_ results checkResolution
  where
    checkResolution (TestResolution id (Left err)) = throwIO $ TestFailedException err
    checkResolution (TestResolution id _) = putStrLn $ "Test \"" ++ id ++ "\" -- OK"
