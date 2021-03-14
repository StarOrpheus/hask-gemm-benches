{-# LANGUAGE BangPatterns #-}

module Lib
    ( runParTests
    , Test(..)
    , TestResolution(..)
    ) where

import Text.Read
import Data.Either
import Data.Semigroup ((<>))

import System.IO
import System.Exit
import System.Process
import System.IO.Error
import System.Environment
import Control.Parallel.Strategies
import Control.Monad

import System.Random
import Options.Applicative

data Test = Test { testData :: FilePath
                 , testStr  :: String
                 , testName :: String
                 } deriving (Show, Eq)

data TestResolution = TestResolution { testRunId     :: String
                                     , testRunResult :: Either String ()
                                     } deriving (Show)

formatTestName :: String
               -> String
formatTestName testId = "Test - " ++ show testId ++ ".data"

createTestFile :: Test
               -> IO ()
createTestFile (Test testData _ testName) = writeFile (formatTestName testName) testData

runSolution :: FilePath   -- Solution executable path
            -> [String]   -- List of argv
            -> IO (Handle, Handle, Handle, ProcessHandle)
runSolution path args = runInteractiveProcess path args Nothing Nothing

getStudentAnswer :: FilePath    -- Solution executable path
                 -> Test
                 -> IO (Either String (String, ExitCode))
getStudentAnswer solution test@(Test _ query testId) = do
  createTestFile test
  (ans, exitCode) <- getInternal
  return $ either Left (\ans -> Right (ans, exitCode)) (checkIO ans)
  where
    getInternal = do
      (inp, out, _, pid) <- runSolution solution [formatTestName testId, query]
      !ans      <- tryIOError $ hGetLine out
      !exitCode <- waitForProcess pid
      return (ans, exitCode)
    checkExitCode exitCode ans = case exitCode of
                                   ExitSuccess      -> pure ans
                                   ExitFailure code -> makeError "Solution's exit code != 0, is " code
    checkIO ans = either (makeError "Cannot read solution's output: ") pure ans
    makeError prefix err = Left (prefix ++ show err)

getRealAnswer :: Test
              -> IO (Either String Bool)
getRealAnswer (Test _ query testId) = do
  eitherText <- tryIOError $ readFile $ formatTestName testId
  let result = hasSubStr <$> eitherText
  return $ case result of
   Left err  -> Left $ "Failed to open the test file: " ++ ioeGetErrorString err
   Right res -> Right res
  where
    hasSubStr :: String -> Bool
    hasSubStr "" = False
    hasSubStr left
      | take (length query) left == query   = True
      | otherwise                           = hasSubStr (tail left)

runParTest :: FilePath   -- Path to student's executable
           -> Test       -- Given test
           -> Eval (IO TestResolution)
runParTest solution test = do
  studentResultIo <- rpar $ getStudentAnswer solution test
  ourAnswerIo <- rseq $ getRealAnswer test
  rseq studentResultIo
  let resolutionIo = makeResolution studentResultIo ourAnswerIo
  return resolutionIo

  where
    asResolution = TestResolution (testName test)

    makeResolution studentResultIo ourAnswerIo = do studentResult <- studentResultIo
                                                    ourAnswer <- ourAnswerIo
                                                    let resolution = compareSolutions studentResult ourAnswer
                                                    return resolution

    compareSolutions :: Either String (String, ExitCode)
                     -> Either String Bool
                     -> TestResolution
    compareSolutions (Left err) _ = asResolution (Left err)
    compareSolutions (Right (out, ExitSuccess)) (Left err) = asResolution (Left $ "Error expected, but student solution returned zero: " ++ err)
    compareSolutions (Right (out, ExitFailure err)) (Left err2) = asResolution (Right ())
    compareSolutions (Right (out, code)) (Right result)
      | result && (out == "Yes")      = asResolution $ Right ()
      | not result && (out == "No")   = asResolution $ Right ()
      | otherwise                     = asResolution $ Left $ do let firstLine = "Student answer: " ++ show out ++ " with code " ++ show code
                                                                 let secondLine = "Our answer: " ++ if result then "Yes" else "No"
                                                                 firstLine ++ "\n" ++ secondLine

runParTests :: FilePath
            -> [Test]
            -> IO [TestResolution]
runParTests solution tests = do
  let testEvals = runParTest solution <$> tests
  sequence $ runEval $ sequence testEvals
