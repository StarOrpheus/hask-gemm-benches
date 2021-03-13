module Main where

import Lib
import System.Environment
import Control.Monad
import Control.Exception

newtype TestFailedException = TestFailedException String 
    deriving (Show)

instance Exception TestFailedException

simpleEq = Test "aaa" "aaa" "data equals query"

simpleYes = Test "aaa" "a" "simple char search - positive"

simpleNo = Test "aaa" "b" "simple char search - negative"

checkPrefixMove = Test "ababar" "abar" "naive prefix automato test"

main :: IO ()
main = do
    let tests = [ simpleEq
                , simpleYes
                , checkPrefixMove
                , simpleNo
                ]
    args <- getArgs
    if length args /= 1 then do
        putStrLn "Path to student solution expected as the only one argument"
    else do
        let solution = head args
        results <- runParTests solution tests
        forM_ results checkResolution

    where 
        checkResolution (TestResolution id (Left err)) = throwIO $ TestFailedException err
        checkResolution (TestResolution id _) = putStrLn $ "Test \"" ++ id ++ "\" -- OK!"
        