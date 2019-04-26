-- @Author: rjules
-- @Date:   2019-04-24 15:13:52
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-26 09:05:11

module Main where
import System.Environment
import System.Exit
import Control.Exception

import CheckArgs
import PrintUsage
import Split
import CheckFile


parserArg :: [String] -> IO (Int, Float, [Pixel])
parserArg (n:e:file:[]) = do
    pixels <- checkFile file
    print checkedN
    print checkedE
    printList print pixels
    return (checkedN, checkedE, pixels)
    where
        checkedN = checkN n
        checkedE = checkE e
parserArg _ = error "Number of argument is invalid."

printList :: (Show a) => (a -> IO ()) -> [a] -> IO ()
printList _ [] = return ()
printList f (x:xs) = do
    f x
    printList f xs

main :: IO ()
main = do
    args <- getArgs
    res <- try (parserArg args) :: IO (Either SomeException (Int, Float, [Pixel]))
    case res of
        Left err -> do
            print err
            exitWith $ ExitFailure 84
        Right _ -> exitSuccess
