-- @Author: rjules
-- @Date:   2019-04-24 15:13:52
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-26 05:23:23

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
    return (checkN n, checkE e, pixels)
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
        Left err -> printUsage
        Right (n, e, pixels) -> do
            print n
            print e
            printList print pixels
