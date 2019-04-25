-- @Author: rjules
-- @Date:   2019-04-24 15:13:52
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-25 22:19:46

import System.Environment
import System.Exit

print_usage :: IO()
print_usage = do
    putStrLn("USAGE: ./imageCompressor n e IN")
    putStrLn("")
    putStrLn("n     number of colors in the final image")
    putStrLn("e     convergence limit")
    putStrLn("in    path to the file containing the colors of the pixels")
    exitWith $ ExitFailure 84

isInteger :: String -> Bool
isInteger str =
    case (reads str) :: [(Integer, String)] of
        [(_, "")] -> True
        _         -> False

isFloat :: String -> Bool
isFloat str =
    case (reads str) :: [(Float, String)] of
        [(_, "")] -> True
        _         -> False

check_e :: String -> IO ()
check_e str
    | isFloat str && f > 0 = putStr("")
    | otherwise = print_usage
    where
        f = read str :: Float

check_n :: String -> IO ()
check_n str
    | isInteger str && f > 0 = return ()
    | otherwise = print_usage
    where
        f = read str :: Integer

parserArg :: [String] -> IO ()
parserArg args = do
    check_n (args !! 0)
    check_e (args !! 1)

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 3
        then parserArg args
        else print_usage