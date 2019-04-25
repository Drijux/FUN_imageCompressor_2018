-- @Author: rjules
-- @Date:   2019-04-25 23:19:20
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-25 23:44:18

module CheckArgs (
    isInteger,
    isFloat,
    checkE,
    checkN
) where

import System.Exit

import PrintUsage

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

checkE :: String -> IO ()
checkE str
    | isFloat str && f > 0 = putStr("")
    | otherwise = printUsage
    where
        f = read str :: Float

checkN :: String -> IO ()
checkN str
    | isInteger str && f > 0 = return ()
    | otherwise = printUsage
    where
        f = read str :: Integer