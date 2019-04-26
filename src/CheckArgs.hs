-- @Author: rjules
-- @Date:   2019-04-25 23:19:20
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-26 03:43:51

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

checkE :: String -> Float
checkE str
    | isFloat str && f > 0 = f
    | otherwise = error "Invalid e"
    where
        f = read str :: Float

checkN :: String -> Int
checkN str
    | isInteger str && f > 0 = f
    | otherwise = error "Invalid n"
    where
        f = read str :: Int
