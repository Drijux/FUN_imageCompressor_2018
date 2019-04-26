-- @Author: rjules
-- @Date:   2019-04-26 01:29:48
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-26 03:06:51

module Split where

split :: String -> Char -> [String]
split [] _ = [""]
split (c:cs) delim
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where
        rest = split cs delim
