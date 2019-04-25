-- @Author: rjules
-- @Date:   2019-04-24 15:13:52
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-25 23:45:13

module Main where
import System.Environment

import CheckArgs
import PrintUsage

parserArg :: [String] -> IO ()
parserArg args = do
    checkN (args !! 0)
    checkE (args !! 1)
    contenu <- readFile (args !! 2)
    return ()

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 3
        then parserArg args
        else printUsage
