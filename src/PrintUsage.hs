-- @Author: rjules
-- @Date:   2019-04-25 23:41:29
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-25 23:43:13

module PrintUsage where

import System.Exit

printUsage :: IO()
printUsage = do
    putStrLn("USAGE: ./imageCompressor n e IN")
    putStrLn("")
    putStrLn("n     number of colors in the final image")
    putStrLn("e     convergence limit")
    putStrLn("in    path to the file containing the colors of the pixels")
    exitWith $ ExitFailure 84
