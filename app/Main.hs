-- @Author: rjules
-- @Date:   2019-04-24 15:13:52
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-05-02 16:04:02

module Main where
import System.Environment
import System.Exit
import Control.Exception
import System.Random
import Text.Printf

import CheckArgs
import PrintUsage
import Split
import CheckFile
import RandList
import GetClusters

printList :: (Show a) => (a -> IO ()) -> [a] -> IO ()
printList _ [] = return ()
printList f (x:xs) = do
    f x
    printList f xs

averageInt :: [Int] -> Float
averageInt tab = tot' / len'
    where
        tot' = fromIntegral tot :: Float
        tot = sum tab
        len = length tab
        len' = fromIntegral len :: Float

createListColorR :: Pixel -> Int
createListColorR pixel = colorR (color pixel)

createListColorG :: Pixel -> Int
createListColorG pixel = colorG (color pixel)

createListColorB :: Pixel -> Int
createListColorB pixel = colorB (color pixel)

sumCluster :: Cluster -> Cluster
sumCluster cl
    | length (pixels cl) > 0 = Cluster newcentroid (pixels cl)
    | otherwise = cl
    where
        r = map createListColorR (pixels cl)
        g = map createListColorG (pixels cl)
        b = map createListColorB (pixels cl)
        r' = averageInt r
        g' = averageInt g
        b' = averageInt b
        newcentroid = Centroid r' g' b'

runCentroid :: Float -> [Pixel] -> [Centroid] -> [Cluster] -> [Cluster]
runCentroid e pixels centroids cluster
    | cluster == cluster'' = cluster''
    | otherwise = runCentroid e pixels newCentroids cluster''
    where
    cluster' = map sumCluster cluster
    newCentroids = map centroid cluster'
    cluster'' = getCluster e pixels newCentroids

launchIC :: [String] -> IO ()
launchIC args = do
    obj <- (parserArg args)
    centroid <- getRandPixel (pixel obj) (read (args !! 0) :: Int)
    let cluster = getCluster (e obj) (pixel obj) centroid
    printClusters $ runCentroid (e obj) (pixel obj) centroid cluster

main :: IO ()
main = do
    args <- getArgs
    res <- try (launchIC args) :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith $ ExitFailure 84
        Right _ -> exitSuccess
