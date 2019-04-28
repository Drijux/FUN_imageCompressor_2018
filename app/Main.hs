-- @Author: rjules
-- @Date:   2019-04-24 15:13:52
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-28 07:17:53

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

data Cluster = Cluster {
    centroid :: Centroid,
    pixels :: [Pixel]
} deriving (Show)

data Obj = Obj {
    n :: Int,
    e :: Float,
    pixel :: [Pixel]
} deriving (Show)

printList :: (Show a) => (a -> IO ()) -> [a] -> IO ()
printList _ [] = return ()
printList f (x:xs) = do
    f x
    printList f xs

printPoint :: Point -> IO ()
printPoint point = printf "(%d,%d)" (x point) (y point)

printColor :: Color -> IO ()
printColor color = printf " (%d,%d,%d)\n" (colorR color) (colorG color) (colorB color)

printPixel :: [Pixel] -> IO ()
printPixel [] = return ()
printPixel (p:pixels) = do
    printPoint (point p)
    printColor (color p)
    printPixel pixels


printClusters :: [Cluster] -> IO ()
printClusters [] = return ()
printClusters (cl:clusters) = do
    putStrLn "--"
    printf "(%f,%f,%f)\n-\n" (centrR (centroid cl)) (centrG (centroid cl)) (centrB (centroid cl))
    printPixel (pixels cl)
    printClusters clusters

parserArg :: [String] -> IO Obj
parserArg (n:e:file:[]) = do
    pixels <- checkFile file
    return $ Obj checkedN checkedE pixels
    where
        checkedN = checkN n
        checkedE = checkE e
parserArg _ = error "Number of argument is invalid."

initCluster :: Centroid -> Cluster
initCluster centroid = Cluster centroid []

initClusters :: [Centroid] -> [Cluster]
initClusters centroid = map initCluster centroid

calcDistance :: Centroid -> Centroid -> Float
calcDistance mc sc = tot
    where
        r = ((centrR mc) - (centrR sc)) * ((centrR mc) - (centrR sc))
        g = ((centrG mc) - (centrG sc)) * ((centrG mc) - (centrG sc))
        b = ((centrB mc) - (centrB sc)) * ((centrB mc) - (centrB sc))
        tot = sqrt(r + g + b)

findClosest :: Pixel -> [Centroid] -> Centroid
findClosest pixels (c:lcs) = findClosest' pixels lcs c

findClosest' :: Pixel -> [Centroid] -> Centroid -> Centroid
findClosest' _ [] mc = mc
findClosest' pixel (c:lcs) mc
    | first <= second = findClosest' pixel lcs mc
    | otherwise = findClosest' pixel lcs c
    where
        first = calcDistance mc (pixelToCentroid pixel)
        second = calcDistance c (pixelToCentroid pixel)

addCluster :: Pixel -> [Cluster] -> Centroid -> [Cluster]
addCluster _ [] _ = []
addCluster p (cl:clusters) c
    | (centroid cl) == c = newCluster:clusters
    | otherwise = cl:addCluster p clusters c
    where
        oldPixels = pixels cl
        newCluster = Cluster c (p:oldPixels)

fillClusters :: [Pixel] -> [Cluster] -> [Centroid] -> [Cluster]
fillClusters [] clusters _ = clusters
fillClusters (p:pixels) clusters centroids =
    fillClusters pixels newClusters centroids
    where
        newClusters = addCluster p clusters mc
        mc = findClosest p centroids

getCluster :: Float -> [Pixel] -> [Centroid] -> [Cluster]
getCluster e pixels centroids = fillClusters pixels clusters centroids
    where
        clusters = initClusters centroids


launchIC :: [String] -> IO ()
launchIC args = do
    obj <- (parserArg args)
    centroid <- getRandPixel (pixel obj) (read (args !! 0) :: Int)
    let cluster = getCluster (e obj) (pixel obj) centroid
    -- print cluster
    -- print $ (length (pixels (cluster !! 0)))
    printClusters cluster

main :: IO ()
main = do
    args <- getArgs
    res <- try (launchIC args) :: IO (Either SomeException ())
    case res of
        Left err -> do
            print err
            exitWith $ ExitFailure 84
        Right _ -> exitSuccess
