-- @Author: rjules
-- @Date:   2019-04-28 07:34:08
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-28 21:43:04

module GetClusters (
    getCluster,
    printClusters,
    parserArg
) where

import Text.Printf


import CheckArgs
import CheckFile
import RandList


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

printCentroid :: Centroid -> IO ()
printCentroid centroid = do
    printf "(%.2f,%.2f,%.2f)\n-\n" (centrR centroid) (centrG centroid) (centrB centroid)

printClusters :: [Cluster] -> IO ()
printClusters [] = return ()
printClusters (cl:clusters) = do
    putStrLn "--"
    -- printf "(%f,%f,%f)\n-\n" (centrR (centroid cl)) (centrG (centroid cl)) (centrB (centroid cl))
    printCentroid (centroid cl)
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
    | first < second = findClosest' pixel lcs mc
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