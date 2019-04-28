-- @Author: rjules
-- @Date:   2019-04-27 23:28:39
-- @Last Modified by:   rjules
-- @Last Modified time: 2019-04-28 06:06:49

module RandList (
    getRandPixel,
    pixelToCentroid
) where

import System.Random
import CheckFile

pixelToCentroid :: Pixel -> Centroid
pixelToCentroid pixel = Centroid r g b
    where
        r = fromIntegral (colorR $ color pixel) :: Float
        g = fromIntegral (colorG $ color pixel) :: Float
        b = fromIntegral (colorB $ color pixel) :: Float

removeArray :: [a] -> Int -> [a]
removeArray xs idx = start ++ end
    where
        start = take idx xs
        end = drop (idx + 1) xs

getRandElem :: [a] -> Int -> IO [a]
getRandElem xs n = getRandElem' xs n []

getRandElem' :: [a] -> Int -> [a] -> IO [a]
getRandElem' [] _ randElems = return randElems
getRandElem' _ 0 randElems = return randElems
getRandElem' xs n randElems = do
    let len = length xs
    idx <- randomRIO (0, (len - 1) :: Int)
    getRandElem' (removeArray xs idx) (n-1) ((xs!!idx) : randElems)

getRandPixel :: [Pixel] -> Int -> IO [Centroid]
getRandPixel pixels n = do
    randPixels <- getRandElem pixels n
    return $ map pixelToCentroid randPixels