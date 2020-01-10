module Compressor
    ( imageCompressor,
    getRandomNumber,
    getRandomPixelColor,
    ) where
import System.Environment
import Data.List
import System.IO
import System.Exit
import Parsing
import Display

getRandomNumber :: Int -> Int
getRandomNumber seed = seed

getRandomPixelColor :: [Pixel] -> Int -> Color
getRandomPixelColor pixels seed = getPixelColor (head (snd (splitAt (getRandomNumber seed) pixels)))

defineClusters :: Int -> [Pixel] -> [Color] -> Int -> [Color]
defineClusters 0 pixels colors seed = colors
defineClusters n pixels colors seed
    | (elem (getRandomPixelColor pixels seed) colors) = defineClusters n pixels colors (seed + 1)
    | otherwise = defineClusters (n - 1) pixels ((getRandomPixelColor pixels seed):colors) 0

fillAllClusters :: [Color] -> [Color] -> [Pixel] -> [[Pixel]] -> [[Pixel]]
fillAllClusters _ [] pixel buckets = buckets
fillAllClusters clusters (x:xs) pixel buckets = fillAllClusters clusters xs pixel (buckets ++ [fillCluster clusters x pixel])
    where
        fillCluster :: [Color] -> Color -> [Pixel] -> [Pixel]
        fillCluster clusters cluster colors = [x | x <- colors, (fst (findClosest clusters x (Color 1000 1000 1000)) == cluster)]
        findClosest :: [Color] -> Pixel -> Color -> (Color, Pixel)
        findClosest [] pixel closest = (closest, pixel)
        findClosest (x:xs) pixel closest
            | (distColor x (getPixelColor pixel)) <= (distColor closest (getPixelColor pixel)) = findClosest xs pixel x
            | otherwise = findClosest xs pixel closest

checkConvergence :: Double -> [Color] -> [Color] -> Bool
checkConvergence e clusters new_clusters = isItSuperior e (map calcDifference (zip clusters new_clusters))
    where
        isItSuperior :: Double -> [Double] -> Bool
        isItSuperior _ [] = False
        isItSuperior e (x:xs)
            | x >= e = True
            | otherwise = isItSuperior e xs
        calcDifference :: (Color, Color) -> Double
        calcDifference (col1, col2) = abs $ (distColor col1 col2)

kmeanLoop :: Int -> Double -> [Pixel] -> [Color] -> ([Color], [[Pixel]])
kmeanLoop n e pixels clusters
    | (checkConvergence e clusters new_clusters) = kmeanLoop n e pixels new_clusters
    | otherwise = (clusters, pixelsBuckets)
    where
        pixelsBuckets = (fillAllClusters clusters clusters pixels [])
        new_clusters = recalculateMean pixelsBuckets []
        recalculateMean :: [[Pixel]] -> [Color] -> [Color]
        recalculateMean [] colors = colors
        recalculateMean (x:xs) colors = recalculateMean xs (colors ++ [(Color ((sum (map getColorR (map getPixelColor x))) / (fromIntegral (length x))) ((sum (map getColorG (map getPixelColor x))) / (fromIntegral (length x))) ((sum (map getColorB (map getPixelColor x))) / (fromIntegral (length x))))])

imageCompressor :: Int -> Double -> String -> IO()
imageCompressor n e file = do
    contents <- readFile file
    let pixels = getPixelsList (lines contents) []
    let clusters = defineClusters n pixels [] 0
    let kmeanRet = (kmeanLoop n e pixels clusters)
    let final_clusters = fst kmeanRet
    let final_pixels = snd kmeanRet
    printOutput n final_clusters final_pixels
