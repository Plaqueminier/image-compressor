module Display
    ( printOutput
    ) where
import System.Environment
import Data.List
import System.IO
import System.Exit
import Parsing
import Text.Printf

printClusterColor :: Color -> IO()
printClusterColor color = do
    putStrLn "--"
    (printf "(%.2f,%.2f,%.2f)\n" (getColorR color) (getColorG color) (getColorB color)) :: IO()
    putStrLn "-"

printAllPixels :: [Pixel] -> IO()
printAllPixels [] = return()
printAllPixels (x:xs) = do
    print x
    printAllPixels xs

printOutput :: Int -> [Color] -> [[Pixel]] -> IO()
printOutput 0 [] [] = return()
printOutput n (x:xs) (y:ys) = do
    printClusterColor x
    printAllPixels y
    printOutput (n - 1) xs ys
