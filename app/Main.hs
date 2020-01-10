module Main where

import Compressor
import System.Environment
import Data.List
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [n, e, file] -> imageCompressor (read n) ((read e) :: Double) file
        otherwise -> putStrLn "USAGE: ./imageCompressor n e IN\n\n\tn\tnumber of colors in the final image\n\te\tconvergence limit\n\tIN\tpath to the file containing the colors of the pixels"
