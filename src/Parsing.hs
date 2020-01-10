module Parsing
    ( Position(..),
    Color(..),
    Pixel(..),
    getPosition,
    getPosX,
    getPosY,
    getColor,
    getColorR,
    getColorG,
    getColorB,
    getPixel,
    getPixelPosition,
    getPixelColor,
    getPixelsList,
    distColor,
    ) where
import System.Environment
import Data.List
import System.Exit

data Position = Position Double Double

instance Show Position where
    show (Position x y) = "(" ++ (takeWhile (/= '.') (show x)) ++ "," ++ (takeWhile (/= '.') (show y)) ++ ")"

instance Eq Position where
    (Position x1 y1) == (Position x2 y2) = x1 == x2 && y1 == y2

getPosX :: Position -> Double
getPosX (Position x _) = x

getPosY :: Position -> Double
getPosY (Position _ y) = y

getPosition :: String -> (Position, String)
getPosition input = (Position (read (getX input)) (read (getY input)), getRest input)
    where
        getX :: String -> String
        getX = takeWhile (/= ',') . tail . dropWhile (/= '(')
        getY :: String -> String
        getY = takeWhile (/= ')') . tail . dropWhile (/= ',')
        getRest :: String -> String
        getRest = tail . dropWhile (/= ')')


data Color = Color Double Double Double
instance Show Color where
    show (Color r g b) = "(" ++ (takeWhile (/= '.') (show r)) ++ "," ++ (takeWhile (/= '.') (show g)) ++ "," ++ (takeWhile (/= '.') (show b)) ++ ")"

instance Eq Color where
    (Color r1 g1 b1) == (Color r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2

getColor :: String -> (Color, String)
getColor input = (Color (read (getR input)) (read (getG input)) (read (getB input)), getRest input)
    where
        getR :: String -> String
        getR = takeWhile (/= ',') . tail . dropWhile (/= '(')
        getG :: String -> String
        getG = takeWhile (/= ',') . tail . dropWhile (/= ',')
        getB :: String-> String
        getB = takeWhile (/= ')') . tail . dropWhile (/= ',') . tail . dropWhile (/= ',')
        getRest :: String -> String
        getRest = tail . dropWhile (/= ')')

getColorR :: Color -> Double
getColorR (Color r _ _ ) = r

getColorG :: Color -> Double
getColorG (Color _ g _ ) = g

getColorB :: Color -> Double
getColorB (Color _ _ b ) = b

data Pixel = Pixel Position Color
instance Show Pixel where
    show (Pixel pos color) = show pos ++ " " ++ show color

getPixel :: String -> Pixel
getPixel input = Pixel pos color
        where
            pos = fst (getPosition input)
            color = fst (getColor (snd (getPosition input)))

getPixelPosition :: Pixel -> Position
getPixelPosition (Pixel pos _ ) = pos

getPixelColor :: Pixel -> Color
getPixelColor (Pixel _ color) = color

getPixelsList :: [String] -> [Pixel] -> [Pixel]
getPixelsList [] pixels = pixels
getPixelsList (x:xs) pixels = getPixelsList xs ((getPixel x):pixels)

distPixel :: Pixel -> Pixel -> Double
distPixel (Pixel _ c1) (Pixel _ c2) = distColor c1 c2

distColor :: Color -> Color -> Double
distColor (Color r1 g1 b1) (Color r2 g2 b2) = sqrt(((r1 - r2) ^ 2) + ((g1 - g2) ^ 2) + ((b1 - b2) ^ 2))
