{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative
import           System.Environment
import qualified System.IO.Strict as Strict
import           Text.AsciiArt

data Mode
    = Inplace FilePath
    | Read FilePath
    | Pipe

main :: IO ()
main = do
    mode <- getArgs <&> \case
        []           -> Pipe
        ["-i", file] -> Inplace file
        [file]       -> Read file
    input <- case mode of
        Inplace file -> Strict.readFile file
        Read    file -> readFile file
        Pipe         -> getContents
    let inputLines = lines input
        plane = planeFromList ' ' inputLines
        width = maximum (fmap length inputLines)
        height = length inputLines
    let unicodeArt
            = unlines
            . fmap trimRight
            . planeToList height width
            . renderAsciiToUnicode
            $ plane
    case mode of
        Inplace file -> writeFile file unicodeArt
        Read    _    -> putStr unicodeArt
        Pipe         -> putStr unicodeArt

trimRight :: String -> String
trimRight = reverse . dropWhile (== ' ') . reverse

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
