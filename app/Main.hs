{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative
import           System.Environment
import           Text.AsciiArt

main :: IO ()
main = do
    (input, output) <- getArgs >>= \case
        []           -> liftA2 (,) getContents (pure putStr)
        ["-i", file] -> liftA2 (,) (readFile file) (pure (writeFile file))
        [file]       -> liftA2 (,) (readFile file) (pure putStr)
    let inputLines = lines input
        plane = planeFromList ' ' inputLines
        width = maximum (fmap length inputLines)
        height = length inputLines
    output
        . unlines
        . fmap trimRight
        . planeToList height width
        . renderAsciiToUnicode
        $ plane

trimRight :: String -> String
trimRight = reverse . dropWhile (== ' ') . reverse
