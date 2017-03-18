{-# LANGUAGE LambdaCase      #-}
module Main where

import           Text.AsciiArt
import           System.Environment

main :: IO ()
main = do
    input <- getArgs >>= \case
        []       -> getContents
        file : _ -> readFile file
    let inputLines = lines input
        plane = planeFromList ' ' inputLines
        width = maximum (fmap length inputLines)
        height = length inputLines
    putStrLn $ unlines $ planeToList height width $ renderAsciiToUnicode $ plane
