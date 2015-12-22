{-# LANGUAGE OverloadedStrings #-}

module Main where

import  GHC.IO                      (FilePath)
import  Control.Applicative         ((<$>))
import  Data.List                   (sort)
import  Data.List.Split             (splitOn)

inputFile :: FilePath
inputFile = "data/day-two/input.txt"

type Dimensions = (Int, Int, Int) 

area :: Dimensions -> Int
area (l, w, h) = len * 2 + breadth * 2 + height * 2 + least
    where len       = l * w
          breadth   = w * h
          height    = h * l
          least     = minimum [len, breadth, height]

ribbonLength :: Dimensions -> Int
ribbonLength (l, w, h) = let smallest   = sort [l, w, h]
                             l1         = head smallest
                             l2         = head $ tail smallest
                             ribbon     = l1 * 2 + l2 * 2
                             bow        = l * w * h
                         in ribbon + bow

dimensions :: String -> Dimensions
dimensions line = let dims  = splitOn "x" line
                      l     = read $ head dims
                      w     = read $ head $ tail dims
                      h     = read $ head $ tail $ tail dims
                  in (l, w, h)

main = do
    dimensionData           <- lines <$> readFile inputFile
    let presentDimensions   =  map dimensions dimensionData 
    let totalArea           =  sum $ map area presentDimensions
    let totalRibbon         =  sum $ map ribbonLength presentDimensions
    putStrLn $ "Total square feet of wrapping paper: "  ++ show totalArea
    putStrLn $ "Total feet of ribbon: "                 ++ show totalRibbon 
