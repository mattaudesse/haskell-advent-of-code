{-# LANGUAGE OverloadedStrings #-}

import              GHC.IO                      (FilePath)

inputFile :: FilePath
inputFile = "data/day-one/input.txt"

climb :: Char -> Int
climb '(' = 1
climb ')' = -1
climb _   = 0

follow :: String -> Int
follow = foldl (\acc dir -> acc + climb dir) 0

toBasementOne :: String -> Int
toBasementOne dirs = trek dirs 0 0 where
    trek (d:ds) steps floor
        | floor == -1   = steps
        | otherwise     = trek ds (steps + 1) (floor + climb d)

main :: IO () 
main = do
    directions <- readFile inputFile
    let floor               = follow directions 
    let stepsToBasementOne  = toBasementOne directions
    print floor
    print stepsToBasementOne
