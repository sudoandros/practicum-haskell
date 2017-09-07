module Main where

import System.IO
import Game


main :: IO ()
main = do
    hndl <- openFile "./app/field.txt" ReadMode
    str <- hGetLine hndl
    let l = read str :: [(Int, Int)]
    str <- hGetLine hndl
    let w = read str :: Int
    str <- hGetLine hndl
    let h = read str :: Int
    hClose hndl
    startGame (Field l w h True)