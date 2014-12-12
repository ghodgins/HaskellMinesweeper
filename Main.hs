module Main where

import Minesweeper

main :: IO ()
main = do
    let board = createEmptyBoard 8 8
    putStrLn $ show board
    putStrLn "Hey! This isn't Minesweeper!"