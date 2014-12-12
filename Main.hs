module Main where

import Minesweeper

main :: IO ()
main = do
    let board = createEmptyBoard 8 8
    putStrLn $ show board

    putStrLn "\n"

    let modBoard = modifyBoard board (1, 1) (VisibleNumSquare 2)

    case modBoard of
        (Left msg)    -> putStrLn msg
        (Right board) -> putStrLn $ show board

    putStrLn "Hey! This isn't Minesweeper!"