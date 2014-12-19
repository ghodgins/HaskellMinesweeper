module Main where

import Minesweeper


minePoints :: [Point]
minePoints = [(2,1), (4,6), (6,2), (3,5)]

main :: IO ()
main = do
    let board = createGameBoard 12 12 minePoints
    putStrLn $ show board

    --let modBoard = modifyBoard board (1, 1) (VisibleNumSquare 2)
    --putStrLn $ show modBoard

    --putStrLn "Hey! This isn't Minesweeper!"

    gameLoop board
    

gameLoop :: Board -> IO ()
gameLoop board = do
    move <- getUserMove
    let newBoard = modifyBoard board move (VisibleNumSquare 5)
    putStrLn $ show newBoard
    gameLoop newBoard


getUserMove :: IO Point
getUserMove = do
    putStrLn "Please enter your next move's X coordinate: "
    x <- getLine
    putStrLn "Please enter your next move's Y coordinate: "
    y <- getLine
    return ((read x), (read y))