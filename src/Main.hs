module Main where

import Minesweeper


minePoints :: [Point]
minePoints = [(2,1), (4,6), (6,2), (3,5), (7,2)]

main :: IO ()
main = do
    let board = createGameBoard 8 8 minePoints
    putStrLn $ show board

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