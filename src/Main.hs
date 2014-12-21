module Main where

import Minesweeper
import Types


minePoints :: [Point]
minePoints = [(2,1), (4,6), (6,2), (3,5), (7,2)]

main :: IO ()
main = do
    let game = createGame 8 8 minePoints
    gameLoop game
    

gameLoop :: Game -> IO ()
gameLoop game = do
    putStrLn $ show game

    move <- getUserMove
    let game' = reveal game move

    gameLoop game'


getUserMove :: IO Point
getUserMove = do
    putStrLn "Please enter your next move's X coordinate: "
    x <- getLine
    putStrLn "Please enter your next move's Y coordinate: "
    y <- getLine
    return ((read x), (read y))