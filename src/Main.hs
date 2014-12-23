module Main where

import Minesweeper
import Types

import System.Random

data Move = Reveal
          | Flag

main :: IO ()
main = do
    rng <- newStdGen
    let game = createGame 8 8 24 rng
    gameLoop game  

gameLoop :: Game -> IO ()
gameLoop game = do
    putStrLn $ show game

    userMove <- getUserMove
    let game' = case userMove of
                    (1, move) -> reveal game move
                    (2, move)   -> flag game move
    case game' of
        (Game Play _ _) -> gameLoop game'
        (Game Won _ _)  -> putStrLn $ "\nYou won the game!\n\n" ++ (show game')
        (Game Lost _ _)  -> putStrLn $ "\nYou lost the game!\n\n" ++ (show game')


getUserMove :: IO (Int, Point)
getUserMove = do
    putStrLn "Would you like to:\n1. Reveal or 2. Flag? "
    moveType <- getLine
    let moveType' = read $ moveType

    putStrLn "Please enter your next move's X coordinate: "
    x <- getLine
    putStrLn "Please enter your next move's Y coordinate: "
    y <- getLine

    let move = ((read x), (read y))

    return (moveType', move)
