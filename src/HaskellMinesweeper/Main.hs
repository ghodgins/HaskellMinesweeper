module Main where

import Graphics.UI.WX hiding (Point)

import HaskellMinesweeper.Minesweeper
import HaskellMinesweeper.Types

import HaskellMinesweeper.Interface

main :: IO ()
main = do start splashScreen