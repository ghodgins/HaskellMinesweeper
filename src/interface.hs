module Main where
import Graphics.UI.WX

--on Mac compile using: ghc --make interface.hs

w = 200
h = 200
main :: IO ()
main
  = start hello

hello :: IO ()
hello
  = do   f <- frame [ text := "Minesweeper", clientSize := sz w h ]
         p <- panel f []
         st <- staticText p [ text := "Choose difficulty" ]
         b <- button p [ text := "easy" , position := pt 5 20 ,on command := startgame  10]
         b <- button p [ text := "medium" , position := pt 5 45 , on command := startgame  20]
         b <- button p [ text := "hard" , position := pt 5 70 ,on command := startgame  30 ]

         set p [clientSize := sz w h]


startgame ::  Int -> IO ()
startgame a 
    = do     f <- frame [ text := "Minesweeper", clientSize := sz w h ]
             p <- panel f []
            -- st <- staticText p [ text := "lets go" ]
             b <- buttons a p
             set p [clientSize := sz w h]

--need to change to return a list of buttons
-- this method is to create the x number of buttons based on level of difficulty.. currently creates 1 button
buttons :: Int -> Panel () ->  IO (Button ())
buttons 0 p = return                    --return once all buttons are created
buttons a p = button p [text := "_", position := pt 10 10]