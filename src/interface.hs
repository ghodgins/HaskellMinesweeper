module Main where
import Graphics.UI.WX

--on Mac compile using: ghc --make interface.hs

w = 300
h = 400
main :: IO ()
main
  = start splashScreen

splashScreen :: IO ()
splashScreen
    = do  
        vbitmap <- variable [value := Nothing]

        f <- frame [ text := "Minesweeper", clientSize := sz w h ]
        p <- panel f [on paint := onPaint vbitmap, bgcolor := white, fullRepaintOnResize := False]
        openImage p vbitmap "background.bmp"

        st <- staticText f [ text := "Choose difficulty", position := pt 100 185]
        b <- button p [ text := "easy" , position := pt 85 205, clientSize := sz 150 50, on command := startgame  10]
        b <- button p [ text := "medium" , position := pt 85 230, clientSize := sz 150 50 , on command := startgame  20]
        b <- button p [ text := "hard" , position := pt 85 255, clientSize := sz 150 50 ,on command := startgame  30 ]

        set p [clientSize := sz w h]
    where
        openImage sw vbitmap fname
            = do
                bm <- bitmapCreateFromFile fname
                closeImage vbitmap
                set vbitmap [value := Just bm]
                bmsize <- get bm size
                set sw [virtualSize := bmsize]
                repaint sw
        closeImage vbitmap
          = do mbBitmap <- swap vbitmap value Nothing
               case mbBitmap of
                 Nothing -> return ()
                 Just bm -> objectDelete bm
        onPaint vbitmap dc viewArea
          = do mbBitmap <- get vbitmap value
               case mbBitmap of
                 Nothing -> return () 
                 Just bm -> drawBitmap dc bm pointZero False []


startgame ::  Int -> IO ()
startgame a 
    = do     f <- frame [ text := "Minesweeper", clientSize := sz w h ]
             p <- panel f []
            -- st <- staticText p [ text := "lets go" ]
             --b <- buttons a p
             set p [clientSize := sz w h]

--need to change to return a list of buttons
-- this method is to create the x number of buttons based on level of difficulty.. currently creates 1 button
--buttons :: Int -> Panel () ->  IO (Button ())
--buttons 0 p = return                    --return once all buttons are created
--buttons a p = button p [text := "_", position := pt 10 10]