module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Monad
import Minesweeper 
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
        b <- button p [ text := "easy" , position := pt 85 205, clientSize := sz 150 50, on command := onStartGame f 10]
        b <- button p [ text := "medium" , position := pt 85 230, clientSize := sz 150 50 , on command := onStartGame f 20]
        b <- button p [ text := "hard" , position := pt 85 255, clientSize := sz 150 50 ,on command := onStartGame f 30 ]

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
        onStartGame f size
          = do 
                close f
                startGame size

makeMineSweeperButton :: Frame() -> Int -> Int -> IO (BitmapButton())
makeMineSweeperButton f r c = bitmapButton f [picture := "water.bmp", text := "Ok" , position := pt ((31*r)+10) ((31*c)+10) ]

createGuiGridRow :: Int -> Frame() -> Int -> [IO (BitmapButton())]
createGuiGridRow s f r = map (makeMineSweeperButton f r) [0..s]


createGuiGrid :: Int -> Frame() -> [[IO (BitmapButton())]]
createGuiGrid s f = map (createGuiGridRow s f) [0..s]

startGame ::  Int -> IO ()
startGame s
    = do     f <- frame [ text := "Minesweeper", clientSize := sz ((31*(s+1))+20) ((31*(s+1))+20) ]
             p <- panel f []

             let buttons = createGuiGrid s f
             out <- sequence $ concat buttons

             mapM (\x -> set x [on click := onLeftClick f x, on clickRight := onRightClick f x]) $ out
             mapM (\x -> prepareInitial f x) $ out 

             --ok <- bitmapButton f [picture := "water.bmp", text := "Ok" ]

             --set ok [on command := onMineClick f ok]
             set p [clientSize := sz w h]
    where
        prepareInitial f ok
            = do
                bm <- bitmapCreateFromFile "water.bmp"
                bitmapButtonSetBitmapLabel ok bm
        onLeftClick f ok pt
            = do
                bm <- bitmapCreateFromFile "mine.bmp"
                bitmapButtonSetBitmapLabel ok bm
        onRightClick f ok pt
            = do
                bm <- bitmapCreateFromFile "flag.bmp"
                bitmapButtonSetBitmapLabel ok bm
