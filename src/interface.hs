{-#LANGUAGE RecordWildCards #-}
--on Mac compile using: ghc --make interface.hs
module Main where
import Graphics.UI.WX hiding (Point)
import Graphics.UI.WXCore hiding (Point)
import Control.Monad
import Data.Traversable hiding (get, sequence, mapM)
import Control.Monad.State.Lazy hiding (get)
import System.Random

import Minesweeper
import Types
import Square

w = 300
h = 400
main :: IO ()
main
  = start splashScreen

splashScreen :: IO ()
splashScreen
    = do  
        vbitmap <- variable [value := Nothing]

        f <- frame [ text := "Minesweeper", clientSize := sz w h, resizeable := False]
        p <- panel f [on paint := onPaint vbitmap, bgcolor := white, fullRepaintOnResize := False]
        openImage p vbitmap "background.bmp"

        st <- staticText f [ text := "Choose difficulty", position := pt 100 185]
        b <- button p [ text := "easy" , position := pt 85 220, clientSize := sz 150 25, on command := onStartGame f 9]
        b <- button p [ text := "medium" , position := pt 85 245, clientSize := sz 150 25 , on command := onStartGame f 14]
        b <- button p [ text := "hard" , position := pt 85 270, clientSize := sz 150 25 , on command := onStartGame f 24 ]

        set p [clientSize := sz w h]
    where
        openImage sw vbitmap fname
            = do
                bm <- bitmapCreateFromFile fname
                set vbitmap [value := Just bm]
                bmsize <- get bm size
                set sw [virtualSize := bmsize]
                repaint sw
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
makeMineSweeperButton f r c = bitmapButton f [picture := "water.bmp", text := "Ok" , position := pt ((31*c)+10) ((31*r)+90)]

createGuiGridRow :: Int -> Frame() -> Int -> [IO (BitmapButton())]
createGuiGridRow s f r = map (makeMineSweeperButton f r) [0..s]


createGuiGrid :: Int -> Frame() -> [[IO (BitmapButton())]]
createGuiGrid s f = map (createGuiGridRow s f) [0..s]

mapAccumM :: Monad m => (c -> a -> m (c,b)) -> c -> [a] -> m (c,[b])
mapAccumM f init xs = do
  (acc,rev) <- foldM (\(acc,ys) x -> do
                        (acc',y) <- f acc x
                        return (acc',y:ys)) (init,[]) xs
  return (acc, reverse rev)

data Move = Reveal
          | Flag

scalarToPoint :: Game -> Int -> (Int, Int)
scalarToPoint Game{..} scalar = (scalar `mod` (length board), quot scalar (length board))

startGame ::  Int -> IO ()
startGame s
    = do     
             vbitmap <- variable [value := Nothing]
             rng <- newStdGen
             gameState <- varCreate $ createGame (s+1) (s+1) 8 rng
             f <- frame [ text := "Minesweeper", clientSize := sz ((31*(s+1))+20) ((31*(s+1))+100), resizeable := False]
             p <- panel f [on paint := onPaint vbitmap, fullRepaintOnResize := False, position := pt (quot (((31*(s+1))+20)-316) 2) 10]

             let buttons = createGuiGrid s f
             out <- sequence $ concat buttons

             mapAccumM (\i x -> do 
                r <- set x [on click := onLeftClick p vbitmap gameState f out x i, on clickRight := onRightClick p vbitmap gameState f out x i]
                return (i+1, r)) 0 out

             openImage p vbitmap "topbanner.bmp"
             refreshTiles gameState f out 
             set p [clientSize := sz 330 80]
    where
        refreshTiles gameState f out
            = do
                mine <- bitmapCreateFromFile "mine.bmp"
                water <- bitmapCreateFromFile "water.bmp"
                flag <- bitmapCreateFromFile "flag.bmp"
                zero <- bitmapCreateFromFile "0.bmp"
                one <- bitmapCreateFromFile "1.bmp"
                two <- bitmapCreateFromFile "2.bmp"
                three <- bitmapCreateFromFile "3.bmp"
                four <- bitmapCreateFromFile "4.bmp"
                five <- bitmapCreateFromFile "5.bmp"

                mapAccumM (\i x -> do 
                    game <- varGet gameState

                    case squareState game (scalarToPoint game i) of
                        (VisibleMineSquare)     -> bitmapButtonSetBitmapLabel x mine
                        (HiddenMineSquare)      -> bitmapButtonSetBitmapLabel x water
                        (HiddenNumSquare _)     -> bitmapButtonSetBitmapLabel x water
                        (FlaggedSquare _)       -> bitmapButtonSetBitmapLabel x flag

                        -- Apologies about the next few lines, I attempted to insert text
                        -- into the buttons on top of image but it messed up the
                        -- rendering, so unfortunately we are dealing with providing a case for each number
                        (VisibleNumSquare 0)  -> bitmapButtonSetBitmapLabel x zero 
                        (VisibleNumSquare 1)  -> bitmapButtonSetBitmapLabel x one 
                        (VisibleNumSquare 2)  -> bitmapButtonSetBitmapLabel x two 
                        (VisibleNumSquare 3)  -> bitmapButtonSetBitmapLabel x three 
                        (VisibleNumSquare 4)  -> bitmapButtonSetBitmapLabel x four 
                        (VisibleNumSquare 5)  -> bitmapButtonSetBitmapLabel x five 
                    return (i+1, Nothing)) 0 out
        onLeftClick sw vbitmap gameState f out ok i pt
            = do
                game <- varGet gameState
                varSet gameState $ reveal game $ scalarToPoint game i
                game <- varGet gameState
                refreshTiles gameState f out
                case game of
                    (Game Won _ _)  -> openImage sw vbitmap "win.bmp"
                    (Game Lost _ _)  -> openImage sw vbitmap "lose.bmp"
                    (Game _ _ _)  -> putStrLn $ ""
                putStrLn $ show game
        onRightClick sw vbitmap gameState f out ok i pt
            = do
                game <- varGet gameState
                varSet gameState $ flag game $ scalarToPoint game i
                game <- varGet gameState
                refreshTiles gameState f out
                case game of
                    (Game Won _ _)  -> openImage sw vbitmap "win.bmp"
                    (Game Lost _ _)  -> openImage sw vbitmap "lose.bmp"
                    (Game _ _ _)  -> putStrLn $ ""
                putStrLn $ show game
        openImage sw vbitmap fname
            = do
                bm <- bitmapCreateFromFile fname
                set vbitmap [value := Just bm]
                repaint sw
        onPaint vbitmap dc viewArea
          = do mbBitmap <- get vbitmap value
               case mbBitmap of
                 Nothing -> return () 
                 Just bm -> drawBitmap dc bm pointZero False []
