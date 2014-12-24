{-#LANGUAGE RecordWildCards #-}
--on Mac compile using: ghc --make interface.hs
module Interface where
import Graphics.UI.WX hiding (Point)
import Graphics.UI.WXCore hiding (Point)
import Control.Monad
import Data.Traversable hiding (get, sequence, mapM)
import Control.Monad.State.Lazy hiding (get)
import System.Random

import Minesweeper
import Types
import Square
import Solver

-- Screen shown to user when the program starts, shows difficulty levels and allows
-- the user to select one of them 
splashScreen :: IO ()
splashScreen
    = do  
        vbitmap <- variable [value := Nothing]

        f <- frame [ text := "Minesweeper", clientSize := sz 300 400, resizeable := False]
        p <- panel f [on paint := onPaint vbitmap, bgcolor := white, fullRepaintOnResize := False]

        -- Buttons
        st <- staticText f [ text := "Choose difficulty", position := pt 100 185]
        b <- button p [ text := "easy" , position := pt 85 220, clientSize := sz 150 25, on command := close f >> startGame 9]
        b <- button p [ text := "medium" , position := pt 85 245, clientSize := sz 150 25 , on command := close f >> startGame 14]
        b <- button p [ text := "hard" , position := pt 85 270, clientSize := sz 150 25 , on command := close f >> startGame 24 ]

         -- Background image on the slashscreen
        openImage p vbitmap "img/background.bmp"
        set p [clientSize := sz 300 400]
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

-- Produces a minesweeper button for the 
makeMineSweeperButton :: Frame() -> Int -> Int -> IO (BitmapButton())
makeMineSweeperButton f r c = bitmapButton f [picture := "img/water.bmp", text := "Ok" , position := pt ((31*c)+10) ((31*r)+120)]

-- Creates a row of minesweeper buttons
createGuiGridRow :: Int -> Frame() -> Int -> [IO (BitmapButton())]
createGuiGridRow s f r = map (makeMineSweeperButton f r) [0..s]

-- creates a grid of minesweeper buttons
createGuiGrid :: Int -> Frame() -> [[IO (BitmapButton())]]
createGuiGrid s f = map (createGuiGridRow s f) [0..s]

mapAccumM :: Monad m => (c -> a -> m (c,b)) -> c -> [a] -> m (c,[b])
mapAccumM f init xs = do
  (acc,rev) <- foldM (\(acc,ys) x -> do
                        (acc',y) <- f acc x
                        return (acc',y:ys)) (init,[]) xs
  return (acc, reverse rev)

scalarToPoint :: Game -> Int -> (Int, Int)
scalarToPoint Game{..} scalar = (scalar `mod` (length board), quot scalar (length board))

startGame ::  Int -> IO ()
startGame s
    = do     
             vbitmap <- variable [value := Nothing]

             rng <- newStdGen
             gameState <- varCreate $ createGame (s+1) (s+1) (s*2) rng

             f <- frame [ text := "Minesweeper", clientSize := sz ((31*(s+1))+20) ((31*(s+1))+130), resizeable := False]
             p <- panel f [on paint := onPaint vbitmap, fullRepaintOnResize := False, position := pt (quot (((31*(s+1))+20)-316) 2) 10]
             set p [clientSize := sz 330 80]

             let buttons = createGuiGrid s f
             tiles <- sequence $ concat buttons

             mapAccumM (\i x -> do 
                r <- set x [on click := onLeftClick p vbitmap gameState f tiles x i, on clickRight := onRightClick p vbitmap gameState f tiles x i]
                return (i+1, r)) 0 tiles

             
             refreshTiles gameState f tiles 
             refreshBanner p vbitmap gameState

             bp <- panel f [position := pt (quot (((31*(s+1))+20)-300) 2) 90]
             b <- button bp [ text := "New Game" , position := pt 4 4, clientSize := sz 150 25, on command := close f >> splashScreen]
             b <- button bp [ text := "Attempt Move" , position := pt 150 4, clientSize := sz 150 25, on command := attemptMove f gameState tiles ]
             set bp [clientSize := sz 330 30]
    where
        refreshTiles gameState f tiles
            = do
                -- 
                mine <- bitmapCreateFromFile "img/mine.bmp"
                water <- bitmapCreateFromFile "img/water.bmp"
                flag <- bitmapCreateFromFile "img/flag.bmp"

                -- 
                zero <- bitmapCreateFromFile "img/0.bmp"
                one <- bitmapCreateFromFile "img/1.bmp"
                two <- bitmapCreateFromFile "img/2.bmp"
                three <- bitmapCreateFromFile "img/3.bmp"
                four <- bitmapCreateFromFile "img/4.bmp"
                five <- bitmapCreateFromFile "img/5.bmp"
                six <- bitmapCreateFromFile "img/6.bmp"
                seven <- bitmapCreateFromFile "img/7.bmp"
                eight <- bitmapCreateFromFile "img/8.bmp"

                mapAccumM (\i x -> do 
                    game <- varGet gameState

                    case squareState game (scalarToPoint game i) of
                        (VisibleMineSquare)     -> bitmapButtonSetBitmapLabel x mine
                        (HiddenMineSquare)      -> bitmapButtonSetBitmapLabel x water
                        (HiddenNumSquare _)     -> bitmapButtonSetBitmapLabel x water
                        (FlaggedSquare _)       -> bitmapButtonSetBitmapLabel x flag

                        -- Apologies about the next few lines, we attempted to insert text
                        -- into the buttons on top of image but it messed up the
                        -- rendering, so unfortunately we are dealing with providing a case for each number
                        (VisibleNumSquare 0)  -> bitmapButtonSetBitmapLabel x zero 
                        (VisibleNumSquare 1)  -> bitmapButtonSetBitmapLabel x one 
                        (VisibleNumSquare 2)  -> bitmapButtonSetBitmapLabel x two 
                        (VisibleNumSquare 3)  -> bitmapButtonSetBitmapLabel x three 
                        (VisibleNumSquare 4)  -> bitmapButtonSetBitmapLabel x four 
                        (VisibleNumSquare 5)  -> bitmapButtonSetBitmapLabel x five 
                        (VisibleNumSquare 6)  -> bitmapButtonSetBitmapLabel x six 
                        (VisibleNumSquare 7)  -> bitmapButtonSetBitmapLabel x seven 
                        (VisibleNumSquare 8)  -> bitmapButtonSetBitmapLabel x eight 

                    return (i+1, Nothing)) 0 tiles
        -- attemptMove tires to use theautosolver in other to generate a result 
        attemptMove f gameState tiles
            = do
                game <- varGet gameState
                varSet gameState $ performMove game $ frontierEquations game $ visibleFrontier game                
                game <- varGet gameState

                refreshTiles gameState f tiles
                putStrLn $ show game
        -- Leftclick is the reveal action in the minesweeper application
        onLeftClick sw vbitmap gameState f out ok i pt
            = do
                game <- varGet gameState
                varSet gameState $ reveal game $ scalarToPoint game i

                refreshTiles gameState f out
                refreshBanner sw vbitmap gameState

        -- Right click is the flagging action within minesweeper.s
        onRightClick sw vbitmap gameState f out ok i pt
            = do
                game <- varGet gameState
                varSet gameState $ flag game $ scalarToPoint game i
                game <- varGet gameState
                refreshTiles gameState f out

                refreshBanner sw vbitmap gameState
        refreshBanner sw vbitmap gameState
            = do
                game <- varGet gameState
                case game of
                    (Game Won _ _)  -> openImage sw vbitmap "img/win.bmp"
                    (Game Lost _ _)  -> openImage sw vbitmap "img/lose.bmp"
                    (Game _ _ _)  ->openImage sw vbitmap "img/topbanner.bmp"
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
