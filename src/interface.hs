--on Mac compile using: ghc --make interface.hs
module Main where
import Graphics.UI.WX hiding (Point)
import Graphics.UI.WXCore hiding (Point)
import Control.Monad
import Data.Traversable hiding (get, sequence, mapM)
import Control.Monad.State.Lazy hiding (get)

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

        f <- frame [ text := "Minesweeper", clientSize := sz w h ]
        p <- panel f [on paint := onPaint vbitmap, bgcolor := white, fullRepaintOnResize := False]
        openImage p vbitmap "background.bmp"

        st <- staticText f [ text := "Choose difficulty", position := pt 100 185]
        b <- button p [ text := "easy" , position := pt 85 205, clientSize := sz 150 50, on command := onStartGame f 9]
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
makeMineSweeperButton f r c = bitmapButton f [picture := "water.bmp", text := "Ok" , position := pt ((31*c)+10) ((31*r)+10)]

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

minePoints :: [Point]
minePoints = [(2,1), (4,6), (6,2), (3,5), (7,2)]

startGame ::  Int -> IO ()
startGame s
    = do     
             gameState <- varCreate $ createGame (s+1) (s+1) minePoints
             f <- frame [ text := "Minesweeper", clientSize := sz ((31*(s+1))+20) ((31*(s+1))+20) ]
             p <- panel f []

             let buttons = createGuiGrid s f
             out <- sequence $ concat buttons

             mapAccumM (\i x -> do 
                r <- set x [on click := onLeftClick gameState f out x i, on clickRight := onRightClick gameState f out x i]
                return (i+1, r)) 0 out

             mapM (\x -> prepareInitial f x) $ out 
             set p [clientSize := sz w h]
    where
        prepareInitial f ok
            = do
                bm <- bitmapCreateFromFile "water.bmp"
                bitmapButtonSetBitmapLabel ok bm
        refreshTiles gameState f out ok
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
                    putStrLn $ show game
                    r <- putStrLn $ show game

                    case squareState game ((i `mod` 10), quot i 10) of
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
                    return (i+1, r)) 0 out
        onLeftClick gameState f out ok i pt
            = do
                game <- varGet gameState
                varSet gameState (reveal (game) ((i `mod` 10), quot i 10))
                game <- varGet gameState
                refreshTiles gameState f out ok
                case game of
                    (Game Won _ _)  -> putStrLn $ "\nYou won the game!\n\n"
                    (Game Lost _ _)  -> putStrLn $ "\nYou lost the game!\n\n"
                    (Game _ _ _)  -> putStrLn $ ""
                putStrLn $ show game
        onRightClick gameState f out ok i pt
            = do
                game <- varGet gameState
                varSet gameState (flag (game) ((i `mod` 10), quot i 10))
                game <- varGet gameState
                refreshTiles gameState f out ok
                case game of
                    (Game Won _ _)  -> putStrLn $ "\nYou won the game!\n\n"
                    (Game Lost _ _)  -> putStrLn $ "\nYou lost the game!\n\n"
                putStrLn $ show game
