import System.Random
import System.Environment
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST

update :: Int -> a -> [a] -> [a]
update n x' []     = []
update 0 x' (x:xs) = x' : xs
update n x' (x:xs) = x : update (n-1) x' xs

eliminate :: Int -> [a] -> [a]
eliminate n []     = []
eliminate 0 (x:xs) = xs
eliminate n (x:xs) = x : eliminate (n-1) xs

data Ant = Ant (Int,Int) Int -- x,y,dir

fromRandom :: ((Int,Int),(Int,Int)) -> Int -> Ant
fromRandom ((xi,yi),(xf,yf)) rand = let
    sizex = xf-xi+1
    sizey = yf-yi+1
    dir = mod rand 4
    x = xi + mod (div rand 4) sizex
    y = yi + mod (div rand (4*sizex)) sizey
    in Ant (x,y) dir

inside :: ((Int,Int),(Int,Int)) -> (Int,Int) -> Bool
inside ((xi,yi),(xf,yf)) (x,y) =
    xi <= x && x <= xf && yi <= y && y <= yf

dirMove :: (Int,Int) -> Int -> (Int,Int)
dirMove (x,y) 0 = (x+1,y)
dirMove (x,y) 1 = (x,y-1)
dirMove (x,y) 2 = (x-1,y)
dirMove (x,y) 3 = (x,y+1)

step :: ((Int,Int),(Int,Int)) -> Int -> Ant -> Ant
step bounds rand (Ant (x,y) dir) = let
  nxtInside = inside bounds (dirMove (x,y) dir)
  dir' =
    if mod rand 10 < 2 || not nxtInside then let
      dir1 = mod (dir + 1 + 2 * (mod rand 2)) 4
      dir2 = mod (dir1 + 2) 4
      in if inside bounds (dirMove (x,y) dir1)
        then dir1 else dir2
      else dir
    in Ant (dirMove (x,y) dir') dir'

generate' :: [Int] -> [Ant] -> STArray s (Int,Int) Int -> ST s (STArray s (Int,Int) Int)
generate' (r1:r2:rands) ants board =
    if null ants then return (board) else do
    -- Pick random ant:
    let i = mod r1 (length ants)
    let ant@(Ant (x,y) _) = ants !! i
    -- Paint ant position:
    writeArray board (x,y) 1
    -- Move ant
    bounds <- getBounds board
    let ant'@(Ant (x',y') _) = step bounds r2 ant
    -- Destroy ant if it lives on a nonsolid cell:
    cell <- readArray board (x',y')
    if cell == 1 then generate' rands (eliminate i ants) board
    else generate' rands (update i ant' ants) board

generate :: Int -> Int -> [Int] -> ST s (STArray s (Int,Int) Int)
generate size nants rands = do
    let (rs1,rs2) = splitAt (nants) rands
    -- Generate the ants:
    board <- newArray ((0,0),(size-1,size-1)) 0 :: ST s (STArray s (Int,Int) Int)
    bounds <- getBounds board
    let ants = map (fromRandom bounds) rs1
    let ants' = map (\(Ant p d) -> Ant p (mod (d+2) 4)) ants
    generate' rs2 (ants++ants') board

printLevel :: Array (Int,Int) Int -> IO ()
printLevel board = do
    let ((xi,yi),(xf,yf)) = bounds board
    let cells = [[if board ! (x,y) == 1 then "#" else "·" | x <- [xi..xf]]++["\n"] | y <- [yi..yf]]
    mapM_ putStr (map unwords cells)

createLevel :: Int -> Int -> Int -> Array (Int,Int) Int
createLevel size nants seed = let
    gen = mkStdGen seed
    in runSTArray $ generate size nants (randoms gen)

main = do
    pname <- getProgName
    args <- getArgs
    if length args /= 3 then
        putStrLn $ "Usage: "++pname++" <size> <ants> <seed>"
    else do
        let lvl = createLevel (read (args !! 0)) (read (args !! 1)) (read (args !! 2))
        printLevel lvl
