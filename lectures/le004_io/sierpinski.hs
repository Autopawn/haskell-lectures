import System.IO
import System.Environment

arround :: [(Int,Int)]
arround = [(x,y) | x<-[0,1,2],  y<-[0,1,2] , (x,y) /= (1,1)]

header :: String
header = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"++
    "\t<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"

footer :: String
footer = "</svg>"

square :: String -> Int -> (Int,Int) -> String
square col s (x,y) =
    "\t\t<rect x=\""++show x++"\" y=\""++show y++
    "\" width=\""++show s++"\" height=\""++show s++
    "\" fill=\"" ++ col ++ "\"/>"

sierpinski :: Handle -> (Int,Int) -> [String] -> IO ()
sierpinski _ _ [] = return ()
sierpinski hdle (sx,sy) (col:cols) = do
    let level = 1 + length cols
        size = 3^level
    -- Central:
    hPutStrLn hdle (square col size (sx+size,sy+size))
    -- Otros:
    mapM_ (\(x,y) -> sierpinski hdle (sx+x*size,sy+y*size) cols) arround

main = do
    [input,output] <- getArgs
    colors <- readFile input
    handle <- openFile output WriteMode
    hPutStrLn handle header
    sierpinski handle (0,0) (lines colors)
    hPutStrLn handle footer
    hClose handle
