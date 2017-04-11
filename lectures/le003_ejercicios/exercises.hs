import System.Environment
import qualified Data.List as L

main = do
    args <- getArgs
    putStr (head args ++ "\n")


pairs' :: [a] -> [(a,a)]
pairs' [] = []
pairs' (x:xs) = [(x,y) | y <- xs] ++ pairs' xs
----
pairs :: Ord a => [a] -> [(a,a)]
pairs xs = pairs' (L.sort xs)


pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = let
    pre = 0 : pascal (n-1)
    pos = reverse pre
    in zipWith (+) pre pos


isFactorial :: Integer -> Bool
isFactorial n = let
    factorials = scanl1 (*) [1..]
    geqFactorial = head (dropWhile (<n) factorials)
    in geqFactorial == n


combis :: Int -> [a] -> [[a]]
combis _ [] = []
combis 0 _ = []
combis 1 xs = [[x] | x<-xs]
combis n li@(x:xs) = map (x:) (combis (n-1) li) ++ combis n xs

intersec :: (Eq a) => [a] -> [a] -> [a]
intersec as bs = filter (\a -> elem a bs) as
