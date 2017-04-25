import qualified Data.List as L

merge :: [(Int,Int)] -> [(Int,Int)]
merge = merge' . L.sort

merge' :: [(Int,Int)] -> [(Int,Int)]
merge' [] = []
merge' [x] = [x]
merge' ((a,b):(c,d):xs)
    | b >= c  = merge' ((min a c, max b d) : xs)
    | otherwise = (a,b) : merge' ((c,d):xs)

cycles :: [(Int,Int)] -> Bool
cycles [] = False
cycles arcs = cycles' [fst (head arcs)] arcs

cycles' :: [Int] -> [(Int,Int)] -> Bool
cycles' past arcs
    | cycle     = True
    | null adjs = cycles unadjs
    | otherwise = cycles' (past++neighs) unadjs
    where
    (adjs,unadjs) = L.partition (\(a,b) -> elem a past || elem b past) arcs
    neighs = map (\(a,b) -> if elem a past then b else a) adjs
    cycle = L.nub neighs /= neighs || (not . null) (L.intersect neighs past)
