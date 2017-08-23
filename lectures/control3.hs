data Prob a = Prob [(a,Float)] deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

instance Applicative Prob where
    pure x = Prob [(x,1.0)]
    Prob fs <*> Prob xs = Prob $ (map (\(f,pf) -> (\(a,pa) -> (f a,pf*pa))) fs) <*> xs

instance Monad Prob where
    return x = Prob [(x,1.0)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = let
    multAll (Prob inner_xs, p) = map (\(x,r) -> (x,p*r)) inner_xs
    in Prob $ concat $ map multAll xs

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,0.5),(Tails,0.5)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,0.1),(Tails,0.9)]

flipThree :: Prob [Coin]
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return [a,b,c]

subsets :: Int -> [a] -> [[a]]
subsets _ [] = []
subsets 0 _ = []
subsets 1 xs = map (\x -> [x]) xs
subsets n (x:xs) = (map (x:) (subsets (n-1) xs)) ++ subsets n xs

{-
*Main> Prob [((*2),0.5),((+2),0.5)] <*> Prob [(10,0.4),(20,0.4),(30,0.2)]
Prob [(20,0.2),(40,0.2),(60,0.1),(12,0.2),(22,0.2),(32,0.1)]
*Main> flipThree
Prob [([Heads,Heads,Heads],2.5e-2),([Heads,Heads,Tails],0.225),([Heads,Tails,Heads],2.5e-2),([Heads,Tails,Tails],0.225),([Tails,Heads,Heads],2.5e-2),([Tails,Heads,Tails],0.225),([Tails,Tails,Heads],2.5e-2),([Tails,Tails,Tails],0.225)]
-}

