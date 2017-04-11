main = do
    print "Hola mundo"

-- Usando if..then..else
signo1 :: Int -> Int
signo1 i = if (i==0) then 0 else (if i<0 then -1 else 1)

-- Usando guards
signo2 :: Int -> Int
signo2 i
    | i == 0    = 0
    | i < 0     = -1
    | otherwise = 1

nombrar :: [String] -> String
nombrar []    = "-Nadie-"
nombrar [a]   = a ++ "."
nombrar [a,b] = a ++ " y " ++ b ++ "."
nombrar (a:as)  = a ++ ", " ++ nombrar as

nombrar' :: [String] -> String
nombrar' li = case li of
    []     -> "Nadie."
    [a]    -> a ++ "."
    [a,b]  -> a ++ " y " ++ b ++ "."
    (a:as) -> a ++ ", " ++ nombrar as

rTriangs :: [(Int,Int,Int)]
rTriangs = [ (a,b,c) | c <- [1..],
                       b <- [1..c],
                       a <- [1..b], a^2 + b^2 == c^2]

mean :: [Float] -> Float
mean xs = let
   suma = sum xs
   cant = length xs
   in suma / fromIntegral cant


baile :: [(String,Bool)] -> [(String,String)]
baile ps = let
    hombres = [nombre | (nombre,mujer) <- ps, not mujer]
    mujeres = [nombre | (nombre,mujer) <- ps, mujer]
    parejas [] []         = []
    parejas (h:hs) []     = (h,"NADIE") : parejas hs []
    parejas [] (m:ms)     = ("NADIE",m) : parejas [] ms
    parejas (h:hs) (m:ms) = (h,m) : parejas hs ms
    in parejas hombres mujeres
