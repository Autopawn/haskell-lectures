import Debug.Trace (trace)

findKey :: Show k => (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) =
    trace (show k) (if k == key then Just v else findKey key xs)
