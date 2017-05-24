import Foobar

data Persona = Persona {rut :: Int, edad :: Int}

instance Show Persona where
    show (Persona r e) = "Persona " ++ show r ++ " " ++ show e
instance Eq Persona where
    (==) a b = rut a == rut b
instance Ord Persona where
    compare a b = compare (rut a) (rut b)
