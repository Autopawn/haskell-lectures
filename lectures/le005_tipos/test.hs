data Figura = Circulo {a :: Float} | Rectángulo {a :: Float, b :: Float}

-- data Persona = Vivo String Int [String] | Muerto String Int

data Persona = Persona {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    genero :: Bool,
    objetos :: [String]
}

main = do
    let sa = Circulo 20
    let sb = Rectángulo 10 30
    print (a sa)
    print (a sb)
