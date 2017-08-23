# Ejercicio 1 (40 puntos)

Colocar el término que calza con la definición:

Map - Lista de asociación - Monad - List - seq/deepseq - trace - Functor - Set - Applicative - MVar

____________ Typeclass a cuyas instancias se les puede aplicar `fmap`.

____________ Conjunto de valores.

____________ Variable de sincronización, utilizada para la comunicación entre threads concurrentes.

____________ Fuerza evaluación estricta.

____________ Imprime un valor en pantalla, incluso en una función pura.

____________ Valores asociados a llaves, almacenados utilizando conjuntos.

____________ Secuencia de valores.

____________ Contexto contenedor de un valor que cuenta con un operador `bind` que permite conectarlo con otros.

____________ Typeclass que pueden combinar una instancia con funciones y otra con valores para esas funciones.

____________ Valores asociados a llaves, almacenados utilizando listas.

# Ejercicio 2 (60 puntos)

Se tiene la siguiente instanciación de `Monad`:

```haskell
data Prob a = Prob [(a,Float)] deriving (Show)

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

instance Applicative Prob where
    pure x = Prob [(x,1.0)]
    Prob fs <*> Prob xs =
		Prob $ (map (\(f,pf) -> (\(a,pa) -> (f a,pf*pa))) fs) <*> xs

instance Monad Prob where
    return x = Prob [(x,1.0)]
    m >>= f = flatten (fmap f m)
    fail _ = Prob []

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = let
    multAll (Prob inner_xs, p) = map (\(x,r) -> (x,p*r)) inner_xs
    in Prob $ concat $ map multAll xs
```

A continuación, unos ejemplos de uso:

```haskell
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

*Main> Prob [((*2),0.5),((+2),0.5)] <*> Prob [(10,0.4),(20,0.4),(30,0.2)]
Prob [(20,0.2),(40,0.2),(60,0.1),(12,0.2),(22,0.2),(32,0.1)]
*Main> flipThree
Prob [([Heads,Heads,Heads],2.5e-2),([Heads,Heads,Tails],0.225),
([Heads,Tails,Heads],2.5e-2),([Heads,Tails,Tails],0.225),
([Tails,Heads,Heads],2.5e-2),([Tails,Heads,Tails],0.225),
([Tails,Tails,Heads],2.5e-2),([Tails,Tails,Tails],0.225)]
```

## Pregunta 1:

Explique cual es el propósito del type `Prob`.

## Pregunta 2:

Explique cómo y para qué se puede usar la instanciación de `Prob` como `Applicative`, aproveche el ejemplo.

## Pregunta 3:

Explique lo que hace la función `flatten`.

## Pregunta 4:

Explique cómo y para qué se puede usar la instanciación de `Prob` como `Monad`, aproveche el ejemplo.

# Ejercicio 3 (40 puntos)

Escriba una función que obtenga todos los subconjuntos de tamaño $n$ de una lista:

```
subsets :: Int -> [a] -> [[a]]
```

Puede usar el computador.

