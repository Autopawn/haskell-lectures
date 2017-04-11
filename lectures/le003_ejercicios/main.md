---
title: "Ejercicios I y Tarea I"
author: Francisco J. A. Casas B.
date: 11 de abril del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

## Obtención de pares

Implemente la función:

```haskell
pairs :: Ord a => [a] -> [(a,a)]
```

Que entrega todos los posibles pares de a dos elementos de una lista, de manera que para cada par el primer elemento siempre es menor que el segundo.

### Podría servir:
* `L.sort` (`import qualified Data.List as L`).

## SOLUCIÓN: Obtención de pares

```haskell
import qualified Data.List as L

pairs' :: [a] -> [(a,a)]
pairs' [] = []
pairs' (x:xs) = [(x,y) | y <- xs] ++ pairs' xs
----
pairs :: Ord a => [a] -> [(a,a)]
pairs xs = pairs' (L.sort xs)
```

## Intersección de Conjuntos

Implemente la función:

```haskell
intersec :: Eq a => [a] -> [a] -> [a]
```

Que recibe dos conjuntos (en forma de lista) y entrega los elementos que están en ambos.

### Podría servir:
* `filter`
* `elem`

## SOLUCIÓN: Intersección de Conjuntos

```haskell
intersec :: Eq a => [a] -> [a] -> [a]
intersec as bs = filter (\a -> elem a bs) as
```

### Pregunta

Si las listas fueran ordenadas ¿Se podría hacer más eficiente?

## Triángulo de Pascal

Implemente la función:

```haskell
pascal :: Int -> [Int]
```

Que entregue la $n$-ésima fila del triángulo de Pascal (`[1]` es la fila `0`).

```
              1
            1   1
          1   2   1
        1   3   3   1
      1   4   6   4   1
    1   5  10  10   5   1
  1   6  15  20  15   6   1
1   7  21  35  35  21   7   1
```

### Podrían servir:
* `reverse`
* `zipWith (+)`

## SOLUCIÓN: Triángulo de Pascal

```haskell
pascal :: Int -> [Int]
pascal 0 = [1]
pascal n = let
    pre = 0 : pascal (n-1)
    pos = reverse pre
    in zipWith (+) pre pos
```

## Detección de factoriales

Implemente la función:

```haskell
isFactorial :: Integer -> Bool
```

Que indique si un número $n$ puede escribirse como $m!$ $(n,m \in \mathbb{Z})$.

## Detección de factoriales

Puede obtener una lista infinita con los números factoriales usando la función `*` con `scanl`:

```haskell
scanl :: (b -> a -> b) -> b -> [a] -> [b]
```

`scanl` es como `foldl` pero retorna todos los estados por los que pasa el acumulador.

Más aun, puede utilizar `scanl1` que no requiere un valor inicial para el acumulador, pues lo obtiene de la lista.

```haskell
scanl1 :: (a -> a -> a) -> [a] -> [a]
```

### Podrían servir:
* `filter`
* `dropWhile`
* `takeWhile`

## SOLUCIÓN: Detección de factoriales

```haskell
isFactorial :: Integer -> Bool
isFactorial n = let
    factorials = scanl1 (*) [1..]
    geqFactorial = head (dropWhile (<n) factorials)
    in geqFactorial == n
```

## Combinaciones con repetición

Implemente la función:

```haskell
combis :: Int -> [a] -> [[a]]
```

Que entrega todas las posibles combinaciones de tamaño $n$ con reposición de una lista (sólo una vez cada una).

**e.g.:**
```haskell
ghci> combis 1 "abc"
["a","b","c"]
ghci> combis 2 "abc"
["aa","ab","ac","bb","bc","cc"]
ghci> combis 3 "abc"
["aaa","aab","aac","abb","abc",
 "acc","bbb","bbc","bcc","ccc"]
ghci> combis 2 "abcd"
["aa","ab","ac","ad","bb","bc","bd","cc","cd","dd"]
```

## SOLUCIÓN: Combinaciones con repetición

```haskell
combis :: Int -> [a] -> [[a]]
combis _ [] = []
combis 0 _ = []
combis 1 xs = [[x] | x<-xs]
combis n li@(x:xs) =
    map (x:) (combis (n-1) li) ++ combis n xs
```

## Tarea 1 1/2 : Mezcla de rangos

Implemente la función:

```haskell
rangeMerge :: [(Int,Int)] -> [(Int,Int)]
```

Que recibe una lista de rangos $[a,b]$ y junta aquellos que se superponen hasta representar la unión de estos con la mínima cantidad posible.

## Tarea 1 2/2 : Detector de ciclos

Realizar un programa que se pueda compilar con `ghc`, que lea un archivo con los arcos de un grafo no dirigido:

```
(1,3)
(1,4)
(5,4)
(1,2)
(3,4)
```

Y imprima `1` si tiene ciclos o `0` si no tiene ciclos.

Cualquier arco de un nodo a si mismo implica la presencia de un ciclo.

**NOTA:** Aunque puede utilizar `Data.Set` para mayor eficiencia, no es necesario.

## Tarea 1 2/2 : Recomendaciones

El archivo a leer debe ser especificado con el primer argumento de la línea de comandos.

El siguiente programa de ejemplo imprime el primer argumento:

```haskell
import System.Environment

main = do
    args <- getArgs
    putStr (head args ++ "\n")
```

Para leer el archivo se recomienda el uso de las funciones `readFile` y `lines`.

Para parsear se recomienda usar
```haskell
let arcs = map read <lineas> :: [(Int,Int)]
```
