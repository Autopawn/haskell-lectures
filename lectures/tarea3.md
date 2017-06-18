---
title: "Arrays y Tarea 3"
author: Francisco J. A. Casas B.
date: 06 de junio del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

# Arrays

## Arrays

En programación funcional el uso de *arrays* es un problema:

* La **inmutabilidad** implica que si se tiene una función para modificar sólo una posición del array, se tendrá que entregar una copia nueva del mismo, y esto es $O(n)$.

* Esto se evita en otras estructuras que se construyen recursivamente, como los árboles de `Data.Set`, sin embargo estas tienen una complejidad de acceso $O(log(n))$ y la idea de un *Array* es tener $O(1)$.

## Implementaciones de Arrays

<!-- ofrece varios tipos de *arrays*, todos son *non-strict*, osea que los elementos están sujetos a las reglas de la *lazyness*. -->
En Haskell existen varias implementaciones de *arrays*:

* El paquete `Data.Array`, ofrece arreglos inmutables en que varios elementos se pueden "actualizar" de una tanda (Lo que sigue siendo $O(n)$).

* Los arreglos mutables de `Data.Array.IO`, a lo cuales se puede acceder de manera impura (sus funciones son `IO`).

* Los arreglos mutables de `Data.Array.ST`, permiten trabajar dentro de la Monad `ST`, mucho menos impura que `IO`, pudiendo incluso usarse en una función pura, siempre que se deje de modificar antes de que esta termine.

<!-- TODO: Comprobar esto ^ -->

## Data.Array

Para crear un `Array i e` se utiliza:

```haskell
array :: Ix i => (i,i) -> [(i,e)] -> Array i e
```

La *type class* `Ix` se utiliza para mapear un subrango continuo de valores a enteros, osea que se refiere a *datatypes* que pueden utilizarse como índices.

El primer argumento de esta función indica el rango de los índices y la segunda es una lista que contiene los pares `(indice,valor)` para inicializar el arreglo.

## Data.Array

Para obtener el valor en una posición se puede usar:

```haskell
(!) :: Ix i => Array i e -> i -> e
```

Para actualizar varios elementos de un array, creando un array inmutable nuevo (esto es $O(n)$):

```haskell
(//) :: Ix i => Array i e -> [(i, e)] -> Array i e
```

Documentación [AQUÍ](http://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array.html).

## Data.Array.IO

```haskell
import Data.Array.IO
main = do
  arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
  -- ^ Nuevo arreglo con índices de 1 a 10,
  -- todos inicializados en 37.
  a <- readArray arr 1
  -- ^ Leer posición 1.
  writeArray arr 1 64
  -- ^ Escribir 64 en posición 1.
  b <- readArray arr 1
  -- ^ Leer posición 1.
  print (a,b)
```

Documentación [AQUÍ](https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array-IO.html).

## Data.Array.ST

```haskell
import Control.Monad.ST
import Data.Array.ST

buildPair :: ST s (Int, Int)
buildPair = do
  arr <- newArray (1,10) 37 :: ST s (STArray s Int Int)
  a <- readArray arr 1
  writeArray arr 1 64
  b <- readArray arr 1
  return (a,b)
```

Documentación [AQUÍ](https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array-ST.html).

## Monad ST

Dentro de otra función se puede utilizar la función:

```haskell
runST :: (forall s. ST s a) -> a
```

Que retorna el valor de una computación en la Monad ST, el estado interno de la computación es inaccessible para el resto del programa.

Por ejemplo:

```haskell
let (a,b) = runST buildPair
```

## Type classes

Las *type classes* `Data.Array.IArray` y `Data.Array.MArray` proveén métodos para todas las instancias de arreglos inmutables y mutables respectivamente.

Estas continen funciones como `bounds` y `getBounds`, para obtener el rango de los índices de sus respectivos arreglos.

Documentación [AQUÍ](https://hackage.haskell.org/package/array-0.5.1.1/docs/Data-Array-IArray.html) y [AQUÍ](http://hackage.haskell.org/package/array-0.2.0.0/docs/Data-Array-MArray.html).

# Números aleatorios

## System.Random

El paquete `System.Random` proveé la *type class* `RandomGen` para generadores de números aleatorios.

También proveé `StdGen` un generador estándar, instancia de `RandomGen`.

## StdGen

Para usar este generador se pueden usar las siguientes funciones:

```haskell
mkStdGen :: Int -> StdGen
-- ^ Crea un StdGen a partir de un valor semilla.
next :: RandomGen g => g -> (Int, g)
-- ^ Entrega un número al azar y el siguiente estado
-- del generador.
split :: RandomGen g => g -> (g, g)
-- Crea dos generadores diferentes a partir de uno.
randoms :: RandomGen g => g -> [a]
-- ^ Entrega una lista infinita con todos los números
-- que generará el generador a partir de su estado
-- actual.
```

Documentación [AQUÍ](http://hackage.haskell.org/package/random-1.0.0.2/docs/System-Random.html#t:StdGen).

# Tarea 3

## Tarea 3

Suponga que se tiene una matriz de $0$ que representan terreno, sobre la que se quiere construír caminos aleatoriamente (el $1$ represeta caminos) de tal manera que nunca quede un callejón sin salida (Pueden haber rotondas si, osea un camino puede terminar en 2x2 celdas $1$).

Una forma de hacerlo es utilizando hormigas, las hormigas van avanzando mientras contruyen el camino.

```haskell
data Ant = Ant (Int,Int) Int
-- Ant (x,y) dir
-- dir = 0 : moving right
-- dir = 1 : moving top
-- dir = 2 : moving left
-- dir = 3 : moving bottom
```

## Tarea 3 - Ejemplos

```
. . . . # # # # # # # #
. . . . # . . # . . . #
. . . . # . . # . . . #
. . . . # . . # . . # #
. . . . # . . # . . # .
. . . . # . . # . . # .
. . . . # . . # . . # .
# # # # # # # # # # # .
# . . . . . . . . . # .
# # # # # # # # # # # #
# . . . . . . . . . # #
# # # # # # # # # # # #
```

## Tarea 3 - Ejemplos

```
# # # # # # # # # . # #
# . . . . . . . # . # #
# . . . . . . . # . # #
# . . . . . . . # . # #
# . . . . . . . # # # #
# . . . . . . . . . . #
# . . . . . . . . . . #
# # # # # # # # # # . #
# # # . # # # . . # . #
# # . . # # # . . # . #
# # . . . . # . . # # #
# # # # # # # . . . # #
```

## Tarea 3 - Ejemplos

```
# # # # # # # # # # # #
# # # # # # # # # # # #
. . . . . . # # # . . .
. . . . . . # . # . . .
. . . . . . # . # . . .
. . . . . . # # # . . .
. . . . . . # . . # # #
. # # # . . # . . # . #
. # . # . . # . . # # #
# # . # . . # . . . . #
# . . # . . # . . # # #
# # # # # # # . . # # #
```

## Tarea 3 - Ejemplos

```
# # # # . . . . # # # #
# . . # . . . . # # # #
# . . # . . . . # . . .
# . . # . . . . # . . .
# # # # . . . . # # . .
. . # # . . . . . # . .
. . # # . . . . . # . .
. . . # # # # # # # # #
. . . # # . . . . . . #
. . . # # . . . . . . #
. . . # # . . . . . . #
. . . # # # # # # # # #
```

## Tarea 3 - Ejemplos

```
. . . # # # # # # # # #
. . . # . # # # # # # #
. . . # # # . . . . . .
. # # # # . . . . . . .
# # # # # . . . . . . .
# . . . . . . . . . . .
# . . . . . . . . . . .
# . . . . . . . . . . .
# # # # # # # # # # . .
# # # # # # # # # # . .
# . . . . . . . # . . .
# # # # # # # # # . . .
```
## Tarea 3 - Ejemplos

```
# # . . # # # # # # # #
# # . . # . . # . . # #
# # . . # . . # . . # #
# . . . # # # # . . # #
# . . . . . . . . . # .
# . . . . . . . . . # .
# . . . . . . . . . # #
# . . . . . . . . . # #
# . . . . . . . . . # #
# . . . . . . . . . # #
# . . . . . . . . . # #
# # # # # # # # # # # #
```

## Tarea 3 - Ejemplos

```
. . . # # # # # # # # #
# # # # # # # # # . . #
# . . . . # # . # . . #
# . . . . . # # # # # #
# . . . . . . # # . . .
# . . . . . . # # . . .
# . . . . . . . . . . .
# . . . . . . . . . . .
# . . # # . . . . . . .
# . . # # . . . . . . .
# . . # # . . . . . . .
# # # # # . . . . . . .
```

## Tarea 3 - Ejemplos

```
# # # # . . . . . . . .
# . . # . . # # # # # .
# . . # . . # . . . # .
# . . # . . # . . . # .
# . . # # # # # # # # .
# . . # . # # # # # # .
# . . # # # # # # # # .
# . . # . . . . . . . .
# . . # . . . . . . . .
# . . # . . . . . . . .
# . . # . . . . . . . .
# # # # . . . . . . . .
```

## Tarea 3 - Ejemplos

```
. . . . . # # # # # # #
. . . . . # . . . . # #
. . . . # # . . . . # #
# # # # # # . . . . # #
# . . # . # . . . . # #
# . . # . # . . . . # #
# . . # . # . . . . . #
# . . # # # # . . . # #
# . . . . . # . . . # .
# # # # # # # . . . # .
# # # # # # # # # # # .
# # # # # . . . . . . .
```

## Tarea 3

* Inicialmente se colocan hormigas en posiciones aleatorias en la matriz.

* Por cada hormiga que se coloca se coloca otra exactamente en la misma posición pero mirando en la dirección contraria $(dir+2)\ mod\ 4$.

## Tarea 3

En cada paso:

* Se elige una hormiga al azar dentro de las disponibles.

* Se cambia la celda donde esa hormiga está pisando por $1$.

* Se avanza la hormiga.

* Si la nueva posición de la hormiga tiene un $1$, es destruída.

El algoritmo termina cuando ya no quedan hormigas.

Ahora algunas funciones para hormigas:

## Tarea 3 - Hormigas

Crear una partir un número aleatorio:

```haskell
fromRandom :: ((Int,Int),(Int,Int)) -> Int -> Ant
fromRandom ((xi,yi),(xf,yf)) rand = let
  sizex = xf-xi+1
  sizey = yf-yi+1
  dir = mod rand 4
  x = xi + mod (div rand 4) sizex
  y = yi + mod (div rand (4*sizex)) sizey
  in Ant (x,y) dir
```

## Tarea 3 - Hormigas

Funciones auxiliares para el movimiento:

```haskell
inside :: ((Int,Int),(Int,Int)) -> (Int,Int) -> Bool
inside ((xi,yi),(xf,yf)) (x,y) =
    xi <= x && x <= xf && yi <= y && y <= yf

dirMove :: (Int,Int) -> Int -> (Int,Int)
dirMove (x,y) 0 = (x+1,y)
dirMove (x,y) 1 = (x,y-1)
dirMove (x,y) 2 = (x-1,y)
dirMove (x,y) 3 = (x,y+1)
```

## Tarea 3 - Hormigas

Dar un paso, asegurando que no se sale de los límites, con una hormiga:

```haskell
step :: ((Int,Int),(Int,Int)) -> Int -> Ant -> Ant
step bounds rand (Ant (x,y) dir) = let
  nxtInside = inside bounds (dirMove (x,y) dir)
  dir' =
    if mod rand 10 < 2 || not nxtInside then let
      dir1 = mod (dir + 1 + 2 * (mod rand 2)) 4
      dir2 = mod (dir1 + 2) 4
      in if inside bounds (dirMove (x,y) dir1)
        then dir1 else dir2
      else dir
    in Ant (dirMove (x,y) dir') dir'
```

## Tarea 3 - Listas

Funciones que podrían servirle para trabajar con lisas:

```haskell
update :: Int -> a -> [a] -> [a]
update n x' []     = []
update 0 x' (x:xs) = x' : xs
update n x' (x:xs) = x : update (n-1) x' xs

eliminate :: Int -> [a] -> [a]
eliminate n []     = []
eliminate 0 (x:xs) = xs
eliminate n (x:xs) = x : eliminate (n-1) xs
```

## Tarea 3 - Librerías

*Imports* que podrían servirle:

```haskell
import System.Random
import System.Environment
import Control.Monad.ST
import Data.Array.IArray
import Data.Array.ST
```

## Tarea 3 - Objetivo

El objetivo principal es trabajar con un *array mutable* y hacerlo dentro de una función pura:

```haskell
createLevel :: Int -> Int -> Int -> Array (Int,Int) Int
createLevel size nants seed = let
    rands = randoms (mkStdGen seed)
    in runSTArray $ generate size nants rands
```

Para lograrlo, nuestra función `generate` debe trabajar dentro de la Monad `ST s`, que permite hacer operaciones sobre arreglos `STArray s i e` (No se preocupe por la `s`, déjela ahí no más), `i` debería ser `(Int,Int)` porque se usará un arreglo bidimensional y `e` debería ser `Int` o `Bool`, lo que más acomode para representar los valores en las celdas.

## Tarea 3 - Función generate

La función generate en el ejempo anterior debería tener la siguiente firma:

```haskell
generate :: Int -> Int -> [Int] ->
  ST s (STArray s (Int,Int) Int)
```

Y utilizar las funciones `writeArray`, `readArray`, `newArray` y `getBounds`.

## Tarea 3 - Input y Output

La idea es que se pueda llamar al programa desde la línea de comandos, entregándole el tamaño, la cantidad de hormigas (luego se duplica) y la semilla aleatoria.

## Tarea 3 - Input y Output

```
$ ./tarea3.exe 12 4 9001
. . . . # # # # # # # #
# # # # # . . # # . . #
# . . . . . . # # . . #
# . . . . . . # # . . #
# . . . . . . # # . . #
# . . . . . . # # # . #
# # . . . . . # . # # #
# # . . . # # # # # # #
# # . . . # # # # # # .
# # . . . . . . # # # .
# # . . . . . . # . . .
# # # # # # # # # . . .
```
