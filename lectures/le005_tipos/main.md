---
title: "Datatypes"
author: Francisco J. A. Casas B.
date: 11 de abril del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

## Repaso

Teníamos:

1. **Types**: Los tipos de datos.

2. **Typeclasses**: *Interfaces* para tipos de datos.

3. **Type synonyms**: Sinónimos para ciertos tipos, equivalente a un **typedef** en C.

Los **type synonyms** se hacen de la siguiente manera:

```haskell
type String = [Char]
```

## Types

Para declarar un tipo de dato nuevo se utiliza:
```haskell
data Tipo = Tipo TipoCampo1 TipoCampo2 ...
```

El primer `Tipo` es el nombre del **type**, el segundo `Tipo` es el nombre del **type constructor**, por ahora se dejarán iguales.

## Types

Aquí un ejemplo de *type* para almacenar personas con *nombre*, *edad* y una *lista de objetos*, que se representarán como `String`.

```haskell
data Persona = Persona String Int [String]
```

Para obtener estos campos (**fields**) se utiliza **pattern matching**:

```haskell
cumpleanios :: [String] -> Persona -> Persona
cumpleanios regalos (Persona nombre edad objetos) =
    Persona nombre (edad+1) (regalos++objetos)
```

## Types

También se puede utilizar **pattern matching** en *bindings*:

```haskell
cumpleanios :: [String] -> Persona -> Persona
cumpleanios regalos cumpleañero = let
    Persona nombre edad objetos = cumpleañero
    in Persona nombre (edad+1) (regalos++objetos)
```

## Varios Type Constructors

Es posible crear un **type** con varios **type constructors**:

```haskell
data Persona =
    Vivo String Int [String] | Muerto String Int
```

* Es común que el **type constructor** tenga el mismo nombre que el **type** si sólo hay un **type constructor**, en este caso hay 2.

* Ojo que `Vivo` y `Muerto` no son **types**, osea que no pueden aparecer en la firma de una función, son formas diferentes en que el **type** `Persona` se puede presentar.

* Podemos ver a `Persona` en este caso como una `union` en `C`, pero que además sabe cual de las formas está utilizando.

## Varios Type Constructors

Las funciones que utilicen `Persona` ahora se deben definir para los dos **pattern matchings** posibles (igualmente se puede para uno solo pero la definición estaría incompleta y puede generar excepciones en *runtime*):

```haskell
cumpleanios :: [String] -> Persona -> Persona
cumpleanios regalos (Vivo nombre edad objetos) =
    Vivo nombre (edad+1) (regalos++objetos)
cumpleanios regalos (Muerto nombre edad) =
    Muerto nombre (edad+1)
```

## Varios Type Constructors

* Notar que los **type constructors**, además de su función en el pattern matching, son funciones que entregan un tipo `Type`:

```haskell
> :t Vivo
Vivo :: String -> Int -> [String] -> Persona
> :t Muerto
Muerto :: String -> Int -> Persona
```

## Varios Type Constructors

```haskell
data Figura = Circulo Float | Rectángulo Float Float

area :: Figura -> Float
area (Circulo r) = pi*r^2
area (Rectángulo x y) = x*y
```

## Tipos recursivos

Se pueden declarar tipos que contienen campos del mismo tipo, por ejemplo un árbol binario de enteros:

```haskell
data Tree = Node Tree Int Tree | Null
```

## Tipos parametrizados

También se pueden declarar tipos incompletos, generalizando el árbol binario:

```haskell
data Tree a = Node (Tree a) a (Tree a) | Null
```

Ahora `Tree` sería un tipo de dato incompleto, pero `Tree Int` no.

## Listas

Esto permite definir tipos de datos más avanzados, por ejemplo las listas enlazadas:

```haskell
data List a = Empty | Cons a (List a)
```

## El Tipo Maybe

Se puede definir una versión alternativa de `Maybe` como a continuación:

```haskell
data Maybe a = Nothing | Just a
```

## El Tipo Maybe

Es común que `Maybe a` sea el *type* de retorno para funciones que en ciertos casos fallarían en entregar un valor del tipo `a`, por ejemplo:

```haskell
> import Data.List (findIndex)
> :t findIndex
findIndex :: (a -> Bool) -> [a] -> Maybe Int
```

Indica que podría entregar un `Just 3` si el primer objeto de la lista en cumplir la condición está en la posición $3$ o `Nothing` si no hay ningún objeto.

## Record Syntax

Supóngase un **type** con muchos *fields*, por ejemplo personas con *nombre*,*apellido*,*edad*,*género*,*objetos*:

```haskell
data Persona =
    Persona String String Int Bool [String]
```

Puede ser poco práctico realizar pattern matching para obtener cada *field*, así que se piensa definir las siguientes funciones:

```haskell
nombre   (Persona n _ _ _ _) = n
apellido (Persona _ a _ _ _) = a
edad     (Persona _ _ e _ _) = e
genero   (Persona _ _ _ g _) = g
objetos  (Persona _ _ _ _ o) = o
```

## Record Syntax

Estas funciones se crean automáticamente si el **type** se declara utilizando **record syntax**:

```haskell
data Persona = Persona {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    genero :: Bool,
    objetos :: [String]
}
```

Las funciones tienen estas firmas:

```haskell
> :t apellido
apellido :: Persona -> String
> :t objetos
objetos :: Persona -> [String]
```

## Record Syntax

Otra ventaja de la record syntax es que permite obtener un objeto nuevo a partir de la modificación de miembros específicos de otro objeto.

```haskell
renombrar :: String -> String -> Persona -> Persona
renombrar nom ape pers =
    pers {nombre = nom, apellido = ape}
```

## Record Syntax

Así, se puede cambiar:

```haskell
cumpleanios :: [String] -> Persona -> Persona
cumpleanios regalos pers = let
    Persona nom ape edad gen objs = persona
    in Persona nom ape (edad+1) gen (regalos++objs)
```

por:

```haskell
cumpleanios :: [String] -> Persona -> Persona
cumpleanios regalos pers = pers {
    edad = (edad pers)+1,
    objetos = regalos++(objetos pers)}
```

## Instanciando Typeclasses

Para instanciar un **type** en una **typeclass** y que por lo tanto tenga acceso a las funciones definidas para instancias de dicha **typeclass**, es necesario definir las funciones de la **typeclass**:

```haskell
instance Eq Persona where
    (==) :: Persona -> Persona -> Bool
    (==) a b = nombre a == nombre b &&
        apellido a == apellido b

instance Ord Persona where
    compare :: Persona -> Persona -> Ordering
    compare a b = let
        apeOrd = compare (apellido a) (apellido b)
        nomOrd = compare (nombre a) (nombre b)
        in if apeOrd == EQ then nomOrd else apeOrd
```

## Deriving

Despúes de una declaración de type podemos agregar un `deriving` para que el compilador instancie **automágicamente** el type a las **typeclasses** señaladas, esto sólo funciona con **typeclases** *built-in* y el compilador decide cómo implementarlo (y no siempre puede hacerlo).

```haskell
data Persona = Persona {
    nombre :: String,
    apellido :: String,
    edad :: Int,
    genero :: Bool,
    objetos :: [String]
} deriving (Show, Read, Eq, Ord)
```

* Ahora, por ejemplo, se puede usar `sort` sobre  una lista de personas.

* El operador de comparación por defecto definido para `Eq` compara todos los elementos, lo mismo que `compare`.
