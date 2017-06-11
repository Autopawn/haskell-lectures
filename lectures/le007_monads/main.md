---
title: "Functors, Applicative Functors y Monads"
author: Francisco J. A. Casas B.
date: 06 de junio del 2017
output: beamer_presentation
theme: Warsaw
highlight: tango
colortheme: seagull
---

## Functors

La *typeclass* `Functor` es para cosas sobre las que se puede hacer un `map`, se implementa de la siguiente manera:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Notar que para que `f` pueda cumplir con esa firma, debe ser un *tipo incompleto*.

Se puede pensar en `fmap` como en una función que toma una función y un functor y la aplica al functor o como una función que toma una función y la eleva para que opere en functores.

En particular `map` es la implementación de `fmap` para Listas.

## Ejemplos de Functors

Por la definición podemos pensar en las instancias de `Functor` como cajas que permiten aplicar funciones a su contenido (o computaciones que permiten aplicar funciones a su resultado). *Matemáticamente hablando, almacenar algo es lo mismo que computarlo*.

* `Maybe` es un `Functor`, donde `fmap` aplica la función al contenido de un `Just`.

```haskell
> fmap (+2) Nothing
Nothing
> fmap (+2) (Just 20)
Just 22
```

## Ejemplos de Functors

`Either a` es un `Functor` donde `fmap` aplica la función al valor si es `Right`.

Cómo los `Functor` deben ser del tipo `* -> *` y `Either` es `* -> * -> *` (donde `*` significa *un tipo concreto*) es necesario instanciarlo con un argumento.

```haskell
> let a = Right "hola" :: Either Int String
> let b = Left 2 :: Either Int String
> fmap (++"bien") a
Right "holabien"
> fmap (++"bien") b
Left 2
```

Se puede ver que si `fmap` toma una función `b -> c`, un `Either a b` quedaría cómo `Either a c`.

## Ejemplos de Functors

`Map k` es un `Fuctor` donde `fmap` aplica la función a los valores asociados a las llaves.

```haskell
> let k = fromList [('a',1),('b',2)]
> fmap (+10) k
fromList [('a',11),('b',12)]
```

## Reglas de Functors

La clase `Functor` tiene unas reglas relacionadas con su propósito y uso, las cuales debemos asegurar como buena práctica, pero que Haskell no puede probar automáticamente.

1. La función sólo modifica los valores *internos* del `Functor` y nada más, esto se prueba con:
    ```haskell
    fmap id f = id f
    ```
2. La segunda regla dice que componer dos funciones y mapear la función resultante sobre un functor debe ser lo mismo que mapear primero una y luego la otra sobre el functor.
    ```haskell
    fmap (g1 . g2) f = fmap g1 (fmap g2 f)
    ```

## Otros Functors

`IO` también es un `Functor`, al aplicar `fmap` a un `IO` se le aplicará la función al valor que *entrega*.

```haskell
> :t getLine
getLine :: IO String
> :t fmap words getLine
fmap words getLine :: IO [String]
```

Se podría hacer:

```haskell
main = do
    (w:ws) <- fmap words getLine
    putStrLn $ "Primera palabra: " ++ w
```

## Otros Functors

* Uno muy interesante es `(->) r`, la forma permitida de instanciar `r ->`, osea, una función que recibe un argumento del tipo `r` es también un `Functor`.

Para entenderlo, la firma de `fmap` quedaría así al reemplazar:

```haskell
fmap :: (a -> b) -> f a -> f b
fmap :: (a -> b) -> ((->) r) a -> ((->) r) b
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

Y básicamente `fmap` es lo mismo que concatenar otra función al resultado.

```haskell
> fmap (3*) (100 +) 2
306
```

## Applicative Functors

Es posible almacenar funciones en `Functors`, por ejemplo se puede hacer una lista de funciones de comparación:

```haskell
> :t map (<) ['a'..'z']
map (<) ['a'..'z'] :: [Char -> Bool]
```

¿Cómo evaluar estas funciones con los valores de otro `Functor`?

Osea, hacer cosas como: `Just (+3) $ Just 2 = Just 5`.

Para eso existe la *typeclass* `Applicative`, que extiende `Functor`:

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

## Applicative

```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

* `pure` toma un valor cualquiera y crea el functor que contiene (o entrega) únicamente dicho valor.

* `<*>` toma un functor con una función y la aplica sobre otro functor con valores argumentos para esa función.

Una **regla** que se debe cumplir es `pure f <*> xs = fmap f xs`.

## Ejemplos de Applicative Fuctors

Ejemplo de listas como *Applicative Functors*:

```haskell
> [(*)1,(+)2,(-)1] <*> [1,2,3,4]
[1,2,3,4,3,4,5,6,0,-1,-2,-3]
> (map (*) [-1,0,1]) <*> [1,2,3,4]
[-1,-2,-3,-4,0,0,0,0,1,2,3,4]
```

Ejemplo de `Maybe` y currificación:

```haskell
> pure (+) <*> Just 3 <*> Just 5  
Just 8  
> pure (+) <*> Just 3 <*> Nothing  
Nothing
```

## Ejemplos de Applicative Fuctors

Se tiene el operador `f <$> x = fmap f x`:
```haskell
> (*) <$> [2,5,10] <*> [8,10,11]  
[16,20,22,40,50,55,80,100,110]  
```

`IO` también es un *applicative functor*, de hecho su implementación de `return` es su implementación de `pure` (ambas son la misma cosa pero `return` es para `Monads`).

## Reglas de Applicative Functors

```haskell
             pure id <*> v  =  v
pure (.) <*> u <*> v <*> w  =  u <*> (v <*> w)
         pure f <*> pure x  =  pure (f x)
              u <*> pure y  =  pure ($ y) <*> u
```

La gracia es que permiten combinar computaciones diferentes, como IOs o computaciones que pueden fallar (un `Maybe`), etc.

## Monads

Los Monads son como `Applicative Functors` mejorados otro nivel más.

Si tenemos un valor *fancy* y una función que toma un valor normal pero entrega un valor *fancy*, ¿Cómo le metemos el primer valor *fancy* a esa función sin que explote?

Osea, se busca tener este operador:

```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

## Monads

```haskell
class Monad m where
    return :: a -> m a  

    (>>=) :: m a -> (a -> m b) -> m b  

    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  

    fail :: String -> m a  
    fail msg = error msg  
```

## Ejemplos de Monads

La Monad `IO` es conveniente para expresar una secuencia de operaciones `IO`:

```haskell
putStr "Hello " >>= (\_ -> putStrLn "world!")
```

Es equivalente a usar la notación `do`:

```haskell
do putStr "Hello "
putStrLn "world!"
```

## Ejemplos de Monads

Puede usarse para manejar errores:

```haskell
foo :: Either String a
foo = case computeSomething of
    Left x -> case computeSomethingElse x of
        Left y -> computeAnotherThing x y
        Right e -> Right e
    Right e -> Right e
```

Aprovechando que `Either` es una `Monad`, se puede hacer:

```haskell
foo :: Either String a
foo = do
    x <- computeSomething
    y <- computeSomethingElse x
    computeAnotherThing x y
```

## Ejemplos de Monads

También se puede usar con `Maybe`:

```haskell
instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing
```

```haskell
delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return (a:as')
delete _  []    = Nothing
```

## Notación do

Como las `Monads` son tan útiles (sobretodo para `IO`), se les creó esta syntax especial:

```haskell
thing1 >>= \x ->
func1 x >>= \y ->
thing2 >>= \_ ->
func2 y >>= \z ->
return z
```

```haskell
do
    x <- thing1
    y <- func1 x
    thing2
    z <- func2 y
    return z
```

## Listas como Monads

```haskell
instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []  

> [3,4,5] >>= \x -> [x,-x]  
[3,-3,4,-4,5,-5]

> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]
```

## Leyes de las Monads

```haskell
 return x >>= f  =  f x
   m >>= return  =  m
(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)

```

<!-- ## Monoids

Los `Monoids` se usan cuando se presentan para funciones binarias asociativas que tienen un valor que funciona como la identidad respecto a esa función.

```haskell
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

## Monoids

Las listas son clases asociativas:

```haskell
> "la" ++ ("di" ++ "da") ++ ""
"ladida"
```

También lo son los **enteros**, tanto para la **suma** como la **multiplicación**, en ese caso se requiere más de una instanciación como `Monoid`, pero esto no es posible...

## Newtype

La keyword `newtype` permite construír un tipo con sólo un constructor cuyo parámetro es otro tipo, se vuelve así un "gemelo bastardo" que se puede instanciar de otras formas.

```haskell
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  

newtype All = All { getAll :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  
```

`Any` y `All` son "gemelos bastardos" de `Bool`, que se pueden instanciar de forma diferente en la typeclass `Monoid`.

## Newtype

```haskell
instance Monoid Any where  
    mempty = Any False  
    Any x `mappend` Any y = Any (x || y)  

instance Monoid All where  
    mempty = All True  
    All x `mappend` All y = All (x && y)  
``` -->
