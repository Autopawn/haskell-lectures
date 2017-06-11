# Control 1 - Programación funcional en Haskell

Nombre:_____________________________________  Rol:______________________________

# Verdadero o Falso (35 puntos)

__ La programación funcional es un tipo de programación declarativa.

__ Ni las constantes ni las variables static pueden generar side-effects.

__ En Haskell no es posible hacer un programa con side-effects.

__ La inferencia de tipo hace que no sea necesaria la conversión entre tipos.

__ La lazyness no puede mejorar la eficiencia de un programa, pero permite tipos
de datos infinitos y reducir syntaxis.

__ Las listas e IO son tipos de datos incompletos.


# De un ejemplo de (15 puntos)

Función de orden superior  __________________________

Tipo de dato enumerable    __________________________

Función impura             __________________________


# Indique si son Types, Typeclasses o Type Synonyms (30 puntos)

Num         ____________________    Eq          ____________________

Int         ____________________    [Char]      ____________________

Foldable    ____________________    String      ____________________

Integer     ____________________    Bool        ____________________

Ord         ____________________    Ordering    ____________________

Char        ____________________    Double      ____________________

IO          ____________________    Maybe Int   ____________________

[(Int,Int)] ____________________

# Preguntas (20 puntos)

1) Dada la función:

asdf :: Ord a => [a] -> Bool
asdf []         = True
asdf [x1]       = True
asdf (x1:x2:xs) = x1 <= x2 && asdf (x2:xs)

Explique qué es lo que hace y póngale un mejor nombre.

________________________________________________________________________________

________________________________________________________________________________

________________________________________________________________________________


2) Dada la función:

divisoresEn :: Int -> [Int] -> Bool
divisoresEn 1 _         = True
divisoresEn x []        = False
divisoresEn x (y:ys)
	| mod x y == 0  = divisoresEn (div x y) (y:ys)
	| otherwise     = divisoresEn x ys

Explique qué es lo que hace.

________________________________________________________________________________

________________________________________________________________________________

________________________________________________________________________________

________________________________________________________________________________


