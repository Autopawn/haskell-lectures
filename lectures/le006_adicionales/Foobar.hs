module Foobar where

type Triangle = (Float,Float,Float)

area :: Triangle -> Float
area tri@(a,b,c) = let
    p = perimeter tri / 2
    in sqrt (p*(p-a)*(p-b)*(p-c))

perimeter :: Triangle -> Float
perimeter (a,b,c) = a+b+c
