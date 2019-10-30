module Practica1 where

--1 Función: middle
-- Recibe una 5-tupla y regresa el 3er elemento.
middle :: (Int,Int,Int,Int,Int) -> Int
middle (a,b,c,d,e) =(c)

--2 Función: add
-- Recibe una 4-tupla y regresa la suma de sus elementos.
add :: (Int, Int, Int, Int) -> Int
add (a,b,c,d) = (a+b+c+d)

--3 Función: greater
-- Recibe una 3-tupla y regresa el elemento que sea mayor.
greater :: (Int, Int, Int) -> Int
greater (a,b,c) = if(a > b && a>c) then a else if(b > a && b>c) then b else c


--4 Función: less
-- Recibe una 3-tupla y regresa el elemento que sea menor.
less :: (Int, Int, Int) -> Int
less (a,b,c) = if(a < b && a<c) then a else if(b < a && b<c) then b else c

-- 5 Función: isPair
-- Recibe una 4-tupla y regresa True si todos sus elementos son pares, False en otro caso.
isPair :: (Int, Int, Int, Int) -> Bool
isPair (a,b,c,d) = if (mod a 2==0) && (mod b 2==0) && (mod c 2==0) && (mod d 2==0) then True else False


swip :: (a, a) -> (a, a)
swip = error "Implementar."


sort :: (a,a,a,a) -> (a,a,a,a)
sort = error "Implementar."
