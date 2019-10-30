module Practica3 where

{-  1
Función: getHead
Recibe una lista y regresa la cabeza de la lista (primer elemento de la lista).
-}
getHead  :: [a] -> a
getHead [] = error "No hay elementos"
getHead (x:xs) = x
getHead (x:xs) = getHead xs

{-  2
Función: getLast
Recibe una lista y regresa el último elemento de la lista.
-}
getLast :: [a] -> a
getLast [] = error "No hay elementos"
getLast (x:[]) = x
getLast (x:xs) = getLast xs


{-  3
Función: lengthList
Recibe una lista y regresa el tamaño de la lista (cuántos elementos tiene).
-}
lengthList :: [a] -> Int
lengthList [] = 0
lengthList (x:xs) = 1 + lengthList(xs)

{-  4
Función: addAll
Recibe una lista y regresa la suma de los elementos de la lista.
-}
addAll :: [Int] -> Int
addAll [] = 0
addAll (x:xs) = x + addAll(xs)

{-  5
Función: inversion
Recibe una lista y regresa la lista invertida.
-}
inversion :: [a] -> [a]
inversion [] = []
inversion (x:xs) = inversion(xs)++[x]

{- INASISTENES A CLASE
Función: isEmpty
Recibe una lista y regresa True si la lista es vacía, False en otro caso.
-}
isEmpty :: [a] -> Bool
isEmpty = error "IMplementar"

{-  EXTRA (cp)
Función: getMaximum
Recibe una lista y regresa el elemento más grande.
-}
getMaximum :: [Int] -> Int
getMaximum [] = 0
getMaximum (x:xs)=if x > getMaximum(xs) then x else getMaximum(xs)

	   