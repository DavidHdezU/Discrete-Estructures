module Practica2 where

{-  1 
Función: fibo
Recibe un entero n y regresa el fibonacci de n. -}
fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

{-  2
Función: factorial
Recibe un entero n y regresa el factorial de n. -}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

{-  3
Función: triangle_number
Recibe un entero n y regresa la suma de los n números naturales
	desde 1 hasta n.
-}
--A esto se le conoce como número triangular.
triangle_number :: [a] -> Int -> [a]
triangle_number [] n = []
triangle_number (x:xs) n = x : (triangle_number(xs))(n)

{-  4
Función: power
Recibe un número x y un exponente n y regresa el resultado de x^n -}
power :: Integer -> Integer -> Integer
power n 0 = 1
power n x = n*(power n (x-1))

{-  5
Función: isPair
Recibe un número n. Regresa True cuando n es par, False en otro caso.
-}
isPair :: Int -> Bool
isPair 0 = True
isPair 1 = False
isPair n = (isPair (n-2)) 
{-  6
Función: isOdd
Recibe un número n. Regresa True cuando n es impar, False en otro caso.
-}
isOdd :: Int -> Bool
isOdd 0 = False
isOdd 1 = True
isOdd n = (isOdd (n-2))

{-  7
Función: add_pairs 
Recibe un número n y regresa la suma de los enteros positivos desde 2 hasta n.
Si n es impar entonces mandar mensaje de error.
-}
add_pairs :: Int -> Int
add_pairs 2 = 2
add_pairs n = n + add_pairs(n-1)
{-  Inasistentes a clase
Función: mult
Recibe dos números 'a' y 'b', regresa el resultado de a*b.
	La multiplicación debe ser una suma recursiva, es decir: a*b = a*a*a*... tantas veces como b
	Ejemplo: 2*5 = 2*2*2*2*2
-}
mult :: Int -> Int -> Int
mult = error "Implementar."

{-  EXTRA (cp)
Función: division
Recibe dos números 'a' y 'b', regresa el resultado de a/b.
	La división debe ser mediante resta.
	Ejemplo: division 10 2 representa 
			           10/2 = 1+((10-2)/2) = 1+(8/2)                      Aquí se ejemplifica la resta a-b										   
										   = 1+(1+(6/2))
										   = 1+(1+(1+(4/2)))
										   = 1+(1+(1+(1+(2/2))))
										   = 1+(1+(1+(1+(1+(0/2)))))
										   = 1+(1+(1+(1+(1+0))) 
										   = 5
-}
-- Hint: en la llamada recursiva deberán restar 'b' a 'a'. En otras palabras; a = a-b
division :: Int -> Int -> Int
division = error "Implementar."