module Practica5 where

-- Dato para representar expresiones aritméticas
data EA = N Int | Var String | Pos EA | Neg EA | Sum EA EA | Res EA EA | Mult EA EA | Div EA EA | Modulo EA EA | Pot EA EA 

-- Ejercicio 7 --
-- Se define la instancia Show para el tipo de dato EA de tal manera que las instancias creadas estén en notación infija --
instance Show EA where
 show (N n) = show n
 show (Var c) = c
 show (Pos ea) = "(+" ++ show ea ++ ")"
 show (Neg ea) = "(-" ++ show ea ++ ")"
 show (Sum ea1 ea2) = show ea1 ++ "+" ++ show ea2
 show (Res ea1 ea2) = show ea1 ++ "-" ++ show ea2
 show (Mult ea1 ea2) = show ea1 ++ "*" ++ show ea2
 show (Div ea1 ea2) = show ea1 ++ "/" ++ show ea2
 show (Modulo ea1 ea2) = show ea1 ++ "%" ++ show ea2
 show (Pot ea1 ea2) =  show ea1 ++ "^" ++ show ea2


-- Ejercicio 1: Operador binario --
-- recibe dos enteros y regresa su representación del tipo de dato EA con el constructor Sum EA EA --
pasarsumaEA :: Int -> Int -> EA
pasarsumaEA n m = Sum (N n) (N m)

-- Ejercicio 2 --
--recibe dos enteros y regresa su representación del tipo de dato EA con el constructor Res EA EA --
pasarrestaEA :: Int -> Int -> EA
pasarrestaEA n m = Res (N n) (N m)

-- Ejercicio 3 --
--recibe dos enteros y regresa su representación del tipo de dato EA con el constructor Mult EA EA --
pasarmultEA :: Int -> Int -> EA
pasarmultEA n m = Mult (N n) (N m)

-- Ejercicio 4 --
--recibe dos enteros y regresa su representación del tipo de dato EA con el constructor Div EA EA --
pasardivEA :: Int -> Int -> EA
pasardivEA n m = Div (N n) (N m)

-- Ejercicio 5 --
--recibe dos enteros y regresa su representación del tipo de dato EA con el constructor Modulo es EA EA --
pasarmoduloEA :: Int -> Int -> EA
pasarmoduloEA n m = Modulo (N n) (N m)

-- Ejercicio 6 --
--recibe dos enteros y regresa su representación del tipo de dato EA con el constructor Pot EA EA --
pasarpotEA :: Int -> Int -> EA
pasarpotEA n m = Pot (N n) (N m)

-- Ejercicio 8: Operadores unarios --
-- recibe dos EA cuyo operador principal de cada uno es un operador unario y esta aplciado a un número. La función regresa True en caso de que el primer parámetro sea mayor que el segundo, regresa False en otro caso --
mayorqueUnario :: EA -> EA -> Bool
mayorqueUnario (Pos(N n)) (Neg(N m)) = if n >= 0 then True else False
mayorqueUnario (Pos(N n)) (Pos(N m)) = if n > m then True else False
mayorqueUnario (Neg(N n)) (Neg(N m)) = if n < m then True else False
mayorqueUnario (Neg(N n)) (Pos(N m)) = if m >= 0 then False else True



-- Ejercicio 9: Evaluacion --
-- recibe una EA y regresa su evaluación --
--eval (Res (Sum (Sum (N 2) (Mult (N 3) (N 9))) (Mult (N 4) (N 2))) (Div (Mult (N 7) (N 4)) (N 2)))
--23
eval :: EA -> Int
eval (N n) = n
eval (Sum n m) = eval n + eval m
eval (Res n m) = eval n - eval m 
eval (Mult n m)= (eval n) * (eval m)
eval (Div n m) = div (eval n) (eval m)
