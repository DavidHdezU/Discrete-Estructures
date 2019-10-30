module Pracica5 where

data Natural = Cero | S Natural deriving (Eq,Show)

-- 1
-- recibe un entero y regresa su tipo de dato Natural. --
to_nat :: Int -> Natural
to_nat 0 = Cero
to_nat 1 = S Cero
to_nat n = S (to_nat (n - 1))


-- 2
addition :: Natural -> Natural -> Natural
addition n Cero = n
addition (S n) (S m) = S(addition n (S m))

--3
mult :: Natural -> Natural -> Natural
mult = error "Implementar."


-- 4
-- Recibe dos naturales `n' y `m' y regresa True si n<m, False en otro caso. --
lt :: Natural -> Natural -> Bool
lt n Cero =  False
lt Cero n = True
lt (S n) (S m) = lt n m 


-- 5
-- recibe un natural `n' y regresa True si `n' es par, False en otro caso.
pair :: Natural -> Bool
pair Cero = True
pair (S Cero)= False 
pair (S (S n)) = pair n

-- 6
--recibe dos naturales `n' y `m' y regresa True si `n' = `m' es par, False en otro caso.
equal :: Natural -> Natural -> Bool
equal Cero Cero = True
equal Cero (S Cero) = False
equal (S n) (S m) = equal n m


-- 7
min_nat :: [Natural] -> Natural
min_nat = error "Implementar."


-- 8
to_int :: Natural -> Int
to_int Cero = 0
to_int (S n) = 1 + (to_int(n))
