module Practica7 where

--
data Prop = PTrue | PFalse | Var Nombre | Neg Prop | Conj Prop Prop | Disy Prop Prop | Impl Prop Prop | Syss Prop Prop

type Nombre = String
instance Show Prop where
    show (PTrue) = "True"
    show (PFalse) = "False"
    show (Var a) =  a
    show(Neg prop) = " ¬ " ++ show prop
    show(Conj prop1 prop2) = show prop1 ++ " ∨ " ++ show prop2
    show(Disy prop1 prop2) = show prop1 ++ " ∧ " ++ show prop2
    show(Impl prop1 prop2) = show prop1 ++ " --> " ++ show prop2
    show(Syss prop1 prop2) = show prop1 ++ " <--> " ++ show prop2


-- Ejercicio 1
-- Esto podría ser de ayuda : ¬ ∧ ∨ → ↔ 


-- Ejercicio 2
cuenta :: Prop -> Int
cuenta(PFalse) = 0
cuenta(PTrue) = 0
cuenta(Var p) = 0
cuenta(Neg p) = 1 + cuenta(p)
cuenta(Conj p q) = 1 + cuenta(p) + cuenta(q)
cuenta(Disy p q) = 1 + cuenta(p) + cuenta(q)
cuenta(Impl p q) = 1 + cuenta(p) + cuenta(q)
cuenta(Syss p q) = 1 + cuenta(p) + cuenta(q)

-- Ejercicio 3
variables :: Prop -> [Nombre]
variables(PFalse) = []
variables(PTrue) = []
variables(Var a) = [a]
variables(Neg p) = variables(p)
variables(Conj p q) = variables(p) ++ variables(q)
variables(Disy p q) = variables(p) ++ variables(q)
variables(Impl p q) = variables(p) ++ variables(q)
variables(Syss p q) = variables(p) ++ variables(q)


-- Ejercicio 4
deMorgan :: Prop -> Prop
deMorgan PTrue = PTrue
deMorgan PFalse = PFalse
deMorgan (Var p) = Var p
deMorgan (Conj p q) = Conj p q
deMorgan (Disy p q) = Disy p q
deMorgan (Impl p q) = Impl p q
deMorgan (Syss p q) = Syss p q
deMorgan (Neg(Conj p q)) = Disy (Neg p) (Neg q)
deMorgan (Neg(Disy p q)) = Conj (Neg p) (Neg q)



-- Ejercicio 5
equiv_op :: Prop -> Prop
equiv_op PTrue = PTrue
equiv_op PFalse = PFalse
equiv_op (Var p) = Var p
equiv_op (Conj p q) = Conj p q
equiv_op (Disy p q) = Disy p q
equiv_op (Impl p q) = Conj (Neg p) q
equiv_op (Syss p q) = Disy (Conj (Neg p) q) (Conj p (Neg q))



-- Ejercicio 6
dobleNeg :: Prop -> Prop 
dobleNeg PTrue = PTrue
dobleNeg PFalse = PFalse
dobleNeg (Var a) = Var a
dobleNeg (Neg (Neg e)) = dobleNeg(e)
dobleNeg (Impl a b) = Impl (dobleNeg(a)) (dobleNeg(b))
dobleNeg (Syss a b) = Syss (dobleNeg(a)) (dobleNeg(b))
dobleNeg (Conj a b) = Conj (dobleNeg(a)) (dobleNeg(b))
dobleNeg (Disy a b) = Disy (dobleNeg(a)) (dobleNeg(b))

-- Ejercicio7
distribuitividad :: Prop -> Prop
distribuitividad (Disy p (Conj q r)) = Conj (Disy (distribuitividad(p)) (distribuitividad(q))) (Disy (distribuitividad(p)) (distribuitividad(r)))
distribuitividad (Conj p (Disy q r)) = Disy (Conj (distribuitividad(p)) (distribuitividad(q))) (Conj (distribuitividad(p)) (distribuitividad(r)))
distribuitividad PFalse = PFalse
distribuitividad PTrue = PTrue
distribuitividad (Var a) = Var a
distribuitividad (Neg a) = Neg (distribuitividad(a))
distribuitividad (Conj p q) = Conj (distribuitividad(p)) (distribuitividad(q))
distribuitividad (Disy p q) = Conj (distribuitividad(p)) (distribuitividad(q))

 


