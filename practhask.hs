type Var = String
data LT = LTVar Var | LTAbs Var LT | LTApl LT LT

-- Derivació de show per a mostrar els lambda-termes de forma correcte
-- Exemple de lambda-terme = LTAbs "x1" (LTAbs "x2" (LTApl (LTVar "x1") (LTApl (LTVar "x2") (LTVar "x3"))))
-- El resultat de l'anterior és (\x1. (\x2. (x1 (x2 x3))))
--
-- Exemple de lambda-terme = LTAbs "x" (LTAbs "y" (LTAbs "z" (LTApl (LTApl (LTApl (LTVar "x") (LTVar "z")) (LTVar "z")) (LTApl (LTVar "y") (LTApl (LTVar "x") (LTVar "z"))))))
-- El retultat de l'anterior és (\x. (\y. (\z. (((x z) z) (y (x z))))))
--
-- Exemple de lambda-terme = LTApl (LTAbs "n" (LTAbs "f" (LTAbs "x" (LTApl (LTVar "f") (LTApl (LTApl (LTVar "n") (LTVar "f")) (LTVar "x")))))) (LTAbs "g" (LTAbs "y" (LTApl (LTVar "g") (LTVar "y"))))
-- El resultat de l'anterior és ((\n. (\f. (\x. (f ((n f) x))))) (\g. (\y. (g y))))
instance Show LT where
    show (LTVar x) = x
    show (LTAbs x lt) = "(\\"++ x ++ ". " ++ show lt ++ ")"
    show (LTApl lt1 lt2) = "(" ++ show lt1 ++ " " ++ show lt2 ++ ")"

-- Es subsitueix el primer string pel segon
--subst :: LT -> String -> String -> LT 
--subst (LTVar x) y z = if x == y then LTVar z else LTVar x

-- Funció per mirar si una variable apareix dins un lambda-terme
-- Exemple: varInLT (LTAbs "x" (LTVar "y")) "z" -> False
-- Exemple: varInLT (LTApl (LTVar "y") (LTAbs "z" (LTVar "x"))) "x" -> True
varInLT :: LT -> String -> Bool
varInLT (LTVar x) y = x == y 
varInLT (LTAbs x lt) y = if x == y then True else varInLT lt y
varInLT (LTApl lt1 lt2) y = varInLT lt1 y || varInLT lt2 y

-- Funció per mirar quines variables estan lligades a un lambda-terme
boundVars :: LT -> [String]
boundVars (LTVar x) = []
boundVars (LTAbs x lt) = x:boundVars lt -- Si hi ha una abstraccio afegim la variable a la llista de variables lligades
boundVars (LTApl lt1 lt2) = concat [boundVars lt1, boundVars lt2]

-- Estoy en ello
allVars :: LT -> [String]
allVars (LTVar x) = [x]
allVars (LTAbs x lt) = if elem x (allVars lt) then ["a"] else ["b"]

-- Estoy en ello
freeVars :: LT -> [String]
freeVars (LTVar x) = [x] -- Si nomes hi ha una variable, llavors és lliure
freeVars (LTAbs x lt) = if varInLT lt x then freeVars lt else []
freeVars (LTApl lt1 lt2) = concat [freeVars lt1, freeVars lt2]