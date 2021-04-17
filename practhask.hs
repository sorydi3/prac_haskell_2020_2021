-- 1
import Data.List
type Var = String
data LT  = Va Var | La Var LT | AP LT LT

--"Show lambda terme"

instance Show LT where 
    show (Va x) = x
    show (La v lt) = "(\\"++v++"."++ show lt ++ ")"
    show (AP lt lv) = "("++ show lt++" "++ show lv ++ ")"


instance Eq LT where 
    (==) (Va x) (Va x') = x==x'

-- working ---
freeVars :: LT -> [Var]
freeVars (Va v) = ([v])
freeVars (AP a b) = freeVars a `union` freeVars b
freeVars (La v lt) = delete v $ freeVars lt

-- freeAndBoundVars (La "y" (AP (Va "y") (La "x" (AP (Va "x") (Va "z")))))
freeAndBoundVars :: LT -> ([Var],[Var])
freeAndBoundVars (Va v) = ([v],[])
freeAndBoundVars (AP a b) = (fst (freeAndBoundVars a) `union`  fst (freeAndBoundVars b),snd (freeAndBoundVars a) `union` snd (freeAndBoundVars b))
freeAndBoundVars (La v lt) = (delete v $ fst (freeAndBoundVars lt),if v `elem` fst (freeAndBoundVars lt) then v:snd (freeAndBoundVars lt) else snd (freeAndBoundVars lt))


fresh :: Var ->LT
fresh x = Va x 

-- PRE : Un lamba Terme de tipus LT
-- POST : Retorna una llista de totes les variables de un lamba term ( lliures y lligades)
variables :: LT->[Var]
variables (Va e) = [e]
variables (AP a b) = variables a `union` variables b
variables (La x lt) = variables lt `union` [x]


--subs_i (La "x" (AP (Va "x") (Va "x"))) "x" (AP (Va "y") (Va "z"))
--subs_i :: LT->Var->LT->LT
--subs_i var@(Va v) x e = if v==x then e else var
--subs_i ap@(AP a b) x e = AP (subs_i a x e) (subs_i b x e)   
--subs_i la@(La v lt) x e | x == v = subs_i lt x e
--                        | otherwise = La v (subs_i lt x e)

-- SUBSTITUCIO ALFA                        
subs_a :: LT->Var->LT->LT
subs_a var@(Va v) x e = if v==x then e else var
subs_a ap@(AP a b) x e = AP (subs_a a x e) (subs_a b x e)   
subs_a la@(La v lt) x e | x == v = la
                        | otherwise = La v (subs_a lt x e)


charToString :: [Char] -> [Var]
charToString [] =[]
charToString (c:cs) = [c]:charToString cs

-- subs (La "x" (AP (Va "x") (La "x" (Va "x")))) (AP (Va "y") (Va "z"))

-- WORKING PARTIALLY WITH --\\ alfa (La "x" (Va "x")) ["x"] --> RESULT EQUAL --> (\t.t) --
alfa::LT ->[Var]->LT
alfa (AP a b) vars = AP (alfa a vars) (alfa b vars)
alfa (La x lt) vars | x `elem` vars = La t $ (alfa e' vars)
                    | otherwise = La x  $ (alfa lt vars)
                    where
                     var = (variables lt)
                     v = charToString ['a'..'z']
                     t = last [ x | x <- v ,let lis = var `union` vars ,not (x`elem` lis)]
                     e' = subs_a lt x (fresh t) 
alfa e _ = e


-- PRE : UN REDEX 
-- POST : RETORNA UN ALTRE LAMBDA TERME DESPRES DE FER LA SUBSTITUCIO

subs::LT->LT->LT
subs var@(Va x) _ = error "No es un redex"
subs (La x lt) v = subs_a lt' x v
      where
          lt'= alfa lt (freeVars v)
          


                                

--subs var@(Va x) _ = error "No es un redex"
--subs (AP a b) e =  AP (subs a b) e 
--subs exp@(La x lt) e = subs_i exp x e 


-- 3 -- 

esta_normal::LT->Bool
esta_normal (Va v) = True
esta_normal (La v lt) = case lt of
    (La v t) -> esta_normal t
    (AP a b) -> esta_normal a && esta_normal b
    (Va a) -> False
esta_normal (AP a b) =  dreta && esquerra
    where 
    dreta = case a of
        (La v t) -> esta_normal t
        (AP c c') -> esta_normal c && esta_normal c'
        (Va e) -> True
    esquerra = case b of
        (La v t) -> esta_normal t
        (AP c c') -> esta_normal c && esta_normal c'
        (Va e) -> True

-- 3 

-- 4 

-- 5 

-- 6 

-- 7

-- 8

-- 9

-- 10