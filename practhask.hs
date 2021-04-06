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


-- 2


-- working ---
freeVars :: LT -> [Var]
freeVars (Va v) = ([v])
freeVars (AP a b) = freeVars a `union` freeVars b
--        delete    lt
--          |        |
--($) :: (a -> b) -> a -> b 
freeVars (La v lt) = delete v $ freeVars lt

-- freeAndBoundVars (La "y" (AP (Va "y") (La "x" (AP (Va "x") (Va "z")))))
freeAndBoundVars :: LT -> ([Var],[Var])
freeAndBoundVars (Va v) = ([v],[])
freeAndBoundVars (AP a b) = (fst (freeAndBoundVars a) `union`  fst (freeAndBoundVars b),snd (freeAndBoundVars a) `union` snd (freeAndBoundVars b))
freeAndBoundVars (La v lt) = (delete v $ fst (freeAndBoundVars lt),if v `elem` fst (freeAndBoundVars lt) then v:snd (freeAndBoundVars lt) else snd (freeAndBoundVars lt))

-- subs_i (La "x" (AP (Va "x") (Va "x"))) "x" (AP (Va "y") (Va "z"))
subs_i :: LT->Var->LT->LT
subs_i var@(Va v) x e = if v==x then e else var
subs_i ap@(AP a b) x e = AP (subs_i a x e) (subs_i b x e)   
subs_i la@(La v lt) x e | x == v = subs_i lt x e
                        | otherwise = La v (subs_i lt x e)


--  subs (La "x" (AP (Va "x") (La "x" (Va "x")))) (AP (Va "y") (Va "z"))
--WORKING ON IT
subs::LT->LT->LT
subs var@(Va x) _ = error "No es un redex"
subs (AP a b) e =  AP (subs a b) e 
subs exp@(La x lt) e = subs_i exp x e





-- 3 -- 

esta_normal::LT->Bool
esta_normal (Va v) = True
--esta_normal (La v lt) | lt == LT
esta_normal (AP a b) = esta_normal a

-- 3 

-- 4 

-- 5 

-- 6 

-- 7

-- 8

-- 9
--sdsdsd--