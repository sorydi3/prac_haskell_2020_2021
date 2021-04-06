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


freeAndBoundVars :: LT -> ([Var],[Var])
freeAndBoundVars (Va v) = ([v],[])
freeAndBoundVars (AP a b) = (fst (freeAndBoundVars a) `union`  fst (freeAndBoundVars b),snd (freeAndBoundVars a) `union` snd (freeAndBoundVars b))
freeAndBoundVars (La v lt) = (delete v $ fst (freeAndBoundVars lt),if v `elem` fst (freeAndBoundVars lt) then v:snd (freeAndBoundVars lt) else snd (freeAndBoundVars lt))

subs_i :: LT->Var->LT->LT
subs_i var@(Va v) x e = if v==x then e else var
subs_i ap@(AP a b) x e = AP (subs_i a x e) (subs_i b x e)   
subs_i la@(La v lt) x e | x == v = subs_i lt x e
                        | otherwise = La v (subs_i lt x e)


subs::LT->LT->LT
subs var@(Va x) _ = error "No es un redex"
subs (AP a b) e =  AP (subs a b) e 
subs exp@(La x lt) e = subs_i exp x e



-- 3 -- 

-- 3 

-- 4 

-- 5 

-- 6 

-- 7

-- 8

-- 9
--sdsdsd--