-- 1
import Data.List
import Debug.Trace
type Var = String
data LT  = Va Var | La Var LT | AP LT LT
-- Llista de lletres de l'alfabet com a possibles noms de variables
possible_vars = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

--"Show lambda terme"

instance Show LT where 
    show (Va x) = x
    show (La v lt) = "(\\"++v++". "++ show lt ++ ")"
    show (AP lt lv) = "("++ show lt++" "++ show lv ++ ")"


instance Eq LT where 
    (==) (Va x) (Va x') = x==x'


-- NO FUNCIONAAA ESTOY EN ELLO
-- helper functions for association lists
type PairList a = [(a,a)]
-- Donat valor de l'equerra retorna el de la dreta
getValueRight :: (Eq a, Show a) => a -> PairList a-> Maybe a
getValueRight x p@((a,b):ps)
    | (length p) == 0 = Nothing
    | otherwise = if x == a then Just b else getValueRight x ps
getValueRight x p = Nothing

addPair :: a -> b -> [(a, b)] -> [(a, b)]
addPair a b l = (a,b):l

-- Donat el valor de la dreta et retorna el de l'esquerra
getValueLeft :: (Eq a, Show a) => a -> PairList a -> Maybe a
getValueLeft x = getValueRight x . map (\(a,b) -> (b,a))


(=*=) :: LT -> LT -> Bool
a =*= b = eq [] a b where
  eq list (La x lt1) (La y lt2) = eq (addPair x y list) lt1 lt2
  eq list (Va x) (Va y) = if (length list) == 0 then x == y else getValueRight x list == (Just y) && (Just x) == getValueLeft y list
  eq list (AP lt1 lt2) (AP lt3 lt4) = eq list lt1 lt3 && eq list lt2 lt4 -- Simplement mirar si els lt son iguals dos a dos
  eq list _ _ = False


--          Funció que retorna una tupla amb les variables lliures i lligades del lambda-terme donat.
-- Param 1: Lambda-terme a avaluar.
-- Retorna: Tupla (<freeVars>,<boundVars>) amb dos llistes, la primera representant les variables lliures
--          de <param 1> i la segona representant les variables lligades.
freeAndBoundVars :: LT -> ([Var],[Var])
freeAndBoundVars (Va v) = ([v],[]) -- La variable s'afegeix a la llista de lliures
freeAndBoundVars (La v lt) = (
    delete v (fst (freeAndBoundVars lt)), -- S'elimina <v> de la llista de variables lliures perquè ara estarà lligada
    if v `elem` fst (freeAndBoundVars lt) -- Si <v> està a la llista de variables lliures de <lt>
    then (snd (freeAndBoundVars lt))++[v] -- Llavors s'afegeix a la llista de lligades
    else snd (freeAndBoundVars lt)) -- Sino es torna a cridar a la funció amb <lt>
freeAndBoundVars (AP a b) = (
    fst (freeAndBoundVars a) `union` fst (freeAndBoundVars b),
    snd (freeAndBoundVars a) `union` snd (freeAndBoundVars b)) -- S'uneixen els resultats d'aplicar la funció als dos lambda-termes


--          Substitueix a un lambda-terme una variable lliure per el lambda-terme indicat sense realitzar 
--          caputra de variables.
-- Param 1: Lambda-terme on realitzar la substitució.
-- Param 2: Variable a substituir.
-- Param 3: Nou lamda-terme substitut.
-- Retorna: Retorna el lambda-terme <param 1> on s'han substituit les instàncies lliures de 
--          <param 2> per <param 3> sense realitzar captura.
subst :: LT -> Var -> LT -> LT
subst x@(Va v) old_val new_val = iSubst x old_val new_val []
subst x@(La v lt) old_val new_val = iSubst x old_val new_val [v] -- Es crida a iSubst afegint la variable v com a lligada
subst (AP lt1 lt2) old_val new_val = (AP (iSubst lt1 old_val new_val []) (iSubst lt2 old_val new_val []))


--          Funció d'inmersió de subst. Realitza el mateix però s'ha de passar un paràmetre addicional
--          indicant quines són les variables lligades en el moment de cridar a la funció.
-- Param 1: Lambda-terme on realitzar la substitució.
-- Param 2: Variable a substituir.
-- Param 3: Nou lamda-terme substitut.
-- Param 4: Llista de variables que estan lligades en el moment de cridar la funció.
-- Retorna: Retorna el lambda-terme <param 1> on s'han substituit les instàncies lliures de 
--          <param 2> per <param 3> sense realitzar captura.
iSubst :: LT -> Var -> LT -> [Var] -> LT
iSubst (Va v) old_val new_val bVars = if (v == old_val) && not (elem v bVars) then new_val else Va v -- Si és la variable que es vol canviar i no està lligada, llavors substituir per el nou valor
iSubst x@(La v lt) old_val new_val bVars = 
    if elem v (fst (freeAndBoundVars new_val)) -- Si la variable que la abstracció està lligant està dins de les variables lliures del nou lamda-terme
    then iSubst (La fnbv (subst lt v (Va fnbv))) old_val new_val nbVars -- Llavors s'ha defer una alpha-conversió canvinat les <v> per un altre nom per evitar la captura
    else (La v (iSubst lt old_val new_val (v:bVars))) -- Sinó es pot cridar altre cop a iSubst amb el <lt> i afegint <v> a les variables lligades
    where
        fnbv = firstNonBoundVar bVars -- Primera variable lliure
        nbVars = replaceFirst v fnbv bVars -- Les noves variables lligades canviant la que estava abans lligada per la nova
iSubst (AP lt1 lt2) old_val new_val bVars = (AP (iSubst lt1 old_val new_val bVars) (iSubst lt2 old_val new_val bVars))


--          Indica si un lambda-terme està en forma normal o no.
-- Param 1: El lambda-terme a comprovar.
-- Retorna: Cert si està en forma normal, fals altrament.
esta_normal :: LT -> Bool
esta_normal (Va v) = True
esta_normal (La v lt) = esta_normal lt
esta_normal (AP (La v lt) _) = False -- Si hi ha una aplicació on el LT esquerra és una abstracció, llavors ja no està en forma normal
esta_normal (AP a b) = esta_normal a && esta_normal b 


--          Funció que rep un lambda-terme que sigui un redex i el resol. 
--          Si el lambda-terme passat no és un redex, retorna el mateix lambda-terme.
-- Param 1: Lambda-terme sobre el que realitzar la reducció.
-- Retorna: El lambda-terme <param 1> amb la substitució feta, si no és un redex retorna el mateix lambda-terme.
beta_redueix :: LT -> LT
beta_redueix x@(Va v) = x
beta_redueix x@(La v lt) = x
beta_redueix (AP (La v lt1) lt2) = subst lt1 v lt2
beta_redueix x@(AP lt1 lt2) = x


--          Funció on es passa un lambda-terme i s'aplica la primera beta-reducció en ordre normal.
--          Si el lambda-terme passat no té cap redex, retorna el mateix lambda-terme.
-- Param 1: Lambda-terme sobre el que realitzar la reducció.
-- Retorna: El lambda-terme <param 1> amb la primera beta-reducció en ordre normal feta, 
--          si no és un redex retorna el mateix lambda-terme.
redueix_un_n :: LT -> LT
redueix_un_n x@(Va v) = x
redueix_un_n x@(La v lt) = La v (redueix_un_n lt)
redueix_un_n x@(AP (La v lt1) lt2) = beta_redueix x
redueix_un_n x@(AP lt1 lt2) = AP (redueix_un_n lt1) (redueix_un_n lt2)


--          Funció on es passa un lambda-terme i s'aplica la primera beta-reducció en ordre aplicatiu.
--          Si el lambda-terme passat no té cap redex, retorna el mateix lambda-terme.
-- Param 1: Lambda-terme sobre el que realitzar la reducció.
-- Retorna: El lambda-terme <param 1> amb la primera beta-reducció en ordre aplicatiu feta, 
--          si no és un redex retorna el mateix lambda-terme.
redueix_un_a::LT->LT
redueix_un_a x@(Va v) = x
redueix_un_a x@(La v lt) | not (esta_normal lt) = La v (redueix_un_a lt)
                         | otherwise = x
redueix_un_a x@(AP y@(La v lt1) lt2) = redueix_un_n $  (AP  (if not(esta_normal y) then (redueix_un_a y) else y) (if not(esta_normal lt2) then (redueix_un_a lt2) else lt2))
redueix_un_a x@(AP lt1 lt2) = AP (redueix_un_n lt1) (redueix_un_n lt2)


--          Normalitza un lambda-terme, retorna la llista de passes fetes fins arribar a la forma normal seguin l'ordre normal.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Llista de lambda-termes, seqüència de reduccions, des del lambda-terme original <param 1> fins
--          el lambda-terme en forma normal.
l_normalitza_n :: LT -> [LT]
l_normalitza_n x@(Va v) = [x]
l_normalitza_n x = if esta_normal x then [x] else [x] ++ l_normalitza_n (redueix_un_n x) -- S'agrupen els dos casos, s'ha de fer el mateix tant si és una abstracció com si és una aplicació


--          Normalitza un lambda-terme, retorna la llista de passes fetes fins arribar a la forma normal seguin l'ordre aplicatiu.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Llista de lambda-termes, seqüència de reduccions, des del lambda-terme original <param 1> fins
--          el lambda-terme en forma normal.
l_normalitza_a :: LT -> [LT]
l_normalitza_a ap@(Va a) = [ap]
l_normalitza_a x = if esta_normal x then [x] else [x] ++ l_normalitza_a (redueix_un_a x)


--          Normalitza un lambda-terme seguin l'ordre normal, retorna una tupla amb el nombre de passos necessàris per arribar a la forma normal
--          i la forma normal del lambda-terme.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Tupla amb el primer valor com el nombre de passos a realitzar per arribar a la forma normal
--          i el segon valor el lambda-terme en forma normal.
normalitza_n :: LT -> (Integer, LT)
normalitza_n x = iNormalitza redueix_un_n x 0


--          Normalitza un lambda-terme seguin l'ordre aplicatiu, retorna una tupla amb el nombre de passos necessàris per arribar a la forma normal
--          i la forma normal del lambda-terme.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Tupla amb el primer valor com el nombre de passos a realitzar per arribar a la forma normal
--          i el segon valor el lambda-terme en forma normal.
normalitza_a :: LT -> (Integer, LT)
normalitza_a x = iNormalitza redueix_un_a x 0



------------------------------------------------------------------
-- Altres funcions necessàries per al funcionament del programa --
------------------------------------------------------------------

--          De la llista de possibles noms de variables, retorna la primera que no estigui lligada
-- Param 1: Llista de variables lligades
-- Retorna: Primera variable que estigui lliure, és a dir, que no estigui a la llista passada
firstNonBoundVar :: [Var] -> Var
firstNonBoundVar bVars = (possible_vars \\ bVars)!!0 -- Es fa la diferència entre les llistes i s'agafa el primer element


--          Reemplaça la primera instància de <param 1> per <param 2> a la llista donada mirant d'esquerra a dreta.
-- Param 1: Paràmetre a buscar i substituir
-- Param 2: Nou paràmetre substitut de <param 1>
-- Param 3: Llista on realitzar la cerca i substitució
-- Retorna: La llista original però la primera instància de <param 1> substituida per <param 2>
replaceFirst :: (Eq a) => a -> a -> [a] -> [a]
replaceFirst _ _ [] = []
replaceFirst a x (b:bc) | a == b    = x:bc 
                     | otherwise = b : replaceFirst a x bc


--          Inmersió de la funció normalitza_n i normalitza_a. 
--          Realitza la mateixa funció excepte que a aqueste se li passa dos paràmetres extres.
-- Param 1: Funció a aplicar sobre el <param 2>
-- Param 2: Lambda-terme sobre el que buscar la forma normal.
-- Param 3: Nombre de passes que s'han realitzat fins al moment de cridar la funció.
-- Retorna: El mateix que normalitza_n i normalitza_a
iNormalitza :: (LT -> LT) -> LT -> Integer -> (Integer, LT)
iNormalitza _ x@(Va v) n = (n,x)
iNormalitza f x n = if esta_normal x then (n,x) else iNormalitza f (f x) (n+1)


-------------------------------------------------------------------------------------------------------
------------------------------------------- METE LLENGUATGE -------------------------------------------
-------------------------------------------------------------------------------------------------------
identitat = (La "x" (Va "x"))
true = (La "x" (La "y" (Va "x")))
false = (La "x" (La "y" (Va "y")))
meta_not = (La "t" (AP (AP (Va "t") (false)) (true)))

---------------------------------------- OPERANDS ----------------------------------------
-- Rep dos lambda-termes i aplica una AND lògica entre ells
meta_and :: LT -> LT -> LT --------- ---> No es pot posar and pq ja existeix aquesta funcio
meta_and lt1 lt2 = snd (normalitza_n (AP (AP ((La "x" (La "y" (AP (AP (Va "x") (Va "y")) false)))) lt1) lt2))

-- Rep dos lambda-termes i aplica una OR lògica entre ells
meta_or :: LT -> LT -> LT
meta_or lt1 lt2 = snd (normalitza_n (AP (AP (La "x" (La "y" (AP (AP (Va "x") (true)) (Va "y")))) (lt1)) (lt2)))

-- Rep dos lambda-termes i aplica una XOR lògica entre ells
meta_xor :: LT -> LT -> LT
meta_xor lt1 lt2 = snd (normalitza_n (AP (AP (La "x" (La "y" (AP (AP (Va "x") (AP (AP (Va "y") (false)) (true))) (Va "y")))) (lt1)) (lt2)))

------------------------------------ TUPLES I LLISTES ------------------------------------
meta_fst = (La "x" (AP (Va "x") true))
meta_snd = (La "x" (AP (Va "x") false))
tupla = (La "x" (La "y" (La "p" (AP (AP (Va "p") (Va "x")) (Va "y")))))

----------------------------------------- NOMBRES ----------------------------------------
zero = (La "f" (La "x" (Va "x")))
un = (La "f" (La "x" (AP (Va "f") (Va "x"))))
dos = (La "f" (La "x" (AP (Va "f") (AP (Va "f") (Va "x")))))
tres = (La "f" (La "x" (AP (Va "f") (AP (Va "f") (AP (Va "f") (Va "x"))))))
quatre = (La "f" (La "x" (AP (Va "f") (AP (Va "f") (AP (Va "f") (AP (Va "f") (Va "x")))))))
cinc = (La "f" (La "x" (AP (Va "f") (AP (Va "f") (AP (Va "f") (AP (Va "f") (AP (Va "f") (Va "x"))))))))

suc = (La "n" (La "f" (La "x" (AP (AP (Va "n") (Va "f")) (AP (Va "f") (Va "x"))))))


-------------------------------------------------------------------------------------------------------
----------------------------------------------- DE BRUIJN NOTATION ------------------------------------
-------------------------------------------------------------------------------------------------------
type Nombre = Integer
type Context=[Var]
data LTdB = Nat Nombre |Ap LTdB LTdB | L Var LTdB

t1::LT
t1 = (La "z" (AP (AP (Va "z") (Va "x")) (Va "y")))

t2::LT
t2 = (AP (AP (Va "x") (Va "z")) (Va "y"))

t3::LT
t3 = (La "z" (AP (Va "x") (La "x" (Va "x"))))

instance Eq LTdB where
    (==) (Nat x) (Nat x') = x==x'

-- TRET DE STACK OVERFLOW, MAYBE??
-- https://stackoverflow.com/questions/47704945/haskell-lambda-alpha-equivalence
-- varN :: Eq a => a -> [a] -> Int
-- varN a xs = v 0 xs where
--   v n (x:xs) = if a == x then n else v (n+1) xs

-- a =*= b = eq [] [] a b in where
--   eq k l (Lam n x) (Lam m y) = eq (n:k) (m:l) x y
--   eq k l (Var n) (Var m) = varN n k == varN m l
--   eq k l (App f u) (App g v) = eq k l f g && eq k l u v
--   eq k l _ _ = False


instance Show LTdB where 
    show (Nat x) = show x
    show (L _ lt) = "(\\."++ show lt ++ ")"
    show (Ap lt lv) = "("++ show lt++" "++ show lv ++ ")"

index::Context->Var->Integer
index [] _ = error "variable no esta a la llista"
index (x:xs) x' | x == x' = 0
                | otherwise = 1 + index xs x'


i_deBruijn::LT->[Var]->LTdB
i_deBruijn va@(Va x) xs  = Nat (index xs x)
i_deBruijn la@(La x lt) xs = L x (i_deBruijn lt (x:xs))
i_deBruijn ap@(AP a b) xs = Ap (i_deBruijn a xs) (i_deBruijn b xs)


a_deBruijn::LT->LTdB
a_deBruijn lt = i_deBruijn lt ["x","y","z","a","b","c"]
    
--de_deBruijn::LTdB-> LT


