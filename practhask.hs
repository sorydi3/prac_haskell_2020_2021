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


--redueix_un_a

--          Normalitza un lambda-terme, retorna la llista de passes fetes fins arribar a la forma normal.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Llista de lambda-termes, seqüència de reduccions, des del lambda-terme original <param 1> fins
--          el lambda-terme en forma normal.
l_normalitza_n :: LT -> [LT]
l_normalitza_n x@(Va v) = [x]
l_normalitza_n x = if esta_normal x then [x] else [x] ++ l_normalitza_n (redueix_un_n x) -- S'agrupen els dos casos, s'ha de fer el mateix tant si és una abstracció com si és una aplicació

-- l_normalitza_a

--          Normalitza un lambda-terme, retorna una tupla amb el nombre de passos necessàris per arribar a la forma normal
--          i la forma normal del lambda-terme.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Tupla amb el primer valor com el nombre de passos a realitzar per arribar a la forma normal
--          i el segon valor el lambda-terme en forma normal.
normalitza_n :: LT -> (Integer, LT)
normalitza_n x = iNormalitza_n x 0

--          Inmersió de la funció normalitza_n. Realitza la mateixa funció excepte que a aqueste se li passa un paràmetre extra
--          per així comptar quantes passes (crides a la funció) s'han fet.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Param 2: Nombre de passes que s'han realitzat fins al moment de cridar la funció.
-- Retorna: El mateix que normalitza_n
iNormalitza_n :: LT -> Integer -> (Integer, LT)
iNormalitza_n x@(Va v) n = (n,x)
iNormalitza_n x n = if esta_normal x then (n,x) else iNormalitza_n (redueix_un_n x) (n+1)


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
