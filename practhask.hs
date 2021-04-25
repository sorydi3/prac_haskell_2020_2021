import Data.List
type Var = String
data LT  = Va Var | La Var LT | AP LT LT
-- Llista de lletres de l'alfabet com a possibles noms de variables
possible_vars = ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

-- Mostra per pantalla un lambda terme de tipus <LT> 
instance Show LT where 
    show (Va x) = x
    show (La v lt) = "(\\"++v++". "++ show lt ++ ")"
    show (AP lt lv) = "("++ show lt++" "++ show lv ++ ")"

-- Comprova si Dos LT son iguals utilitzant l'igualitat de de Bruijn 
instance Eq LT where 
    (==) lt1 lt2 = (a_deBruijn lt1) == (a_deBruijn lt2)

-- Per ordenar es compten el nombre total de variables que hi ha, tant lliures com lligades
instance Ord LT where
    lt1 <= lt2 = (length (fst (freeAndBoundVars lt1))) + ((length (snd (freeAndBoundVars lt1)))) <= (length (fst (freeAndBoundVars lt2))) + ((length (snd (freeAndBoundVars lt2))))


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


--          Funció d'immersió de subst. Realitza el mateix però s'ha de passar un paràmetre addicional
--          indicant quines són les variables lligades en el moment de cridar a la funció.
-- Param 1: Lambda-terme on realitzar la substitució.
-- Param 2: Variable a substituir.
-- Param 3: Nou lamda-terme substitut.
-- Param 4: Llista de variables que estan lligades en el moment de cridar la funció.
-- Retorna: Retorna el lambda-terme <param 1> on s'han substituit les instàncies lliures de 
--          <param 2> per <param 3> sense realitzar captura.
iSubst :: LT -> Var -> LT -> [Var] -> LT
iSubst (Va v) old_val new_val bVars = 
    if (v == old_val) && not (elem v bVars) 
    then new_val 
    else Va v -- Si és la variable que es vol canviar i no està lligada, llavors substituir per el nou valor
iSubst x@(La v lt) old_val new_val bVars = 
    if elem v (fst (freeAndBoundVars new_val)) -- Si la variable que la abstracció està lligant està dins de les variables lliures del nou lamda-terme
    then iSubst (La fnbv (subst lt v (Va fnbv))) old_val new_val nbVars -- Llavors s'ha de fer una alpha-conversió canvinat les <v> per un altre nom per evitar la captura
    else (La v (iSubst lt old_val new_val (v:bVars))) -- Sinó es pot cridar altre cop a iSubst amb el <lt> i afegint <v> a les variables lligades
    where
        fnbv = firstNonBoundVar (bVars++(fst (freeAndBoundVars new_val))) -- Primera variable lliure que no estigui dins els variables lliures del nou valor, es fa aixi perquè sino dona problemes de recursivitat
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
redueix_un_n x@(La v lt) = La v (snd (iRedueix_un_n lt))
redueix_un_n x@(AP (La v lt1) lt2) = beta_redueix x
redueix_un_n x@(AP lt1 lt2) = 
    if (fst new_lt1) 
    then AP (snd new_lt1) (lt2) 
    else AP (snd new_lt1) (snd new_lt2)
    where
        new_lt1 = iRedueix_un_n lt1
        new_lt2 = iRedueix_un_n lt2


--          Funció d'immersió de redueix_un_n on es retorna un paràmetre addicional indicant si s'ha realitzat alguna beta-reducció o no
-- Param 1: Lambda-terme sobre el que realitzar la reducció
-- Retorna: Tupla on el primer valor indica si s'ha realitzat una beta-reducció i el segon valor és el lambda-terme resultant d'aplicar la funció
iRedueix_un_n :: LT -> (Bool, LT)
iRedueix_un_n x@(Va v) = (False, x) -- S'indica que no s'ha realitzat cap beta-reducció
iRedueix_un_n x@(La v lt) = (fst red, La v (snd red))
    where
        red = iRedueix_un_n lt
iRedueix_un_n x@(AP (La v lt1) lt2) = (True, beta_redueix x) -- S'indica que ja s'ha realitzar una beta-reducció
iRedueix_un_n x@(AP lt1 lt2) = 
    if (fst new_lt1) -- Si ja s'ha realitzat una beta-reducció a l'esquerra no fa falta fer-ho a la dreta
    then (fst new_lt1, (AP (snd new_lt1) (lt2)))
    else (fst new_lt2, (AP (snd new_lt1) (snd new_lt2))) -- Si no s'ha fet cap beta-reducció, llavors s'intenta fer a la dreta
    where
        new_lt1 = iRedueix_un_n lt1
        new_lt2 = iRedueix_un_n lt2


--          Funció on es passa un lambda-terme i s'aplica la primera beta-reducció en ordre aplicatiu.
--          Si el lambda-terme passat no té cap redex, retorna el mateix lambda-terme.
-- Param 1: Lambda-terme sobre el que realitzar la reducció.
-- Retorna: El lambda-terme <param 1> amb la primera beta-reducció en ordre aplicatiu feta, 
--          si no és un redex retorna el mateix lambda-terme.
redueix_un_a :: LT -> LT
redueix_un_a x@(Va v) = x
redueix_un_a x@(La v lt) | not (esta_normal lt) = La v (redueix_un_a lt)
                         | otherwise = x
redueix_un_a x@(AP y@(La v lt1) lt2) = redueix_un_n $ 
    (AP  (
            if not(esta_normal y) 
            then (redueix_un_a y) 
            else y
        ) 
        (
            if not(esta_normal lt2)
            then (redueix_un_a lt2)
            else lt2
        )
    )
redueix_un_a x@(AP lt1 lt2) = AP (redueix_un_n lt1) (redueix_un_n lt2)


--          Normalitza un lambda-terme, retorna la llista de passes fetes fins arribar a la forma normal seguint l'ordre normal.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Llista de lambda-termes, seqüència de reduccions, des del lambda-terme original <param 1> fins
--          el lambda-terme en forma normal.
l_normalitza_n :: LT -> [LT]
l_normalitza_n x@(Va v) = [x]
l_normalitza_n x = if esta_normal x then [x] else [x] ++ l_normalitza_n (redueix_un_n x) -- S'agrupen els dos casos, s'ha de fer el mateix tant si és una abstracció com si és una aplicació


--          Normalitza un lambda-terme, retorna la llista de passes fetes fins arribar a la forma normal seguint l'ordre aplicatiu.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Llista de lambda-termes, seqüència de reduccions, des del lambda-terme original <param 1> fins
--          el lambda-terme en forma normal.
l_normalitza_a :: LT -> [LT]
l_normalitza_a ap@(Va a) = [ap]
l_normalitza_a x = if esta_normal x then [x] else [x] ++ l_normalitza_a (redueix_un_a x)


--          Normalitza un lambda-terme seguint l'ordre normal, retorna una tupla amb el nombre de passos necessàris per arribar a la forma normal
--          i la forma normal del lambda-terme.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Tupla amb el primer valor com el nombre de passos a realitzar per arribar a la forma normal
--          i el segon valor el lambda-terme en forma normal.
normalitza_n :: LT -> (Integer, LT)
normalitza_n x = iNormalitza redueix_un_n x 0


--          Normalitza un lambda-terme seguint l'ordre aplicatiu, retorna una tupla amb el nombre de passos necessàris per arribar a la forma normal
--          i la forma normal del lambda-terme.
-- Param 1: Lambda-terme sobre el que buscar la forma normal.
-- Retorna: Tupla amb el primer valor com el nombre de passos a realitzar per arribar a la forma normal
--          i el segon valor el lambda-terme en forma normal.
normalitza_a :: LT -> (Integer, LT)
normalitza_a x = iNormalitza redueix_un_a x 0




-------------------------------------------------------------------------------------------------------
------------------------------------------- METE LLENGUATGE -------------------------------------------
-------------------------------------------------------------------------------------------------------
identitat = (La "x" (Va "x"))
true = (La "x" (La "y" (Va "x")))
false = (La "x" (La "y" (Va "y")))
meta_not = (La "t" (AP (AP (Va "t") (false)) (true)))

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

suma = (La "m" (La "n" (La "f" (La "x" (AP (AP (Va "m") (Va "f")) (AP (AP (Va "n") (Va "f")) (Va "x")))))))
prod = (La "m" (La "n" (La "f" (La "x" (AP (AP (Va "m") (AP (Va "n") (Va "f"))) (Va "x"))))))
eszero = (La "n" (AP (AP (Va "n") (La "x" false)) true))
prefn = (La "f" (La "p" (AP (AP (tupla) (false)) (AP (AP (AP (meta_fst) (Va "p")) (AP (meta_snd) (Va "p"))) (AP (Va "f") (AP (meta_snd) (Va "p")))))))
prec = (La "n" (La "f" (La "x" (AP (meta_snd) (AP (AP (Va "n") (AP (prefn) (Va "f"))) (AP (AP (tupla) (true)) (Va "x")))))))

punt_fixe_T = (AP (La "x" (La "y" (AP (Va "y") (AP (AP (Va "x") (Va "x")) (Va "y"))))) (La "x" (La "y" (AP (Va "y") (AP (AP (Va "x") (Va "x")) (Va "y"))))))

fact = (AP (punt_fixe_T) (La "f" (La "n" (AP (AP (AP (eszero) (Va "n")) (un)) (AP (AP (prod) (Va "n")) (AP (Va "f") (AP (prec) (Va "n"))))))))


-------------------------------------------------------------------------------------------------------
----------------------------------------------- DE BRUIJN NOTATION ------------------------------------
-------------------------------------------------------------------------------------------------------
type Nombre = Int
context = ["x","y","z","a","b","c","q","s","f","n"]
type Context = [Var] -- Llista de variables per el Context
data LTdB = Nat Nombre | Ap LTdB LTdB | L LTdB -- Tipus de dades per representar els lambdes termes en format debruijn


--          Instancia d'igualitat <Eq> per comparar dues termes en format de Bruijn
-- Param 1: Lambda Terme en format de Bruijn <LTdB>
-- Param 2: Lambda Terme en format de Bruijn <LTdB>
-- Retorna: True si les dues termes son iguals altrament Fals
instance Eq LTdB where
    (==) (Nat x) (Nat x') = x==x'
    (==) (L l1) (L l2) = l1 == l2
    (==) (Ap l1 l2) (Ap l1' l2') = l1==l1' && l2==l2'
    (==) _  _ = False



--          Instancia  <show> per poder mostrar per pantalla els termes en format de Bruijn
-- Param 1: Lambda Terme en format de Bruijn <LTdB>
-- Param 2: Lambda Terme en format de Bruijn <LTdB>
-- Retorna: Mostra per pantalla el terme amb el seguent format ex --> \.0 equival a \x.x 
instance Show LTdB where 
    show (Nat x) = show x
    show (L lt) = "(\\."++ show lt ++ ")"
    show (Ap lt lv) = "("++ show lt++" "++ show lv ++ ")"


--          Funció que rep un <LT> i retorna aquest mateix terme pero en format de Bruijn <LTdB> 
-- Param 1: Lambda terme que volem transformar en format de Bruijn
-- Retorna: El lambda terme en forma de Bruijn <LTdB>
a_deBruijn :: LT -> LTdB
a_deBruijn lt = i_deBruijn lt context -- li passem el lamda i la llista del context


--          Funció inmersiva que rep un  LT i retorna aquest mateix terme pero en format de Bruijn
-- Param 1: Lambda terme que volem transformar en format de Bruijn
-- Param 2: Llista variables del context
-- Retorna: El lambda terme en forma de Bruijn <LTdB>
i_deBruijn :: LT -> Context -> LTdB
i_deBruijn va@(Va x) xs  = Nat (index xs x)
i_deBruijn la@(La x lt) xs = L (i_deBruijn lt (x:xs))
i_deBruijn ap@(AP a b) xs = Ap (i_deBruijn a xs) (i_deBruijn b xs)


--          Funció que rep un <LTdB> i retorna aquest mateix terme pero en format de Bruijn <LT> 
-- Param 1: Lambda terme que volem transformar en format LTdB
-- Retorna: El lambda terme en forma de Bruijn <LT>
de_deBruijn :: LTdB -> LT
de_deBruijn ltd = i_de_deBruijn ltd context


--          Funció inmersiva que rep un  LTdB i retorna aquest mateix terme pero en format LT
-- Param 1: Lambda terme que volem transformar en format LT
-- Param 2: Llista variables del context
-- Retorna: El lambda terme en forma de Bruijn LT
i_de_deBruijn :: LTdB -> Context -> LT
i_de_deBruijn va@(Nat v) xs = Va (xs!!v)
i_de_deBruijn la@(L lt) xs = La t' (i_de_deBruijn lt (t':xs))
                            where
                              v = charToString ['a'..'z'] -- Generem possibles noms de variables
                              t' = last (reverse [x | x<-v, not (x `elem` xs)]) -- triem un nom de variables tinguen en compte el context

i_de_deBruijn ap@(Ap l1 l2) xs = AP (i_de_deBruijn l1 xs) (i_de_deBruijn l2 xs) 



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
                        | otherwise = b:(replaceFirst a x bc)


--          Immersió de la funció normalitza_n i normalitza_a. 
--          Realitza la mateixa funció excepte que a aqueste se li passa dos paràmetres extres.
-- Param 1: Funció a aplicar sobre el <param 2>
-- Param 2: Lambda-terme sobre el que buscar la forma normal.
-- Param 3: Nombre de passes que s'han realitzat fins al moment de cridar la funció.
-- Retorna: El mateix que normalitza_n i normalitza_a
iNormalitza :: (LT -> LT) -> LT -> Integer -> (Integer, LT)
iNormalitza _ x@(Va v) n = (n,x)
iNormalitza f x n = if esta_normal x then (n,x) else iNormalitza f (f x) (n+1)


--          Retorna l'index d'un element x dins d'una llista
-- Param 1: Llista d'elements
-- Param 2: L'element que estem buscan
-- Retorna: Retorna la posicio de l'element x ,i si no hi es llaçem una excepcio
index :: Context -> Var -> Int
index [] _ = error "variable no esta a la llista"
index (x:xs) x' | x == x' = 0
                | otherwise = 1 + index xs x'


--          Transforma una llista de chars en una llista de Var 
-- Param 1: Llista de chars per tranformar
-- Retorna: El mateix que normalitza_n i normalitza_a
charToString :: [Char] -> [Var]
charToString [] = []
charToString (c:cs) = [c]:charToString cs

------------------------------------- TERMES PER PROVAR EL FUNCIONAMENT ---------------------------

t1::LT
t1 = (La "z" (AP (AP (Va "z") (Va "x")) (Va "y")))

t1_d::LTdB
t1_d = a_deBruijn t1

t2::LT
t2 = (AP (AP (Va "x") (Va "z")) (Va "y"))

t2_d::LTdB
t2_d = a_deBruijn t2

t3::LT
t3 = (La "z" (AP (Va "x") (La "x" (Va "x"))))

t3_d::LTdB
t3_d = a_deBruijn t3

t5::LT
t5 = (AP (La "x" (AP (Va "x") (Va "x"))) (La "x" (AP (Va "x") (Va "x"))) )

t5_d::LTdB
t5_d = a_deBruijn t5


t6::LT
t6 = (La "x" (La "y" (La "s" (La "z" (AP (AP (Va "x") (Va "s")) (AP (AP (Va "y") (Va "s")) (Va "z")) )))))

t6_d::LTdB
t6_d = a_deBruijn t6


t7::LT
t7 = (La "x" (La "y" (La "s" (La "z" (AP (AP (Va "x") (Va "z")) (AP (AP (Va "y") (Va "s")) (Va "z")) )))))

t7_d::LTdB
t7_d = a_deBruijn t7

t8 :: LT
t8 = AP meta_fst (AP (AP tupla zero) un)

t9::LT
t9 = (AP (AP (La "x" (La "y" (AP (Va "x") (Va "y")))) (Va "w")) (Va "z"))

t9_d::LTdB
t9_d = a_deBruijn t9

t10::LT
t10 = (AP (AP (La "s" (La "q" (AP (AP (Va "s") (Va "q")) (Va "q")))) (La "q" (Va "q"))) (Va "q"))

t10_d::LTdB
t10_d = a_deBruijn t10


suma_1_2 = normalitza_n (AP (AP (suma) (un)) (dos))
suma_5_5 = normalitza_n (AP (AP (suma) (cinc)) (cinc))
prod_4_5 = normalitza_n (AP (AP (prod) (quatre)) (cinc))
fact_0 = normalitza_n (AP fact zero)
fact_1 = normalitza_n (AP fact un)
fact_2 = normalitza_n (AP fact dos)
fact_3 = normalitza_n (AP fact tres)
