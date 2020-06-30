
{-# LANGUAGE DatatypeContexts #-}

data (Ord a) => BST a = Void | Node { 
    val :: a, 
    left ,right :: BST a 
} deriving (Eq, Ord, Read, Show)

createBSTree :: Ord a => [a] -> BST a
createBSTree = insert_list Void

-- Esercizio 1 : somma i valori di un albero
    -- dato un nodo sommo i valori di left e right ricorsivamente
sum_all::(Ord a, Num a) => BST a -> a
sum_all Void = 0
sum_all (Node val l r) = val + (sum_all l) + (sum_all r)


-- Esercizio 2 somma i valori dispari di un albero usando odd 
sum_odd :: Integral a => BST a -> a
sum_odd Void = 0
sum_odd (Node val l r) | odd val   = val + (sum_odd l) + (sum_odd r)
                       | otherwise = (sum_odd l) + (sum_odd r)


-- Esercizio 3 valuta se le somme di tutti gli alberi di una lista sono uguali
        -- presa una lista di alberi [t1,...,tn] determina se le somme
        -- s1 , . . . , sn dei valori degli elementi di ogni ti sono tutte uguali fra loro.
samesums::(Ord a, Num a) => [BST a] -> Bool 
samesums [] = True 
samesums (x:[]) = True
samesums (x:xs) = let sum_t = sum_all x in all (\tree -> sum_t == (sum_all tree) ) xs 
                    -- la somma del primo albero deve essere uguale a tutte quelle degli alberi successivi 

--Esercizio 4 scrivere un predicato bstElem per determinare se un valore `e presente in un BST.
bstElem:: Ord a => a -> BST a -> Bool
bstElem _ Void = False 
bstElem x (Node val l r) | x == val = True
                         | x < val = bstElem x l 
                         | x > val = bstElem x r 

-- Esercizio 5 inserimento di un valore 
insert:: Ord a => BST a -> a -> BST a 
insert Void x = Node x Void Void -- crea root 
insert bst x = case( compare x (val bst)) of 
                  EQ -> bst 
                  LT -> let l = left bst in bst{ left = insert l x }
                  GT -> let r = right bst in bst{ right = insert r x }

-- bst = insert Void 10 
-- insert bst 5 
-- insert bst 12 

insert_list:: Ord a => BST a -> [a] -> BST a 
insert_list bst [] = bst 
insert_list bst (x:xs) = insert_list (insert bst x) xs 

-- bst = Void 
-- insert_list bst [10,5,12]

-- Esercizio 6 lista ordinata dei valori 
bst2List :: Ord a => BST a -> [a]
bst2List Void = []
bst2List bst = case bst of   -- visita in order 
                        Node x Void r -> list_right (x:[]) r
                        Node x l Void -> list_left  (x:[]) l
                        Node x l r    -> (bst2List l) ++ x : (bst2List r)
                       -- list left accumula i valori in ordine 
                       -- list right fa il reverse essendo il più grande in fondo  
                       where
                           list_left::Ord a => [a] -> BST a -> [a] 
                           list_left xs Void = xs -- foglia
                           list_left xs (Node val Void Void) = val : xs -- un figlio
                           list_left xs (Node val l Void)    = list_left (val : xs ) l
                           list_left xs bst                  = xs ++ (bst2List bst)
                           
                           list_right::Ord a => [a] -> BST a -> [a]
                           list_right xs Void =  xs -- foglia
                           list_right xs (Node val Void Void) = ( val : xs ) -- un figlio
                           list_right xs (Node val Void r)    = list_right (val : xs ) r
                           list_right xs bst                  = xs ++ (bst2List bst)

-- Esercizio 7 semplice funzione di ordinamento di liste usando gli esercizi precedenti
    -- presa una lista creao l'albero e mi faccio tornare la lista 
order :: Ord a => [a] -> [a]
order = bst2List . (insert_list Void)

-- Esercizio 8 Funzione di filtro degli elementi 
filtertree :: Ord a => (a -> Bool) -> BST a -> [a]
filtertree _ Void = []
filtertree p bst = filtertreeAux p (bst2List bst)
                where  -- mia implementazione di filter 
                    filtertreeAux::( a -> Bool) -> [a] -> [a]
                    filtertreeAux _ [] = []
                    filtertreeAux p (x:xs) | p x       = x : filtertreeAux p xs
                                           | otherwise = filtertreeAux p xs

filtertree_ :: Ord a => (a -> Bool) -> BST a -> [a]
filtertree_ _ Void = []
filtertree_ p bst = filter p (bst2List bst)

-- Esercizio 9
-- Si scriva una funzione annotate che costruisca un nuovo BST che in ogni nodo contenga, al posto del valore originale,
-- una coppia composta dal medesimo valore e dall’altezza del nodo stesso (la lunghezza del massimo cammino, cioè 1 + max(height(sx),height(dx))


-- Esercizio 10
-- Determinare se un albero binario ha la seguente proprietà: per ogni nodo le altezze dei ﬁgli destro e sinistro diﬀeriscono al massimo di 1.

--- WBST --- 
data (Ord a) => WBST a = WVoid | WNode {
    wval :: a,
    weight :: Int,
    wleft, wright :: WBST a
} deriving (Eq, Ord, Read, Show)

get_w::Ord a => WBST a -> Int
get_w WVoid = -1 
get_w bst = weight bst 

insertWBST:: Ord a => WBST a -> a -> WBST a 
insertWBST WVoid x = WNode x 0 WVoid WVoid
insertWBST wbst x = let v = wval wbst
                        l = wleft wbst
                        r = wright wbst in case (compare v x) of 
                                                EQ -> wbst
                                                LT -> let l_ins = insertWBST l x in wbst{ weight = max (get_w l_ins) (get_w r), wleft = l_ins }
                                                GT -> let r_ins = insertWBST r x in wbst{ weight = max (get_w r_ins) (get_w l), wright = r_ins }

-- Esercizio 12
-- Si scriva una funzione diff2next che, dato un albero binario di ricerca, costruisce un albero binario di ricerca (annotato) di coppie
-- dove il primo elemento di ogni coppia è l’elemento dell’albero originale mentre il secondo elemento `e Just(la diﬀerenza rispetto al valore successivo),
-- secondo l’ordinamento dei valori contenuti, oppure Nothing per il nodo di valore massimo

-- Esercizio 13
-- Si scriva una funzione che dato un BST ne restituisce la lista degli elementi ottenuti visitando l’albero a livelli.

_foldTree :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b
_foldTree _ z  Void        = z
_foldTree f z (Node x l r) = f x (_foldTree f z l) (_foldTree f z r)
            -- ricorsivamente sull'albero sinistro e destro 
            -- z valore di inizio -> root come primo passo 

-- Esercizio 14 calcolare l'altezza di un albero con _foldb
treeheight :: Ord a => BST a -> Int
treeheight = _foldTree (\ _ h_l h_r -> 1 + (max h_l h_r) ) (-1)

-- Esercizio 15 Si riscriva la funzione annotate dell’Esercizio 9 usando opportunamente _foldb
-- Esercizio 16 Si riscriva la funzione almostBalanced dell’Esercizio 10 usando opportunamente _foldb

-- Esercizio 17
-- Si scriva una funzione maxDiameter che data una lista l di BST determina il massimo dei diametri dei BST di l
-- Il diametro di un BST `e la lunghezza del massimo cammino fra due nodi, indipendentemente dall’orientamento degli archi


-- Esercizio 18
-- Si scriva un predicato isBST, usando opportunamente fold, che dato un albero veriﬁca se i valori in esso contenuti soddisfano la propriet`a strutturale dei Binary Search Trees
isBST::Ord a => BST a -> Bool 
isBST Void               = True 
isBST (Node _ Void Void) = True 
isBST (Node x l r)       = (isBST l) && (isBST r) && ( case l of 
                                                            Void -> True -- se è foglia 
                                                            otherwise -> x > val l ) -- False se non rispetta 
                                                  && ( case r of 
                                                            Void -> True 
                                                            otherwise -> x < val r )


-- non fatti es AVL 

-- Esercizio 21 Si riscriva la funzione bst2list 
        -- per ogni nodo x applico la visita in order 
        -- la ricorsione all'interno di foldTree mi permette di ottenere il risultato voluto 
bst2List_fold::Ord a => BST a -> [a] 
bst2List_fold = _foldTree (\x lleft lright -> lleft ++ x : lright ) [] 

-- Esercizio 22 Si riscriva la funzione filtertree dell’Esercizio 8 usando opportunamente _foldb
filterTreeFold :: Ord a => (a -> Bool) -> BST a -> [a]
filterTreeFold f = _foldTree (\x list_l list_r -> case f x of True -> list_l ++ x : list_r; otherwise -> list_l ++ list_r ) []

-- Esercizio 23 

-- Esercizio 24
-- Si scriva una funzione limitedVisit che dato un BST e due valori x,y costruisce la lista (ordinata) degli elementi dell’albero compresi nell’intervallo di valori da x a y
limitedVisit :: Ord a => a -> a -> BST a -> [a]
limitedVisit _ _ Void                 = []
limitedVisit x y bst          | x > y = limitedVisit y x bst
limitedVisit x y (Node v l r)         = if (v >= x && v <= y) then
                                                        (limitedVisit x y l) ++ v : (limitedVisit x y r)
                                                        else
                                                            if (v < x) then -- devo andare a destra che è più grande
                                                                limitedVisit x y r -- v < x
                                                            else
                                                                limitedVisit x y l -- v > y

-- Esercizio 25 
-- shiftTozero applicato ai bst 
    -- il minimo è tutto a sinistra 
    -- applico funzione meno a tutti i nodi 

findMin::(Ord a, Num a) => BST a -> a 
findMin Void = error "can't apply to an empty BST"
findMin (Node x Void Void ) = x 
findMin (Node x Void r)     = x 
findMin (Node x l r ) = findMin l 

shiftTozeroAux::(Ord a, Num a) => a -> BST a -> BST a
shiftTozeroAux v_min = _foldTree (\x -> Node (x-v_min)) Void

shiftTozero::(Ord a, Num a) => BST a -> BST a
shiftTozero bst = shiftTozeroAux (findMin bst) bst
