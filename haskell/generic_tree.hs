
-- not binary tree !!
data Tree a = Void | Node a [Tree a] deriving (Eq,Show)


-- Esercizio 1 Si scriva una generalizzazione della funzione foldr delle liste per Alberi Generici
treefold :: (a->[b]->b) -> b -> Tree a -> b
treefold _ z Void = z
treefold f z (Node x branches) = f x (map (treefold f z) branches)
                                        -- map per ogni ramo 


-- Esercizio 2 Si scriva una funzione height per calcolare l’altezza di un albero usando opportunamente la _1treefold
height :: Tree a -> Int
height = treefold(\_ heights -> 1 + foldr max (-1) heights ) (-1)
                    -- per ogni nodo, 1 + il max tra le altezze dei branches 
                                            -- foldr essendo una lista di altezze di [branch]


-- Esercizio 3  Si scriva una funzione simplify per eliminare i ﬁgli Void ridondanti 
simplify :: Tree a -> Tree a
simplify = treefold (\x branches -> Node x (deleteVoids branches) ) Void
                where
                    deleteVoids :: [Tree a] -> [Tree a]
                    deleteVoids xs = foldr (\x acc -> case x of 
                                                        Void -> acc
                                                        otherwise -> x : acc 
                                                        ) [] xs

-- Esercizio 4 
-- Si scrivano le generalizzazioni delle funzioni foldr e foldl delle liste per Alberi Generici aventi i seguenti tipi 
-- (abbiamo bisogno di due “zeri” corrispondenti all’albero vuoto e alla lista di alberi vuota):

 -- treefoldr :: (Eq a,Show a) => (a−>b−>c)−>c−>(c−>b−>b)−>b−>Tree a−>c 
 -- treefoldl :: (Eq a,Show a) => (b−>a−>c)−>c−>(c−>b−>b)−>b−>Tree a−>c
 -- Con queste fold non c’e bisogno di costruire la lista intermedia a cui applicare la funzione di “aggregazione” ma si esegue il lavoro man mano.



-- Es da 5 a 7 hanno bisogno delle funzioni sopra

-- Esercizio 8 Funzione per la trasposta di un albero ( = inverto i figli per ogni nodo )
traspose:: Tree a -> Tree a 
traspose Void        = Void
traspose (Node x []) = Node x []
traspose (Node x branches) = Node x $ reverse (map traspose branches)

-- Esercizio 9  Si scriva un predicato issymm che stabilisce se un albero ha una forma simmetrica 
-- (cioè è uguale, non considerando il contenuto, al suo trasposto)

isSymm :: Tree a -> Bool
isSymm Void = True
isSymm (Node _ []) = True 
isSymm tree = areSimilar tree (traspose tree)
               where
                    areSimilar:: Tree a -> Tree a -> Bool 
                    areSimilar Void Void = True -- caso base 
                    areSimilar (Node _ branch1) (Node _ branch2) = areSimilarVec branch1 branch2
                    areSimilar _ _ = False  -- se no hanno conformazione node | branches come caso sopra sono diversi 

                    areSimilarVec::[Tree a] -> [Tree a] -> Bool
                    areSimilarVec [] [] = True 
                    areSimilarVec (x:xs) (y:ys) = (areSimilarVec xs ys ) && areSimilar x y 
                    areSimilarVec _ _ = False

-- tree = Node 10 [Void, Void , Node 5 [Void, Void] , Void, Void] 
-- treeT = Node 10 [Void,Void,Node 5 [Void,Void],Void,Void]
-- True 

-- Esercizio 11 diameter

-- Esercizio 12 maxPathWeight

-- Esercizio 13 visita preorder

