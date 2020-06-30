-- MATRICI RAPPRESENTATE COME LISTA DI RIGHE 

type Matrix a = [[a]] 
-- Esercizio 1 
-- data una matrice ne calcola le dimensioni, se la matrice `e ben formata (righe con len uguale)
-- altrimenti restituisce (-1,-1)

allTheSame:: (Eq a) => [a] -> Bool
allTheSame [] = False 
allTheSame (x:xs) = all (== x) xs

--row_dim::Num a=> Matrix a -> [Int]
--row_dim [[]] = []
--row_dim [] = []
--row_dim (m:ms) = length m : row_dim ms 

-- non tiene conto del caso in cui ho [[],[]]
matrix_dim::(Eq a, Num a)=> Matrix a -> [Int]        
matrix_dim m | m == [[]] = [] 
             | allTheSame (row_dim m) = length (head m) : [length (row_dim m)]
             | True = [1,-1]
             where row_dim = \ m -> case m of [] -> []
                                              (m:ms) -> length m : row_dim ms 

-- Esercizio 2 funzione vettore somma delle colonne 
            -- per semplicità faccio la matrice trasposta e sommo 
traspose:: Matrix a -> Matrix a 
traspose ([]:_) = []
traspose x = (map head x) : traspose (map tail x)  -- map head [[1,2],[1,2]]  --> [1,1]


sumAux::Num a => Matrix a -> [a]
sumAux [] = []
sumAux (mx:mxs) = [sum_ mx] ++ sumAux mxs
               where sum_ mx = foldl (+) 0 mx 

colsums::Num a => Matrix a -> [a]
colsums mx = sumAux $ traspose mx

-- senza fare la matrice trasposta
-- somma gli i-esimi elementi di ogni riga 

somma_liste :: Num a => [a] -> [a] -> [a]
somma_liste xs     []     = xs
somma_liste []     ys     = ys
somma_liste (x:xs) (y:ys) = (x+y) : somma_liste xs ys

colsums2 :: Num a => Matrix a -> [a]
colsums2 mx = foldl somma_liste (head mx) (tail mx)
    
-- Esercizio 3 calcolare la somma a segni alternati dei valori di ogni colonna
    

-- Esercizio 4 calcolare la lista di coppia di valori minimi e massimi di ogni colonna
calcolaMinMaxRiga :: Ord a => [(a,a)] -> [a] -> [(a,a)]
calcolaMinMaxRiga xs []                     = xs
calcolaMinMaxRiga [] ys                     = map (\x -> (x,x)) ys
calcolaMinMaxRiga ((min_x,max_x):xs) (y:ys) = ( min min_x y, max max_x y ) : calcolaMinMaxRiga xs ys

colMinMax :: Ord a => Matrix a -> [(a,a)]
colMinMax mx = foldl calcolaMinMaxRiga [] mx

-- Esercizio 5 determinare se una matrice è triangolare inferiore

--  Esercizio 6: determinare se una matrice è triangolare superiore

-- Esercizio 7 determinare se una matrice è diagonale
  -- & fra 6 e 7 

-- Esercizio 8 verifica se una matrice quadrata m `e convergente con raggio r
-- Una matrice quadrata M di ordine n si dice convergente con raggio r se il modulo della somma degli elementi di ogni riga, 
-- escluso quello sulla diagonale, è inferiore a r

-- Esercizio 9 calcolare la matrice trasposta

traspose_ :: Matrix a -> Matrix a
traspose_ [] = []
traspose_ mx = foldr merge [] mx
                    where
                        merge :: [a] -> Matrix a -> Matrix a
                        merge []     mxs      = mxs
                        merge xs     []       = map (:[]) xs
                        merge (x:xs) (mx:mxs) = (x:mx) : (merge xs mxs)

-- Esercizio 10 calcolare se una matrice è simmetrica, cioè uguale alla sua trasposta
isSym:: Eq a => Matrix a -> Bool
isSym mx = mx == (traspose_ mx)

-- Esercizio 11 prodotto matriciale -- si assume matrici compatibili 
matrix_prd :: Num a => Matrix a -> Matrix a -> Matrix a
matrix_prd mx1 mx2 = map (\row1 -> map (dotp row1) (traspose_ mx2)) mx1
                            where
                                dotp :: Num a => [a] -> [a] -> a
                                dotp  _  [] = 0
                                dotp  [] _  = 0
                                dotp  (x:xs) (y:ys) = (x*y) + (dotp  xs ys)

-- a mx1 applico la funzione: 
-- per ogni sua lista, applico la funzione dotp tra lista e trasposta di mx2
