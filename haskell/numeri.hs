-- esericizio 1 <<-- def fattoriale 
-- IMPORTANTE L'ORDINE DI DEFINIZIONE DEI CASI BASE 
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

facs :: Int -> Int
facs n = foldl(*) 1 [1..n]


-- esercizio 2 <<-- def coefficente binomiale 
coef_bin:: Int -> Int -> Int
coef_bin n 1 = n     
coef_bin n k  = ( facs n ) `div` ( (facs k)*( facs (n-k) ) )


-- esercizio 3 <<-- combinazioni di n elementi di una lista di numeri  
combinations:: Int -> [a] -> [[a]]    -- prende un intero e una lista di numeri --> ritorna una lista di liste 
combinations 0 _ = [ [] ]
combinations _ [] = []
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ (combinations n xs) 
-- map mostrando gli x delle n-1 combinazioni su tail xs  ++ le n combinazioni su xs


-- senza list comprhension
combinations_alt:: Int -> [a] -> [[a]]
combinations_alt 0 _ = [ [] ]
combinations_alt _ [] = [] 
combinations_alt k (x:xs) = x_start ++ others
    where x_start = [ x : rest | rest <- combinations_alt (k-1) xs ]
          others  = if k <= length xs then combinations_alt k xs else []    

-- calcolo la combinazione tra tail e k-1
-- add ++  combinazione tra k e xs se non ho finito i numeri della lista 