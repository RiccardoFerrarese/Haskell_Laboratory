-- data una lista rimuove gli elementi nelle posizioni pari ( prelude:: delete x xs ) 
-- foldl f z l, usando come valore iniziale z, applica f a tutti gli elementi della lista da sinistra a destra

-- esercizio 1   -->  funzione che data una lista ne costruisce una rimuovendo gli elementi di posizione pari

odd_index_element [] = []
odd_index_element [x] = [x]
odd_index_element (x:xs) = x : odd_index_element  (tail xs)

-- esercizio 2 --> funzione che calcola la somma degli elementi di posizione dispari di una lista. 

sum_odd x = foldl (+) 0 (odd_index_element x)

-- esercizio 3 --> quicksort polimorfo
-- prendendo come pivot il primo elemento  
quicksort :: (Ord a) => [a] -> [a]    ---- -> qualsiasi tipo ordinabile
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted


qsort [] = []
qsort (a:x) = qsort [ b | b <- x, b<=a ] ++ [a] ++ qsort [ b | b <- x, b>a ]

qsort2 [] = []
qsort2 (a:x) = qsort2 [ b | b <- x, b<=a ] ++ (a : qsort2 [ b | b <- x, b>a ] )

-- esercizio 4 --> Scrivere una funzione che calcola i 2 minori elementi dispari di una lista (se esistono)
-- (x, y | x e y dispari, x < all_element, y > x & y < all_element ) 

-- elementi dispari in una lista 
odd_element:: [Int] -> [Int]
odd_element list = [ x | x<-list, x `mod` 2 == 1 ]

min_two_odd:: [Int] -> [Int]
min_two_odd list = take 2 (qsort (odd_element list)) 

-- soluzione alternativa --> ordino e rimuovo gli elementi pari 
removeEven :: Integral a => [a] -> [a]
removeEven l = filter (\a -> (mod a 2) /= 0) l

min_two_odd_2:: Integral a => [a] -> [a]
min_two_odd_2 list = take 2 $ qsort $ removeEven list

-- esercizio 5 --> presa una lista di interi, lista di coppie di cui 
                        -- 1: elemento della posizione 
                        -- 2: somma elementi successivi 


sum_:: [Int] -> Int
sum_ x = foldl (+) 0 x 

coppie1:: [Int] -> [[Int]]
coppie1 [] = []  
coppie1 (x:xs) = [[x, (sum_ xs)]] ++ coppie1 xs 


-- esercizio 6 --> presa una lista di interi, lista di coppie di cui 
                        -- 1: elemento della posizione 
                        -- 2: somma elementi antecedenti a lui

coppieAux:: [Int] -> Int -> [[Int]]
coppieAux [x] n = [[x, n]] 
coppieAux (x:xs) acc = [[x, acc]] ++ coppieAux xs (acc+x) -- mantengo accumulatore elementi precedenti 

coppie2:: [Int] -> [[Int]]
coppie2 x = coppieAux x 0

-- esercizio 7 data una lista costruisce un nuova lista che contiene gli elementi diminuiti del valore minimo.
                -- non visitare gli elementi più di una volta 

minElem::[Int]->Int
minElem [] = 0
minElem [x] = x
minElem (x:y:xs) 
 |x > y = minElem (y:xs)
 |x < y = minElem (x:xs)
 |x == y = minElem (x:xs)

shiftToZeroAux:: [Int] -> Int -> [Int]
shiftToZeroAux [] _ = [] 
shiftToZeroAux (x:xs) n = [x-n] ++ shiftToZeroAux xs n

shiftToZero:: [Int] -> [Int]
shiftToZero x | x == [] = []
              | True = 
                  let min = minElem x 
                  in shiftToZeroAux x min 