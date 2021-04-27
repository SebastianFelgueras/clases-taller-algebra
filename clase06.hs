--EJERCICIO 1
productoria :: [Int] -> Int 
productoria [] = 1
productoria (n:tn) = n * productoria tn

--EJERCICIO 2
sumarN :: Int -> [Int] -> [Int] 
sumarN _ [] = []
sumarN n lista = (head lista + n) : sumarN n (tail lista)

--EJERCICIO 3
sumarElPrimero :: [Int] -> [Int]
sumarElPrimero [] = []
sumarElPrimero l = sumarN (head l) l

--EJERCICIO 4
sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (ultimo l) l

ultimo :: [a] -> a
ultimo [a] = a
ultimo (_:t) = ultimo t 

--EJERCICIO 5
pares :: [Int] -> [Int]
pares [] = []
pares (h:t) | even h = h : pares t
            | otherwise = pares t

--EJERCICIO 6
quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (h:t) | n == h = t
               | otherwise = h : quitar n t

--EJERCICIO 7
quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (h:t) | n == h    = quitarTodas n t
                    | otherwise = h : quitarTodas n t

--EJERCICIO 8
hayRepetidos :: [Int] -> Bool 
hayRepetidos [] = False 
hayRepetidos (h:t) = pertenece h t || hayRepetidos t

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False 
pertenece n (h:t) = n == h || pertenece n t

--EJERCICIO 9
eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (h:t) = h : eliminarRepetidosAlFinal ( quitarTodas h t)

--EJERCICIO 10
eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio l = revertir (eliminarRepetidosAlFinal (revertir l))

revertir :: [a] -> [a]
revertir [] = []
revertir l = ultimo l : revertir (sacarUltimo l)

sacarUltimo :: [a] -> [a]
sacarUltimo [a] = []
sacarUltimo (h:t) = h : sacarUltimo t 

--EJERCICIO 11
maximo :: [Int] -> Int 
maximo [n,i] = max n i
maximo [n] = n
maximo (h:h2:t) = max (max h h2) (maximo t)

--EJERCICIO 12
ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = a : ordenar (quitar a l)
 where a = minimo l

minimo :: Ord a =>[a] -> a 
minimo [n,i] = min n i
minimo [n] = n
minimo (h:h2:t) = min (min h h2) (minimo t)

--EJERCICIO 13
reverso :: [Int] -> [Int]
reverso = revertir

--EJERCICIO 14
concatenar :: [Int] -> [Int] -> [Int]
concatenar [] l = l
concatenar l1 l2 = concatenar (sacarUltimo l1) (ultimo l1:l2)

--EJERCICIO 15
zipi :: [a] -> [b] -> [(a,b)]
zipi [] _ = []
zipi _ [] = []
zipi (h1:t1) (h2:t2) = (h1,h2) : zipi t1 t2