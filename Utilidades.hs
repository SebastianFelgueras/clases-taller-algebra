module Utilidades where
type Set a = [a]

vacio :: Set a
vacio = []

pertenece :: Eq a => a -> Set a -> Bool
pertenece _ [] = False
pertenece a (h:t) = a == h || pertenece a t

agregar ::Eq a => a -> Set a -> Set a
agregar a c | pertenece a c = c
            | otherwise     = a : c

union :: Eq a => Set a -> Set a -> Set a
union [] b = b
union (a:as) b = union as (agregar a b)

interseccion :: Eq a => Set a -> Set a -> Set a
interseccion [] _ = []
interseccion (a:as) b | pertenece a b = agregar a (interseccion as b)
                      | otherwise     = interseccion as b

diferencia :: Eq a => Set a -> Set a -> Set a
diferencia [] _ = []
diferencia (a:as) b | not (pertenece a b) = agregar a (diferencia as b)
                    | otherwise           = diferencia as b

diferenciaSimetrica :: Eq a => Set a -> Set a -> Set a
diferenciaSimetrica a b = (a `union` b) `diferencia` (a `interseccion` b)

agregarATodos :: Eq a => a -> Set (Set a) -> Set (Set a)
agregarATodos _ [] = []
agregarATodos a (x:xs) = agregar a x `agregar` agregarATodos a xs

generarTuplas :: Eq a => Eq b => a -> Set b -> Set (a,b)
generarTuplas a [] = []
generarTuplas a (x:xs) = agregar (a,x) (generarTuplas a xs)

productoCartesiano :: Eq a => Eq b => Set a -> Set b -> Set (a,b)
productoCartesiano [] _ = []
productoCartesiano (a:as) b = generarTuplas a b `union` productoCartesiano as b

ultimo :: [a] -> a
ultimo [a] = a
ultimo (_:t) = ultimo t 

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar n (h:t) | n == h = t
               | otherwise = h : quitar n t

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas n (h:t) | n == h    = quitarTodas n t
                    | otherwise = h : quitarTodas n t

hayRepetidos :: [Int] -> Bool 
hayRepetidos [] = False 
hayRepetidos (h:t) = pertenece h t || hayRepetidos t

maximo :: Integral a => [a] -> a 
maximo [n,i] = max n i
maximo [n] = n
maximo (h:h2:t) = max (max h h2) (maximo t)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = a : ordenar (quitar a l)
 where a = minimo l

minimo :: Ord a =>[a] -> a 
minimo [n,i] = min n i
minimo [n] = n
minimo (h:h2:t) = min (min h h2) (minimo t)

mcd :: Integral a => a -> a -> a
mcd a 0 = abs a
mcd a b = mcd b (a `mod` b)

esPrimo :: Integral a => a -> Bool 
esPrimo 1 = False 
esPrimo n = esPrimoInterna n (round (sqrt (fromIntegral n))) 
 where
     esPrimoInterna :: Integral a => a -> a -> Bool 
     esPrimoInterna n 1 = True 
     esPrimoInterna n i = mod n i /= 0 && esPrimoInterna n (i-1)

nEsimoPrimo :: Integral a => a -> a 
nEsimoPrimo n = nEsimoPrimoAux n 1 1
    where
        nEsimoPrimoAux n i k | a && i == n = k
                             | a = nEsimoPrimoAux n (i+1) (k+1)
                             | otherwise = nEsimoPrimoAux n i (k+1)
            where 
                a = esPrimo k

generarPrimosHasta :: Integral a => a -> [a]
generarPrimosHasta n = generarPrimosAux n 1
    where
      generarPrimosAux n i | n < i = []
        | esPrimo i = i: generarPrimosAux n (i+1)
        | otherwise = generarPrimosAux n (i+1) 

coprimizar :: Integral a => a -> a -> (a,a)
coprimizar a b = (a `div` k, b `div` k)
    where 
        k = mcd a b