-- EJERCICIO 1
bolitasEnCajas :: Int -> Int -> [[Int]]
bolitasEnCajas n k = iterarSobreListas n k [[]]

union :: Eq a => [a] -> [a] -> [a]
union [] b = b
union (a:as) b = union as (a:b)

agregarATodos :: Eq a => a -> [[a]] -> [[a]]
agregarATodos _ [] = []
agregarATodos a (x:xs) = (a:x) : agregarATodos a xs

iterar :: Int -> [[Int]] -> [[Int]] --k lista base
iterar 0 _ = []
iterar i ls = agregarATodos i ls `union` iterar (i-1) ls

iterarSobreListas :: Int -> Int -> [[Int]] -> [[Int]] -- k listas base
iterarSobreListas 0 _ ls = ls
iterarSobreListas n k ls = iterarSobreListas (n-1) k (iterar k ls)

reemplazarNelemento :: [a] -> Int -> a -> [a] --lista indice nuevoElem
reemplazarNelemento (l:ls) 0 k = k:ls
reemplazarNelemento (l:ls) n k = l:reemplazarNelemento ls (n-1) k

--EJERCICIO 2
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False 
pertenece n (h:t) = n == h || pertenece n t

bolitasEnCajasConPrimeraNoVacia :: Int -> Int -> [[Int]]
bolitasEnCajasConPrimeraNoVacia n k = iterador a
    where 
        a = bolitasEnCajas n k
        iterador :: [[Int]] -> [[Int]]
        iterador [] = []
        iterador (l:ls) | pertenece 1 l = l: iterador ls
                        | otherwise = iterador ls

--EJERCICIO 3
estaOrdenada :: [Int] -> Bool 
estaOrdenada [] = True 
estaOrdenada [a] = True 
estaOrdenada (a:b:ls) = a <= b && estaOrdenada (b:ls)

hayRepetidos ::Eq a => [a] -> Bool 
hayRepetidos [] = False 
hayRepetidos (h:t) = pertenece h t || hayRepetidos t

listasKelementosHastaN :: Int -> Int -> [[Int]]
listasKelementosHastaN k n = devolver a
    where
        a = bolitasEnCajas k n
        devolver :: [[Int]] -> [[Int]]
        devolver [] = []
        devolver (l:ls) | estaOrdenada l && not (hayRepetidos l) = l: devolver ls 
                        | otherwise = devolver ls
   
--EJERCICIO 4
permutaciones :: Eq a => [a] -> [[a]]
permutaciones [a] = [[a]]
permutaciones (l:ls) = insertarEnTodosTodos l (permutaciones ls)

insertarEnTodosTodos :: Eq a => a -> [[a]] -> [[a]]
insertarEnTodosTodos _ [] = []
insertarEnTodosTodos a (l:ls) = insertarEnTodos a l `union` insertarEnTodosTodos a ls

insertarEn :: a -> Int -> [a] -> [a]
insertarEn a 1 ls = a:ls
insertarEn a n (l:ls) = l: insertarEn a (n-1) ls

insertarEnTodos :: a -> [a] -> [[a]]
insertarEnTodos a l = internal (length l+1) a l
    where
        internal :: Int -> a -> [a] -> [[a]]
        internal 0 _ _ = []
        internal k a ls = insertarEn a k ls : internal (k-1) a ls 

generarSucesion :: a -> Int -> [a]
generarSucesion _ 0 = []
generarSucesion a n = a: generarSucesion a (n-1)

eliminar :: Eq a => a -> [a] -> [a]
eliminar _ [] = []
eliminar a (l:ls) | l == a = eliminar a ls
                  | otherwise = l: eliminar a ls

filtrarRepeticiones :: Eq a => [a] -> [a]
filtrarRepeticiones [] = []
filtrarRepeticiones (l:ls) = l:filtrarRepeticiones (eliminar l ls)

sucesionesAB :: Int -> Int -> [[Char]]
sucesionesAB n m = filtrarRepeticiones (permutaciones a)
 where
     a = generarSucesion 'a' n ++ generarSucesion 'b' m

--EJERCICIO 5
sucesionesABC :: Int -> Int  -> Int -> [[Char]]
sucesionesABC n m k = filtrarRepeticiones (permutaciones a)
 where
     a = generarSucesion 'a' n ++ generarSucesion 'b' m ++ generarSucesion 'c' k

--EJERCICIO 6
subconjuntos :: Eq a => Int -> [a] -> [[a]] --combinatorio
subconjuntos k ls = filtrarRepeticionesSet (filtrarInconsistencias (generarCombinatorio ls k))

filtrarInconsistencias :: Eq a => [[a]] -> [[a]]
filtrarInconsistencias [] = []
filtrarInconsistencias (l:ls) | hayRepetidos l = filtrarInconsistencias ls
                              | otherwise = l: filtrarInconsistencias ls

sonMismoSet :: Eq a => [a] -> [a] -> Bool 
sonMismoSet [] _ = True 
sonMismoSet (l:ls) bs = pertenece l bs && sonMismoSet ls bs

eliminarMismoSet :: Eq a => [a] -> [[a]] -> [[a]]
eliminarMismoSet _ [] = []
eliminarMismoSet a (l:ls) | l `sonMismoSet` a = eliminarMismoSet a ls
                          | otherwise = l: eliminarMismoSet a ls

filtrarRepeticionesSet :: Eq a => [[a]] -> [[a]]
filtrarRepeticionesSet [] = []
filtrarRepeticionesSet (l:ls) = l:filtrarRepeticionesSet (eliminarMismoSet l ls)

generarSingleton :: [a] -> [[a]]
generarSingleton [] = []
generarSingleton (l:ls) = [l] : generarSingleton ls

generarCombinatorio :: Eq a => [a] -> Int -> [[a]]
generarCombinatorio ls 1 = generarSingleton ls
generarCombinatorio ls n = agregarATodosTodo ls (generarCombinatorio ls (n-1))

agregarATodosTodo :: Eq a => [a] -> [[a]] -> [[a]]
agregarATodosTodo [] _ = []
agregarATodosTodo (l:ls) xs = agregarATodos l xs ++ agregarATodosTodo ls xs
{-
    [] = []
    [1,2,3,4] 2 = [[1,2],[3,4],]
-}
