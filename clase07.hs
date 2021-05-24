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

partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = a `union` agregarATodos n a
    where a = partesN (n-1)

generarTuplas :: Eq a => Eq b => a -> Set b -> Set (a,b)
generarTuplas a [] = []
generarTuplas a (x:xs) = agregar (a,x) (generarTuplas a xs)

productoCartesiano :: Eq a => Eq b => Set a -> Set b -> Set (a,b)
productoCartesiano [] _ = []
productoCartesiano (a:as) b = generarTuplas a b `union` productoCartesiano as b