import Utilidades

--EJERCICIO 1
cadaEcTieneSoluc :: [(Int, Int, Int)] -> Bool
cadaEcTieneSoluc [] = True
cadaEcTieneSoluc (e:es) = tieneSolucion e && cadaEcTieneSoluc es

tieneSolucion :: (Int, Int, Int) -> Bool
tieneSolucion (coef, r, modulo) = r `mod` Utilidades.mcd coef modulo == 0

--EJERCICIO 2
--Si ya esta simplificado, es trivial que si o si las ecuaciones por separado tienen solución
tieneSolucionSimplif :: [(Int, Int)] -> Bool
tieneSolucionSimplif (e:es) =  True 

--EJERCICIO 4
dirichlet :: Integral a => a -> a -> a
dirichlet r m = dirichletAux r m 0
    where
        dirichletAux r m i | i `mod` m == r && esPrimo i = i
                | otherwise = dirichletAux r m (i+1)
{-
aX = r (mod)
mod|ax-r


-}