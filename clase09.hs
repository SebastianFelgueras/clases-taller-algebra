import Utilidades ( interseccion, maximo )
--EJERCICIO 1
--numero base a la cual convertir
digitos :: Integral a => a -> a -> [a]
digitos 0 _ = []
digitos n b = n `mod` b : digitos (n `div` b) b

--EJERCICIO 2
numero :: Integral a => [a] -> a -> a
numero n b = numeroAux n b 0
 where
     numeroAux :: Integral a => [a] -> a -> a -> a
     numeroAux [] _ _ = 0
     numeroAux (n:ns) b potencia = n*(b^potencia) + numeroAux ns b (potencia+1)

--EJERCICIO 3
divisores :: Integral a => a -> [a]
divisores n = divisoresAux n 1
    where
        divisoresAux :: Integral a => a -> a -> [a]
        divisoresAux n i | abs n == i = [i]
                         | n `mod` i == 0 = i : divisoresAux n (i+1)
                         | otherwise = divisoresAux n (i+1)

--EJERCICIO 4
mcdDef :: Integral a => a -> a -> a
mcdDef 0 n = n
mcdDef n 0 = n
mcdDef a b = Utilidades.maximo (divisores a `Utilidades.interseccion` divisores b)

--EJERCICIO 6
mcd :: Integral a => a -> a -> a
mcd a 0 = abs a
mcd a b = mcd b (a `mod` b)

--Ejercicio 8
mcm :: Integral a => a -> a -> a
mcm 0 0 = 0
mcm a b = (a*b) `div` mcd a b

--EJERCICIO 9
--Devuelve (s,t,mcd)
emcd :: Integral a => a -> a -> (a,a,a)
emcd a b = subida pasarASubida vec
    where
        vec = reverse aRevertir
        (sigma,tau,aRevertir,mcd) = bajada a b
        pasarASubida = (sigma,tau,mcd)
        -- recibe a b y devuelve (ultimo s, ultimo t, vector con pares (a,b) del primero al ultimo,mcd)
        bajada :: Integral a => a -> a -> (a,a,[(a,a)],a)
        bajada a b | a `mod` b == 0 = casoDirecto a b
                   | b `mod` (a `mod` b) == 0 = (1,-(a `div` b),[],abs (a `mod` b))
                   | otherwise = (s,t,(a,b):vec,mcd)
                        where
                            (s,t,vec,mcd) = bajada b (a `mod` b) 
                            casoDirecto a b | a >= b = (0,1,[],b)
                                | otherwise = (1,0,[],a)  
        subida :: Integral a => (a,a,a) -> [(a,a)] -> (a,a,a)
        subida tri [] = tri
        subida (sigma,tau,mcd) ((a,b):xs) = subida (tau,sigma-(a `div` b)*tau,mcd) xs

--EJERCICIO 10
--Considerar que es una ecuacion diofantica y conozco una solucion por ejercicio 9
--La idea es despejar k de una_solucion_de_S + k * b' = 0 como si k perteneciese a R
--y despues ver si el ceiling o el floor de k generan un y positivo en esa recta y el que lo genere de los dos, 
--determina con que valor de k me quedo. Despues solo reemplazo en la formula de la diofántica para obtener s y t
emcdConSMasChico :: Integral a => a -> a -> (a,a)
emcdConSMasChico x1 x2 = (s1 + elegirK * b,t1 - elegirK * a)
    where
        (a,b) = coprimizar x1 x2
        (s1,t1,_) = emcd a b 
        k = -(fromIntegral s1/fromIntegral b)
        elegirK | s1 + floor k * b >=0 = floor k
          | otherwise = ceiling k

coprimizar :: Integral a => a -> a -> (a,a)
coprimizar a b = (a `div` k, b `div` k)
    where 
        k = mcd a b
