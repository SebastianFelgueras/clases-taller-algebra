potencias :: Int -> Int -> Int
potencias 0 _ = 1
potencias n i = i^n + potencias (n-1) i

g1 :: Int -> Int -> Int 
g1 i n = potencias n i - potencias (i-1) i

g2 :: Int -> Int 
g2 n = g2' n n

g2' :: Int -> Int -> Int 
g2' _ 0 = 0
g2' n i = g2' n (i-1) + g1 i n

--lo interpreto como que suma solo si es par
g3 :: Int -> Int 
g3 1 = 0
g3 n | even n = 2^n + g3 (n-1)
     | otherwise = g3 (n-1)

digitosIguales :: Int  -> Bool
digitosIguales n | n < 10 = True
                 | n < 100 = (mod n 10) == (div (mod n 100) 10)
                 | otherwise = ((mod n 10) == (div (mod n 100) 10)) && digitosIguales (div n 10)

sumatoriaDigitosIguales :: Int -> Int 
sumatoriaDigitosIguales 0 = 0
sumatoriaDigitosIguales n | digitosIguales n = n + sumatoriaDigitosIguales (n-1)
                          | otherwise = sumatoriaDigitosIguales (n-1)