parteEntera :: Float -> Int 
parteEntera n | n >= 0 && n < 1 = 0
              | otherwise = 1 + parteEntera (n-1)

multiploTres :: Int -> Bool 
multiploTres n | n==0 = True 
               | n<0 = False 
               | otherwise = multiploTres (n-3)

sumaImpares :: Int -> Int 
sumaImpares n | n == 1 = 1
              | otherwise = (n-1)*2+1 + sumaImpares(n-1)

medioFact :: Int ->Int 
medioFact 0 = 1
medioFact 1 = 1
medioFact n = n * medioFact (n-2)

sumaDigitos :: Int -> Int 
sumaDigitos 0 = 0
sumaDigitos n = mod n 10 + sumaDigitos (div n 10)

digitosIguales :: Int  -> Bool
digitosIguales n | n < 10 = True
                 | n < 100 = (mod n 10) == (div (mod n 100) 10)
                 | otherwise = ((mod n 10) == (div (mod n 100) 10)) && digitosIguales (div n 10)