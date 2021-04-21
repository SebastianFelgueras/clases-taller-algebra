import Debug.Trace
-- EJERCICIO 7
factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

mayorFactHasta :: Int -> Int
mayorFactHasta n = factorial ((mayorFactHastaInterno n 1)-1)

mayorFactHastaInterno :: Int -> Int -> Int --devuelve el numero cuyo factorial se pasa de n
mayorFactHastaInterno n i | factorial i > n = i
                          | otherwise       = mayorFactHastaInterno n (i+1) 

-- EJERCICIO 8
esFact :: Int -> Bool 
esFact n = esFactInterno n 0

esFactInterno :: Int -> Int -> Bool
esFactInterno n i | factorial i == n = True
                  |  n == i          = False 
                  | otherwise        = esFactInterno n (i+1)

-- EJERCICIO 9 (anda pero es MUY lento para numeros no tan grandes)
fibonacci :: Int -> Int --n esimo de la sucesion de fibo
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

esFibonacci :: Int -> Bool --a partir del 5 crece mas rapido que una lineal
esFibonacci 4 = False 
esFibonacci n | n <= 5    = True 
              | otherwise = esFibonacciInterno n 1

esFibonacciInterno :: Int -> Int -> Bool 
esFibonacciInterno n i | n == i           = False 
                       | fibonacci i == n = True 
                       | otherwise        = esFibonacciInterno n (i+1)

-- EJERCICIO 10
esPrimo :: Int -> Bool
esPrimo 1 = True 
esPrimo n = esPrimoInterna n 2

esPrimoInterna :: Int -> Int -> Bool 
esPrimoInterna n i | n == i         = True 
                   | n `mod` i == 0 = False
                   | otherwise      = esPrimoInterna n (i+1) 


esSumaInicialDePrimos :: Int -> Bool 
esSumaInicialDePrimos n = esSumaInicialDePrimosInterna n 1 0

esSumaInicialDePrimosInterna :: Int -> Int -> Int -> Bool 
--esSumaInicialDePrimosInterna n i suma | trace ("suma:" ++ show suma) False = undefined
esSumaInicialDePrimosInterna 1 _ _ = True 
esSumaInicialDePrimosInterna n i suma | suma == n = True
                                      | n == i = False
                                      | esPrimo i = esSumaInicialDePrimosInterna n (i+1) (suma+i)
                                      | otherwise = esSumaInicialDePrimosInterna n (i+1) suma

--EJERCICIO 11
sumaDivisores :: Int -> Int 
sumaDivisores n = sumaDivisoresInterna n 1 0 

sumaDivisoresInterna :: Int -> Int -> Int -> Int 
sumaDivisoresInterna n k suma | k == n = suma + n
                              | esDivisor n k = sumaDivisoresInterna n (k+1) (suma+k)
                              | otherwise     = sumaDivisoresInterna n (k+1) suma

esDivisor :: Int -> Int -> Bool --si k es divisor de n
esDivisor n k = n `mod` k == 0

tomaValorMax :: Int -> Int -> Int 
tomaValorMax n1 n2 = tomaValorMaxInterna n1 n2 n1 0

tomaValorMaxInterna :: Int -> Int -> Int -> Int -> Int
tomaValorMaxInterna n1 n2 i maximo_actual | i > n2           = maximo_actual
                                          | a >= n1 && a<=n2 && a > maximo_actual = tomaValorMaxInterna n1 n2 (i+1) a
                                          | otherwise        = tomaValorMaxInterna n1 n2 (i+1) maximo_actual
                                        where
                                            a = sumaDivisores i

--EJERCICIO 12
tomaValorMin :: Int -> Int -> Int 
tomaValorMin n1 n2 = tomaValorMinInterna n1 n2 n1 (sumaDivisores n2)

tomaValorMinInterna :: Int -> Int -> Int -> Int -> Int
tomaValorMinInterna n1 n2 i maximo_actual | i > n2 && maximo_actual > n2 = 0
                                          | i > n2 = maximo_actual
                                          | a >= n1 && a<=n2 && a < maximo_actual = tomaValorMinInterna n1 n2 (i+1) a
                                          | otherwise        = tomaValorMinInterna n1 n2 (i+1) maximo_actual
                                        where
                                            a = sumaDivisores i

--EJERCICIO 13
tienePrimoGemelo :: Int -> Bool  
tienePrimoGemelo n = esPrimo n && esPrimo (n+2)

primosGem :: Int -> Int 
primosGem n = primosGemInterna n 3 0

primosGemInterna :: Int -> Int -> Int -> Int 
primosGemInterna n i contador | (n-1) <= i             = contador
                              | tienePrimoGemelo i = primosGemInterna n (i+1) (contador+1)
                              | otherwise          = primosGemInterna n (i+1) contador

--EJERCICIO 14
proxPrimosGem :: Int -> (Int,Int)
proxPrimosGem n = proxPrimosGemInterna (n+1)

proxPrimosGemInterna :: Int -> (Int,Int)
proxPrimosGemInterna i | tienePrimoGemelo i = (i,i+2)
                       | otherwise          = proxPrimosGemInterna (i+1)  

--EJERCICIO 15
largoSecuencia :: Int -> Int 
largoSecuencia n = largoSecuenciaInterna n 0

largoSecuenciaInterna :: Int -> Int -> Int 
largoSecuenciaInterna 1 contador = contador
largoSecuenciaInterna n contador | even n    = largoSecuenciaInterna (div n 2) (contador+1)
                                 | otherwise = largoSecuenciaInterna (3*n+1) (contador+1)

secuenciaMasLargaHasta10000 :: Int  
secuenciaMasLargaHasta10000 = secuenciaMasLargaHasta10000Interna 1 0

secuenciaMasLargaHasta10000Interna :: Int -> Int -> Int 
secuenciaMasLargaHasta10000Interna 10000 mas_larga = mas_larga
secuenciaMasLargaHasta10000Interna i mas_larga = secuenciaMasLargaHasta10000Interna (i+1) (generaMasLarga i mas_larga)

generaMasLarga :: Int -> Int -> Int 
generaMasLarga a b | largoSecuencia a > largoSecuencia b = a
                   | otherwise = b