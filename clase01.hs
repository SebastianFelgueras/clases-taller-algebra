f x y = x*x+y*y
g x y z = x+y+z*z

doble x = 2*x
suma x y = x+y

normaVectorial x1 x2 = sqrt (x1*x1+ x2*x2)
funcionConstante8 x = 8

absoluto :: Int -> Int 
absoluto x | x < 0 = x*(-1)
           |otherwise = x

maximoabsoluto :: Int -> Int -> Int 
maximoabsoluto x y | a > b = a
                    | otherwise = b
                    where 
                        a = absoluto x
                        b = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x>y && x>z = x
              | y>z && y>x = y
              | otherwise = z

algunoEs0 :: Int -> Int -> Bool 
algunoEs0 x y = x==0 || y==0

ambosSon0 :: Int -> Int -> Bool 
ambosSon0 x y = x==0 && y==0


esMultiploDe :: Int -> Int -> Bool 
esMultiploDe x y = mod x y == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int 
digitoDecenas x = div (mod x 100) 10