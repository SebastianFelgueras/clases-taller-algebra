estanRelacionados :: (Floating t,Ord t) => t->t->Bool 
estanRelacionados x y | x > 7.0 && y > 7.0 = True 
                      | x <= 3 && y<=3 = True 
                      | x > 3 && x <= 7 && y>3 && y<=7 = True 
                      | otherwise = False 

prodInt :: (Num t) => (t,t)->(t,t)->t
prodInt (x1,y1) (x2,y2) = x1*x2+y1*y2

todoMenor :: (Num t, Ord t) => (t,t)->(t,t)->Bool 
todoMenor (x1,y1) (x2,y2) = x1<x2 && y1<y2

distanciaPuntos :: Floating t => (t,t)->(t,t)->t
distanciaPuntos (x1,y1) (x2,y2) = sqrt (prodInt a a)
        where
            a = (x2-x1,y2-y1)

sumaTerna ::(Int,Int,Int)->Int
sumaTerna (x,y,z) = x+y+z

posicPrimerPar :: (Int,Int,Int)->Int 
posicPrimerPar (x,y,z) | even x = 1 
                        | even y = 2
                        | even z = 2
                        |otherwise = 4

crearPar :: x->y->(x,y)
crearPar x y = (x,y)

invertir :: (x,y)->(y,x)
invertir (x,y) = (y,x)
