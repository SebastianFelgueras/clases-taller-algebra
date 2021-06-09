type Complejo = (Float,Float)

re :: Complejo -> Float
re (r,i) = r

im :: Complejo -> Float
im (r,i) = i

conjugado :: Complejo -> Complejo
conjugado (r,i) = (r,-i)

suma :: Complejo -> Complejo -> Complejo
suma (r1,i1) (r2,i2) = (r1+r2,i1+i2)

producto :: Complejo -> Complejo -> Complejo
producto (a,b) (c,d) = (a*c-b*d,a*d+b*c)

inverso :: Complejo -> Complejo
inverso (a,b) = (a/mc,-b/mc)
  where
    mc = a**2+b**2

cociente :: Complejo -> Complejo -> Complejo
cociente a b = a `producto` inverso b

potencia :: Complejo -> Int -> Complejo
potencia _ 0 = (1,0)
potencia a n = a `producto` potencia a (n-1)

solucionesCuadratica :: Float -> Float -> Float -> (Complejo, Complejo)
solucionesCuadratica a b c = ((realuno,imaginaria),(realdos,-imaginaria))
    where
        realuno | discriminante >=0 = (-b+sqrt discriminante)/(2*a)
          | otherwise = (-b)/(2*a)
        realdos | discriminante >=0 = (-b-sqrt discriminante)/(2*a)
          | otherwise = (-b)/(2*a)
        discriminante = b**2-a*4*c
        imaginaria | discriminante < 0 = sqrt (-discriminante)/(2*a)
          | otherwise = 0

modulo :: Complejo -> Float
modulo (a,b) = sqrt (a**2+b**2)

pasarACartesianas :: Float -> Float -> Complejo
pasarACartesianas r theta = (r*cos theta,r*sin theta)

argumento :: Complejo -> Float
argumento (a,b) = angulo
  where
      angulo 
        | a >= 0 && b >= 0 = atan (b/a) 
        | a >= 0 && b < 0  = 2*pi-atan (-b/a)
        | a < 0  && b >= 0 = pi-atan (-b/a)
        | a < 0  && b < 0  = pi+atan (b/a)

raizCuadrada :: Complejo -> (Complejo,Complejo)
raizCuadrada z = ((r,theta),(r,theta+pi))
  where
      r = sqrt (modulo z)
      theta = argumento z/2

raicesNEsimas :: Int -> [Complejo]
raicesNEsimas n = raicesNEsimasAux n 0
  where
      raicesNEsimasAux :: Int -> Int -> [Complejo]
      raicesNEsimasAux n i
        | i >= n    = []
        | otherwise = raizActual : raicesNEsimasAux n (i+1)
        where
            arg = (2.0*pi*fromIntegral i)/fromIntegral n
            raizActual = (cos arg,sin arg) 

potenciasRaizNEsima :: Int -> Int -> [Complejo]
potenciasRaizNEsima n k = elevarATodas raizk n
    where
        raizk = raicesNEsimas n!!k
        elevarATodas :: Complejo -> Int -> [Complejo]
        elevarATodas _ (-1) = []
        elevarATodas z i = potencia z i : elevarATodas z (i-1)
{-
(a+bi)*(c+di) = a*c+adi+bic-bd = ac-bd + (ad+bc) * i
-}