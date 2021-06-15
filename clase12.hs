import Utilidades

type Polinomio = [Float]
type Monomio = (Float,Int)

limpiar :: Polinomio -> Polinomio
limpiar [] = []
limpiar (e:es) | e == 0 = limpiar es
 | otherwise = e:es

grado :: Polinomio -> Int
grado p = length p - 1

evaluar :: Polinomio -> Float -> Float
evaluar [] _ = 0
evaluar (p:ps) x = p*x**fromIntegral (grado (p:ps))+ evaluar ps x

suma :: Polinomio -> Polinomio -> Polinomio
suma a b = limpiar (sumaAux a b)
  where
        sumaAux :: Polinomio -> Polinomio -> Polinomio
        sumaAux a b
            | length a == length b = sumaAuxDirecta a b
            | length a < length b  = head b: sumaAux a (tail b)
            | otherwise = head a : sumaAux (tail a) b
            where
                sumaAuxDirecta :: Polinomio -> Polinomio -> Polinomio
                sumaAuxDirecta [] [] = []
                sumaAuxDirecta (c:cs) (d:ds) =  c+d:sumaAuxDirecta cs ds

productoPorEscalar :: Polinomio -> Float -> Polinomio
productoPorEscalar [] _ = []
productoPorEscalar _ 0 = []
productoPorEscalar (a:as) x = a*x:productoPorEscalar as x

resta :: Polinomio -> Polinomio -> Polinomio
resta a b = a `suma` productoPorEscalar b (-1)

productoPorMonomio :: Polinomio -> Monomio -> Polinomio
productoPorMonomio p (a,0) = productoPorEscalar p a
productoPorMonomio p (a,n) = productoPorMonomio p (a,n-1) ++ [0]

producto :: Polinomio -> Polinomio -> Polinomio
producto [] _ = []
producto (p:ps) q = productoPorMonomio q (p,gr)  `suma` producto ps q
    where gr = fromIntegral (grado (p:ps))

hacerPolinomio :: Monomio -> Polinomio
hacerPolinomio (a,n) = a:nulo n
  where
      nulo 0 = []
      nulo n = 0:nulo (n-1)

derivadaMonomio :: Monomio -> Monomio
derivadaMonomio (a,n) = (a*fromIntegral n,n-1)

derivada :: Polinomio -> Polinomio
derivada [] = []
derivada [a] = []
derivada (a:as) = gr*a : derivada as
    where gr = fromIntegral (grado (a:as))

derivadaNesima :: Polinomio -> Int -> Polinomio
derivadaNesima p 0 = p
derivadaNesima p n = derivadaNesima (derivada p) (n-1)

primerCociente :: Polinomio -> Polinomio -> Monomio
primerCociente (p:ps) (q:qs)
  | grp >= grq = (p/q,grp-grq)
  where
      grp = grado (p:ps)
      grq = grado (q:qs)

primerResto :: Polinomio -> Polinomio -> Polinomio
primerResto a b 
  | grado a >= grado b = a `resta` productoPorMonomio b (primerCociente a b) 

division :: Polinomio -> Polinomio -> (Polinomio, Polinomio)
division p [] = error "Division por cero!" 
division [] _ = ([],[])
division p q  | grado p < grado q = ([],p)
  | otherwise = (c:a,b)
  where
      r = primerResto p q
      (c,n) = primerCociente p q
      (a,b) = division r q

hacerMonico :: Polinomio -> Polinomio
hacerMonico [] = []
hacerMonico (p:ps) = productoPorEscalar (p:ps) (1/p)

mcdP :: Polinomio -> Polinomio -> Polinomio
mcdP a b = hacerMonico (mcdPAux a b)
  where
      mcdPAux :: Polinomio -> Polinomio -> Polinomio
      mcdPAux a [] = a
      mcdPAux a b | grado a < grado b = mcdPAux b a
        | otherwise = mcdPAux b (snd (a `division` b)) 

--h = 0.0 == (mcdP [1, 0, 2, 0, 1, 0] [1, 0, 0, 0, -1])!!1

{-
               HACER CUANDO LO VEA EN ALGEBRA
multiplicidad :: Float -> Polinomio -> Int
multiplicidad a p =
  where
      multiplicidadAux-}

--Racional = (p,q) / q>=0 y p ortogonal q
type Racional = (Int,Int)

armarR :: Int -> Int -> Racional
armarR a b = (s*p,q)
    where
        s = signum a * signum b
        (p,q) = coprimizar a b

sumaR :: Racional -> Racional -> Racional
sumaR (a1,a2) (b1,b2) = armarR p q
  where
      p = a1*b2+b1*a2 
      q = a2*b2

multiplicaR :: Racional -> Racional -> Racional
multiplicaR (a1,a2) (b1,b2) = armarR (a1*b1) (a2*b2)

potenciaR :: Racional -> Int -> Racional
potenciaR (p,q) n = armarR (p^n) (q^n)