--Ejercicio 1--
--(a)--
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Redundant bracket" #-}
f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16 
--(b)--
g :: Integer -> Integer
g 8 = 16
g 16 = 4
g 131 = 1
--(c)--
h :: Integer -> Integer
h x = f (g x) 
--(d)--
k :: Integer -> Integer
k x = g (f x) 
--Ejercicio 2--
--(a)--
absoluto :: Integer -> Integer
absoluto x | x >= 0 = x
           | otherwise = -x
--(b)--
maximoabsoluto :: Integer -> Integer -> Integer
maximoabsoluto x y | absoluto x >= absoluto y = x
                   | otherwise = y
--(c)--
maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z
--(d)--
--Sin Pattern Matching--
algunoEs0 :: Integer -> Integer -> Bool
algunoEs0 x y   | x == 0 = True
                | y == 0 = True
                | otherwise = False
--Con Pattern Matching--
algunoes0 :: Integer -> Integer -> Bool
algunoes0 x 0 = True
algunoes0 0 y = True
algunoes0 x y = False
--(e)--
--Sin Pattern Matching--
ambosSon0 :: Integer -> Integer -> Bool
ambosSon0 x y   | x == 0 && y == 0 = True
                | otherwise = False
--Con Pattern Matching--
ambosson0 :: Integer -> Integer -> Bool
ambosson0 0 0 = True
ambosson0 x y = False
--(f)--
mismoIntervalo :: Integer -> Integer -> Bool
mismoIntervalo x y  | x <=3 && y <= 3 = True
                    | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
                    | x > 7 && y > 7 = True
                    | otherwise = False
--(g)--
sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | (x == y) && (x == z) = x
                    | x == y = x + z
                    | x == z = x + y
                    | y == z = x + y
                    | otherwise = x + y + z
--(h)--
esMultiploDe :: Integer -> Integer -> Bool
esMultiploDe x y    | mod x y == 0 = True
                    | otherwise = False
--(i)--
digitoUnidades :: Integer -> Integer
digitoUnidades x = mod x 10
--(j)--
digitoDecenas :: Integer -> Integer
digitoDecenas x = mod (div x 10) 10
--Ejercicio 3--
estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y   | x == 0 || y == 0 = False
                        | mod x y == 0 =  True
                        | otherwise = False
--Ejercicio 4--
--(a)--
prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (x,y) (v,w) = (x*v) + (y*w)
--(b)--
todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (x,y) (v,w)   | (x < v) && (y < w) = True
                        | otherwise = False
--(c)--
distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x,y) (v,w) = sqrt(((v - x) ** 2) + ((w - y)**2))
--(d)--
sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x,y,z) = x + y + z
--(e)--
sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (x,y,z) k    | k <= 0 = 0 
                                | mod x k == 0 && mod y k == 0 && mod z k == 0 = x + y + z
                                | mod x k == 0 && mod y k == 0 = x + y 
                                | mod x k == 0 && mod z k == 0 = x + z
                                | mod y k == 0 && mod z k == 0 = y + z
                                | mod x k == 0 = x
                                | mod y k == 0 = y
                                | mod z k == 0 = z
                                | otherwise = 0 
--(f)--
posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (x,y,z)    | mod x 2 == 0 = 0
                        | mod y 2 == 0 = 1
                        | mod z 2 == 0 = 2
                        | otherwise = 4
--(g)--
crearPar :: a -> b -> (a,b) 
crearPar x y = (x,y)
--(h)--
invertir :: (a,b)-> (b,a) 
invertir (x,y) = (y,x)
--Ejercicio 5--
todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (x,y,z) | (pf(x) > pg(x) && pf(y) > pg(y) && pf(z) > pg(z)) = True
                    | otherwise = False
pf :: Integer -> Integer
pf n    | n <= 7 = n^2
        | otherwise = 2+n - 1
pg :: Integer -> Integer 
pg n    | mod n 2 == 0 = div n 2
        | otherwise = 3+n + 1
--Ejercicio 6--
bisiesto :: Integer -> Bool
bisiesto x      | mod x 4 /= 0 = False
                | mod x 100 == 0 && mod x 400 /= 0 = False
                | otherwise = True
--Ejercicio 7--
distanciaManhattan :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (x,y,z) (a,b,c) = abs((x-a)+(y-b)+(z-c))
--Ejercicio 8--
comparar :: Integer -> Integer -> Integer
comparar x y    | sumaUltimosDosDigitos (x) < sumaUltimosDosDigitos(y) = 1
                | sumaUltimosDosDigitos (x) > sumaUltimosDosDigitos(y) = -1
                | sumaUltimosDosDigitos (x) == sumaUltimosDosDigitos(y) = 0

sumaUltimosDosDigitos :: Integer -> Integer 
sumaUltimosDosDigitos x = mod (abs(x)) 10 + mod (div (abs(x)) 10) 10


todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x   | x <= 9 = True
                        | mod (div x 10) 10 /= mod x 10 = False
                        | otherwise = todosDigitosIguales (div x 10)