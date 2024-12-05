{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use even" #-}
{-# HLINT ignore "Use odd" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))
--Práctica 4--
--Ejercicio 1--
fibonacci :: Integer -> Integer
fibonacci n     | n == 0 = 0
                | n == 1 = 1
                | n >= 1 = fibonacci (n-1) + fibonacci (n-2)
--Ejercicio 2--
parteEntera :: Float -> Integer
parteEntera n   | n > (-1) && n < 0  = 0
                | n >= 0 && n < 1 = 0
                | n >= 1 = 1 + parteEntera (n-1)
                | n <= (-1) = (-1) + parteEntera (n+1)
--Ejercicio 3--
esDivisible :: Integer -> Integer -> Bool
esDivisible x 0 = undefined
esDivisible 0 y = True
esDivisible x y | x < y = False
                | x < 0 = esDivisible (x+y) y
                | x > 0 = esDivisible (x-y) y
--Ejercicio 4--
sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares x = sumaImpares (x-1) + (2*x-1)
--Ejercicio 5-- 
medioFact :: Integer -> Integer
medioFact 0 = 1
medioFact 1 = 1
medioFact x = x * medioFact (x-2)
--Ejercicio 6--
sumaDigitos :: Integer -> Integer
sumaDigitos n   | n <= 9 = n
                | otherwise = mod n 10 + sumaDigitos (div n 10)
--Ejercicio 7--
todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n   | n <= 9 = True
                        | mod (div n 10) 10 /= mod n 10 = False
                        | otherwise = todosDigitosIguales (div n 10)
--Ejercicio 8--
iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantidadDigitos n - i))) 10

cantidadDigitos :: Integer -> Integer
cantidadDigitos n   | n == 0 = 0
                    | otherwise = 1 + cantidadDigitos (div n 10)
--Ejercicio 9--
esCapicua :: Integer -> Bool
esCapicua n     | n <= 9 = True
                | iesimoDigito n 1 /= mod n 10 = False
                | otherwise = esCapicua (div (mod n (10 ^ (cantidadDigitos n - 1))) 10)
--Ejercicio 10--
--(a)--
f1 :: Integer -> Integer
f1 n    | n == 0 = 1
        | otherwise =2^n + f1 (n-1)
--(b)--
f2 :: Integer -> Integer -> Integer
f2 n q  | n == 0 = 0
        | otherwise = q^n + f2 (n-1) q
--(c)--
f3 :: Integer -> Integer -> Integer
f3 n = f2 (2*n)
--(d)--
f4 :: Integer -> Integer -> Integer
f4 n q  = f3 n q - f2 (n-1) q
--Ejercicio 11--
--(a)--
eAprox :: Integer -> Float
eAprox n    | n == 0 = 1
            | otherwise = 1/fromIntegral (factorial (n)) + eAprox (n-1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)
--(b)--
e = eAprox 10
--Ejercicio 12--
raizDe2aprox :: Integer -> Float
raizDe2aprox n = sucesion n - 1

sucesion :: Integer -> Float
sucesion n    | n == 0 = 2
              | otherwise = 2 + 1 / raizDe2aprox (n-1)
--Ejercicio 13--
sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble n m  | n == 0 = 0
                    | otherwise = sumatoriaInterna (n-1) m + sumatoriaInterna n m

sumatoriaInterna :: Integer -> Integer -> Integer
sumatoriaInterna n m | m == 0 = 0
                     | otherwise = n^m + sumatoriaInterna n (m-1)
--Ejercicio 14--
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n m     | n == 0 = 0
                        | otherwise = sumaPotenciasM q n m + sumaPotenciasM q (n-1) m
sumaPotenciasM :: Integer -> Integer -> Integer -> Integer
sumaPotenciasM q n m    | m == 0 = 0
                        | otherwise = q^(n+m) + sumaPotenciasM q n (m-1)
--Ejercicio 15--
sumaRacionales :: Integer -> Integer -> Float
sumaRacionales p q      | p == 0 = 0
                        | otherwise = sumaRacionalesAbajo p q + sumaRacionalesAbajo (p-1) q
sumaRacionalesAbajo :: Integer -> Integer -> Float
sumaRacionalesAbajo p q | q == 1 = fromInteger p
                        | otherwise = fromIntegral (p) / fromIntegral (q) + sumaRacionalesAbajo p (q-1)
--Ejercicio 16--
--(a)--
menorDivisor :: Integer -> Integer
menorDivisor n  | n == 1 = 1
                | otherwise = menorDivisorHasta n 2
menorDivisorHasta :: Integer -> Integer -> Integer
menorDivisorHasta n q   | mod n q == 0 = q
                        | otherwise = menorDivisorHasta n (q+1)
--(b)--
esPrimo :: Integer -> Bool
esPrimo n       | n /= 1 && menorDivisor n == n = True
                | otherwise = False
--(c)--
sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = comparaDivisores n m 2

comparaDivisores :: Integer -> Integer -> Integer -> Bool
comparaDivisores n m i  | (i > n) || (i > m) = True
                        | (mod n i == 0) && (mod m i == 0) = False
                        | otherwise = comparaDivisores n m (i + 1)
--(d)--
