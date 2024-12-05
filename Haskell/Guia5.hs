{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}
--Práctica 5--
--Ejercicio 1--
--(a)--
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud(xs)
--(b)--
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs
--(c)--
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs
--(d)--
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]
--Ejercicio 2--
--(a)--
pertenece :: (Eq t) => t -> [t] -> Bool 
pertenece _ [] = False
pertenece n (x:xs)  | n == x = True
                    | otherwise = pertenece n (xs)
--(b)--
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:xs) | x /= head(xs) = False
                    | otherwise = todosIguales (xs)
--(c)--
todosDistintos :: (Eq t) => [t] -> Bool 
todosDistintos [] = True
todosDistintos [_] = True
todosDistintos (x:xs)   | pertenece x xs = False
                        | otherwise = todosDistintos xs
--(d)--
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [_] = False
hayRepetidos (x:xs)     | todosDistintos (x:xs) == True = False
                        | otherwise = True
--(e)--
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar elem (x:xs)  | elem == x = xs
                    | otherwise = x : quitar elem (xs)
--(f)--
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos elem (x:xs) | elem == x = quitarTodos elem (xs)
                        | otherwise = x : quitarTodos elem (xs)
--(g)--
eliminarRepetidos  :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x (xs))
--(h)--
--mismosElementos :: (Eq t) => [t] -> [t] -> Bool
