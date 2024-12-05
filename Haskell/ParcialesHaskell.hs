{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldr" #-}
--Ejercicio 1--
division :: Int -> Int -> Float
division a b = (fromIntegral a) / (fromIntegral b)

porcentajeDeVotosAfirmativos :: [(String,String)] -> [Int] -> Int -> Float
porcentajeDeVotosAfirmativos xs ys v = (division (sumaVotosAfirmativos ys) v) * 100

sumaVotosAfirmativos :: [Int] -> Int
sumaVotosAfirmativos [] = 0
sumaVotosAfirmativos (x:xs) = x + sumaVotosAfirmativos xs
--Ejercicio 2--
formulasInvalidas :: [(String,String)] -> Bool
formulasInvalidas [] = False
formulasInvalidas ((a,b):(xs))   | a == b = True 
                                 | pertenece a xs || pertenece b xs = True 
                                 | otherwise = formulasInvalidas xs

pertenece :: String -> [(String,String)] -> Bool
pertenece _ [] = False
pertenece n ((a,b):xs) | n == a || n == b = True
                       | otherwise = pertenece n xs

