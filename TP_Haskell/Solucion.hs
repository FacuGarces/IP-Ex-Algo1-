module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { DNI1,apellidoYNombre1}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula a | ord a >= 97 && ord a <= 122 = True
              | otherwise = False

-- EJ 2
letraANatural :: Char -> Int
letraANatural a = ord a - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar c n | esMinuscula c == False = c
              | otherwise = chr (ord 'a' + mod (ord c - ord 'a' + n) 26) 

-- EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (x:xs) n | esMinuscula x = ( desplazar x n ) : cifrar xs n
                | otherwise = x : cifrar xs n

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar (x:xs) n | esMinuscula x = (desplazar x (-n)) : descifrar xs n
                   | otherwise = x : (descifrar xs n)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista xs = cifrarListaAux xs 0 (length xs) 

cifrarListaAux :: [String] -> Int -> Int -> [String] 
cifrarListaAux [] _ _= []
cifrarListaAux (x:xs) i l | i == l = [cifrar x i]
                          | otherwise = (cifrar x i ) : cifrarListaAux (xs) (i+1) l

-- EJ 7
frecuencia :: String -> [Float]
frecuencia (x:xs) = frecuenciaAux (x:xs) (darVuelta(todasLasMinisculas 26)) -- Notar que darVuelta(todasLasMinisculas 26) es la lista de minusculas

darVuelta :: String -> String -- Da vuelta un string
darVuelta [] = []
darVuelta (x:xs) = darVuelta xs ++ [x]

todasLasMinisculas :: Int -> [Char] -- Una función que nos permite obtener la lista de las minusculas 
todasLasMinisculas 0 = []
todasLasMinisculas n = chr(96 + n) : todasLasMinisculas (n-1)
                  

frecuenciaAux :: String -> String -> [Float] -- Calcula el porcentaje de cada letra en la palabra 
frecuenciaAux _ [] = []
frecuenciaAux palabra (y:ys) = porcentaje palabra y : frecuenciaAux palabra ys

contar :: String -> Char -> Float -- Cuenta la cantidad de apariciones de una letra en la palabra 
contar [] _ = 0
contar (x:xs) letra | letra == x = 1 + contar xs letra
                    | otherwise = contar xs letra

porcentaje :: String -> Char -> Float
porcentaje [] _ = 0
porcentaje (x:xs) letra | eliminarMayus(x:xs) == [] = 0
                        |  otherwise = (contar (x:xs) letra / fromIntegral (length (eliminarMayus(x:xs)))) * 100

eliminarMayus :: String -> String 
eliminarMayus [] = []
eliminarMayus (x:xs) | ord (x) >= 97 && ord(x) <= 122 = (x) : eliminarMayus xs
                     | otherwise = eliminarMayus xs
-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente frase n = (chr (posicion (frecuencia (cifrar frase n)) (masGrande (frecuencia (cifrar frase n))) + 97 ),(masGrande (frecuencia (cifrar frase n))))

masGrande :: [Float] -> Float
masGrande [n] = n
masGrande (x:y:xs) | x >= y = masGrande (x : xs)
                   | otherwise = masGrande (y:xs)

posicion :: [Float] -> Float -> Int
posicion [] _ = -1
posicion lista elemento | elemento == head lista = 0
                       | otherwise = 1 + posicion (tail lista) elemento


-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado [] _ = False 
esDescifrado (x:xs) (y:ys)  | pertenece (y:ys) (listaDeCifrados (x:xs) 25) = True
                            | otherwise = False
                           

listaDeCifrados :: String -> Int -> [String] -- Crea una lista con todos los posibles cifrados de la palabra
listaDeCifrados palabra 0 = [palabra]
listaDeCifrados palabra n = (cifrar palabra n) : (listaDeCifrados palabra (n-1))

pertenece :: String -> [String] -> Bool
pertenece _ [] = False
pertenece elem (x:xs)    | elem == x = True
                         | otherwise = pertenece elem xs

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (x:xs) = paresConX x xs ++ todosLosDescifrados xs

sonDescifrados :: String -> String -> Bool -- Revisa que dos palabras sean cifrados una de la otra
sonDescifrados s1 s2 = sonDescifradosAux s1 s2 1

sonDescifradosAux :: String -> String -> Int -> Bool -- Recorre los descifrados hasta 25
sonDescifradosAux s1 s2 n   | n > 25 = False
                            | s1 == descifrar s2 n = True
                            | otherwise = sonDescifradosAux s1 s2 (n + 1)

paresConX :: String -> [String] -> [(String, String)] -- Genera los pares de los cifrados
paresConX _ [] = []
paresConX x (y:ys)  | sonDescifrados x y = (x, y) : (y, x) : paresConX x ys
                    | otherwise = paresConX x ys

-- EJ 11
expandirClave :: String -> Int -> String 
expandirClave clave n = expandirAux clave clave n


expandirAux :: String -> String -> Int -> String -- Expande la clave hasta n, se usa una función auxiliar porque la primera no permite pasarle la lista original y que cree otra
expandirAux _ _ 0 = []
expandirAux original [] m = expandirAux original original m
expandirAux original (x:xs) m = x : expandirAux original xs (m-1)


-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere texto clave = cifrarVigenereAux texto (expandirClaveHasta texto clave )

cifrarVigenereAux :: String -> String -> String -- Cifra cada letra de (t:ts) por separado
cifrarVigenereAux [] _ = []
cifrarVigenereAux (t:ts) (k:ks) = desplazar t (ord k - ord 'a') : cifrarVigenereAux ts ks


expandirClaveHasta :: String -> String -> String --Expande la clave hasta lenght de (t:ts)
expandirClaveHasta [] _ = []
expandirClaveHasta (t:ts) (c:cs) = c: (expandirClaveHasta ts (cs ++ [c]))


-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere texto clave = descifrarVigenereAux texto (expandirClaveHasta texto clave )

descifrarVigenereAux :: String -> String -> String
descifrarVigenereAux [] _ = []
descifrarVigenereAux (t:ts) (k:ks) = desplazar t (-(ord k - ord 'a')) : descifrarVigenereAux ts ks -- Descifra cada letra de (t:ts) por separado

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ (x:[]) = x
peorCifrado mensaje (x:y:xs) | snd (armaTuplas mensaje x) <= snd (armaTuplas mensaje y) = peorCifrado mensaje (x:xs)
                             | otherwise = peorCifrado mensaje (y:xs) 

armaTuplas :: String -> String -> (String,Int)
armaTuplas mensaje clave = ((cifrarVigenere mensaje clave),(calculaDistancia mensaje (cifrarVigenere mensaje clave)))

calculaDistancia :: String -> String -> Int
calculaDistancia [] [] = 0
calculaDistancia (x:xs) (y:ys) = valorAbsoluto (letraANatural x - letraANatural y) + calculaDistancia xs ys

valorAbsoluto :: Int -> Int
valorAbsoluto n | n >= 0 = n
                | n < 0 = (-n)

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere mensajes claves cifrado = combinacionesVigenereAux (combinacionesVigenereAux2 mensajes claves) cifrado

combinacionesVigenereAux :: [(String,String)] -> String -> [(String, String)]
combinacionesVigenereAux (x:[]) _ = []
combinacionesVigenereAux (x:xs) cifrado | cifrarVigenere (fst x) (snd x) == cifrado = x : combinacionesVigenereAux xs cifrado
                                        | otherwise = combinacionesVigenereAux xs cifrado
-- con la lista de tuplas que arma combinacionesVigenereAux2, esta funcion las agarra y chequea si efecticamente cumple la especificacion
-- esta hecha por separado para poder utilizarla mas a gusto, ya que seria mas complicado hacerlo directamente en la funcion original

combinacionesVigenereAux2 :: [String] -> [String] -> [(String,String)]
combinacionesVigenereAux2 _ [] = []
combinacionesVigenereAux2 mensajes (x:xs) = combinaciones mensajes x ++ combinacionesVigenereAux2 mensajes xs
-- esta funcion pertmite armar una lista de tuplas con todas las combinaciones posibles de mensajes y clvaes

combinaciones :: [String] -> String -> [(String,String)]
combinaciones [] _ = []
combinaciones (x:xs) clave = (x,clave) : combinaciones xs clave