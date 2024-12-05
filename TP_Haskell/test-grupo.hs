import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.


runGrupoTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    esMinuscula 'd' ~?= True,
    esMinuscula 'D' ~?= False,
    esMinuscula 'l' ~?= True,
    esMinuscula 'K' ~?= False
    ]

testsEjletraANatural = test [
    letraANatural 'a' ~?= 0,
    letraANatural 'b' ~?= 1,
    letraANatural 'k' ~?= 10,
    letraANatural 'z' ~?= 25
    ]

testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd',
    desplazar 't' 8 ~?= 'b',
    desplazar 'A' 5 ~?= 'A',
    desplazar 'A' (-5) ~?= 'A',
    desplazar 'h' 5 ~?= 'm',
    desplazar 'l' 26 ~?= 'l',
    desplazar 'l' (-27) ~?= 'k',
    desplazar 'z' 0 ~?= 'z',
    desplazar 'z' (-2) ~?= 'x'
    ]

testsEjcifrar = test [
    cifrar "" 8998898 ~?= "",
    cifrar "computacion" 3 ~?= "frpsxwdflrq",
    cifrar "computacion" 8 ~?= "kwuxcbikqwv",
    cifrar "trivialote" 315 ~?= "wulyldorwh",
    cifrar "klqofsfxi" 3 ~?= "notrivial",
    cifrar "SIqofsfxi" 3 ~?= "SItrivial",
    cifrar "RoToRcIto" 19 ~?= "RhThRvImh",
    cifrar "uno dos tres" 45 ~?= "ngh whl mkxl"
    ]

testsEjdescifrar = test [
    descifrar "" 39795558 ~?= "",
    descifrar "frpsxwdflrq" 3 ~?= "computacion",
    descifrar "kwuxcbikqwv" 8 ~?= "computacion",
    descifrar "wulyldorwh" 315 ~?= "trivialote",
    descifrar "notrivial" 3 ~?= "klqofsfxi",
    descifrar "SItrivial" 3 ~?= "SIqofsfxi",
    descifrar "RhThRvImh" 19 ~?= "RoToRcIto",
    descifrar "ngh whl mkxl" 123 ~?= "uno dos tres"
    ]

testsEjcifrarLista = test [
    cifrarLista [] ~?= [],
    cifrarLista ["", "", ""] ~?= ["", "", ""],
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
    cifrarLista ["mate", "datos", "compu"] ~?= ["mate", "ebupt", "eqorw"],
    cifrarLista ["Aaron", "fAcu", "keVin", "fraN"] ~?= ["Aaron", "gAdv", "mgVkp", "iudN"]
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "amarillo") [25.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,12.5,0.0,0.0,25.0,12.5,0.0,12.5,0.0,0.0,12.5,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "aMARILLO") [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "MAYUSCULA") [0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
    expectlistProximity (frecuencia "MezClaDo") [20.0,0.0,0.0,0.0,20.0,0.0,0.0,0.0,0.0,0.0,0.0,20.0,0.0,0.0,20.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,20.0],
    expectlistProximity (frecuencia "a B c") [50.0,0.0,50.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

testsEjcifradoMasFrecuente = test [
    cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336),
    expectAnyTuplaAprox (cifradoMasFrecuente "aaHHvvPPv" 10) [('f', 60.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "aaHHvvPP" 0) [('a', 50.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "americana" 29) [('d', 33.333336)],
    expectAnyTuplaAprox (cifradoMasFrecuente "americana" 26) [('a', 33.333336)],
    expectAnyTuplaAprox (cifradoMasFrecuente "kHYTRRED" 15) [('z', 100.0)],
    expectAnyTuplaAprox (cifradoMasFrecuente "Uno Uno" 0)  [('n', 50.0)] -- hay que usar expectAny (puede ser 'n'/'o')
    ]

testsEjesDescifrado = test [
    esDescifrado "taller" "compu" ~?= False,
    esDescifrado "abcdef" "zabcde" ~?= True, 
    esDescifrado "ABCxyz" "ABClmn" ~?= True,
    esDescifrado "diferencial" "mronanwlrju" ~?= True,
    esDescifrado "diferencial" "mronanwlrjl" ~?= False,
    esDescifrado "MATRIZ" "MATRIZ" ~?= True,
    esDescifrado "MATRIZ" "MATRIS" ~?= False,
    esDescifrado "lentes pez" "pirxiw tid" ~?= True
    ]

testsEjtodosLosDescifrados = test [
    todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")],
    todosLosDescifrados [] ~?= [],
    todosLosDescifrados [""] ~?= [],
    todosLosDescifrados ["MAYO", "AZUL", "azul", "MAYo"] ~?= [],
    expectPermutacion (todosLosDescifrados ["exapg", "reloj", "obilg", "pilar"]) [("exapg", "pilar"), ("reloj", "obilg"), ("obilg", "reloj"), ("pilar", "exapg")],
    expectPermutacion (todosLosDescifrados ["ufpk", "juez", "vgql", "itdy"]) [("ufpk", "juez"), ("ufpk", "vgql"), ("ufpk", "itdy"), ("juez", "ufpk"), ("juez", "vgql"), ("juez", "itdy"), ("vgql", "ufpk"), ("vgql", "juez"), ("vgql", "itdy"), ("itdy", "ufpk"), ("itdy", "juez"), ("itdy", "vgql")],
    expectPermutacion (todosLosDescifrados ["cinco", "patos", "itmhl", "abcde"]) [("patos", "itmhl"), ("itmhl", "patos")],
    expectPermutacion (todosLosDescifrados ["vector", "Vector", "lusjeh"]) [("vector", "lusjeh"), ("lusjeh", "vector")]
    ]

testsEjexpandirClave = test [
    expandirClave "compu" 8 ~?= "compucom",
    expandirClave "dinosaurio" 5 ~?= "dinos",
    expandirClave "dinosaurio" 16 ~?= "dinosauriodinosa",
    expandirClave "corta" 1 ~?= "c",
    expandirClave "larga" 21 ~?= "largalargalargalargal"
    ]

testsEjcifrarVigenere = test [
    cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
    cifrarVigenere "fotocopia" "taza" ~?= "yosovooit",
    cifrarVigenere "" "any" ~?= "",
    cifrarVigenere "esternocleidomastoideo" "esternocleidomastoideo" ~?= "ikmiiacewiqgcyakmcqgic",
    cifrarVigenere "libros" "zzz" ~?= "khaqnr"
    ]

testsEjdescifrarVigenere = test [
    descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
    descifrarVigenere "tavfttnoh" "pico" ~?= "estrellas",
    descifrarVigenere "" "pico" ~?= "",
    descifrarVigenere "ncbxpzouzf" "loor" ~?= "congelador",
    descifrarVigenere "ikmiiacewiqgcyakmcqgic" "esternocleidomastoideo" ~?= "esternocleidomastoideo"
    ]

testsEjpeorCifrado = test [
    peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
    peorCifrado "calculadora" ["calculadora", "aaa", "hunit"] ~?= "aaa",
    peorCifrado "jamon" ["zi", "jamon", "c"] ~?= "c",
    peorCifrado "" ["a", "b", "c"] ~?= "a"
    ]

testsEjcombinacionesVigenere = test [
    combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
    combinacionesVigenere [""] ["l"] "pop" ~?= [],
    combinacionesVigenere ["fin", "jueves"] ["tp", "s"] "bmwnwk" ~?= [("jueves", "s")],
    expectPermutacion (combinacionesVigenere ["zahws", "audio", "gaowz"] ["pi", "wi", "eq"] "video") [("gaowz", "pi"), ("zahws", "wi")]
    ]


-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)