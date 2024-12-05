# Practica 7
# Parte 1
# 1)
def pertenece(lista: list, elemento) -> bool: 
    for i in range(len(lista)):
        if lista[i] == elemento: 
            return True
    else: 
        return False
# 2)
def divide_a_todos(lista: list[int], elemento: int) -> bool:
    for i in range(len(lista)):
        if lista[i] % elemento != 0:
            return False
    return True

# 3)
def suma_total(lista: list[int]) -> int:
    suma = 0
    for i in range(len(lista)):
        suma = suma + lista[i]
    return suma

# 4)
def problema_ordenados(lista: list[int]) -> bool:
    for i in range(len(lista) -1):
        if lista[i] > lista[i+1]:
            return False
    return True

# 5) 
def longuitud_mayor_a_7(lista: list[str]) -> bool:
    for i in range(len(lista)):
        if len(lista[i]) > 7:
            return True
    return False  

# 6)
def quitar_espacios_blanco(txt: str) -> str:
    txt_sin_espacios: str = []
    for i in txt:
        if i != " ":
            txt_sin_espacios.append(i)
    return txt_sin_espacios
def darVuelta(palabra: str) -> str:
    inverso: str = []
    for i in range(len(palabra)-1,-1,-1): 
        inverso += palabra[i]
    return inverso

def esPalindromo(palabra: str) -> bool:
    txt = quitar_espacios_blanco(palabra)
    inverso_txt = darVuelta(txt)
    for i in range(len(palabra)):
        if txt[i] != inverso_txt[i]:
            return False
    return True

# 7)
minusculas: list =  ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]
mayusculas : list = "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z".split()
numeros : list = "0 1 2 3 4 5 6 7 8 9".split()
def contraseña_fuerte(password: str) -> str:
    if len(password) > 8:
        for i in range(len(password)):
            if password[i] in numeros: 
                for i in range(len(password)):
                    if password[i] in minusculas:
                        for i in range(len(password)):
                            if password[i] in mayusculas:
                                return "Verde"
    if len(password) < 5:
        return "Rojo"
    return "Amarillo"

# 8) 
def banco(movimientos: list) -> int:
    res: int = 0
    for transaccion in movimientos:
        if transaccion[0] == "I":
            res += transaccion[1]
        else:
            res -= transaccion[1]
    return res
# 9)
vocales = "a e i o u".split()
def tresVocalesDistintas(palabra: str) -> bool:
    vocalesGuardadas: list = []
    for letra in palabra:
        if esVocal(letra) and not pertenece(vocalesGuardadas, letra):
            vocalesGuardadas.append(letra)
    return len(vocalesGuardadas) > 3


def esVocal(letra: str) -> bool:
    return pertenece(vocales, letra)

# Parte 2:
# 1) 
def ceroEnPares(lista: list[float]) -> list: 
    for i in range(len(lista)):
        if (i % 2 != 0):
            lista[i] = 0
    return lista
# 2) 
def ceroEnParesV2(lista: list[float]) -> list: 
    nueva_lista : list[float] = lista
    for i in range(len(lista)):
        if (i % 2 != 0):
            nueva_lista[i] = 0
    return nueva_lista

# 3)
def borrar_vocales(palabra: str)  -> str:
    res: str = ""
    for letra in palabra:
        if not pertenece(vocales,letra):
            res += letra
    return res

# 4)
def reemplazar_vocales(palabra: str) -> str:
    res : str = ""
    for letra in palabra: 
        if pertenece(vocales,letra):
            res += "_" 
        else:
            res += letra
    return res

# 5) lo hice antes para el palindromo

# 6)
def eleminar_repetidos(s:str) -> str:
    lista_unicos : str = ""
    for letra in s:
        if letra not in lista_unicos:
            lista_unicos += letra
        else:
            continue
    return lista_unicos

# Ejercicio 3
def aprobado(notas: list[int]) -> int:
    promedio : float = suma_total(notas) / len(notas)
    for nota in notas:
        while nota > 4 and promedio > 7:
            return 1
    for nota in notas: 
        while nota > 4 and promedio >= 4: 
            return 2
    return 3

# Ejercicio 4
# 1)
def estudiantes() -> list[str]: 
    res: list[str] = []
    while not pertenece(res,"listo"):
        estudiante: str = input("Nombre del estudiante, ingresar listo para terminar: ")
        res.append(estudiante)
    res.remove("listo")
    return res

# 2) 
def simulacion() -> list:
    historial: list[(str, float)] = []
    while True:
            operacion: str = input(("Ingresar operación (C/D/X): ")).upper()
            if operacion != "X":
                monto: int= input(("Monto: "))
                historial.append((operacion, monto))
            else:
                return historial

# Ejercicio 5
# 1)
def pertenece_a_cada_uno(ListaDeListas: list[list[int]], elemento: int) -> bool: 
    res: list = []
    for lista in ListaDeListas:
        if pertenece(lista, elemento):
            res.append(True)
        else:
            res.append(False) 
    return res
# 2)
def esMatriz(m:list[list[int]]) -> bool:
    if (len(m) == 0) or (len(m[0]) == 0):
        return False
    for i in range(0,len(m),1):
        if len(m[i]) != len(m[0]):
            return False
    return True

# 3)
def filas_ordenadas(m:list[list[int]]) -> list[bool]:
    res: list[bool] = []
    for lista in m:
        if ordenada(lista):
           res.append(True)
        else:
            res.append(False) 
    return res

def ordenada(lista: list[int]) -> bool:
    for i in range(len(lista)-1):
        if lista[i] > lista[i+1]:
            return False
    else:
        return True




