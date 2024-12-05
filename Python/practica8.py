# Practica 8
# Ejercicio 1
# 1)
def contar_lineas(nombre_archivo : str) -> int:
    archivo = open(nombre_archivo,"r")
    suma: int = 0
    for linea in archivo.readlines():
        suma += 1
    archivo.close()
    return suma 

# 2)
def existe_palabra(nombre_archivo: str, p: str) -> bool:
    archivo = open(nombre_archivo, "r")
    lista = archivo.readlines()
    if pertenece(lista, p):
        return True
    return False
    
def pertenece(lista: list, elemento) -> bool: 
    for i in range(len(lista)):
        if lista[i] == elemento: 
            return True
    else: 
        return False

# 3)
def cantidad_de_apariciones(nombre_archivo: str, e: str) -> int:
    sumador: int = 0
    archivo = open(nombre_archivo,'r')
    contenidoDelArchivo = archivo.read()
    palabrasDelArchivo = contenidoDelArchivo.split()
    for palabraDelArchivo in palabrasDelArchivo:
        if e == palabraDelArchivo:
            sumador+=1
    archivo.close()
    return sumador

# Ejercicio 2
def clonar_sin_comentarios(nombre_archivo: str) -> None:
    archivo = open(nombre_archivo, "r")
    clonado = open("clon.txt","w")
    lineas = archivo.readlines()
    for linea in lineas:
        if not linea.strip()[0] == "#":
            clonado.write(linea)
    archivo.close()
    clonado.close()

# Ejercicio 3
def reverso(nombre_archivo: str) -> None:
    archivo = open(nombre_archivo, "r")
    invertido = open("reverso.txt", "w")
    lineas = archivo.readlines()
    lineasreverso = invertir_lista(lineas)
    lineasreverso[0] = lineasreverso[0] + "\n"
    for linea in lineasreverso:
        invertido.write(linea)
    archivo.close()
    invertido.close()

def invertir_lista(lista: list) -> list:
    lista_invertida: list = []
    for i in range(len(lista)-1, -1,-1):
        lista_invertida.append(lista[i])
    return lista_invertida

# Ejercicio 4
def agregar_frase_al_final(nombre_archivo: str, frase: str) -> None: 
    archivo = open(nombre_archivo, "a")
    archivo.write("\n"+ frase)
    archivo.close()

# Ejercicio 5
def agregar_frase_al_principio(nombre_archivo: str, frase: str) -> None:
    archivo = open(nombre_archivo, "r")
    lineas = [frase + "\n"] + archivo.readlines()
    archivo.close()
    archivo = open(nombre_archivo, "w")
    for linea in lineas: 
        archivo.write(linea)
    archivo.close()

from queue import LifoQueue as Pila
import random

# Ejercicio 8
def generar_nros_al_azar(cantidad: int, desde: int, hasta: int) -> Pila[int]:
    res : Pila = Pila()
    for i in range(cantidad):
        res.put(random.randint(desde, hasta))
    return res

# Ejercicio 9
def cantidad_elementos(p:Pila)->int:
    contenido=[]
    contador:int=0
    while not p.empty():
        contenido.append(p.get())
        contador+=1
    for elemento in invertir_lista(contenido):
        p.put(elemento)
    return contador
def invertir_lista(lista):
    lista_invertida = []
    for i in range(len(lista) - 1, -1, -1):
        lista_invertida.append(lista[i])
    return lista_invertida
# Ejercicio 10
def buscar_el_maximo(p:Pila)->int:
    maximo:int=p.get()
    while not p.empty():
        nuevo_elemento=p.get()
        if nuevo_elemento > maximo:
            maximo=nuevo_elemento
    return maximo
# Ejercicio 11
def esta_bien_balanceada(s:str)->bool:
    res:bool=True
    p=Pila()
    parentesis_abiertos:int=0
    for letra in invertir_lista(s):
        p.put(letra)
    while not p.empty():
        letra_sacada=p.get()
        if letra_sacada=="(":
            parentesis_abiertos+=1
        if letra_sacada==")":
            parentesis_abiertos-=1
        if parentesis_abiertos<0:
            res=False
    if parentesis_abiertos>0:
        res=False
    return res

from queue import Queue as Cola

# Ejercicio 13
def generar_nros_al_azar_cola(cantidad:int,desde:int,hasta:int)->Cola:
    c=Cola()
    p:Pila=generar_nros_al_azar(cantidad,desde,hasta)
    for i in range (cantidad):
        c.put(p.get())
    return c

# Ejercicio 14
def cantidad_elementos(c:Cola)->int:
    contenido=[]
    contador:int=0
    while not c.empty():
        contenido.append(c.get())
        contador+=1
    for elemento in contenido:
        c.put(elemento)
    return contador

# Ejercicio 15
def buscar_el_maximo(c:Cola)->int:
    maximo:int=c.get()
    while not c.empty():
        nuevo_elemento=c.get()
        if nuevo_elemento > maximo:
            maximo=nuevo_elemento
    return maximo

# Ejercicio 16
def armar_secuencia_de_bingo()->Cola[int]:
    lista:list[int]=list(range(1,100))
    random.shuffle(lista)
    cola:Cola[int]=Cola()
    for elemento in lista:
        cola.put(elemento)
    return cola

def jugar_carton_del_bingo(carton:list,bolillero:Cola[int])->int:
    jugadas:int=0
    numeros_marcados:int=0
    bolillero_aux:Cola[int]=Cola()

    #Sigo sacando bolillas hasta que marque todos los numeros
    while numeros_marcados<12:
        bolilla_sacada=bolillero.get()
        bolillero_aux.put(bolilla_sacada)
        if bolilla_sacada in carton:
            numeros_marcados+=1
        jugadas+=1

    #Una vez que marque todos, paso todas las bolillas restantes al bolillero auxiliar
    while not bolillero.empty():
        bolilla_sacada:int = bolillero.get()
        bolillero_aux.put(bolilla_sacada)

    #Luego las devuelvo del bolillero auxiliar al original, para que quede igual que al principio        
    while not bolillero_aux.empty():
        bolilla_sacada:int  = bolillero_aux.get()
        bolillero.put(bolilla_sacada)

    return jugadas

# Ejercicio 19
def agrupar_por_longuitud(nombre_archivo: str) -> dict:
    archivo = open(nombre_archivo,"r")
    res: dict[int] = {}
    lineas = archivo.readlines()
    for linea in lineas: 
        palabras = linea.split()
        for palabra in palabras:
            longitud: int = len(palabra)
            if longitud in res:
                res[longitud] += 1
            else: res[longitud] = 1
    archivo.close()
    return res

#Ejercicio 21
def frecuencias(nombre_archivo:str)->dict:
    archivo = open(nombre_archivo, "r",encoding='utf8')
    lineas=archivo.readlines()
    frec: dict = {}

    for linea in lineas:
        palabras=linea.split()
        for palabra in palabras:
            if palabra not in frec:
                frec[palabra]=1
            else:
                frec[palabra]+=1
    
    archivo.close()
    return frec

def la_palabra_mas_frecuente(nombre_archivo:str)->str:
    frec=frecuencias(nombre_archivo)
    palabra_mas_frecuente:str
    frecuencia_max:int=0

    for (palabra,frecuencia) in frec.items():
        if frecuencia>frecuencia_max:
            frecuencia_max=frecuencia
            palabra_mas_frecuente=palabra

    return palabra_mas_frecuente

print((frecuencias("C:\\Users\\Facu\\Desktop\\ola.txt")).items())
