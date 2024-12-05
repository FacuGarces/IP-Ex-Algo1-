from queue import LifoQueue as Pila
from queue import Queue as Cola
# Ejercicio 1
def reordenar_cola_priorizando_vips(c: Cola) -> Cola:
    listaclientes = []
    ordenados = []
    colanueva = Cola()
    while not c.empty():
        (listaclientes.append(c.get()))
    ordenados = ordenarporvip(listaclientes) 
    for persona in ordenados:
        colanueva.put(persona)
    for persona in listaclientes:
        c.put(persona)
    return colanueva

def ordenarporvip(lista: list[str,str]) -> list:
    listavip: list = []
    listanovip: list = []
    for dupla in lista:
        if dupla[1] == "vip":
            listavip.append(dupla[0])
        elif dupla[1] == "comun":
            listanovip.append(dupla[0])
    return listavip + listanovip

def printColaoPila(colaOpila):
    while not colaOpila.empty():
        print(colaOpila.get())


def torneo_gallinas(estrategias: dict[str,str]) -> dict[str,int]:
    res: dict = {}
    for tupla1 in estrategias.items():
        puntaje: int = 0
        for tupla2 in estrategias.items():
            if tupla1[0] != tupla2[0]:
                if tupla1[1] == "me la banco y no me desvio" and tupla2[1] == "me la banco y no me desvio":
                    puntaje -= 5
                elif tupla1[1] == "me la banco y no me desvio" and tupla2[1] == "me desvio siempre":
                    puntaje += 15
                elif tupla1[1] == "me desvio siempre" and tupla2[1] == "me la banco y no me desvio":
                    puntaje -= 15
                elif tupla1[1] == "me desvio siempre" and tupla2[1] == "me desvio siempre":
                    puntaje -= 10
        res[tupla1[0]] = puntaje
    return res


def filtrar_codigos_primos(codigo_barras: list[int]) -> list[int]:
    res: list[int] = []
    for numero in codigo_barras:
        if esPrimo(sumadigitos(ultimas3cifras(numero))):
            res.append(numero)
    return res


def esPrimo(numero: int) -> bool:
    if numero < 2:
        return False
    for i in range(2,numero-1):
            if numero % i == 0:
                return False
    return True

def ultimas3cifras(numero:int) -> int:
    return numero % 1000

def sumadigitos(numero: int) -> int:
    suma: int = 0
    while numero > 0:
        suma += numero % 10
        numero = (numero - (numero % 10)) // 10
    return suma

def stock_productos(stock_cambios: list[str,int]) -> dict[str,(int,int)]:
    res = {}
    
    for i in range(len(stock_cambios)):
        (nombre, cantidad) = stock_cambios[i]
        if nombre not in res:
            # Si el producto no está en el diccionario, se inicializa con la cantidad actual como mínimo y máximo
            res[nombre] = (cantidad, cantidad)
        else:
            # Si el producto ya está en el diccionario, se actualizan los valores mínimo y máximo si es necesario
            (min_stock, max_stock) = res[nombre]
            if cantidad < min_stock:
                min_stock = cantidad
            if cantidad > max_stock:
                max_stock = cantidad
            res[nombre] = (min_stock, max_stock)
    
    return res


stock_cambios = [("producto1", 10), ("producto2", 5), ("producto1", 15), ("producto2", 3), ("producto3", 7)]
#print(stock_productos(stock_cambios))

def subsecuencia_mas_larga(tipos_pacientes_atendidos: list[str]) -> int:
    cant: int = 0
    max_cant: int = 0
    res: int = 0
    
    for i in range(len(tipos_pacientes_atendidos)):
        if tipos_pacientes_atendidos[i] == "perro" or tipos_pacientes_atendidos[i] == "gato":
            cant += 1
        else:
            if cant > max_cant:
                max_cant = cant
                res = i - cant
            cant = 0

    if cant > max_cant:
        max_cant = cant
        res = len(tipos_pacientes_atendidos) - cant  # Ajuste del índice

    return res
a = ["loro","perro","chancho","gato","gato","gato","perro"]
#print(subsecuencia_mas_larga(a))

def orden_de_atencion(urgentes: Cola, postergables: Cola) -> Cola:
    res: Cola = Cola()
    while not (postergables.empty()):
        res.put(urgentes.get())
        res.put(postergables.get())  
    return res

urgentes: Cola = Cola()
urgentes.put(1)
urgentes.put(2)
urgentes.put(3)

postergables : Cola = Cola()
postergables.put(4)
postergables.put(5)
postergables.put(6)

#printColaoPila(orden_de_atencion(urgentes, postergables))


def alarma_epidemiologica(registros: list[int,str], infecciosas: list[str], umbral: float) -> dict[str,float]:
    res: dict = {}
    dicc_sin_umbral: dict = {}
    dicc_con_todas_las_enfemedades: dict[str,float] = {}
    total_de_pacientes = len(registros)
    for registro in registros:
        if registro[1] not in dicc_con_todas_las_enfemedades.keys():
            dicc_con_todas_las_enfemedades[registro[1]] = 1
        else:
            dicc_con_todas_las_enfemedades[registro[1]] += 1
    dicc_sin_umbral = sacar_elementos_dict(dicc_con_todas_las_enfemedades,infecciosas)
    res = sacar_por_umbral(dicc_sin_umbral, umbral, total_de_pacientes)
    return res

def sacar_elementos_dict(diccionario:dict, lista_claves: list) -> dict:
    nuevo_diccionario: dict = {}
     # Iteramos sobre las claves de la lista
    for clave in lista_claves:
        # Si la clave está en el diccionario original, la añadimos al nuevo diccionario
        if clave in diccionario:
            nuevo_diccionario[clave] = diccionario[clave]
    return nuevo_diccionario

def sacar_por_umbral(diccionario:dict, umbral: float, total: int) -> dict:
    res: dict = {}
    for enfermedad,valor in diccionario.items():
        if  valor / total >= umbral:
            res[enfermedad] = diccionario[enfermedad]
    return res


print(alarma_epidemiologica([(1,"herpes"),(2,"coca"),(3,"herpes")],["herpes","coca"],0.1))