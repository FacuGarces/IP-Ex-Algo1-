from queue import LifoQueue as Pila
from queue import Queue as Cola
# Ejercicio 1
def reordenar_cola_priorizando_vips(c: Cola) -> Cola:
    listaclientes: list[str] = []
    for i in range (len(c)):
        (listaclientes.append(c.get()))


def ordenarporvip(lista: list[str,str]) -> list:
    listavip: list = []
    listanovip: list = []
    for dupla in lista:
        if dupla[1] == "vip":
            listavip.append(dupla[0])
        elif dupla[1] == "comun":
            listanovip.append(dupla[0])
    return listavip + listanovip
 