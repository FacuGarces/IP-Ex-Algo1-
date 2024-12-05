# Ejercicio 1
def acomodar(s1: list) -> list:
    res: list = []
    for boleta_up in s1:
        if boleta_up == "UP":
            res.append(boleta_up)
    for boleta_lla in s1:
        if boleta_lla == "LLA":
            res.append(boleta_lla)
    return res
print(acomodar(["LLA", "UP", "LLA","UP", "LLA", "UP"]))