import random, math

def llegadasPoissonNoHomo(T):
    Lambda = 2*T + 1
    S = []
    t = 0
    while(t <= T):
        U = random.uniform(0,1)
        t = t - (1/Lambda)*math.log(U)
        if t > T:
            return S
        else:
            U = random.uniform(0,1)
            if U <= (2*t + 1)/Lambda:
                S.append((t, "llegada"))

def llegadasPoisson(T, Lambda):
    S = []
    t = 0
    while t <= T:
        U = random.uniform(0,1)
        t = t - (1/Lambda)*math.log(U)
        if t > T:
            return S
        else:
            S.append((t, "llegada"))

def tiempoServicio(mu):
    U = random.uniform(0,1)
    return -math.log(1-U)/mu

def procesaLlegada(evento):
    global t, n, n_a, n_q, servidor_ocupado, mu
    t = evento[0]
    n = n+1
    n_a = n_a + 1
    
    if servidor_ocupado == True:
        n_q = n_q + 1
    else:
        servidor_ocupado = True
        salida = (evento[0] + tiempoServicio(mu), "salida")
        eventos.append(salida)

def procesaSalida(evento):
    global t, n, n_s, n_q, servidor_ocupado
    t = evento[0]
    n = n-1
    n_s = n_s + 1
    if n_q > 0:
        n_q = n_q - 1
        salida = (evento[0] + tiempoServicio(mu), "salida")
        eventos.append(salida)
    else:
        servidor_ocupado = False

n, n_a, n_s, n_q, t = 0, 0, 0, 0, 0
servidor_ocupado = False
mu = 1

eventos = llegadasPoissonNoHomo(2)

print("Al tiempo " + str(t) + " tenemos: ")
print(str(n) + " personas en el sistema; ")
print(str(n_a) + " llegadas;")
print(str(n_s) + " salidas;")
print(str(n_q) + " gente en fila. \n----------\n")

while(eventos):
    eventos.sort()
    
    siguiente_evento = eventos.pop(0)
    
    if siguiente_evento[1] == 'llegada':
        procesaLlegada(siguiente_evento)
    
    if siguiente_evento[1] == 'salida':
        procesaSalida(siguiente_evento)
        
    print("Al tiempo " + str(t) + " tenemos: ")
    print(str(n) + " personas en el sistema; ")
    print(str(n_a) + " llegadas;")
    print(str(n_s) + " salidas;")
    print(str(n_q) + " gente en fila. \n----------\n")
