library(rlist)
set.seed(10)
#De momento, tomaremos lambda(t) = 2t+1 solo para ejemplificar
#Esto hace que lambda = 2T+1 por ser creciente     
llegadasPNH <- function(T){
  lambda = 2*T + 1
  S <- list()
  t <- 0
  
  while(t <= T){
    U <- runif(1,0,1)
    t <- t - (1/lambda)*log(U)
    if(t > T){
      return(S)
    } else{
      U <- runif(1,0,1)
      if(U <= (2*t + 1)/lambda){
        S <- list.append(S,list(tiempo=t, tipo="llegada"))
      }
    }
  }
}

tiempoServicio <- function(MU){
  U <- runif(1,0,1)
  return(-log(1-U)/MU)
}

eventos <- llegadasPNH(1)

n <- 0
n_a <- 0
n_s <- 0
n_q <- 0
t <- 0
mu <- 1
servidor_ocupado <- FALSE

eventos <- llegadasPNH(1)

cat("Al tiempo ", t,
    " tenemos: \n", n,
    " personas en el sistema; \n", n_a,
    " llegadas; \n", n_s, " salidas; \n", n_q, 
    " gente en fila. \n ------- \n")

while(length(eventos) != 0){
  eventos <- list.sort(eventos, tiempo)
  
  siguiente_evento <- eventos[[1]]
  eventos <- list.remove(eventos, 1)
  
  if( siguiente_evento$tipo == "llegada"){
    t <- siguiente_evento$tiempo
    n <- n+1
    n_a <- n_a + 1
    
    if(servidor_ocupado == TRUE){
      n_q <- n_q + 1
    }
    else{
      servidor_ocupado <- TRUE
      salida <- list(tiempo= siguiente_evento$tiempo + tiempoServicio(mu), tipo="salida")
      eventos <- list.append(eventos, salida)
    }
    
  }
  
  if( siguiente_evento$tipo == "salida"){
    t <- siguiente_evento$tiempo
    n <- n-1
    n_s <- n_s + 1
    
    if(n_q > 0){
      n_q <- n_q - 1
      salida <- list(tiempo= siguiente_evento$tiempo + tiempoServicio(mu), tipo="salida")
      eventos <- list.append(eventos, salida)
    }
    else{
      servidor_ocupado <- FALSE
    }
    
  }
  
  cat("Al tiempo ", t,
      " tenemos: \n", n,
      " personas en el sistema; \n", n_a,
      " llegadas; \n", n_s, " salidas; \n", n_q, 
      " personas en fila. \n ------- \n")
  
}

  


