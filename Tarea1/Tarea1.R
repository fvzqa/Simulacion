repeticiones=500
paso=function(pos,dim){
  d=sample(1:dim,1)
  if (runif(1)<0.5){
    pos[d]=pos[d]-1
  } else{
    pos[d]=pos[d]+1
  }
  return(pos)
}


experimento <- function(largo,dim) {
  pos = rep(0,dim)
  for (i in 1:largo){
    pos = paso(pos,dim)
    if(all(pos[1:dim]==0)){
      return(i)
      break()
    }
  }
  if(!(all(pos[1:dim]==0))){
    return(Inf)
  }
}
datos <-  data.frame()
total<-c()
largo<-c(32,64,128,256,512,1024)

for(largo in largo){
for (dimension in 1:8){
  total[1]<-largo
  total[2]<-dimension
  for (i in 1:repeticiones){
  total[i+2]<-experimento(largo,dimension)
}
  datos<-rbind(datos,total)
  colnames(datos)<-c("Longitud", "Dimension",c(1:repeticiones))
}
}

l<-list()

for(i in 1:48){
  l[[i]]<-datos[i,3:502]
  l[[i]]<- l[[i]][l[[i]]<Inf]
    }

barras<-function(dim){
  regresos<-c()
  for(n in 1:6){
    regresos[n]<-length(l[[8*n-(8-dim)]])
  }
#  barplot(regresos, names.arg =c(32,64,128,256,512,1024), xlab="Longitud de la caminata", 
#         ylab="Cantidad de veces que regresó", col=palette("Pastel 2"))
  barplot((regresos/repeticiones)*100, names.arg =c(32,64,128,256,512,1024), xlab="Longitud de la caminata", 
          ylab="", col=palette("Pastel 2"), cex.axis = 1.5, cex.lab = 1.5)
  title(ylab="Porcentaje de las veces que regresó", mgp=c(2.5,1,0), cex.lab=1.5)
  return(regresos)
  }

box<-function(largo){
  regresos<-list()
  pasos<-c()
  for(n in 1:8){
    regresos[[n]]<-l[[n+8*(largo-1)]]
    pasos[n]<-length(regresos[[n]])
  }
  boxplot(regresos, names.arg=c(1:8), col = palette("Pastel 2"), log="y", xlab="Dimensión",
          ylab="", cex.axis=1.5, cex.lab=1.5)
  title(ylab="Pasos (escala logarítmica)", mgp=c(2.5,1,0), cex.lab=1.5)
  return(pasos)
}



png(file="Largo1024-pasos.png",
    width=3000, height=2600, res = 400)
    box(6)
dev.off()
