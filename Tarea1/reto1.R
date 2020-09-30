#Reto 1

paso=function(pos,dim){
  d=sample(1:dim,1)
  if (runif(1)<0.5){
    pos[d]=pos[d]-1
  } else{
    pos[d]=pos[d]+1
  }
  return(pos)
}

caminata <- function(largo,dim) {
  pos = rep(0,dim)
  for (i in 1:largo){
    pos = paso(pos,dim)
  }
  return(pos)
}

tiempos <-  data.frame()
promedios<- data.frame()
total1<-c()
tiempo_prom<-c()

repeticiones=500
for(e in 5:10){
  largo=2**e
  for (dimension in 1:8){
    total1[1]<-largo
    total1[2]<-dimension
    for (i in 1:repeticiones){
      start<-Sys.time()
      caminata(largo,dimension)
      end<-Sys.time()
      total1[i+2]<-(end-start)
    }
    
    tiempos<-rbind(tiempos,total1)
    colnames(tiempos)<-c("Longitud", "Dimension",c(1:repeticiones))
  }
}

t<-c()
promedios<-data.frame()
for(i in 1:48){
  t[1]<-tiempos[i,1]
  t[2]<-tiempos[i,2]
  t[3]<-mean(as.numeric(tiempos[i,3:(repeticiones+2)]))
  promedios<-rbind(promedios,t)
}

#gráficas

png(file="1024-tiempo.png",
    width=3000, height=2600, res = 400)
barplot(promedios[17:24,3], las=1, names.arg = c(1:8), col = palette("Pastel 2"),
        cex.axis = 0.7, cex.lab = 1, xlab = "Dimensión", ylab = "Tiempo")
dev.off()
