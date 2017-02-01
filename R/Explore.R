explore <-function(data="quote data", col="column where prices are", plots="number of random walks generated and ploted")
{
  #MIT - non sublicence and/or sell - licence. Emiliano Hernandez Laos 2017
  #read licence at: https://gitlab.com/emhlaos/quantools
  ups<-0
  downs<-0
  U<-0
  DU<-0
  up<-0
  down<-0
  cero<-0
  elem<-length(data[,1])-1

  for(a in 1:elem)
  {
    if(is.na(data[a+1,col])){break}
    if(data[a+1,col]-data[a,col]>0)
    {
      ups<-data[a+1,col]-data[a,col]+ups
      U<-U+1
    }
    if(data[a+1,col]-data[a,col]<0)
    {
      downs<-data[a+1,col]-data[a,col]+downs
      DU<-DU+1
    }
    if(data[a+1,col]-data[a,col]==0)
    {
      cero<-cero+1
    }
  }
  pU<<- U/elem #probabilidad de subir
  pD<<- DU/elem #probabilidad de bajar
  pC<<- cero/elem #probabiliad de DIF=0
  up<<- ups/U #movimiento promedio de subida
  down<<- downs/DU #movimiento promedio de bajada

  RAND<<-data.frame(NULL)
  color<-rgb(.120,.120,.120,alpha=0.7)
  plot(data[,col], type="l", col="blue", ylim = range(min(data[,col], na.rm=TRUE)-300,max(data[,col], na.rm=TRUE)+300))
  for(w in 1:plots)
  {
  RAND[1,w]<<-data[1,col]
  for(i in 2:elem)
  {
      RAND[i,w]<<-RAND[i-1,w]+sample(c(ups/U,downs/DU,0),size=1,prob=c(U/elem,DU/elem,cero/elem))
  }
  par(new=TRUE)
  plot(RAND[,w], type="l", col=color,ylim =range(min(data[,col], na.rm=TRUE)-300,max(data[,col], na.rm=TRUE)+300))
  }
  RAND[1,]<<-data[1,col]
  for(i in 2:elem)
  {
    RAND[i,w]<<-RAND[i-1,w]+sample(c(ups/U,downs/DU,0),size=1,prob=c(U/elem,DU/elem,cero/elem))
  }

  RAND[1,plots+1]<<-data[1,col]
  for(i in 2:elem){
    sum<-0
    average<-0
    for(k in 1:plots){
      sum<-RAND[i,k]+sum
    }
    average=sum/plots
    RAND[i,plots+1]<<-average
  }
  par(new=TRUE)
  plot(RAND[,plots+1], type="l", col="red",ylim =range(min(data[,col], na.rm=TRUE)-300,max(data[,col], na.rm=TRUE)+300))

}
