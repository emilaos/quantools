RANDanalyzer<-function(data="quote data", col="column where prices are", graph="TRUE/FALSE to plot or not plot results", det="TRUE/FALSE", elemlimit="last number of data to process")
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
if(!det)
{
elem<-length(data[,1])-1
}
if(det){elem<-elemlimit}
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


  RAND<<-NULL
  for(i in 1:elem)
  {
    if(i==1)
    {
      RAND<<-data.frame(data[1,col])
    }

    else{

      RAND<<-rbind(RAND,(RAND[i-1,1]+sample(c(ups/U,downs/DU,0),size=1,prob=c(U/elem,DU/elem,cero/elem))))}
  }
  if(graph)
  {
  plot(data[,col], type="l", col="blue", ylim = range(min(data[,col], na.rm=TRUE)-300,max(data[,col], na.rm=TRUE)+70))
  par(new=TRUE)
  plot(RAND[,1], type="l", col="red",ylim =range(min(data[,col], na.rm=TRUE)-300,max(data[,col], na.rm=TRUE)+70))
  }
}
