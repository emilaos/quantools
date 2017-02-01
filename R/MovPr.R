MovPr <- function(data="quote data", col="column where prices are")
{
  #MIT - non sublicence and/or sell - licence. Emiliano Hernandez Laos 2017
  #read licence at: https://gitlab.com/emhlaos/quantools
  upCount<-0
  downCount<-0
  nochCount<-0
  elements<-length(data[,1])
  for(a in 3:elements-2)
  {
    if(data[a+1,col]-data[a,col]>0&data[a+2,col]-data[a+1,col]>0)
    {
      upCount<-upCount+1
    }
    else if(data[a+1,col]-data[a,col]<0&data[a+2,col]-data[a+1,col]<0)
    {
      downCount<-downCount+1
    }
    else
    {
      nochCount<-nochCount+1
    }
  }
  upProb<<-upCount/elements
  downProb<<-downCount/elements
  nochProb<<-nochCount/elements
}
