"dataTrim" <-
function(chipdata){
  x<-TRUE
  cut<-chipdata
  len <-length(chipdata)
  newlen<-0
  while(x==TRUE){
  mean30<-mean(cut)
  std<-sd(cut)
  plus2SD <-mean30 + (2*std)
  cut<-cut[cut < plus2SD]
  #print("recompute mean and sd")
  mean230<-mean(cut)
  std2<-sd(cut)
  minus2SD <-mean230 - (2*std2)
  #data below -2sd are cut
  cut<-cut[cut > minus2SD]
  len <-length(cut)
  if(newlen==len)
  x<-FALSE
  else
  newlen<-length(cut)
  }
  cut
  }

