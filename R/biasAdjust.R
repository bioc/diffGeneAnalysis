"biasAdjust" <-
function(normalized,numSlides){
 zTrans3<-normalized
 zTrans1<-normalized
  for(x in 1:6){
   nSlides<-numSlides+1
   nthSlide<-2 : nSlides
   geneNum<-length(zTrans3[,1])
   geneAvjs<-(rowSums(zTrans3[1:geneNum,][nthSlide]))/numSlides
   gindex<-which(geneAvjs > 0.47)
   gAvjs<-geneAvjs[geneAvjs >0.47]
   zTrans<-zTrans3[gindex,]
   for(j in 2:nSlides){
   gAvjs2<-gAvjs
   zTrans2<-zTrans[,j]
   zTrans22<-zTrans[,j]
   zTrans2<-zTrans2 - gAvjs2
   regData<-zTrans2
   beforetrim<-regData 
   trim<-dataTrim(beforetrim)
   nmean<-mean(trim)
   nsd<-sd(trim)  
   plus2SD <-nmean + (2*nsd)
   minus2SD <-nmean - (2*nsd)
   zT2index2<-which(zTrans2 > minus2SD)
   gAvjs2<-gAvjs2[zT2index2]
   zTrans22<-zTrans22[zT2index2]
   zTrans23<-zTrans22
   zTrans2<-zTrans2[zT2index2]
   gAvjs3<-gAvjs2
   zT2index<-which(zTrans2 < plus2SD)
   gAvjs3<-gAvjs3[zT2index]
   zTrans23<-zTrans23[zT2index]
   linReg.lm<-lm(zTrans23 ~ gAvjs3)
   param<-coef(linReg.lm)
   zTrans1[1:geneNum,j]<-(zTrans1[1:geneNum,j]-param[[1]])/(param[[2]])
   zTrans3[,j]<-zTrans1[,j]
   }
  }
 param[[1]]
 param[[2]]
 zTrans3
}

