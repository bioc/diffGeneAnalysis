"refGroup" <-
function(biasAdjusted,numSlides,ctrl,exp,pval){
  bAdjusted<-biasAdjusted
  nSlides<-numSlides+1
  nthSlide<-2 : nSlides
  geneNum<-length(biasAdjusted[,1])
  zTrans4<-biasAdjusted[1:geneNum,nthSlide]
  biasAdjusted[1:geneNum,nthSlide]<-(10 ^(biasAdjusted[1:geneNum,nthSlide]))
  ctrls<-ctrl+1
  control<-2:ctrls
  ctrl2<-ctrls+1
  expm<-ctrl2:nSlides
  n1<-0.20 * (geneNum * ctrl)
  n2<-0.20 * (geneNum * exp)
  ctrlavjs<-rowMeans(biasAdjusted[1:geneNum,control])
  ctrlsds<-sd(t(biasAdjusted[,control]))
  expmavjs<-rowMeans(biasAdjusted[1:geneNum,expm])
  expmsds<-sd(t(biasAdjusted[,expm]))
  tstatctrl<-suppressWarnings(ctrlavjs/sqrt(((ctrlsds*ctrlsds)/ctrl)+(1/n1)))
  tstatexpm<-suppressWarnings(expmavjs/sqrt(((expmsds*expmsds)/exp)+(1/n2)))
  dnormctrl<-pnorm(tstatctrl)
  dnormexpm<-pnorm(tstatexpm)
  dnormctrl<-ifelse(dnormctrl > 0.5, 1-dnormctrl,dnormctrl)
  dnormexpm<-ifelse(dnormexpm > 0.5, 1-dnormexpm,dnormexpm)
  pctrl<-dnormctrl * 2
  pexpm<-dnormexpm * 2
   r<-(nrow(zTrans4))
   geneAvjs<-(rowSums(zTrans4[,1:ctrl]))/ctrl
   gindex<-which(geneAvjs > 0.477)
   gAvjs<-geneAvjs[geneAvjs > 0.477]
   zTransR<-zTrans4[gindex,]
   genenum<-nrow(zTransR)
 cut<-nrow(zTransR)
 len<-0
 zTransRef<-zTransR - gAvjs
  while( len != cut){
 cut<-nrow(zTransRef)
 refgrp<-c(zTransRef[,1:ctrl],recursive=TRUE)
 rgrplen<-length(refgrp)
 sdrefgrp<-sd(refgrp)
 sdgenes<-sd(t(zTransRef[,1:ctrl]))
 count<-length((sdgenes))
 refgrpsd2<-sdrefgrp * sdrefgrp
 SdGenes2<-sdgenes * sdgenes
 generatios<-refgrpsd2/SdGenes2
 fvals<-pf(generatios,count-1,ctrl-1)
 l3<-length(fvals)
 for(z in 1:l3){
 if(fvals[z] > 0.5){
 fvals[z]<- 1 - fvals[z]}}
 pvalftest<-fvals *2
 gindex2<-which(sdgenes < sdrefgrp)
 gindex22<-which(pvalftest > pval)
 gindex33<-union(gindex2,gindex22)
 gindex33<-sort(gindex33)
   zTransRef<-zTransRef[gindex33,]
   len<-nrow(zTransRef)
  if(cut==len){break}
 }
 print(sdrefgrp)
 print(len)
 assocAnalysis(bAdjusted,numSlides,ctrl,exp,sdrefgrp,pctrl,pexpm,ctrlavjs,expmavjs,ctrlsds,expmsds,rgrplen)
}

