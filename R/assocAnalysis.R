"assocAnalysis" <-
function(bAdjusted,numSlides,ctrl,exp,sdrefgrp,pctrl,pexpm,ctrlavjs,expmavjs,ctrlsds,expmsds,rgrplen){
 e<-ctrl+1
 ee<-e+1
 ex<-numSlides+1
 geneNum<-length(bAdjusted[,2])
 refgrpNum <- rgrplen * ctrl
 t3<-data.matrix(bAdjusted)
 studentt<-apply(t3, 1, function(x) t.test(x[2:e], x[ee:ex],var.equal=TRUE)$p.value)
 col7studentpval<- studentt #* 2 #$p.value
 ctrlgeneavj<-rowMeans(bAdjusted[,2:e])
 expvals2<-bAdjusted[,ee:ex] - ctrlgeneavj
 avjnewgrp<-rowMeans(expvals2)
 sdnewgrp<-sd(t(expvals2))
 newtstat<-(avjnewgrp/(sqrt(sdnewgrp*sdnewgrp/exp))+(sqrt(sdrefgrp*sdrefgrp/refgrpNum)))
 tresults<-pnorm(newtstat,mean=0,sd=1)
 tresults<-ifelse(tresults > 0.5, 1-tresults,tresults)
 col8pvals<-tresults * 2
 col9<-expmavjs/ctrlavjs
 print("Enter E Value")
 E<-scan("",what="double",n=1)
 print("Enter R Value")
 R<-scan("",what="double",n=1)
 che<-as.double(E)
 chr<-as.double(R)
 group<-NULL
 R<-expmavjs/ctrlavjs
 t<-1/rgrplen
 for(x in 1:geneNum){
 if(pctrl[x] < t &&  pexpm[x] < t 
 && col7studentpval[x] < 0.05 && col8pvals[x] < t && col9[x] > chr)
 {group[x]="A1"}

 else if(pctrl[x] < t && pexpm[x] < t 
 && col7studentpval[x] < 0.05 && col8pvals[x] < t && col9[x] <=(che/chr))
 {group[x]="A2"}

 else if(pctrl[x] < t && pexpm[x] > t 
 && col7studentpval[x] < 0.05 && col9[x] < 0.66)
 {group[x]="A4"}

 else if(pctrl[x] > t && pexpm[x] < t 
 && col7studentpval[x] < 0.05 && col9[x] > chr)
 {group[x]="A3"}

 else{ group[x]="0"}
 }
 results<-matrix(data=NA,nrow=geneNum,ncol=11)
 results[,1] <-bAdjusted[,1]
 results[,2] <-ctrlavjs
 results[,3] <-ctrlsds
 results[,4] <-pctrl
 results[,5] <-expmavjs
 results[,6] <-expmsds
 results[,7] <-pexpm
 results[,8] <-col7studentpval
 results[,9] <-col8pvals
 results[,10] <-col9
 results[,11] <-group

 results
}

