"normalize" <-
function(rawdata,numSlides,ctrl,expm,ctrlbg=0.30,expmbg=0.30){
#x<-TRUE
normOut<-rawdata
slideNum<-2
cslides<-ctrl + 1
eslides<-cslides + 1
lslide<-numSlides + 1

for(c in 2:cslides){
 answer<-rawdata[,c]
 len<-length(answer)
 sorted<-sort(answer)
 subsetSize<- ctrlbg * len
 answer<-sorted[1:subsetSize]
 trimmed<-dataTrim(answer)
 plot<-1
 pl<-curveFit(trimmed,plot)
 dev.off()
 ch3<-menu(c("y","n"),graphics=TRUE,title="Did you like the Histogram Distribution?")
 while(!ch3==1){
 print("Enter Background Percentage Value: 0.30 was your default")
 cbg<-scan("",what="double",n=1)
 ctrlbg<-as.double(cbg)
 subsetSize<- ctrlbg * len
 answer<-sorted[1:subsetSize]
 trimmed<-dataTrim(answer)
 plot<-1
 pl<-curveFit(trimmed,plot)
 dev.off()
 ch3<-menu(c("y","n"),graphics=TRUE,title="Did you like the Histogram Distribution?")
 }
 totset<-rawdata
 values<-1:length(rawdata[,c])
 rawdata[,c]<-(rawdata[,c][values]-pl[1])/pl[2]
 set<-rawdata[,c][rawdata[,c] > 0]
 minLog<-min(set)
 rawdata[,c] [rawdata[,c] <= 0] <-minLog
 rawdata[,c]<-log10(rawdata[,c][values])
 #replace -ve logs from the rawdata
 totset[,c] <- rawdata[,c]
 normOut[,c]<-totset[,c]
}

for(e in eslides:lslide){
answer<-rawdata[,e]
len<-length(answer)
 sorted<-sort(answer)
 subsetSize<- expmbg * len
 answer<-sorted[1:subsetSize]
 trimmed<-dataTrim(answer)
 plot<-1
 pl<-curveFit(trimmed,plot)
 dev.off()
 ch3<-menu(c("y","n"),graphics=TRUE,title="Did you like the Histogram Distribution?")
 while(!ch3==1){
 print("Enter Background Percentage Value: 0.30 was your default")
 ebg<-scan("",what="double",n=1)
 expmbg<-as.double(cbg)
 subsetSize<- expmbg * len
 answer<-sorted[1:subsetSize]
 trimmed<-dataTrim(answer)
 plot<-1
 pl<-curveFit(trimmed,plot)
 dev.off()
 ch3<-menu(c("y","n"),graphics=TRUE,title="Did you like the Histogram Distribution?")
 }
 totset<-rawdata
 values<-1:length(rawdata[,e])
 rawdata[,e]<-(rawdata[,e][values]-pl[1])/pl[2]
 set<-rawdata[,e][rawdata[,e] > 0]
 minLog<-min(set)
 rawdata[,e] [rawdata[,e] <= 0] <-minLog
 rawdata[,e]<-log10(rawdata[,e][values])
 #replace -ve logs from the rawdata
 totset[,e] <- rawdata[,e]
 normOut[,e]<-totset[,e]
}
normOut
}

