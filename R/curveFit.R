"curveFit" <-
function(chipdata,plot){ 
 selectGraph<-function( ){
menu(c(2,3,4,4.5,5,5.5),graphics=TRUE,title="Pick Best Fit")
}

  mx<-max(chipdata)
  mn<-min(chipdata)
  breakrange <- (mx -mn)/10
  x1<-range(chipdata)
  x<-0:10
  par(col.lab="red")
  vals<-hist(chipdata, breaks =mn+x*breakrange,probability=TRUE,plot=FALSE,
  xlim = x1, ylim = NULL, xlab="Total length", main="",angle=45, density=13,col="green",border="red")
  suppressWarnings(par(col="red",new=TRUE,ann=FALSE,xaxt="s",yaxt="s"))
  par(mfrow=c(3,2))
  freqs<-vals$counts 
  fr<-vals$counts[1]
  xs<-NULL
  nmeans<-NULL
  nsds<-NULL
  for(gr in 1:6){
  gr2<-gr-1
  gr3<-gr+1
  gr4<-gr3-1
  if(gr==6){
  sm<-vals$breaks[6]
  fr<-fr+round(0.5*vals$counts[6])
  xs<-c(vals$mids[1:5],vals$breaks[6])
  sys<-sd(xs)
  }
  else if(gr==4){
  sm<-vals$breaks[5]
  fr<-fr+round(0.5*vals$counts[5])
  xs<-c(vals$mids[1:4],vals$breaks[5])
  sys<-sd(xs)
  }
  else{
  sm<-vals$mids[gr3]
  fr<-fr+vals$counts[gr3]
  if(gr==5){
  xs<-c(vals$mids[1:gr])}
  else{
  xs<-c(vals$mids[1:gr3])}
  sys<-sd(xs)
  }
  par(col="red",xaxt="s",yaxt="s",lab=c(5,5,7))
  if(plot==1){
  vals<-suppressWarnings(hist(chipdata, breaks =mn+x*breakrange,freq=FALSE,
  xlim = x1, ylim = NULL, xlab="Total length", main="",angle=45, density=13,col="green",border="red"))}
  else{
  vals<-hist(chipdata, breaks =mn+x*breakrange,freq=TRUE,plot=FALSE,
  xlim = x1, ylim = NULL, xlab="Total length", main="",angle=45, density=13,col="green",border="red")}
  range3<-chipdata[fr] - chipdata[1]
  xs1<-c(chipdata[1:fr],chipdata[1:fr]+range3)
  emean<-mean(xs1)
  tsd<-sd(xs1)
  if(gr==6 || gr==5){
  range2<-xs[gr] - xs[1]
  values<-c(xs,xs+range2)
  le<-2 * gr
  gr5<-gr+2
  values<-c(values[1:gr2],values[gr5:le])}
  else if(gr ==4){
  range2<-xs[gr3] - xs[1]
  values<-c(xs,xs+range2)
  le2<-2 * gr3
  gr5<-gr+2
  values<-c(values[1:gr4],values[gr5:le2])
  }
  else{
  range2<-xs[gr3] - xs[1]
  values<-c(xs,xs+range2)
  le2<-2 * gr3
  gr5<-gr+2
  values<-c(values[1:gr4],values[gr5:le2])
  }
  if(gr==6 || gr==5){
  ys<-c(vals$density[1:gr2],vals$density[gr2:1])}
  else{
  ys<-c(vals$density[1:gr4],vals$density[gr3:1])}
  sm1<-mean(values)
  sys1<-sd(values)
  suppressWarnings(par(col="blue",new=TRUE,ann=FALSE,xaxt="n",yaxt="n"))#,fig=c(0,gr3/10,0,1)))
  f <- function(tsd, emean, values) {
    expr <- expression(1/(sqrt(2*pi)*tsd)*2.71828^-((values-emean)^2/(2*tsd^2)))
    eval(expr)
    }
  p<-c("tsd" = tsd, "emean" = emean)
  N<-do.call(f, c(list(values=values),as.list(p)))
  set.seed(411)
  guess<-c("tsd" = sys, "emean" = sm)
  fcn<- function(p, values, N, N.Err, fcall)
         (N - do.call(fcall, c(list(values = values), as.list(p))))/N.Err
  out <- nls.lm(par = guess, fn = fcn, #jac = fcn.jac,
 fcall = f,
                   values = values, N = N, N.Err = sqrt(N),
                   control = list(nprint = 3, diag = numeric()))
  N1 <- do.call(f, c(list(values = values), out$par))
 dn<-dnorm(xs1,mean=out$par[[2]],sd=out$par[[1]])
 m<-max(vals$density)
 mm<-max(dn)
 fac<-m/mm
 dn<-dn * fac
  points(dn ~ xs1,type="l",lwd=3,col="blue")
  nmeans<-c(nmeans,out$par[[2]])
  nsds<-c(nsds,out$par[[1]])
  }#for loop
 print(nmeans)
 print(nsds)
  ch<-selectGraph()
  ch1<-menu(c("y","n"),graphics=TRUE,title="Confirm Curvefit Selection")
  while(!ch1==1){
  ch<-selectGraph()
  ch1<-menu(c("y","n"),graphics=TRUE,title="Confirm Curvefit Selection")}
  if(ch==1){
  assign("nmean",nmeans[1],inherits=TRUE,env=.GlobalEnv)
  assign("nsd",nsds[1],inherits=TRUE,env=.GlobalEnv)
  res<-c(nmean,nsd)}
  else if(ch==2){
  assign("nmean",nmeans[2],inherits=TRUE,env=.GlobalEnv)
  assign("nsd",nsds[2],inherits=TRUE,env=.GlobalEnv)
  res<-c(nmean,nsd)}
  else if(ch==3){
  assign("nmean",nmeans[3],inherits=TRUE,env=.GlobalEnv)
  assign("nsd",nsds[3],inherits=TRUE,env=.GlobalEnv)
  res<-c(nmean,nsd)}
  else if(ch==4){
  assign("nmean",nmeans[4],inherits=TRUE,env=.GlobalEnv)
  assign("nsd",nsds[4],inherits=TRUE,env=.GlobalEnv)
  res<-c(nmean,nsd)}
  else if(ch==5){
  assign("nmean",nmeans[5],inherits=TRUE,env=.GlobalEnv)
  assign("nsd",nsds[5],inherits=TRUE,env=.GlobalEnv)
  res<-c(nmean,nsd)}
  else if(ch==6){
  assign("nmean",nmeans[6],inherits=TRUE,env=.GlobalEnv)
  assign("nsd",nsds[6],inherits=TRUE,env=.GlobalEnv)
  res<-c(nmean,nsd)}
  else
  print("Pick a Numeric choice between 1 - 6")
 print(res)
}

