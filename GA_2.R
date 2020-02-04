functionX<-function(x,y){
 res=20+x^2+y^2-10*(cos(2*pi*x)+(cos(2*pi*y)))

  return (round(res,digits=3))
}
computeAccuracy<-function(n,l,u){
  acc<-(u-l)/(2^n)
  return(round(acc,digits=3))
}
generateval<-function(lb,ub,popSize){
#  xVal=sample(1:((2^nBits)-1),popSize)
  xVal=runif(popSize,lb,ub)
  return(xVal)
}
computeXOriginalX<-function(xVal,lb,ub,n){
  original<-(xVal-lb)/((ub-lb)/((2^n)-1))

}
dectoBin<-function(x,nBits){
  binary<-paste(as.integer(rev(intToBits(x))), collapse = "")
  binValue<-substr(binary, nchar(binary)-nBits+1, nchar(binary))
}
binToDec<-function(x){
  sum(2^(which(rev(unlist(strsplit(as.character(x),""))==1))-1))
}
computeVal<-function(n,l,u,x){
  val<-l+(((u-l)/((2^n)-1))*x)
  return(round(val,digits=3))
}
findChromNo<-function(cumList,rNo){
  for(i in 1:length(cumList)){
    if(rNo<cumList[i]){
      return(i)
    }
  }
}
findTrueCount<-function(index,chromList){
  count<-0
  for(i in 1:length(chromList)){
    if(index==chromList[i]){
      count<-count+1
    }
  }
  return(count)
}
Applicable<-function(rNo,p){
  if(rNo<=p){
    return("Y")
  }
  else{
    return("N")
  }
}
getIntermediate<-function(p1,p2,site,nBits){
  p1temp<-gsub("\\s+","",p1)
  p2temp<-gsub("\\s+","",p2)
  z1<-paste0(substr(p1temp,1,site),substr(p2temp,site+1,2*nBits))
  z2<-paste0(substr(p2temp,1,site),substr(p1temp,site+1,2*nBits))
  z<-paste(z1,z2)
  return(z)
}
getMutated<-function(p,site){
  ptemp<-gsub("\\s+","",p)
  if(substr(ptemp, site, site)=='0'){
    substr(ptemp, site, site) <- '1'
  }
  else{
    substr(ptemp, site, site) <- '0'
  }
  temp<-paste(substr(ptemp,1,nBits),substr(ptemp,nBits+1,2*nBits))
  return(temp)
}
probType<-"min"
lbX<--5
ubX<-5
lbY<--5
ubY<-5
popSize<-20
nBits<-5
pCo<-0.65
pMu<-0.1
s<-1
stop<-10
averageFitness<-vector()
bitList<-vector()
while(s<=stop){
  xList<-vector()
  yList<-vector()
  xValList<-vector()
  yValList<-vector()
  fXlist<-vector()
  FXlist<-vector()
  probList<-vector()
  cumProbList<-vector()
  rNoList<-vector()
  chromList<-vector()
  trueCountList<-vector()
  if(s==1){
    xValList=generateval(lbX,ubX,popSize)
    yValList=generateval(lbY,ubY,popSize)
  #  xList=generateval(nBits,popSize)
  #  yList=generateval(nBits,popSize)
    for(j in 1:length(xValList)){
      bit=''
      xList=append(xList,computeXOriginalX(xValList[j],lbX,ubX,nBits))
      yList=append(yList,computeXOriginalX(yValList[j],lbY,ubY,nBits))
      bit=paste0(dectoBin(xList[j],nBits)," ",dectoBin(yList[j],nBits))
      bitList=append(bitList,bit)

    }
  }
  else{
    for(j in 1:length(bitList)){
      bitTemp=strsplit(bitList[j],' ')
      xList=append(xList,binToDec(bitTemp[[1]][1]))
      yList=append(yList,binToDec(bitTemp[[1]][2]))  

      xValList=append(xValList,computeVal(nBits,lbX,ubX,xList[j]))
      yValList=append(yValList,computeVal(nBits,lbY,ubY,yList[j]))
      }
    
  }
  for(i in 1:popSize){
   
    z<-functionX(xValList[i],yValList[i])
   
    fXlist=append(fXlist,z)
    if(probType=="max"){
      FXlist=append(FXlist,fXlist[i])
    }
    else{
      if(fXlist[i]==0){
        val=round((1/(fXlist[i]+1)),3)
        FXlist=append(FXlist,val)
      }
      else{
        FXlist=append(FXlist,1/fXlist[i])
      }
    }
  }
  sumFx<-sum(FXlist)
  cum<-0
  for(i in 1:length(bitList)){
    val=round((FXlist[i]/sumFx),3)
    probList=append(probList,val)
    cum<-cum+probList[i]
    cumProbList=append(cumProbList,cum)
    rNoList=append(rNoList,round(runif(1,0,1),3))
  }
  for(i in 1:length(bitList)){
    chromList=append(chromList,findChromNo(cumProbList,rNoList[i]))
  }
  for(i in 1:length(bitList)){
    trueCountList=append(trueCountList,findTrueCount(i,chromList))
  }
 cat("ITERATION",s," -SELECTION\n")
  cat('X',"\t",'Y',"\t",'X-decimal',"\t",'Y-decimal','X-value',"\t",'Y-value',"\t",'f(x)',"\t",'F(X)',"\t",'Pi',"\t",'Cum-Pi',"\t","Random-No","\t","Chromosome","\t",'
TrueCount',"\n")
  for(i in 1:length(bitList)){
    bitTemp<-gsub("\\s+","",bitList[i])
    xTemp<-substr(bitTemp,1,nBits)
    yTemp<-substr(bitTemp,nBits+1,2*nBits)
    cat(xTemp,"\t",yTemp,"\t",xList[i],"\t",yList[i],"\t",xValList[i],"\t",yValList[i],"\t",fXlist[i],"\t",FXlist[i],"\t",probList[i],"\t",cumProbList[i],"\t",rNoList[i],"\t",chromList[i],"\t",trueCountList[i],"\n")
  }
  averageFitness=append(averageFitness,sumFx/popSize)
  newbitList<-vector()
  rNoList<-vector()
  crossOverApplicableList<-vector()
  mutationApplicableList<-vector()
  mutatedChromosome<-vector()
  intermediatePopList<-vector()
  x<-1
  for(i in 1:length(chromList)){
    newbitList=append(newbitList,bitList[chromList[i]])
    rNo=round(runif(1,0,1),3)
    if(i%%2==0){
      rNoList=append(rNoList,rNoList[i-1])
    }
    else{
      rNoList=append(rNoList,rNo)
      x=x+1
    }
    crossOverApplicableList=append(crossOverApplicableList,Applicable(rNoList[i],pCo))
  }
  x<-1
  rNo1List<-vector()
  for(i in seq(1,length(newbitList),2)){
    if(crossOverApplicableList[i]=='Y' && crossOverApplicableList[i+1]=='Y' ){
      coSite<-sample(1:(2*nBits-1),1)
      z<-getIntermediate(newbitList[i],newbitList[i+1],coSite,nBits)
      zTemp<-strsplit(z," ")
      x1Temp<-paste(substr(zTemp[[1]][1],1,nBits),substr(zTemp[[1]][1],nBits+1,2*nBits))
      x2Temp<-paste(substr(zTemp[[1]][2],1,nBits),substr(zTemp[[1]][2],nBits+1,2*nBits))
      intermediatePopList=append(intermediatePopList,x1Temp)
      intermediatePopList=append(intermediatePopList,x2Temp)
      x<-x+1
    }
    else{
      intermediatePopList=append(intermediatePopList,newbitList[i])
      intermediatePopList=append(intermediatePopList,newbitList[i+1])
    }
  }
  for(i in 1:length(newbitList)){
    rNo1List=append(rNo1List,round(runif(1,0,1),3))
    muSite<-sample(1:2*nBits,1)
    mutationApplicableList=append(mutationApplicableList,Applicable(rNo1List[i],pMu))
    if(mutationApplicableList[i]=='Y'){
      mutatedChromosome=append(mutatedChromosome,getMutated(intermediatePopList[i],muSite))
    }
    else{
      mutatedChromosome=append(mutatedChromosome,intermediatePopList[i])
    }
  }
  cat("\nITERATION",s,"-CROSS OVER & MUTATION \n")
  cat('x',"\t",'y',"\t",'r.no',"\t",'crossOver',"\t",'Intermediate x',"\t",'Intermediate y',"\t",'R.No',"\t",'Mutation',"\t",'Chromosome X',"\t",'Chromosome Y',"\n")
  for(i in 1:length(newbitList)){
    bitTemp<-gsub("\\s+","",newbitList[i])
    xTemp<-substr(bitTemp,1,nBits)
    yTemp<-substr(bitTemp,nBits+1,2*nBits)
    intermediatePop<-strsplit(intermediatePopList[i]," ")
    mutatedTemp<-strsplit(mutatedChromosome[i]," ")
   cat(xTemp,"\t",yTemp,"\t",rNoList[i],"\t",crossOverApplicableList[i],"\t",intermediatePop[[1]][1],"\t",intermediatePop[[1]][2],"\t",rNo1List[i],"\t",mutationApplicableList[i],"\t",mutatedTemp[[1]][1],"\t",mutatedTemp[[1]][2],"\n")
  }
  
  s=s+1
  bitList=mutatedChromosome
}
print(averageFitness)
iteration=1:stop
# Plot the bar chart. 
plot(iteration,averageFitness)

