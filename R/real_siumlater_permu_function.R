rm(list=ls())
#for hapmap formate
data=read.delim(file.choose(),header=T,permutation=FALSE)

real.simulate<-function(x,number=100000,permutation=FALSE){
zz = as.matrix(data[,12:ncol(data)])
row<-sample(1:nrow(zz), size=number,replace=T)
col<-sample(1:ncol(zz), size=number,replace=T)
tt<-cbind(row,col)
tt<-unique(tt)
if(permutation==TRUE){per<-sample.int(nrow(zz), size = nrow(zz), replace = FALSE, prob = NULL);zz<-zz[per,]}
ww<-zz[tt]
zz[tt]<-c("N")
print("missing number in simulation data:");print(sum(zz=="N"))
print("missing rate on simulation data:");print(sum(zz=="N")/length(zz))
result<- new.env()
result$zz<-zz
result$ww<-ww
result$tt<-tt
if(permutation==TRUE){result$per<-per}
result<-as.list(result)
return(result)
}

y<-real.simulate(data)

sum(final[tt]==ww)/sum(zz=="N")