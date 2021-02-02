rm(list=ls())
xx=read.delim(file.choose(),header=T)
#for (s in 1:3653){
#xx=read.delim(file.choose(),header=T,nrows=3000,skip=s*3000)

###################################
# computation from here
###################################
y = as.matrix(xx[,12:ncol(xx)])

library(doParallel)
cl <- makeCluster(3)
registerDoParallel(cl)

start=Sys.time()
w=20 #window size
k=5 #k for KNN algorithm
n.step=floor(nrow(y)/w)

final=c()
for(L in 1:n.step){
  if (L <= (n.step-1)){
   x=y[(w*(L-1)+1):(w*L),]
  } else{
   x=y[(w*(L-1)+1):nrow(y),]
  } 
  x.impute<-foreach(i=1:nrow(x),.combine=rbind) %dopar% {
     library(phangorn)
     N.id = which(x[i,]=="N")
     
     if (length(N.id) > 0){
     for (j in 1:length(N.id)){
        x.new = x[i,-N.id[j]]
        x.target=x[-i, N.id[j]]
        x.train=x[-i,-N.id[j]]
##----------
        s=apply(x.train,2,function(x){score.f(x.target,x)})
        ss=sort(s,decreasing=TRUE,index.return=TRUE)
#############
        #temp=cbind(x.target, x.train)
        #new=phyDat(t(temp))
        #nn <- dist.hamming(new)
        #nn<-as.matrix(as.dist(nn))
        #ss=sort(nn[1,-1],decreasing=FALSE,index.return=TRUE)
        #x.tab=table(as.character(x.new[ss$ix[1:k]]))
        #genotype=row.names(x.tab)
        #x[i, N.id[j]]=genotype[which.max(x.tab)]
################
        x.tab=table(as.character(x.new[ss$ix[1:k]]))
        t<-as.matrix(x.tab)
        genotype=row.names(x.tab)
        sw<-as.matrix(combn(as.matrix(x.tab),2)[,combn(as.matrix(x.tab),2)[1,]==max(as.matrix(x.tab))])
        if(sum(sw[2,]<max(t))==dim(sw)[2])
        {
        x[i, N.id[j]]=genotype[which.max(x.tab)]
        }
        else{x[i, N.id[j]]=c("N")}
##---------
     }}
     x[i,]
  }
  final = rbind(final,x.impute)
}
Sys.time()-start

stopCluster(cl)


score.f <- function(target,train,match.score=1, miss.score=0.5, mismatch.score=-5){
 m=length(target)
 #x.tab=table(target,train)
 #id.n = (row.names(x.tab)=="N")
 n.allmiss = sum((train=="N") & (target=="N"))
 n.match = sum(target==train) - n.allmiss
 #n.match = sum(diag(x.tab)[!id.n])
 n.miss = sum(target=="N")+sum(train=="N")- n.allmiss
 n.mismatch = m - n.match -n.miss
 score = (match.score*n.match + miss.score*n.miss + mismatch.score*n.mismatch)/m
 return(score)
}

score.f(x.impute, x.new[,1])