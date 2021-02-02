rm(list=ls())
xx=read.delim(file.choose(),header=T)
#for (s in 1:3653){
#xx=read.delim(file.choose(),header=T,nrows=3000,skip=s*3000)

###################################
# computation from here
###################################
y = as.matrix(xx[,12:ncol(xx)])

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
time=Sys.time()
final=c()
w=20 #window size
k=5 #k for KNN algorithm
n.step=floor(nrow(y)/w)
for(L in 1:n.step){
  if (L <= (n.step-1)){
   x=y[(w*(L-1)+1):(w*L),]
  } else{
   x=y[(w*(L-1)+1):nrow(y),]
  } 
  for(i in 1:nrow(x)){
     library(scrime)
     N.id = which(x[i,]=="N")
     x.new = x[i, -N.id]
     if (length(N.id) > 0){
     for (j in 1:length(N.id)){
	x.target=x[-i, N.id[j]]
	x.train=x[-i,-N.id[j]]
	temp=cbind(x.target, x.train)
	temp<-change(temp)
	nn<-cohen(t(temp))
	ss=sort(nn[1,-1],index.return=TRUE)
	x.tab=table(as.character(x.new[ss$ix[1:k]]))
	genotype=row.names(x.tab)
	x[i, N.id[j]]=genotype[which.max(x.tab)]

     }}
  }
  final = rbind(final,x)
}


Sys.time()-time

score.f(x.impute, x.new[,1])

target=x.impute
train=x.new[,39]


