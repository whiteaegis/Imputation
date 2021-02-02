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
k=13 #k for KNN algorithm
n.step=floor(nrow(y)/w)

final=c()
for(L in 1:n.step){
  if (L <= (n.step-1)){
   x=y[(w*(L-1)+1):(w*L),]
  } else{
   x=y[(w*(L-1)+1):nrow(y),]
  } 
  x.impute<-foreach(i=1:nrow(x),.combine=rbind) %dopar% {
     library(scrime)
     N.id = which(x[i,]=="N")
     x.new = x[i, -N.id]
     if (length(N.id) > 0){
     for (j in 1:length(N.id)){
	x.target=x[-i, N.id[j]]
	x.train=x[-i,-N.id[j]]
	temp=cbind(x.target, x.train)
	temp<-change(temp)
	nn<-smc(t(temp))
	ss=sort(nn[1,-1],index.return=TRUE)
	x.tab=table(as.character(x.new[ss$ix[1:k]]))
	genotype=row.names(x.tab)
	x[i, N.id[j]]=genotype[which.max(x.tab)]
     }}
     x[i,]
  }
  final = rbind(final,x.impute)
}
Sys.time()-start
stopCluster(cl)

change<-function(temp)
	{
	temp[temp%in%c("N")]<-c(1)
	temp[temp%in%c("S","M","R","Y","W","K")]<-c(3)
	temp<-apply(temp,1,tr2)
	temp<-apply(temp,2,as.numeric)
	}

tr2<-function(x)
	{
	t<-x[x!=1]
	t<-t[t!=3]
	a<-table(t)
	genotype=row.names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=4
	x[x==genotype[2]]=2
	return(x)
	}