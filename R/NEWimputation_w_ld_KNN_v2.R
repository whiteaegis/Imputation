rm(list=ls())
xx=read.delim(file.choose(),header=T)
#for (s in 1:3653){
#xx=read.delim(file.choose(),header=T,nrows=3000,skip=s*3000)

###################################
# computation from here
###################################
y = as.matrix(xx[,12:ncol(xx)])
library(popgen)
time=Sys.time()

final=c()
w=20 #window size
k=13 #k for KNN algorithm
n.step=floor(nrow(y)/w)
for(L in 1:n.step){
  if (L <= (n.step-1)){
   x=y[(w*(L-1)+1):(w*L),]
  } else{
   x=y[(w*(L-1)+1):nrow(y),]
  } 
###
    ld=LD(x)
	ld[ld%in%NaN]<-0.0001
    #ld=od(w)
###
    for(i in 1:nrow(x)){
    N.id = which(x[i,]=="N")
     
####     
     w.target=(ld[-i,i]/sum(ld[-i,i]))
### 
     if (length(N.id) > 0){
     for (j in 1:length(N.id)){
        x.new = x[i,-N.id[j]]
        x.target=x[-i, N.id[j]]
        x.train=x[-i,-N.id[j]]

        s=apply(x.train,2,function(x){score.ld.w.1(x.target,x,w.target)})        

        ss=sort(s,decreasing=TRUE,index.return=TRUE)
        x.tab=table(as.character(x.new[ss$ix[1:k]]))
        genotype=row.names(x.tab)
        t<-as.matrix(x.tab)
        sw<-as.matrix(combn(as.matrix(x.tab),2)[,combn(as.matrix(x.tab),2)[1,]==max(as.matrix(x.tab))])        
        if(sum(sw[2,]<max(t))==dim(sw)[2])
        {
        x[i, N.id[j]]=genotype[which.max(x.tab)]
        }
        else{x[i, N.id[j]]=c("N")}

     }}
  }
  final = rbind(final,x)
}

Sys.time()-time

LD<-function(y.1)
{
y.1[y.1%in%c("K","M","R","S","W","Y","N")]<-c(-1)
y.1<-apply(y.1,1,function(x){tr(x)})
A<-LDmat(y.1,typ="haplotype", plotmat = FALSE)
A[upper.tri(A)]<-NA
kk<-as.matrix(as.dist(as.matrix(A), upper = TRUE))
return(kk)
}


score.ld.w.1 <- function(target,train,w.target,match.score=1, miss.score=0.5, mismatch.score=-5){
id.allmiss = which((train=="N") & (target=="N"))
id.match = which((target==train)&(train!="N"))
id.miss = which((train=="N") | (target=="N"))
score <- sum(w.target[id.match]*match.score) + sum(w.target[id.miss]*miss.score) + sum(w.target[-c(id.match,id.miss)]*mismatch.score)
return(score)
}


score.ld.w <- function(target,train,w.target,match.score=1, miss.score=0.5, mismatch.score=-5){
 m=length(target)
 score<-0
 for(i in 1:m){
 n.i.allmiss <- sum((train[i]=="N") & (target[i]=="N"))
 n.i.match <- sum(target[i]==train[i]) - n.i.allmiss
 n.i.miss <- sum(target[i]=="N")+sum(train[i]=="N")- n.i.allmiss
 n.i.mismatch <- 1 - n.i.match -n.i.miss
 w<-w.target[i]
 i.score <- w*(match.score*n.i.match + miss.score*n.i.miss + mismatch.score*n.i.mismatch)
 score<-score+i.score
 }
 return(score)
}


tr<-function(x)
{
t<-x[x!=-1]
genotype=row.names(table(t))
if(length(unique(table(t)))==1)
{
x[x!=genotype[2] & x!=genotype[1]& x!="-1"]<-"-1"
x[x==genotype[1]]<-"1"
x[x==genotype[2]]<-"0"
}
else
{
x[x!=genotype[which.min(table(t))]& x!=genotype[which.max(table(t))]& x!="-1"]<-"-1"
x[x==genotype[which.max(table(t))]]<-"1"
x[x==genotype[which.min(table(t))]]<-"0"
}
return(x)
}

od<-function(w){
a<-matrix(data = NA, nrow = w, ncol = w)
diag(a)<-0
a<-apply(a,1,function(x){ff(x)})
a[upper.tri(a)]<-NA
a<-as.matrix(as.dist(as.matrix(a), upper = TRUE))
return(a)
}

ff<-function(x){
b<-which(x==0)
d<-length(x)
x<-c(c(1:d-b))
return(x)
}

