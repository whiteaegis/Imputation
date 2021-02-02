	rm(list=ls())
	
	ldwknnwindowimpute<-function(y,w=20,k=5,match.score=1, miss.score=0.5,mismatch.score=-5,cd="FALSE"){
	LD<-function(y.1){
	y.1[y.1%in%c("K","M","R","S","W","Y","N","H")]<-c(-1)
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
	tr<-function(x){
	t<-x[x!=-1]
	genotype=row.names(table(t))
	if(length(unique(table(t)))==1){
	x[x!=genotype[2] & x!=genotype[1]& x!="-1"]<-"-1"
	x[x==genotype[1]]<-"1"
	x[x==genotype[2]]<-"0"}
	else{
	x[x!=genotype[which.min(table(t))]& x!=genotype[which.max(table(t))]& x!="-1"]<-"-1"
	x[x==genotype[which.max(table(t))]]<-"1"
	x[x==genotype[which.min(table(t))]]<-"0"
	}
	return(x)
	}
	ff<-function(x){
	b<-which(x==0)
	d<-length(x)
	x<-c(c(1:d-b))
	return(x)
	}
	library(popgen)
	time=Sys.time()
	final=c()
	n.step=floor(nrow(y)/w)
	for(L in 1:n.step){
	if (L <= (n.step-1)){
	x=y[(w*(L-1)+1):(w*L),]
	} else{
	x=y[(w*(L-1)+1):nrow(y),]} 
	ld=LD(x)
	if(sum(ld%in%NaN)!=0)ld[ld%in%NaN]<-0.000001
	for(i in 1:nrow(x)){
	N.id = which(x[i,]=="N")
	w.target=(ld[-i,i]/sum(ld[-i,i]))
	if (length(N.id) > 0){
	for (j in 1:length(N.id)){
	x.new = x[i,-N.id[j]]
	x.target=x[-i, N.id[j]]
	x.train=x[-i,-N.id[j]]
	s=apply(x.train,2,function(x){score.ld.w.1(x.target,x,w.target,match.score, miss.score,mismatch.score)})        
	ss=sort(s,decreasing=TRUE,index.return=TRUE)
	x.tab=table(as.character(x.new[ss$ix[1:k]]))
	genotype=row.names(x.tab)
	if(cd=="TRUE"){
	t<-as.matrix(x.tab)
	sw<-as.matrix(combn(as.matrix(x.tab),2)[,combn(as.matrix(x.tab),2)[1,]==max(as.matrix(x.tab))])        
	if(sum(sw[2,]<max(t))==dim(sw)[2]){
	x[i, N.id[j]]=genotype[which.max(x.tab)]
	}
      else{x[i, N.id[j]]=c("N")}
	}
	else {x[i, N.id[j]]=genotype[which.max(x.tab)]}
	}}}
	final = rbind(final,x)
	}
	print(Sys.time()-time)
	return(final)
	}

	final<-ldwknnwindowimpute(y)