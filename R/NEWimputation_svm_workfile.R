	library(e1071)
	rm(list=ls())
	xx=read.delim(file.choose(),header=T)
	y = as.matrix(xx[,12:ncol(xx)])
	
	final=c()
	w=20 #window size
	n.step=floor(nrow(y)/w)
	for(L in 1:n.step){
 	if (L <= (n.step-1)){
	x=y[(w*(L-1)+1):(w*L),]
 	} else{
	x=y[(w*(L-1)+1):nrow(y),]
	} 
	
	x=y[(20*(1-1)+1):(20*1),]
	N.id = which(x[1,]=="N")
	x.new = x[1,-N.id[3]]
      x.target=x[-1, N.id[3]]
      x.train=x[-1,-N.id[3]]
	x.score<-x
	x.score[x.score%in%c("N")]=-1
	fix(x.score)
	x.score<-apply(t(x.score),1,tr)
	N.id = which(x.score[,1]==-1)
	x.score.result<-as.numeric(x.score[N.id[1],-1])
	x.score.train<-data.frame(x.score[-N.id[1],-1])
	x.score.train<-apply(x.score.train,1,as.numeric)
	x.score.test<-as.numeric(x.score[-N.id[1],1])
	model<-svm(x.score.train,x.score.result)
	answer<-predict(model,t(x.score.test))

#######genotype	

	y = as.matrix(xx[,12:ncol(xx)])
	library(e1071)
	time=Sys.time()

	final=c()
	w=20 #window size
	n.step=floor(nrow(y)/w)
	for(L in 1:n.step){
  
	if (L <= (n.step-1)){
   	x=y[(w*(L-1)+1):(w*L),]
  	} else{
	x=y[(w*(L-1)+1):nrow(y),]
  	} 

	for(i in 1:nrow(x)){
	x.score<-x
	N.id = which(x.score[,i]==-1)
	if (length(N.id) > 0){
	for (j in 1:length(N.id)){
	x.score[x.score%in%c("N")]<-c(-1)
	x.score[x.score%in%c("S","M","R","Y","W","K")]<-c(1)
	x.score<-apply(x.score,1,tr2)
	x.score<-t(x.score)
	N.id = which(x.score[,i]==-1)
	x.score.result<-as.factor(x.score[N.id[j],-i])
	x.score.train<-data.frame(x.score[-N.id[j],-i])
	x.score.train<-apply(x.score.train,1,as.numeric)
	x.score.test<-as.numeric(x.score[-N.id[j],i])
	model<-svm(x.score.train,x.score.result)
	answer<-predict(model,t(x.score.test))
	x.line<-x[i,]
	genotype=row.names(sort(table(x.line), decreasing = TRUE))
	genotype<-genotype[x.line%in%c("A","T","C","G")]
	heterozygous<-x.line[!x.line%in%c("A","T","C","G","N")]
	if(answer==-1){x[N.id[1],1]=c("N")}
	if(answer==2){x[N.id[1],1]=genotype[1]}
	if(answer==0){x[N.id[1],1]=genotype[0]}
	if(answer==1){x[N.id[1],1]=heterozygous}
	
	}#for (j in 1:length(N.id))
	}#if N.id 
	}#for(i in 1:nrow(x))
	final = rbind(final,x)
	}#for(L in 1:n.step)


	x[N.id[1],1]

	dim(ll)	
	y.1<-ll		
	y.1[y.1%in%c("N")]<-c(-1)
	y.1[y.1%in%c("S","M","R","Y","W","K")]<-c(1)
	x.score<-apply(y.1,1,tr2)
	
	tr2<-function(x)
	{
	t<-x[x!=-1]
	t<-t[t!=1]
	a<-table(t)
	genotype=row.names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=2
	x[x==genotype[2]]=0
	return(x)
	}
	

	?apply
	fix(y.1)
	fix(x.score)
	table(x.score)
	table(ll)


	tr<-function(x)
	{
	t<-x[x!=-1]
	genotype=row.names(table(t))
	if(length(unique(table(t)))==1)
	{
	x[x!=genotype[2] & x!=genotype[1]& x!=-1]=-1
	x[x==genotype[1]]=1
	x[x==genotype[2]]=0
	}
	else
	{
	x[x!=genotype[which.min(table(t))]& x!=genotype[which.max(table(t))]& x!=-1]=-1
	x[x==genotype[which.max(table(t))]]=1
	x[x==genotype[which.min(table(t))]]=0
	}
	return(x)
	}


	fff<-c("A","A")
	fff[!fff%in%c("A","T","C","G")]=as.numeric(-1)
	x.score<-apply(t(x.score),1,tr(fff))



