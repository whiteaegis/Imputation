#######genotype imputation svm
	
	rm(list=ls())
	xx=read.delim(file.choose(),header=T)
	y = as.matrix(xx[,12:ncol(xx)])
	library(e1071)
	time=Sys.time()
L=2
	final=c()
	w=20 #window size
	n.step=floor(nrow(y)/w)
	for(L in 1:n.step){
	if (L <= (n.step-1)){
   	x=y[(w*(L-1)+1):(w*L),]
  	} else{
	x=y[(w*(L-1)+1):nrow(y),]
  	} 
i=1;j=1;model<-c()
			for(i in 1:ncol(x)){
			x.score<-x
			N.id = which(x.score[,i]=="N")
			x.score[x.score%in%c("N")]<-c(-1)
			x.score[x.score%in%c("S","M","R","Y","W","K")]<-c(1)
			x.score<-apply(x.score,1,tr2)
			if (length(N.id)>0){
				x.score<-t(x.score)
				for (j in 1:length(N.id)){		
				x.score.result<-as.factor(x.score[N.id[j],-i])
				x.score.train<-data.frame(x.score[-N.id[j],-i])
				x.score.train<-apply(x.score.train,1,as.numeric)
				x.score.test<-as.numeric(x.score[-N.id[j],i])
				model<-svm(x.score.train,x.score.result)
				answer<-predict(model,t(x.score.test))
				x.line<-x[N.id[j],]
				genotype=row.names(sort(table(x.line), decreasing = TRUE))
				genotype<-genotype[genotype%in%c("A","T","C","G")]
				heterozygous<-row.names(table(x.line[x.line%in%c("M","K","S","W","Y","R")]))
				if(answer==2)x[N.id[j],i]=genotype[1]
				if(answer==0)x[N.id[j],i]=genotype[2]
				if(answer==1){x[N.id[j],i]=heterozygous}
				if(answer==-1){x[N.id[j],i]=c("N")}
				}	
			}	
			}	
		final = rbind(final,x)
	}

Sys.time()-time

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



