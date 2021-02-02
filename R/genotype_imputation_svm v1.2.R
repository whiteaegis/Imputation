#######genotype imputation svm
	
	rm(list=ls())
	xx=read.delim(file.choose(),header=T)
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
			N.id = which(x.score[i,]=="N")
			x.score[x.score%in%c("N")]<-c(-1)
			x.score[x.score%in%c("S","M","R","Y","W","K")]<-c(1)
			x.score<-apply(x.score,1,tr2)
			if (length(N.id)>0){
				x.score<-t(x.score)
				for (j in 1:length(N.id)){		
				x.score.result<-as.factor(x.score[i,-N.id[j]])
				x.score.train<-data.frame(x.score[-i,-N.id[j]])
				x.score.train<-apply(x.score.train,1,as.numeric)
				x.score.test<-as.numeric(x.score[-i,N.id[j]])
				model<-svm(x.score.train,x.score.result)
				answer<-predict(model,t(x.score.test))
				x.line<-x[i,]
				genotype=row.names(sort(table(x.line), decreasing = TRUE))
				genotype<-genotype[genotype%in%c("A","T","C","G")]
				heterozygous<-row.names(table(x.line[x.line%in%c("M","K","S","W","Y","R")]))
				if(answer==2)x[i,N.id[j]]=genotype[1]
				if(answer==0)x[i,N.id[j]]=genotype[2]
				if(answer==1){x[i,N.id[j]]=heterozygous}
				if(answer==-1){x[i,N.id[j]]=c("N")}
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



