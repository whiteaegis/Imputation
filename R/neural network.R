#######genotype imputation svm
	library("pnn")
	rm(list=ls())
	xx=read.delim(file.choose(),header=T)
	y = as.matrix(xx[,12:ncol(xx)])
	y = as.matrix(xx[1:50,12:ncol(xx)])
	library("pnn")
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
				x.train.result<-as.matrix(x[i,-N.id])
				x.score.train<-data.frame(x.score[-i,-N.id])
				x.score.train<-apply(x.score.train,1,as.numeric)
				x.score.test<-as.numeric(x.score[-i,N.id[j]])
				t1<-data.frame(y=x.train.result,x.score.train)
				nw<-learn(t1)
				nw.s<-smooth(nw,sigma=0.8)
				answer<-guess(nw.s,x.score.test)$category
				####
				x[i, N.id[j]]=answer
				}	
			}	
			}	
		final = rbind(final,x)
	}

Sys.time()-time

	tr2<-function(x){
	x.1<-x[(x!="-1")&(x!="1")]
	a<-table(x.1)
	genotype=names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=2
	x[x==genotype[2]]=0
	if(length(a)==1)x[x==genotype]<-0
	return(x)
	}




