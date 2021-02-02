#######genotype imputation svm
	
	rm(list=ls())

	xx<-as.matrix(read.delim(file.choose(),header=T,sep=","))
	ww<-as.matrix(read.delim(file.choose(),header=T,sep=","))
	tt<-as.matrix(read.delim(file.choose(),header=T,sep=","))
	y<-xx
	y = as.matrix(xx[,12:ncol(xx)])
	
	time=Sys.time()
	library(doParallel)
	cl <- makeCluster(3)
	registerDoParallel(cl)
	final=c()
	w=20 #window size
	final=c()
	n.step=floor(nrow(y)/w)
	w.lis<-list()
	L=2
	for(L in 1:n.step){
		if (L <= (n.step-1)){
			w.lis[[L]]=as.matrix(y[(w*(L-1)+1):(w*L),])
		} else{
			w.lis[[L]]=as.matrix(y[(w*(L-1)+1):nrow(y),])
		} 
	}
		x.impute<-foreach(L=1:n.step,.combine=rbind) %dopar% {
		x<-as.matrix((data.frame(w.lis[L])))
		colnames(x)<-1:ncol(x)
		sr<-order(apply(x,1,is.N<-function(x){sum(x=="N")}))#search method
		library("pnn")
		for(i in sr){
		x.line<-x[i,]
		genotype<-row.names(sort(table(x.line), decreasing = TRUE))
		genotype<-genotype[genotype%in%c("A","T","C","G")]
		heterozygous<-row.names(table(x.line[x.line%in%c("M","K","S","W","Y","R")]))
		if(length(genotype)==1){
		x[i,][x[i,]%in%"N"]<-genotype}	
		x.score<-x
		N.id = which(x.score[i,]=="N")
		x.score[x.score%in%c("N")]<-c(-1)
		x.score[x.score%in%c("S","M","R","Y","W","K")]<-c(1)
		x.score<-apply(x.score,1,tr2)
		x.score<-t(x.score)
		if (length(N.id)>0){			
			for (j in 1:length(N.id)){		
				x.train.result<-as.matrix(x[i,-N.id])
				x.score.train<-data.frame(x.score[-i,-N.id])
				x.score.train<-apply(x.score.train,1,as.numeric)
				x.score.test<-as.numeric(x.score[-i,N.id[j]])
				t1<-data.frame(y=x.train.result,x.score.train)
				nw<-learn(t1)
				nw.s<-smooth(nw,sigma)
				answer<-guess(nw.s,x.score.test)$category
				x[i, N.id[j]]=answer
				}
			}	
		}	
			
		x
	}

	final = rbind(final,x.impute)
	stopCluster(cl)
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

	sum(final[tt]==ww)/sum(y=="N")

	dim(final)
	sum(final=="N")
	sum(y=="N")

