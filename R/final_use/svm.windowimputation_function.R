
	svm.windowimputation<-function(y,w=20,...){
	time=Sys.time()

	tr2<-function(x){
	x.1<-x[(x!="-1")&(x!="1")]
	a<-table(x.1)
	genotype=names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=2
	x[x==genotype[2]]=0
	if(length(a)==1)x[x==genotype]<-0
	return(x)
	}
	
	library(doParallel)
	cl <- makeCluster(2)
	registerDoParallel(cl)
	final=c()
	w=w #window size
	final=c()
	n.step=floor(nrow(y)/w)
	w.lis<-list()
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
		library(e1071)
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
				x.score.result<-as.factor(x.score[i,-N.id])
				x.score.train<-data.frame(x.score[-i,-N.id])
				x.score.train<-apply(x.score.train,1,as.numeric)
				x.score.test<-as.numeric(x.score[-i,N.id[j]])
				model<-svm(x.score.train,x.score.result,scale=FALSE,kernel="radial")
				answer<-predict(model,t(x.score.test))
				if(sum(summary(answer))==0){
				x[i,N.id[j]]=c("N")
				}else{
				if(answer==2)x[i,N.id[j]]=genotype[1]
				if(answer==0)x[i,N.id[j]]=genotype[2]
				if(answer==1){x[i,N.id[j]]=heterozygous}
				if(answer==-1){x[i,N.id[j]]=c("N")}
				}
			}	
		}	
		}	
		x
	}
	final = rbind(final,x.impute)
	stopCluster(cl)
	Sys.time()-time
	print(Sys.time()-time)
	return(final)
	}