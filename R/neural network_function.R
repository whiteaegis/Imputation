
	pnn.windowimputation<-function(y,w=20,sig=0.5,...){
	time=Sys.time()

	nomode_f<-function(x){
	tab<-table(x)
	out<-names(tab)[tab==max(tab)]
	if(length(out)>1)
		out<-sample(out,1)
	return(out)
	}


	tr2<-function(x){
	x.1<-x[(x!="-1")&(x!="1")]
	a<-table(x.1)
	genotype=names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=2
	x[x==genotype[2]]=0
	if(length(a)==1)x[x==genotype]<-0
	return(x)
	}

	time=Sys.time()
	library(doParallel)
	cl <- makeCluster(3)
	registerDoParallel(cl)
	final=c()
	w=w #window size
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
				nw.s<-smooth(nw,sig)
				answer<-guess(nw.s,x.score.test)$category
				if(c("TRUE")%in%is.na(guess(nw.s,x.score.test))){answer<-"NA"}
				x[i, N.id[j]]<-nomode_f(answer)
				}
			}	
		}		
		x
	}
	final = rbind(final,x.impute)
	stopCluster(cl)
	print(Sys.time()-time)
	return(final)
	}