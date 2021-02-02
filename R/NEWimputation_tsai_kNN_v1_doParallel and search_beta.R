	rm(list=ls())
	
	xx=read.delim(file.choose(),header=T,sep=" ")
	
	score.f <- function(target,train,match.score=1, miss.score=0.5, mismatch.score=-5){
		m=length(target)
		#x.tab=table(target,train)
		#id.n = (row.names(x.tab)=="N")
		n.allmiss = sum((train=="N") & (target=="N"))
		n.match = sum(target==train) - n.allmiss
		#n.match = sum(diag(x.tab)[!id.n])
		n.miss = sum(target=="N")+sum(train=="N")- n.allmiss
		n.mismatch = m - n.match -n.miss
		score = (match.score*n.match + miss.score*n.miss + mismatch.score*n.mismatch)/m
		return(score)
	}
		
	data = as.matrix(xx[1:240,])
	result<-matrix(NA,1,20)
	
	use<-real.simulate(data,1000)
	y<-use.data$zz
	ww<-use$ww
	tt<-use$tt	
	
	###################
	library(doParallel)
	cl <- makeCluster(3)
	registerDoParallel(cl)
			
	time=Sys.time()
	final1=c()
	w=20 #window size
	k=5 #k for KNN algorithm
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
	sr<-order(apply(x,1,is.N<-function(x){sum(x=="N")}))
		for(i in sr){
			N.id = which(x[i,]=="N")
			x.new = x[i, -N.id]
			if (length(N.id) > 0){
				for (j in 1:length(N.id)){
					x.target=x[-i, N.id[j]]
					x.train=x[-i, -N.id]
					s=apply(x.train,2,function(x){score.f(x.target,x)})
					ss=sort(s,decreasing=TRUE,index.return=TRUE)
					x.tab=table(as.character(x.new[ss$ix[1:k]]))
					genotype=row.names(x.tab)
					#ge<-genotype[which.max(x.tab)]
					#ge<-sml(ge)
					#x[i, N.id[j]]<-ge
					x[i, N.id[j]]<-genotype[which.max(x.tab)]
				}
			}
		}
	x
	}	
	final1 <- rbind(final1,x.impute)
	stopCluster(cl)
	Sys.time()-time
	fix(final1)	
	jj<-use.data[,1:11]
	result<-cbind(jj,final1)
	dim(result)
	names(result)<-names(use.data)
	write.table(result, file = "missing_data_finsh_sm.txt", sep = " ", row.names = FALSE,quote=F)

		