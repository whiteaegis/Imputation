
	knnwindowimpute<-function(y,w=20,k=5,weights=TRUE,method="score",match.score=1, miss.score=0.5, mismatch.score=-5,...){

	score.f <- function(target,train,match.score=1, miss.score=0.5, mismatch.score=-5){
	m=length(target)
	n.allmiss = sum((train=="N") & (target=="N"))
	n.match = sum(target==train) - n.allmiss
	n.miss = sum(target=="N")+sum(train=="N")- n.allmiss
	n.mismatch = m - n.match -n.miss
	score = (match.score*n.match + miss.score*n.miss + mismatch.score*n.mismatch)/m
	return(score)
	}
	
		weight_f<-function(ss,x.tab){
	tab<-by(ss,x.tab,sum,na.rm=TRUE)
		out<-names(tab)[tab==max(tab)]
	if(length(out)>1)
		out<-sample(out,1)
	return(out)
	}


	nomode_f<-function(x){
	tab<-table(x)
	out<-names(tab)[tab==max(tab)]
	if(length(out)>1)
		out<-sample(out,1)
	return(out)
	}

	change<-function(temp){
	temp[temp%in%c("N")]<-c(1)
	temp[temp%in%c("S","M","R","Y","W","K")]<-c(3)
	temp<-apply(temp,1,tr2)
	temp<-apply(temp,2,as.numeric)
	}

	tr2<-function(x){
	t<-x[x!=1]
	t<-t[t!=3]
	a<-table(t)
	genotype=row.names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=4
	x[x==genotype[2]]=2
	return(x)
	}

	library(doParallel)
	cl <- makeCluster(2)
	registerDoParallel(cl)
	time=Sys.time()
	final=c()
	n.step=floor(nrow(y)/w)
	w.lis<-list()
	for(L in 1:n.step){
		if (L <= (n.step-1)){
			w.lis[[L]]=as.matrix(y[(w*(L-1)+1):(w*L),])
		}else{
			w.lis[[L]]=as.matrix(y[(w*(L-1)+1):nrow(y),])
		} 
	}

	
	x.impute<-foreach(L=1:n.step,.combine=rbind) %dopar% {
	x<-as.matrix((data.frame(w.lis[L])))
	colnames(x)<-1:ncol(x)
	sr<-order(apply(x,1,is.N<-function(x){sum(x=="N")}))
		if(method=="score"){
		for(i in sr){
			N.id = which(x[i,]=="N")
			x.new = x[i, -N.id]
			if (length(N.id) > 0){
				for (j in 1:length(N.id)){
					x.target=x[-i, N.id[j]]
					x.train=x[-i, -N.id]
					s=apply(x.train,2,function(x){score.f(x.target,x,match.score=match.score, miss.score=miss.score, mismatch.score=mismatch.score)})
					ss=sort(s,decreasing=TRUE)[1:k]
					genotype=as.character(x.new[names(ss)])
					if (weights) 
						x[i, N.id[j]] <- weight_f(ss,genotype)
                			else x[i, N.id[j]] <- nomode_f(genotype)
				}
			}
		}
		}
		if(method=="smc"){
		library(scrime)
		for(i in sr){
			N.id = which(x[i,]=="N")
			x.new = x[i, -N.id]
			if (length(N.id) > 0){
				for (j in 1:length(N.id)){
					temp=x[-i,]
					temp<-change(temp)
					s<-smc(t(temp))
					ss=sort(s[1,-c(1,N.id)],index.return=TRUE)
					x.tab=table(as.character(x.new[ss$ix[1:k]]))
					genotype=row.names(x.tab)
					x[i, N.id[j]]=genotype[which.max(x.tab)]
				}
			}
		}
		}
	x
	}	
	final <- rbind(final,x.impute)
	stopCluster(cl)
	print(Sys.time()-time)
	return(final)
	}
	