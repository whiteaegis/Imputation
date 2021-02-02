	rm(list=ls())

	xx=read.delim(file.choose(),header=T)
	y = as.matrix(xx[1:200,12:ncol(xx)])
	R=10
	L=10
	k=13
	i=1
	start=Sys.time()
	final=y
	for(i in 1:nrow(y)){
		if(c("N")%in%y[i,]){
		if((i-R)>0&(i+L)<nrow(y)){x<-y[c((i-R):i,(i+1):(i+L)),]}
		if((i-R)<=0){x<-y[1:(1+R+L),]}
		if((i+L)>=nrow(y)){x<-y[(nrow(y)-(R+L)):nrow(y),]}
		for(L in 1:nrow(x)){
		N.id = which(x[L,]=="N")
		x.new = x[L, -N.id]
			if (length(N.id) > 0){
			for (j in 1:length(N.id)){
				x.target=x[-L, N.id[j]]
				x.train=x[-L, -N.id]
				s=apply(x.train,2,function(x){score.f(x.target,x)})
				ss=sort(s,decreasing=TRUE,index.return=TRUE)
				x.tab=table(as.character(x.new[ss$ix[1:k]]))
        			genotype=row.names(x.tab)
        			final[i, N.id[j]]=genotype[which.max(x.tab)]
			}
			}
		}
		}
	}
	

Sys.time()-start
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

	


	

	