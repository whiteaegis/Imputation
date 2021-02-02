
	xx=read.delim(file.choose(),header=T)

	score.f <- function(target,train,match.score=1, miss.score=0.5, mismatch.score=-5){
		m=length(target)
		n.allmiss = sum((train=="N") & (target=="N"))
		n.match = sum(target==train) - n.allmiss
		n.miss = sum(target=="N")+sum(train=="N")- n.allmiss
		n.mismatch = m - n.match -n.miss
		score = (match.score*n.match + miss.score*n.miss + mismatch.score*n.mismatch)/m
		return(score)
	}
	
	
	data = as.matrix(xx[1:240,])
	result2<-matrix(NA,2,10)	
	for(dd in 1:10){

	use<-real.simulate(data,150000)
	y<-use$zz
	ww<-use$ww
	tt<-use$tt		

	time=Sys.time()
	final=c()
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

	for(L in 1:n.step){
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
					x[i, N.id[j]]=genotype[which.max(x.tab)]
				}
			}
		}
	final = rbind(final,x)
	}	

	Sys.time()-time

	final1<-knnwindowimpute(y)

	result2[1,dd]<-sum(final[tt]==ww)/sum(use$zz=="N")
	result2[2,dd]<-sum(final1[tt]==ww)/sum(use$zz=="N")


	}
	
	boxplot(t(result2))
	t.test(result2[1,],result2[2,], var.equal = TRUE, paired = T)
	
	install.packages("snow")