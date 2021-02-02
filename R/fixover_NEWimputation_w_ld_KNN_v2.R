	rm(list=ls())
	xx=read.delim(file.choose(),header=T,sep=" ")
######修改search計算方式及平行運算	
###################################
# computation from here
###################################
	fix(xx)
	y = as.matrix(xx[,5:ncol(xx)])
	
	time=Sys.time()
	library(doParallel)
	cl <- makeCluster(3)
	registerDoParallel(cl)
	final=c()
	w=20 #window size
	k=13 #k for KNN algorithm
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
	ld=LD(x)
	if(sum(ld%in%NaN)>0)ld[ld%in%NaN]<-c(0)
	sr<-order(apply(x,1,is.N<-function(x){sum(x=="N")}))#search method
	for(i in sr){
	N.id = which(x[i,]=="N")
	w.target=(ld[-i,i]/sum(ld[-i,i]))###從 ld matrix中提出需要的data 
		if (length(N.id) > 0){
			x.new = x[i,-N.id]	###提出與目標位點同列的sample刪除有missing的
			for (j in 1:length(N.id)){
        			 
        			x.target=x[-i, N.id[j]]	###提出與目標位點同行的所有snp
        			x.train=x[-i,-N.id]	###刪掉目標點其他所有部份且刪除相對sample有missing的部份
				s=apply(x.train,2,function(x){score.ld.w.1(x.target,x,w.target)})        
				ss=sort(s,decreasing=TRUE,index.return=TRUE)
        			x.tab=table(as.character(x.new[ss$ix[1:k]]))
        			genotype=row.names(x.tab)
        			t<-as.matrix(x.tab)
        			sw<-as.matrix(combn(as.matrix(x.tab),2)[,combn(as.matrix(x.tab),2)[1,]==max(as.matrix(x.tab))])        
        			if(sum(sw[2,]<max(t))==dim(sw)[2]){
        				x[i, N.id[j]]=genotype[which.max(x.tab)]
				}else{x[i, N.id[j]]=c("N")}

     			}
		}
	}
	x
	}
	final = rbind(final,x.impute)
	

	Sys.time()-time
	stopCluster(cl)

	LD<-function(y.1){
	library(popgen)
		tr<-function(x){
		t<-x[x!=-1]
		genotype=row.names(table(t))
		if(length(unique(table(t)))==1){
		x[x!=genotype[2] & x!=genotype[1]& x!="-1"]<-"-1"
		x[x==genotype[1]]<-"1"
		x[x==genotype[2]]<-"0"
		}else{
		x[x!=genotype[which.min(table(t))]& x!=genotype[which.max(table(t))]& x!="-1"]<-"-1"
		x[x==genotype[which.max(table(t))]]<-"1"
		x[x==genotype[which.min(table(t))]]<-"0"
		}
		return(x)
		}
	y.1[y.1%in%c("K","M","R","S","W","Y","N")]<-c(-1)###將以上符號轉成-1
	y.1<-apply(y.1,1,function(x){tr(x)})###將轉為1,0
	A<-LDmat(y.1,typ="haplotype", plotmat = FALSE)###算ld
	A[upper.tri(A)]<-NA
	kk<-as.matrix(as.dist(as.matrix(A), upper = TRUE))
	return(kk)
	}


	score.ld.w.1 <- function(target,train,w.target,match.score=1, miss.score=0.5, mismatch.score=-5){
	id.match = which((target==train)&(target!="N"))
	id.miss = which((train=="N") | (target=="N"))
	score <- sum(w.target[id.match]*match.score) + sum(w.target[id.miss]*miss.score) + sum(w.target[-c(id.match,id.miss)]*mismatch.score)
	return(score)
	}
	
	od<-function(w){
	a<-matrix(data = NA, nrow = w, ncol = w)
	diag(a)<-0
	a<-apply(a,1,function(x){ff(x)})
	a[upper.tri(a)]<-NA
	a<-as.matrix(as.dist(as.matrix(a), upper = TRUE))
	return(a)
	}

	ff<-function(x){
	b<-which(x==0)
	d<-length(x)
	x<-c(c(1:d-b))
	return(x)
	}
	

