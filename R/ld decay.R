	rm(list=ls())
	LD<-function(y.1){
	y.1[y.1%in%c("K","M","R","S","W","Y","N")]<-c(-1)
	y.1<-apply(y.1,1,function(x){tr(x)})
	A<-LDmat(y.1,typ="haplotype", plotmat = FALSE)
	A[upper.tri(A)]<-NA
	kk<-as.matrix(as.dist(as.matrix(A), upper = TRUE))
	return(kk)
	}
	?LDmat
	tr<-function(x){
	t<-x[x!=-1]
	genotype=row.names(table(t))
	if(length(unique(table(t)))==1){
	x[x!=genotype[2] & x!=genotype[1]& x!="-1"]<-"-1"
	x[x==genotype[1]]<-"1"
	x[x==genotype[2]]<-"0"}
	else{
	x[x!=genotype[which.min(table(t))]& x!=genotype[which.max(table(t))]& x!="-1"]<-"-1"
	x[x==genotype[which.max(table(t))]]<-"1"
	x[x==genotype[which.min(table(t))]]<-"0"
	}
	return(x)
	}

	remington <- function(x)
  	{
	A <- 10+x
	B <- (2+ x) * (11+ x)
	C <- (3+ x)*(12+12* x+ x*x)
	D<- 45*(2+ x)*(11+ x)
	y<- (A/B)*(1+(C/D))
	return(y)
  	}
	ls()
	#data=read.delim(file.choose(),header=T)
	#fix(data)
	ph.dist<-data[,4][which(data[,3]=="1")]
	library(scrime)
	dim(data)
	dim(ph.dist)
	
	y = as.matrix(data[,12:ncol(data)])
	y.ch1<-y[which(data[,3]==1),]
	library(popgen)
	#y.ch1.rec<-recodeSNPs(y.ch1)
	#A<-LDmat(y.ch1.rec,typ="genotype", plotmat = FALSE)

	dim(y.ch1)

	a<-LD(y.ch1)
	
	combination<-combn(nrow(y.ch1),2)
	combination<-cbind(combination,combination[c(2,1),])
	##
	choos=3000
	##
	cb<-combination[,which(combination[1,]==choos)]
	#cb<-combination
	b<-matrix(data = NA, nrow = ncol(cb), ncol = 2)
	time<-Sys.time()

	for(i in 1:ncol(cb))
	{
	flag<-cb[,i]
	dist<-abs(ph.dist[flag[2]]-ph.dist[flag[1]])
	ld<-a[flag[1],flag[2]]
	b[i,1]<-ld
	b[i,2]<-dist
	}

	x<-b[,2]
	y<-b[,1]
	modele <- nls(y~remington(x*r),trace=TRUE, start=list(r=0.00000001) ) 
	rchapeau<-coef(modele) 
	
	
	plot(x,y, xlim=c(1,1100000), cex=0.5, pch=20, col='gray', main= " ch01 all SNPs", xlab="distance in base pairs", ylab="r2") 
	curve(remington(x*rchapeau ), 0,1100000, add = TRUE, lwd=2, col = "black") 

	Sys.time()-time


	
###########################1
	LD.data=read.delim(file.choose(),header=T)
	site="SL2.40ch01"
	remington <- function(x)
  	{
	A <- 10+x
	B <- (2+ x) * (11+ x)
	C <- (3+ x)*(12+12* x+ x*x)
	D<- 45*(2+ x)*(11+ x)
	y<- (A/B)*(1+(C/D))
	return(y)
  	}
	ph.dist<-LD.data[,13][which((LD.data[,1]==site)|(LD.data[,7]==site))]
	LD.dist<-LD.data[,14][which((LD.data[,1]==site)|(LD.data[,7]==site))]
	#ph.dist<-LD.data[,13]
	#LD.dist<-LD.data[,14]
	x<-ph.dist[-which(is.na(LD.dist))]
	y<-LD.dist[-which(is.na(LD.dist))]
	x<-as.numeric(x)
	y<-as.numeric(y)
	modele <- nls(y~remington(x*r),trace=TRUE, start=list(r=0.00000001) ) 
	rchapeau<-coef(modele) 
	plot(x,y, xlim=c(1,700000), cex=0.5, pch=20, col='gray', main= " ch01 all SNPs", xlab="distance in base pairs", ylab="r2") 
	curve(remington(x*rchapeau ), 0,700000, add = TRUE, lwd=2, col = "black")
################################
