	rm(list=ls())

	root<-"~\\imputation\\pipline\\simulation_data\\seasiarun3t2.hmp(final)"
	setwd(root)
	b_myY<-read.table("South East Asia phenotype 2013.txt", head = TRUE)
	myY<-b_myY[,c(1,3)]
	times<-Sys.time()
	library('MASS')
	library(multtest)
	library(gplots)
	library(compiler) #for cmpfun
	#Import library (each time to start R)
	library(multtest)
	library("gplots")
	source("C:\\Users\\user1\\Documents\\code\\emma.txt")
	#Import GAPIT
	source("C:\\Users\\user1\\Documents\\code\\gapit_functions.txt")

	dir()
	setwd("smc")

	a<-dir()

	myG<- read.delim("C:\\Users\\user1\\Documents\\imputation\\simulation\\gapit\\try_snp.txt",sep=" ",head = F)
	
	data<-read.delim(a[1],sep=" ",head = T)
	for(i in 2:12){data<-rbind(data,read.delim(a[i],sep=" ",head = T))}
	colnames(myG)<-colnames(data)

	trt.data<-rbind(myG[1,],as.matrix(data))
	
	
	
	##### simulation phenotype




	dir.create("gabit")
	setwd("gabit")
	#Step 2: Run GAPIT
	myGAPIT <- GAPIT(
	Y=myY,
	G=trt.data,
	PCA.total=3,
	)

