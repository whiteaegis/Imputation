	rm(list=ls())
	root<-"C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RILsGBSHapmap.f75(final)"
	times<-Sys.time()
	library('MASS')
	library(multtest)
	library(gplots)
	library(compiler) #for cmpfun
	rp<-c("01","02","03","04","05","06","07","08","09",c(10:30))
	mr<-c("01","05","10","50","80")
	dd<-c("knn","wknn","ldknn","wldknn","smc","svm","pnn")
	#Import library (each time to start R)
	library(multtest)
	library("gplots")
	source("C:\\Users\\user1\\Documents\\code\\emma.txt")

	#Import GAPIT
	source("C:\\Users\\user1\\Documents\\code\\gapit_functions.txt")
	setwd(root)
	dir()
	setwd("RILsGBSHapmap.f75_pnn")
	a<-dir()
	data<- read.delim(a[1],sep=" ",head = T)
	for(i in 2:length(a))data<-rbind(data,read.delim(a[i],sep=" ",head = T))
	myG<- read.delim("C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RIL_answer_for_gabit_05\\ril.new.data.txt",sep=" ",head = F)
	colnames(myG)<-colnames(data)

	trt.data<-rbind(myG[1,],as.matrix(data))
	
	trt.data<-trt.data[,1:98]

	b_myY<-read.table("C:\\Users\\user1\\Documents\\imputation\\¥~ªí§Î\\RIL stomata density tassel.txt", head = TRUE)##### simulation phenotype
	
	b2<-b_myY[,2]
	myY<-as.matrix(b_myY[-which(b2==max(b2)),])
	Gn<-as.matrix(myG[1,12:ncol(myG)])
	
	fl<-c()
	for(i in 1:length(myY[,1]))fl[i]<-strsplit(as.matrix(myY[,1]),"-")[[i]][1]
	for(i in 1:length(Gn))myY[which(fl%in%(strsplit(Gn[i],"[.]")[[1]][1])),1]<-as.matrix(Gn[i])
	
	#myY<-cbind(myY[,1],as.numeric(myY[,2]))
	
	myY<-data.frame(X.Trait.<-myY[,1],SD<-as.numeric(myY[,2]))
	colnames(myY)<-c("X.Trait.","SD")
	
	dir.create("gapit")
	setwd("gapit")

	myGAPIT <- GAPIT(
	Y=myY,
	G=trt.data,
	PCA.total=0,
	)

	setwd(root)