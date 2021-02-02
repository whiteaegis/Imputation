	
	rm(list=ls())
	library(graphics)
	myG<- read.delim("C:\\Users\\user1\\Documents\\imputation\\simulation\\gapit\\try_snp.txt",sep=" ",head = T)
	myG<- read.delim("C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RIL_answer_for_gabit_05\\ril.new.data.txt",sep=" ",head = T)

	layout(matrix(1:12, 3, 4, byrow = TRUE), respect = TRUE)

	root<-"C:\\Users\\user1\\Documents\\imputation\\real data"
	setwd(root)	
	myG<- read.delim("seasiarun3t2.hmp.txt" ,head = T)
	dim(myG)


	for(i in 1:12){
	flag<-which(myG[,3]==i)
	a<-as.numeric(myG[flag,4])/100000
	plot(density(a),main=paste("CHR",1:12,sep="_")[i])
	rug(a)
	}


	

	
	
