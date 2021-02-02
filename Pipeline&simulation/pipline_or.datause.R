	
	rm(list=ls())
	source("C:\\Users\\user1\\Documents\\imputation\\code\\knnwindowimpute_function(weight).R")
	source("C:\\Users\\user1\\Documents\\imputation\\code\\ldw_knnwindowimpute_function(search&fix&weight).R")
	source("C:\\Users\\user1\\Documents\\imputation\\code\\neural network_function.R")
	source("C:\\Users\\user1\\Documents\\imputation\\code\\smc_impute_function.R")
	source("C:\\Users\\user1\\Documents\\imputation\\code\\svm.windowimputation_function.R")
	times=Sys.time()
	rp<-c("01","02","03","04","05","06","07","08","09",c(10:30))
	k<-c(1,3,5,7,11,13)#e
	w<-c(10,20,50,100)#d
	match=5
	mismatch=-5
	d=1
	e=3

	data=read.delim(file.choose(),sep=" ",header=T)
	time=Sys.time()
	root<-path.expand("~/imputation/pipline/simulation_data/RILsGBSHapmap.f75/")
	setwd(root)
	for(i in 1:12){
	chr_flag<-i
	chr<-which(data[,3]==chr_flag)	
	use.data<-data[chr,]
	data_names<-paste("RILsGBSHapmap.f75",paste("chr",rp[i],sep=""),sep=".")
	write.table(use.data, file = data_names, sep = " ", row.names = FALSE,quote=FALSE)
	data_title<-use.data[,1:11]
	y<-as.matrix(use.data[,12:ncol(use.data)])
	final<-ldw_knnimpute(y,w=w[d],k=k[e],typ ="haplotype",match.score=match,mismatch.score=mismatch)
	out_data<-cbind(data_title,final)
	result.names<-paste(data_names,"wldknn_result",sep=".")
	write.table(out_data, file = result.names, sep = " ", row.names = FALSE,quote=FALSE)
	}
	Sys.time()-times
