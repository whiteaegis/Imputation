
	rm(list=ls())
	root<-"C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\seasiarun3t2(3rd compere)"
	setwd(root)
	data1<- read.delim("result_for_score_test.txt",sep=",",head = T)
	data2<- read.delim("result_for_score_test(beagle).txt",sep=",",head = T)
	title<-c("Missing rate 01%","Missing rate 05%","Missing rate 10%","Missing rate 50%","Missing rate 80%")


	missing_factor<-rep(rep(1:5,rep(30,5)),12)
	chr_factor<-rep(1:12,rep(150,12))
	r_factor<-rep(1:30,60)

	data<-cbind(data1[,2:8],data2[,2])
	
	for(j in 1:5){
	
	a<-data[missing_factor==j,]
	r_r<-as.factor(r_factor[missing_factor==j])

	h<-c()

	for(i in 1:8){
	b<-tapply(a[,i],r_r,mean)
	h<-rbind(h,b)
	}

	
	u<-t(h)
	colnames(u)<-c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN","Beagle4")
	print(boxplot(u,main=title[j], ylab="Accuracy"))
	}
