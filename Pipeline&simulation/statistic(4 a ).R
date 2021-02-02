
	rm(list=ls())
	root<-"C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\seasiarun3t2(4th sample distribution compard)"
	setwd(root)
	data1<- read.delim("result_for_score_test.txt",sep=",",head = T)
	data2<- read.delim("result_for_score_test(beagle).txt",sep=",",head = T)
	data2<-data2[1:360,]
	
	chr_factor<-rep(1:12,rep(30,12))
	data<-cbind(data1[,2:8],data2[,2]) #for accuracy
	colnames(data)<-c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN","Beagle4")
	boxplot(data,ylab="Accuracy",main="Sample missing distribution")
	result<-rbind(colMeans(data),apply(data,2,function(x){sd(x)/sqrt(length(x))}))
	setwd("C:\\Users\\user1\\Documents\\imputation\\書面整理")
	write.table(result, file = "statistic_for_score(a.4 accuracy).txt", sep = ",", row.names = T,quote=F)
	
	
######時間	
	
	data<-cbind(data1[,9:15])#for time	
	colnames(data)<-c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN")
	
	a<-colSums(data)

	result2<-a/a[1]

	setwd("C:\\Users\\user1\\Documents\\imputation\\書面整理")
	
	write.table(result2, file = "statistic_for_time(a.4).txt", sep = ",", row.names = T,quote=F)


	
	