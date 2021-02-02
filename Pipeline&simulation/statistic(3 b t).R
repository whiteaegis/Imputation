
	rm(list=ls())
	root<-"C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RILsGBSHapmap.f75(3rd compere)"
	setwd(root)
	data1<- read.delim("result_for_score_test.txt",sep=",",head = T)
	data2<- read.delim("result_for_score_test(beagle).txt",sep=",",head = T)
	
	title<-c("Missing rate 01%","Missing rate 05%","Missing rate 10%","Missing rate 50%","Missing rate 80%")
	
	missing_factor<-rep(rep(1:5,rep(30,5)),12)
	chr_factor<-rep(1:12,rep(150,12))
	r_factor<-rep(1:30,60)
	
	data<-cbind(data1[,2:8],data2[,2]) #for accuracy
	
		


	result<-c()
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
	#colnames(u)<-c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN")
	result<-rbind(result,colMeans(u),apply(u,2,function(x){sd(x)/sqrt(length(x))}))
	print(boxplot(u,main=title[j], ylab="Accuracy"))
	}
	setwd("C:\\Users\\user1\\Documents\\")

	rownames(result)<-rep(c("means","standard error"),5)
	write.table(result, file = "statistic_for_score.txt", sep = ",", row.names = T,quote=F)
	
	
######時間	
	
	data<-cbind(data1[,9:15])#for time
	result_time<-c()
	for(j in 1:5){
	a<-data[missing_factor==j,]
	r_r<-as.factor(r_factor[missing_factor==j])
	#r_r<-as.factor(r_factor)
	h<-c()
	for(i in 1:7){
	b<-tapply(a[,i],r_r,sum)
	h<-rbind(h,b)
	}
	u<-t(h)
	colnames(u)<-c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN")
	result_time<-rbind(result_time,colMeans(u))
	}

	plot(x=c(1,2,3,4,5), y=result_time[,1], 
	xlab="Missing rate", ylab="Times(s)",type="b",lty=1,
	pch=1,ylim=c(0,13000),xlim=c(0.5,5.5),xaxt='n',lwd=2)
	axis(side=1,at=c(1,2,3,4,5),labels=c(0.01,0.05,0.1,0.5,0.8),lwd=1.5)
	for(i in 2:7)points(x=c(1,2,3,4,5), y=result_time[,i],type="b",lty=i,pch=(i),col=(i),lwd=2)
	abline(v=c(1,2,3,4,5), col='grey', lwd=0.5)
	legend(0.5, 12000,c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN"),pch=1:7,col=c(1,((c(2:7)))),lty=1:7)

	result2<-result_time/result_time[,1]
	setwd("C:\\Users\\user1\\Documents\\imputation\\書面整理")
	write.table(result2, file = "statistic_for_time.txt", sep = ",", row.names = T,quote=F)


	
	