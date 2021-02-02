	rm(list=ls())
	root<-"C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RILsGBSHapmap.f75(2ed w&k)"
	setwd(root)
	
	data1<- read.delim("RIL_result_for_score_test.txt",sep=",",head = T)
	
	title<-c("Missing rate 01%","Missing rate 05%","Missing rate 10%","Missing rate 50%","Missing rate 80%")
	
	missing_f<-rep(rep(1:5,rep(240,5)),12)
	chr_f<-rep(1:12,rep(1200,12))

	k_f<-factor(data1[,14])
	w_f<-factor(data1[,15])

	data<-data1[,6:9] #for time

######k

	#k<-c(1,3,5,7,11,13)#e

	result_time<-matrix(NA,6,5)
	for(i in 1:5){
	for(j in 1:6){
	a<-data[(missing_f==i)&(k_f==j),]
	result_time[j,i]<-mean(as.numeric(as.matrix(a)))
	}}

	rownames(result_time)<-c(1,3,5,7,11,13)
	colnames(result_time)<-c(0.01,0.05,0.1,0.5,0.8)

	plot(x=c(1,2,3,4,5), y=result_time[1,], 
	xlab="Missing rate", ylab="Times(s)",type="b",lty=1,main="compare different k in time",
	pch=1,ylim=c(0,61),xlim=c(0.5,5.5),xaxt='n',lwd=2)
	axis(side=1,at=c(1,2,3,4,5),labels=c(0.01,0.05,0.1,0.5,0.8),lwd=1.5)
	for(i in 2:6)points(x=c(1,2,3,4,5), y=result_time[i,],type="b",lty=i,pch=(i),col=(i),lwd=2)
	abline(v=c(1,2,3,4,5), col='grey', lwd=0.5)
	legend(0.5, 61,c("1","3","5","7","11","13"),pch=1:6,col=c(1,((c(2:6)))),lty=1:6)

	setwd("C:\\Users\\user1\\Documents\\imputation\\書面整理")
	write.table(result_time, file = "statistic_for_time(2 b k).txt", sep = ",", row.names = T,quote=F)	


#####w

	#w<-c(10,20,50,100)#d
	
	result_time<-matrix(NA,4,5)
	for(i in 1:5){
	for(j in 1:4){
	a<-data[(missing_f==i)&(w_f==j),]
	result_time[j,i]<-mean(as.numeric(as.matrix(a)))
	}}

	rownames(result_time)<-c(10,20,50,100)
	colnames(result_time)<-c(0.01,0.05,0.1,0.5,0.8)

	plot(x=c(1,2,3,4,5), y=result_time[1,], 
	xlab="Missing rate", ylab="Times(s)",type="b",lty=1,main="compare different w in time",
	pch=1,ylim=c(0,120),xlim=c(0.5,5.5),xaxt='n',lwd=2)
	axis(side=1,at=c(1,2,3,4,5),labels=c(0.01,0.05,0.1,0.5,0.8),lwd=1.5)
	for(i in 2:4)points(x=c(1,2,3,4,5), y=result_time[i,],type="b",lty=i,pch=(i),col=(i),lwd=2)
	abline(v=c(1,2,3,4,5), col='grey', lwd=0.5)
	legend(0.5, 100,c(10,20,50,100),pch=1:4,col=c(1,((c(2:4)))),lty=1:4)

	rownames(result_time)<-c(10,20,50,100)
	colnames(result_time)<-c(0.01,0.05,0.1,0.5,0.8)
	

	setwd("C:\\Users\\user1\\Documents\\imputation\\書面整理")
	write.table(result_time, file = "statistic_for_time(2 b w).txt", sep = ",", row.names = T,quote=F)