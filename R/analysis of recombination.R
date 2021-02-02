	rm(list=ls())


	data=read.delim(file.choose(),header=T)
	data1<-as.matrix(data[,12:ncol(data)])
	op<-which(data1=="N", arr.ind = TRUE)	
	u.op<-unique(op[,1])		
	data2<-data1[-u.op,]
	temp<-data2
	tr3<-function(x)###改成抓親本
	{
	a<-x[159]
	b<-x[160]
	x[x==a]=0
	x[x==b]=2
	return(x)
	}
	temp[temp%in%c("N")]<-c(-1)
	temp[temp%in%c("S","M","R","Y","W","K")]<-c(1)
	temp<-apply(temp,1,tr3)
	temp<-apply(temp,2,as.numeric)
	
	temp<-t(temp)
	temp<-temp[,-c(159,160)]
	dim(temp)
	result<-matrix(NA,31624,2)	
	count1<-1
	count2<-2
	for(i in 1:31624){
	a<-temp[count1,]
	b<-temp[count2,]
	count1<-count1+1
	count2<-count2+1
	m1<-as.matrix(table(a,b))
	m2<-as.matrix(table(a,b))
	diag(m2)<-0
	result[i,1]<-i
	result[i,2]<-sum(m2)/sum(m1)
	}
	watch<-result[1:300,]
	plot(watch,type="h")
	fix(result)
