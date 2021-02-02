	rm(list=ls())
	xx<-read.delim(file.choose(),header=T)
	chr_count<-table(xx[,3])*353
	data1<-read.delim(file.choose(),header=T,sep=",")
	data2<-read.delim(file.choose(),header=T,sep=",")

	
######資料整理

	a1<-data1[,2]
	a2<-data1[,3]
	b1<-data2[,2]
	b2<-data2[,3]
	c1<-data1[,8]
	c2<-data1[,9]

	method<-rep(1:4,c(18000,18000,18000,18000))#1:knn,2:wknn,3:ldknn,4:wldknn
	missing<-rep(rep(1:6,rep(250,6)),48)
	chr<-rep(rep(1:12,rep(1500,12)),4)
	match<-rep(c1,4)
	mismatch<-rep(c2,4)
	data<-c(a1,a2,b1,b2)
	data<-cbind(data,method,missing,chr,match,mismatch)
	colnames(data)<-c("accuracy","method","missing","chr","match","mismatch")
	
######chrmosome 間的 z-test 分missing level

	z.prop = function(x1,x2,n1,n2){
	numerator = (x1/n1) - (x2/n2)
	p.common = (x1+x2) / (n1+n2)
	denominator = sqrt(p.common * (1-p.common) * (1/n1 + 1/n2))
	z.prop.ris = numerator / denominator
	return(z.prop.ris)
	}

	lv<-1
	count<-combn(12,2)
	ztest_result<-matrix(NA,12,12)
	for(i in 1:ncol(count)){
	chr1<-data[data[,4]==count[1,i],]	
	t1<-chr1[chr1[,3]==lv,]
	chr2<-data[(data[,4]==count[2,i]),]
	t2<-chr2[chr2[,3]==lv,]
	z.score<-z.prop(chr_count[1]*mean(t1[,1]),chr_count[2]*mean(t2[,1]),chr_count[1],chr_count[2])
	ztest_result[count[1,i],count[2,i]]<-round(pnorm(z.score),4)
	if(pnorm(z.score)>0.5)ztest_result[count[1,i],count[2,i]]<-round((1-pnorm(z.score)),4)
	}

	ztest_result[is.na(ztest_result)]<-c("-")	
	write.table(ztest_result, file = "ztest_result.txt", sep = ",", row.names = FALSE,quote=F)
	table(which(ztest_result>0.05, arr.ind = TRUE)[,1])
	table(which(ztest_result>0.05, arr.ind = TRUE)[,2])
	
	######heatmap 製作
	library(lattice)
	use.data<-data[data[,4]==c(1,3,5),]
	use.data<-use.data[use.data[,2]==c(4),]
	title<-c("Missing rate 01%","Missing rate 05%","Missing rate 10%","Missing rate 25%","Missing rate 50%","Missing rate 80%")
	for(k in 1:6){
	heatmap_result<-matrix(NA,5,5)
	for(i in 1:5){
	for(j in 1:5){
	u<-use.data[use.data[,3]==k,]
	heatmap_result[i,j]<-mean(u[,1][(u[,5]==i)&(u[,6]==j)])
	}}

	colnames(heatmap_result)<-c("-1","-2","-3","-4","-5")
	rownames(heatmap_result)<-c("1","2","3","4","5")
	#heatmap(heatmap_result,xlab="mismatch point",ylab="match point",main="Missing rate 01%",Rowv=NA,Colv=NA)
	print(levelplot(heatmap_result,xlab="match point",ylab="mismatch point",main=title[k],col.regions=heat.colors) )
	}

	dim(u)	
