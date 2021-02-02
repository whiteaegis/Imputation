

	xx<-read.delim(file.choose(),sep=",",header=T)
	data<-as.matrix(xx[,c(2:5,14,15)])
		
	missing<-rep(rep(1:5,rep(240,5)),12)

	library(lattice)
	title<-c("Missing rate 01%","Missing rate 05%","Missing rate 10%","Missing rate 50%","Missing rate 80%")
	for(k in 1:5){
	heatmap_result<-matrix(NA,6,4)
	for(i in 1:6){
	for(j in 1:4){
	u<-data[missing==k,]
	heatmap_result[i,j]<-mean(as.numeric(u[,1:4][(u[,5]==i)&u[,6]==j,]))
	}}
	rownames(heatmap_result)<-c("1","3","5","7","11","13")
	colnames(heatmap_result)<-c("10","20","50","100")

	#heatmap(heatmap_result,xlab="k",ylab="w",main="Missing rate 01%",Rowv=NA,Colv=NA)
	print(levelplot(heatmap_result,xlab="k",ylab="w",main=title[k],col.regions=heat.colors) )
	print(heatmap_result)
	}


	k<-c(1,3,5,7,11,13)#e
	w<-c(10,20,50,100)#d

	