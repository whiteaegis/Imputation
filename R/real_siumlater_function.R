	rm(list=ls())
	#for hapmap formate
	data=read.delim(file.choose(),header=T)
	fix(data)
	
	data.trt<-data[data[,3]==1,]

	real.simulate<-function(x,y,st=12,missing_rate=0.5,method=c(1,2)){
	if(method==1){
	zz = as.matrix(x[,st:ncol(x)])
	number<-ceiling(length(zz)*missing_rate)
	tt<-sample(1:length(zz), size=number)
	ww<-zz[tt]
	zz[tt]<-"N"
	print("missing number in simulation data:");print(sum(zz=="N"))
	print("missing rate on simulation data:");print(sum(zz=="N")/length(zz))
	result<- new.env()
	result$zz<-zz
	result$ww<-ww
	result$tt<-tt
	result$missing_number<-sum(zz=="N")
	result$missing_rate<-sum(zz=="N")/length(zz)
	result<-as.list(result)
	}
	if(method==2){
	kk<-as.matrix(x[,st:ncol(x)])
	zz<-as.matrix(x[,st:ncol(x)])
	n.count.col<-apply(y,2,function(x){a<-sum(x=="N")})
	n.count<-round(as.array(n.count.col)*(nrow(zz)/nrow(y)))
	tt<-c()
	for(i in 1:ncol(zz)){
	n.count.i<-n.count[i]
	cc<-sample(1:nrow(x), size=n.count.i)
	cc<-unique(cc)
	zz[cc,i]<-"N"
	pp<-cbind(cc,rep(i,length(cc)))
	tt<-rbind(tt,pp)
	}
	ww<-kk[tt]
	print("missing number in simulation data:");print(sum(zz=="N"))
	print("missing rate on simulation data:");print(sum(zz=="N")/length(zz))
	result<- new.env()
	result$zz<-zz
	result$ww<-ww
	result$tt<-tt
	result$missing_number<-sum(zz=="N")
	result$missing_rate<-sum(zz=="N")/length(zz)
	result<-as.list(result)
	}
	return(result)
	}

########################################################################
	real.simulate2<-function(x,y,st=12,number=100000,method=1){
	######¶Ã¼Æ«õªÅ
	if(method==1){
	zz = as.matrix(x[,st:ncol(x)])
	row<-sample(1:nrow(zz), size=number,replace=T)
	col<-sample(1:ncol(zz), size=number,replace=T)
	tt<-cbind(row,col)
	tt<-unique(tt)
	ww<-zz[tt]
	zz[tt]<-c("N")
	print("missing number in simulation data:");print(sum(zz=="N"))
	print("missing rate on simulation data:");print(sum(zz=="N")/length(zz))
	result<- new.env()
	result$zz<-zz
	result$ww<-ww
	result$tt<-tt
	result$missing_number<-sum(zz=="N")
	result$missing_rate<-sum(zz=="N")/length(zz)
	result<-as.list(result)
	}

	if(method==2){
	gg<-function(y){
	f<-sum(y=="N")
	return(f)}
	zz = as.matrix(x[,st:ncol(data)])
	kk = as.matrix(x[,st:ncol(data)])
	n.count.col<-apply(y,2,function(x){gg(x)})
	n.count<-round(as.matrix(n.count.col)*(length(zz)/length(y)))

	tt<-c()

	for(i in 1:ncol(zz)){
	n.count.i<-n.count[i,]
	cc<-sample(1:nrow(x), size=n.count.i)
	cc<-unique(cc)
	zz[cc,i]<-"N"
	pp<-cbind(cc,rep(i,length(cc)))
	tt<-rbind(tt,pp)
	}

	ww<-kk[tt]
	print("missing number in simulation data:");print(sum(zz=="N"))
	print("missing rate on simulation data:");print(sum(zz=="N")/length(zz))
	result<- new.env()
	result$zz<-zz
	result$ww<-ww
	result$tt<-tt
	result$missing_number<-sum(zz=="N")
	result$missing_rate<-sum(zz=="N")/length(zz)
	
	result<-as.list(result)
	}
	return(result)
	}



######sum(final[tt]==ww)/sum(zz=="N")