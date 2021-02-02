
	smc_impute<-function(y,k){
	change<-function(temp)
	{
	temp[temp%in%c("N")]<-c(0)
	temp[temp%in%c("S","M","R","Y","W","K")]<-c(2)
	temp<-apply(temp,1,tr2)
	temp<-apply(temp,2,as.numeric)
	}

	tr2<-function(x){
	x.1<-x[(x!="0")&(x!="2")]
	a<-table(x.1)
	genotype=names(sort(a, decreasing = TRUE))
	x[x==genotype[1]]=3
	x[x==genotype[2]]=1
	if(length(a)==1)x[x==genotype]<-0
	return(x)
	}
	
	start=Sys.time()

	for(i in nrow(y)){	
	x<-y
	x.line<-x[i,]
	genotype<-row.names(sort(table(x.line), decreasing = TRUE))
	genotype<-genotype[genotype%in%c("A","T","C","G")]
	heterozygous<-row.names(table(x.line[x.line%in%c("M","K","S","W","Y","R")]))
	if(length(genotype)==1){x[i,][x[i,]%in%"N"]<-genotype}
	}
	
	ff<-t(change(x))
	ff[ff%in%c(0)]<-c("")
	ff<-apply(ff,2,as.numeric)

	result<-knncatimpute(ff,nn=k,dist ="smc")

	nu.y<-t(change(y))
	dim(ff)
	for(i in 1:nrow(result)){
	nu.t<-nu.y[i,]
	t<-y[i,]
	if(length(row.names(table(t[nu.t==1])))==1)result[i,][result[i,]==1]<-row.names(table(t[nu.t==1]))
	if(length(row.names(table(t[nu.t==2])))==1)result[i,][result[i,]==2]<-row.names(table(t[nu.t==2]))
	if(length(row.names(table(t[nu.t==3])))==1)result[i,][result[i,]==3]<-row.names(table(t[nu.t==3]))
	}
	print(Sys.time()-start)
	return(result)
	}
	

	final1<-svm.windowimputation(y,w=100)
	final2<-svm.windowimputation_p(y,w=100)
	sum(result[tt]==ww)/sum(y=="N")
	sum(final[tt]==ww)/sum(y=="N")
	sum(final1[tt]==ww)/sum(y=="N")
	sum(final2[tt]==ww)/sum(y=="N")




