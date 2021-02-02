	rm(list=ls())
	
	root<-path.expand("~\\imputation\\pipline\\simulation_data\\seasiarun3t2(3rd compere)\\")
	setwd(root)
	index.file<- as.matrix(read.delim("manhattan_file_result.txt",sep=",",head = T))
	sub("seasiarun3t2","seasiarun3t2(3rd compere)",index.file[1,])

	times<-Sys.time()

######其他方法
	
	result<-matrix(NA,1750,2)
	count<-1
	for(i in 1:length(index.file)){
	#setwd(index.file[i])
	setwd(sub("seasiarun3t2","seasiarun3t2(3rd compere)",index.file[i]))
	p.data<-dir()[grep("GWAS.Results",dir())]
	for(j in 1:length(p.data)){
	#setwd(index.file[i])
	setwd(sub("seasiarun3t2","seasiarun3t2(3rd compere)",index.file[i]))
	data<-as.matrix(read.csv(p.data[j]))
	setwd(paste(root,"gapit_h 0.5_answer",sep=""))
	p.or.data<-dir()[grep("GWAS.Results",dir())]
	or.data<-as.matrix(read.csv(paste(paste(root,"gapit_h 0.5_answer",sep=""),p.or.data[p.data[j]==p.or.data],sep="\\")))
	use.or.data<-or.data[order(or.data[,1]),]
	use.data<-data[order(data[,1]),]
	result[count,1]<-paste(strsplit(index.file[i],split="\\",fixed = TRUE )[[1]][9],p.data[j],sep="_")
	result[count,2]<-sum(((-log10(as.numeric(use.or.data[,4])))-(-log10(as.numeric(use.data[,4]))))^2)
	setwd(root)
	write.table(result, file = "manhattan_PMSE_result.txt", sep = ",", row.names = FALSE,quote=F)
	count<-count+1
	}
	}
	setwd(root)
	colnames(result)<-c("Names","PMSE")
	write.table(result, file = "manhattan_PMSE_result.txt", sep = ",", row.names = FALSE,quote=F)

######beagle

	root2<-path.expand("~\\imputation\\pipline\\simulation_data\\seasiarun3t2(3rd compere)\\beagle\\seasiarun3t2(3rd compere)")
	setwd(root2)
	index.file2<- as.matrix(read.delim("manhattan_file_result.txt",sep=",",head = T))

	result2<-matrix(NA,250,2)
	count<-1
	for(i in 1:length(index.file2)){
	#setwd(index.file[i])
	setwd(sub("seasiarun3t2","seasiarun3t2(3rd compere)",index.file2[i]))
	p.data<-dir()[grep("GWAS.Results",dir())]
	for(j in 1:length(p.data)){
	#setwd(index.file[i])
	setwd(sub("seasiarun3t2","seasiarun3t2(3rd compere)",index.file2[i]))
	data<-as.matrix(read.csv(p.data[j]))
	setwd(paste(root,"gapit_h 0.5_answer",sep=""))
	p.or.data<-dir()[grep("GWAS.Results",dir())]
	or.data<-as.matrix(read.csv(paste(paste(root,"gapit_h 0.5_answer",sep=""),p.or.data[p.data[j]==p.or.data],sep="\\")))
	use.or.data<-or.data[order(or.data[,1]),]
	use.data<-data[order(data[,1]),]
	result2[count,1]<-paste(strsplit(index.file2[i],split="/",fixed = TRUE )[[1]][2],p.data[j],sep="_")
	result2[count,2]<-sum(((-log10(as.numeric(use.or.data[,4])))-(-log10(as.numeric(use.data[,4]))))^2)
	setwd(root)
	write.table(result2, file = "manhattan_PMSE_result2.txt", sep = ",", row.names = FALSE,quote=F)
	count<-count+1
	}
	}
	

######資料合並

	xx<-rbind(result,result2)
	write.table(xx, file = "manhattan_PMSE_result.txt", sep = ",", row.names = FALSE,quote=F)
	ph_f<-rep(1:5,400)
	rp_f<-rep(1:10,200)
	mis_f<-rep(rep(1:5,rep(50,5)),8)
	met_f<-rep(1:8,rep(250,8))
	
	
######計算mean和SE
	yy<-matrix(NA,10,8)
	
	for(j in 1:5){#missing
	for(i in 1:8){#method
	a<-as.numeric(xx[(mis_f==j)&(met_f==i),2])
	yy[(2*j-1),i]<-mean(a)
	yy[(j*2),i]<-(sd(a)/sqrt(length(a)))
	}
	}

	colnames(yy)<-c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN","Beagle4")

	yy
	
	setwd("C:\\Users\\user1\\Documents\\imputation\\書面整理")
	write.table(yy, file = "statistic_for_LPSE.txt", sep = ",", row.names = T,quote=F)

######畫折線圖

	zz<-matrix(NA,5,8)
	
	for(j in 1:5){#missing
	for(i in 1:8){#method
	a<-as.numeric(xx[(mis_f==j)&(met_f==i),2])
	zz[j,i]<-mean(a)
	}
	}

	zz
	plot(x=c(1,2,3,4,5), y=zz[,1], 
	xlab="Missing rate", ylab="LPSE",type="b",lty=1,
	pch=1,ylim=c(0,550),xlim=c(0.5,5.5),xaxt='n',lwd=2)
	axis(side=1,at=c(1,2,3,4,5),labels=c(0.01,0.05,0.1,0.5,0.8),lwd=1.5)
	for(i in 2:8)points(x=c(1,2,3,4,5), y=zz[,i],type="b",lty=i,pch=(i),col=(i),lwd=2)
	abline(v=c(1,2,3,4,5), col='grey', lwd=0.5)
	legend(0.5, 500,c("KNN","WKNN","LDKNN","WLDKNN","KNNcat","SVM","PNN","Beagle4"),pch=1:8,col=c(1,((c(2:8)))),lty=1:8)
	
	d.data<-data.frame(y=as.numeric(xx[,2]),mis_f=factor(mis_f),met_f=factor(met_f),ph_f=factor(ph_f))
	fit <- aov(y~mis_f*met_f*ph_f,data=d.data)
#	fit <- aov(as.numeric(xx[,2])~factor(mis_f)+factor(met_f)+factor(ph_f)) 
	summary(fit)
	library(agricolae)
	df<-df.residual(fit)
	MSerror<-deviance(fit)/df
	jj<-LSD.test(fit,"met_f",df,MSerror)
	
	ii<-pairw.anova(y=d.data$y,x=d.data$met_f,method="lsd",MSE=MSerror, df.err=df)
	names(ii)
	ii$summary
	

	
