
	rm(list=ls())

source("C:\\Users\\user1\\Documents\\code\\svm.windowimputation_function.R")
source("C:\\Users\\user1\\Documents\\code\\smc_impute_function.R")
source("C:\\Users\\user1\\Documents\\code\\knnwindowimpute_function(weight).R")
source("C:\\Users\\user1\\Documents\\code\\neural network_function.R")
source("C:\\Users\\user1\\Documents\\code\\ldw_knnwindowimpute_function(search&fix&weight).R")
 
	
	root<-path.expand("~/imputation/pipline/simulation_data/seasiarun3t2/")
	s.file.info=read.delim(file.choose(),sep=",",header=T)
	
	result<-matrix(NA,1800,36)
	k<-c(1,3,5,7,11,13)#e
	w<-c(10,20,50,100)#d
	count=1
	times=Sys.time()
	
	for(i in 151:300){
		
		s.file_names<-as.character(s.file.info[6,i])
		missing_flag<-as.character(s.file.info[2,i])

		file_names<-strsplit(s.file_names,"/")[[1]][8]

		result.file<-paste(s.file_names,"result",sep=".")
		setwd(s.file_names)

		txt<-paste(file_names,"missing_data.txt",sep="_")

		wwtxt<-paste(file_names,"missing_data_ww.txt",sep="_")
		tttxt<-paste(file_names,"missing_data_tt.txt",sep="_")
		y<-as.matrix(read.delim(txt,sep=",",header=T))
		tt<-as.matrix(read.delim(tttxt,sep=",",header=T))
		ww<-as.matrix(read.delim(wwtxt,sep=",",header=T))
		dir.create(result.file)	
		setwd(result.file)
		
		
		if(missing_flag=="0.8"){match=5;mismatch=-1;d=4;e=5}
		if(missing_flag=="0.5"){match=2;mismatch=-3;d=4;e=4}
		if(missing_flag=="0.1"){match=1;mismatch=-3;d=3;e=3}
		if(missing_flag=="0.05"){match=1;mismatch=-3;d=3;e=2}
		if(missing_flag=="0.01"){match=1;mismatch=-3;d=4;e=2} 
		if(nrow(y)<w[d]){w<-c(10,20,50,nrow(y))}


		time1<-Sys.time()
			final1<-knnwindowimpute(y,w=w[d],k=k[e],weights=FALSE,match.score=match,mismatch.score=mismatch)
		time2<-Sys.time()
			final2<-knnwindowimpute(y,w=w[d],k=k[e],match.score=match,mismatch.score=mismatch)
		time3<-Sys.time()
			final3<-ldw_knnimpute(y,w=w[d],k=k[e],typ ="haplotype",weights=FALSE,match.score=match,mismatch.score=mismatch)
		time4<-Sys.time()
			final4<-ldw_knnimpute(y,w=w[d],k=k[e],typ ="haplotype",match.score=match,mismatch.score=mismatch)
		time5<-Sys.time()
			final5<-smc_impute(y,k=k[e])
		time6<-Sys.time()
			final6<-svm.windowimputation(y,w=w[d])
		time7<-Sys.time()
			final7<-pnn.windowimputation(y,w=w[d])
		time8<-Sys.time()




############make filenames

		na.knn<-paste(paste("knn",file_names,sep="_"),"txt",sep=".")
		na.wknn<-paste(paste("wknn",file_names,sep="_"),"txt",sep=".")
		na.ldknn<-paste(paste("ldknn",file_names,sep="_"),"txt",sep=".")
		na.wldknn<-paste(paste("wldknn",file_names,sep="_"),"txt",sep=".")
		na.smc<-paste(paste("smc",file_names,sep="_"),"txt",sep=".")
		na.svm<-paste(paste("svm",file_names,sep="_"),"txt",sep=".")
		na.pnn<-paste(paste("pnn",file_names,sep="_"),"txt",sep=".")

############save list
############name

		result[count,1]<-paste(strsplit(s.file_names,"/")[[1]][6],file_names,sep="_")

############accuracy

		result[count,2]<-sum(final1[tt]==ww)/sum(y=="N")
		result[count,3]<-sum(final2[tt]==ww)/sum(y=="N")
		result[count,4]<-sum(final3[tt]==ww)/sum(y=="N")
		result[count,5]<-sum(final4[tt]==ww)/sum(y=="N")
		result[count,6]<-sum(final5[tt]==ww)/sum(y=="N")
		result[count,7]<-sum(final6[tt]==ww)/sum(y=="N")
		result[count,8]<-sum(final7[tt]==ww)/sum(y=="N")



############time

		result[count,9]<-as.numeric(difftime(time2,time1,units = "secs"))
		result[count,10]<-as.numeric(difftime(time3,time2,units = "secs"))
		result[count,11]<-as.numeric(difftime(time4,time3,units = "secs"))
		result[count,12]<-as.numeric(difftime(time5,time4,units = "secs"))
		result[count,13]<-as.numeric(difftime(time6,time5,units = "secs"))
		result[count,14]<-as.numeric(difftime(time7,time6,units = "secs"))
		result[count,15]<-as.numeric(difftime(time8,time7,units = "secs"))


############success point

		result[count,16]<-sum(final1[tt]==ww)
		result[count,17]<-sum(final2[tt]==ww)
		result[count,18]<-sum(final3[tt]==ww)
		result[count,19]<-sum(final4[tt]==ww)
		result[count,20]<-sum(final5[tt]==ww)
		result[count,21]<-sum(final6[tt]==ww)
		result[count,22]<-sum(final7[tt]==ww)


############file root

		result[count,23]<-paste(result.file,na.knn,sep="/")
		result[count,24]<-paste(result.file,na.wknn,sep="/")
		result[count,25]<-paste(result.file,na.ldknn,sep="/")
		result[count,26]<-paste(result.file,na.wldknn,sep="/")
		result[count,27]<-paste(result.file,na.smc,sep="/")
		result[count,28]<-paste(result.file,na.svm,sep="/")
		result[count,29]<-paste(result.file,na.pnn,sep="/")

###########result missing point
		
		result[count,30]<-sum(final1=="N")
		result[count,31]<-sum(final2=="N")
		result[count,32]<-sum(final3=="N")
		result[count,33]<-sum(final4=="N")
		result[count,34]<-sum(final5=="N")
		result[count,35]<-sum(final6=="N")
		result[count,36]<-sum(final6=="N")


############treatment


		setwd(result.file)

		write.table(final1, file = na.knn, sep = ",", row.names = FALSE,quote=F)
		write.table(final2, file = na.wknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final3, file = na.ldknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final4, file = na.wldknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final5, file = na.smc, sep = ",", row.names = FALSE,quote=F)
		write.table(final6, file = na.svm, sep = ",", row.names = FALSE,quote=F)
		write.table(final7, file = na.pnn, sep = ",", row.names = FALSE,quote=F)
		count<-count+1
	print(count)
	setwd(root)
	write.table(result, file = "result_for_score_test_chr02.txt", sep = ",", row.names = FALSE,quote=F)
	
	}
	Sys.time()-times

	head(result)


	ff<-c("Names","knn_accuracy","wknn_accuracy","ldknn_accuracy","wldknn_accuracy","smc_accuracy","svm_accuracy","pnn_accuracy",
		"knn_time","wknn_time","ldknn_time","wldknn_time","smc_time","svm_time","pnn_time",
		"knn_count","wknn_count","ldknn_count","wldknn_count","smc_count","svm_count","pnn_count",
		"knn_root","wknn_root","ldknn_root","wldknn_root","smc_root","svm_root","pnn_root",
		"knn_rmi","wknn_rmi","ldknn_rmi","wldknn_rmi","smc_rmi","svm_rmi","pnn_rmi")

	length(ff)
	colnames(result)<-ff
	
