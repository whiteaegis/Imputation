
	rm(list=ls())

	source("C:\\Users\\user1\\Documents\\code\\svm.windowimputation_function.R")
	source("C:\\Users\\user1\\Documents\\code\\smc_impute_function.R")
	source("C:\\Users\\user1\\Documents\\code\\knnwindowimpute_function(weight).R")
	source("C:\\Users\\user1\\Documents\\code\\neural network_function.R")
	source("C:\\Users\\user1\\Documents\\code\\ldw_knnwindowimpute_function(search&fix&weight).R")
 	
	root<-path.expand("~/imputation/pipline/simulation_data/RILsGBSHapmap.f75/")
	s.file.info<-read.delim(file.choose(),sep=",",header=T)
	
	result<-matrix(NA,1250,15)
	match<-c(1,2,3,4,5)
	mismatch<-c(-1,-2,-3,-4,-5)
	count=1
	times=Sys.time()
	

	for(i in 401:450){
		s.file_names<-as.character(s.file.info[6,i])
		file_names<-strsplit(s.file_names,"/")[[1]][8]
		result.file<-paste(s.file_names,"result1",sep=".")
		setwd(s.file_names)
		txt<-paste(file_names,"missing_data.txt",sep="_")
		wwtxt<-paste(file_names,"missing_data_ww.txt",sep="_")
		tttxt<-paste(file_names,"missing_data_tt.txt",sep="_")
		y<-as.matrix(read.delim(txt,sep=",",header=T))
		tt<-as.matrix(read.delim(tttxt,sep=",",header=T))
		ww<-as.matrix(read.delim(wwtxt,sep=",",header=T))
		dir.create(result.file)	
		setwd(result.file)
		for(e in 1:5){
		for(d in 1:5){
		time1<-Sys.time()
		final1<-knnwindowimpute(y,weights=FALSE,match.score=match[e],mismatch.score=mismatch[d])
		time2<-Sys.time()
		final2<-knnwindowimpute(y,match.score=match[e],mismatch.score=mismatch[d])
		time3<-Sys.time()
		final3<-ldw_knnimpute(y,typ ="haplotype",weights=FALSE,match.score=match[e],mismatch.score=mismatch[d])
		time4<-Sys.time()
		final4<-ldw_knnimpute(y,typ ="haplotype",match.score=match[e],mismatch.score=mismatch[d])
		time5<-Sys.time()
		na.knn<-paste(e,paste(d,paste(paste("knn",file_names,sep="_"),"txt",sep=".")))
		na.wknn<-paste(e,paste(d,paste(paste("wknn",file_names,sep="_"),"txt",sep=".")))
		na.ldknn<-paste(e,paste(d,paste(paste("ldknn",file_names,sep="_"),"txt",sep=".")))
		na.wldknn<-paste(e,paste(d,paste(paste("wldknn",file_names,sep="_"),"txt",sep=".")))
		result[count,1]<-paste(strsplit(s.file_names,"/")[[1]][6],file_names,sep="_")
		result[count,2]<-sum(final1[tt]==ww)/sum(y=="N")
		result[count,3]<-sum(final2[tt]==ww)/sum(y=="N")
		result[count,4]<-sum(final3[tt]==ww)/sum(y=="N")
		result[count,5]<-sum(final4[tt]==ww)/sum(y=="N")
		result[count,6]<-as.numeric(difftime(time2,time1,units = "secs"))
		result[count,7]<-as.numeric(difftime(time3,time2,units = "secs"))
		result[count,8]<-as.numeric(difftime(time4,time3,units = "secs"))
		result[count,9]<-as.numeric(difftime(time5,time4,units = "secs"))
		result[count,10]<-paste(result.file,na.knn,sep="/")
		result[count,11]<-paste(result.file,na.wknn,sep="/")
		result[count,12]<-paste(result.file,na.ldknn,sep="/")
		result[count,13]<-paste(result.file,na.wldknn,sep="/")
		result[count,14]<-e
		result[count,15]<-d
		setwd(result.file)
		write.table(final1, file = na.knn, sep = ",", row.names = FALSE,quote=F)
		write.table(final2, file = na.wknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final3, file = na.ldknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final4, file = na.wldknn, sep = ",", row.names = FALSE,quote=F)
		
		count<-count+1
		}
		}
	setwd(root)
	write.table(result, file = "RIL_result_for_score_test_chr09.txt", sep = ",", row.names = FALSE,quote=F)
	}
	Sys.time()-times

	head(result)
	ff<-c("Names","knn_accuracy","wknn_accuracy","ldknn_accuracy","wldknn_accuracy","knn_time","wknn_time","ldknn_time","wldknn_time","knn_root","wknn_root","ldknn_root","ldwknn_root","match","mismatch")
	
	colnames(result)<-ff

	write.table(result, file = "RIL_result_for_score_test_chr09.txt", sep = ",", row.names = FALSE,quote=F)

