
	rm(list=ls())
	match.score=1, miss.score=0.5, mismatch.score=-5
	
	root<-path.expand("~/imputation/pipline/simulation_data/seasiarun3t2/")
	s.file.info=read.delim(file.choose(),sep=",",header=T)
	
	result<-matrix(NA,18000,9)
	match<-c(1,2,3,4,5)
	mismatch<-c(-1,-2,-3,-4,-5)
	count=1
	times=Sys.time()
	

	for(i in 1:720){
		s.file_names<-as.character(s.file.info[6,i])
		file_names<-strsplit(s.file_names,"/")[[1]][9]
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
		na.knn<-paste(e,paste(d,paste(paste("knn",file_names,sep="_"),"txt",sep=".")))
		na.ldknn<-paste(e,paste(d,paste(paste("wknn",file_names,sep="_"),"txt",sep=".")))
		result[count,1]<-paste(strsplit(s.file_names,"/")[[1]][7],file_names,sep="_")
		result[count,2]<-sum(final1[tt]==ww)/sum(y=="N")
		result[count,3]<-sum(final2[tt]==ww)/sum(y=="N")
		result[count,4]<-as.numeric(difftime(time2,time1,units = "secs"))
		result[count,5]<-as.numeric(difftime(time3,time2,units = "secs"))
		result[count,6]<-paste(result.file,na.knn,sep="/")
		result[count,7]<-paste(result.file,na.ldknn,sep="/")
		result[count,8]<-e
		result[count,9]<-d
		setwd(result.file)
		write.table(final1, file = na.knn, sep = ",", row.names = FALSE,quote=F)
		write.table(final2, file = na.ldknn, sep = ",", row.names = FALSE,quote=F)
		count<-count+1
		}
		}
	setwd(root)
	write.table(result, file = "result_for_score_test.txt", sep = ",", row.names = FALSE,quote=F)
	}
	Sys.time()-times

	head(result)
	colnames(result)<-c("Names","knn_accuracy","wknn_accuracy","knn_time","wknn_time","knn_root","wknn_root","match","mismatch")
	
	rownames(result)<-c()

	fix(result)

	write.table(result1, file = "result_f.txt", sep = ",", row.names = FALSE,quote=F)
	write.table(final_result, file = "result_for_score_test.txt", sep = ",", row.names = FALSE,quote=F)

	sum(result1[701:720,1]==result[1:20,1])

	final_result<-rbind(result1,result[21:nrow(result),])
	dim(final_result)
	#result1 前720個結果 用時9hr i=29 e=5 d=1 "chr_01_10_simulation_09" 
	sum(final1[tt]==ww)/sum(y=="N")
	sum(final[tt]==ww)/sum(y=="N")
