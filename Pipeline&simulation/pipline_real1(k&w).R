
	rm(list=ls())
	
	root<-path.expand("~/imputation/pipline/simulation_data/seasiarun3t2/")
	s.file.info=read.delim(file.choose(),sep=",",header=T)
	
	result<-matrix(NA,4800,19)
	k<-c(1,3,5,7,11,13)#e
	w<-c(10,20,50,100)#d
	count=1
	times=Sys.time()

	
	for(i in 1:200){
		s.file_names<-as.character(s.file.info[6,i])
		missing_flag<-as.character(s.file.info[2,i])
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
		for(e in 1:6){
		for(d in 1:4){

		if(missing_flag=="0.8"){match=5;mismatch=-1}
		if(missing_flag=="0.5"){match=2;mismatch=-3}
		if(missing_flag%in%c("0.01","0.05","0.1")){match=1;mismatch=-3} 

		time1<-Sys.time()
			final1<-knnwindowimpute(y,w=w[d],k=k[e],weights=FALSE,match.score=match,mismatch.score=mismatch)
		time2<-Sys.time()
			final2<-knnwindowimpute(y,w=w[d],k=k[e],match.score=match,mismatch.score=mismatch)
		time3<-Sys.time()
			final3<-ldw_knnimpute(y,w=w[d],k=k[e],typ ="haplotype",weights=FALSE,match.score=match,mismatch.score=mismatch)
		time4<-Sys.time()
			final4<-ldw_knnimpute(y,w=w[d],k=k[e],typ ="haplotype",match.score=match,mismatch.score=mismatch)
		time5<-Sys.time()

############make filenames
		na.knn<-paste(e,paste(d,paste(paste("knn",file_names,sep="_"),"txt",sep=".")))
		na.wknn<-paste(e,paste(d,paste(paste("wknn",file_names,sep="_"),"txt",sep=".")))
		na.ldknn<-paste(e,paste(d,paste(paste("ldknn",file_names,sep="_"),"txt",sep=".")))
		na.wldknn<-paste(e,paste(d,paste(paste("wldknn",file_names,sep="_"),"txt",sep=".")))

############save list
############name

		result[count,1]<-paste(strsplit(s.file_names,"/")[[1]][7],file_names,sep="_")

############accuracy

		result[count,2]<-sum(final1[tt]==ww)/sum(y=="N")
		result[count,3]<-sum(final2[tt]==ww)/sum(y=="N")
		result[count,4]<-sum(final3[tt]==ww)/sum(y=="N")
		result[count,5]<-sum(final4[tt]==ww)/sum(y=="N")

############time

		result[count,6]<-as.numeric(difftime(time2,time1,units = "secs"))
		result[count,7]<-as.numeric(difftime(time3,time2,units = "secs"))
		result[count,8]<-as.numeric(difftime(time4,time3,units = "secs"))
		result[count,9]<-as.numeric(difftime(time5,time4,units = "secs"))

############success point

		result[count,10]<-sum(final1[tt]==ww)
		result[count,11]<-sum(final2[tt]==ww)
		result[count,12]<-sum(final3[tt]==ww)
		result[count,13]<-sum(final4[tt]==ww)

############file root

		result[count,14]<-paste(result.file,na.knn,sep="/")
		result[count,15]<-paste(result.file,na.wknn,sep="/")
		result[count,16]<-paste(result.file,na.ldknn,sep="/")
		result[count,17]<-paste(result.file,na.wldknn,sep="/")

############treatment

		result[count,18]<-e
		result[count,19]<-d
		setwd(result.file)

		write.table(final1, file = na.knn, sep = ",", row.names = FALSE,quote=F)
		write.table(final2, file = na.wknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final3, file = na.ldknn, sep = ",", row.names = FALSE,quote=F)
		write.table(final4, file = na.wldknn, sep = ",", row.names = FALSE,quote=F)

		count<-count+1
		}
		}
	setwd(root)
	write.table(result, file = "result_for_score_test.txt", sep = ",", row.names = FALSE,quote=F)
	}
	Sys.time()-times

	head(result)
	getwd()
	ff<-c("Names","knn_accuracy","wknn_accuracy","ldknn_accuracy","wldknn_accuracy",
		"knn_time","wknn_time","ldknn_time","wldknn_time",
		"knn_count","wknn_count","ldknn_count","wldknn_count",
		"knn_root","wknn_root","ldknn_root","wldknn_root",
		"k","w")
	
	
	colnames(result)<-ff
	rownames(result)<-c()
	setwd(path.expand("~"))
	fix(result)
	getwd()
	write.table(result1, file = "result_f.txt", sep = ",", row.names = FALSE,quote=F)
	write.table(final_result, file = "result_for_score_test.txt", sep = ",", row.names = FALSE,quote=F)

	sum(result1[701:720,1]==result[1:20,1])

	final_result<-rbind(result1,result[21:nrow(result),])
	dim(final_result)
	#result1 前720個結果 用時9hr i=29 e=5 d=1 "chr_01_10_simulation_09" 
	sum(final1[tt]==ww)/sum(y=="N")
	sum(final[tt]==ww)/sum(y=="N")
