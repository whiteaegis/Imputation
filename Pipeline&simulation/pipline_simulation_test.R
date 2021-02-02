	rm(list=ls())
	match.score=1, miss.score=0.5, mismatch.score=-5
	
	root<-path.expand("~/imputation/pipline/simulation_data/seasiarun3t2/")
	s.file.info=read.delim(file.choose(),sep=",",header=T)
	result<-matrix(NA,720,3)
	times=Sys.time()
	for(i in 1:720){
	s.file_names<-as.character(s.file.info[6,i])
	file_names<-strsplit(s.file_names,"/")[[1]][9]
	result.file<-paste(s.file_names,"result",sep=".")
	setwd(s.file_names)
	dir.create(result.file)
	txt<-paste(file_names,"missing_data.txt",sep="_")
	wwtxt<-paste(file_names,"missing_data_ww.txt",sep="_")
	tttxt<-paste(file_names,"missing_data_tt.txt",sep="_")
	y<-as.matrix(read.delim(txt,sep=",",header=T))
	tt<-as.matrix(read.delim(tttxt,sep=",",header=T))
	ww<-as.matrix(read.delim(wwtxt,sep=",",header=T))	
	setwd(result.file)
	time<-Sys.time()
	final<-knnwindowimpute(y)
	result[i,1]<-paste(strsplit(s.file_names,"/")[[1]][7],file_names,sep="_")
	result[i,2]<-sum(final[tt]==ww)/sum(y=="N")
	result[1,3]<-as.numeric(difftime(Sys.time(),time,units = "secs"))
	setwd(result.file)
	write.table(result, file = "test_result.txt", sep = ",", row.names = FALSE,quote=F)
	}
	setwd(root)
	Sys.time()-times

	test<-matrix(NA,720,1)
	for(i in 1:720){
	s.file_names<-as.character(s.file.info[6,i])
	test[i]<-strsplit(s.file_names,"/")[[1]][7]
	}

	

	times2-times1
	a<-times2-times1

	times-time
	b<-times-time

	print(as.numeric(b), digits=15) 

	as.numeric(difftime(Sys.time(),times2,units = "secs"))
	class(difftime(times,time,units = "secs"))

