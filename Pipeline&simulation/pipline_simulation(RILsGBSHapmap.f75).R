	rm(list=ls())
	data<-read.delim(file.choose(),sep=" ",header=T)
	time<-Sys.time()
	root<-path.expand("~/imputation/pipline/simulation_data/RILsGBSHapmap.f75/")

	mr<-c("01","05","10","50","80")
	n<-c(0.01,0.05,0.1,0.5,0.8)
	rp<-c("01","02","03","04","05","06","07","08","09",c(10:30))


	count=1
	result<-matrix(NA,7,600)
	for(i in 1:12){
	chr_flag<-i
	chr<-which(data[,3]==chr_flag)
	use.data<-as.matrix(data[chr,])
	chr_names<-paste("chr",rp[i],sep="_")
	chr_pos<-paste(root,chr_names,sep="")
	dir.create(chr_pos)
		for(u in 1:5){
		sup_filename<-paste(mr[u],"simulation",sep="_")#
		sup_file_pos<-paste(chr_pos,sup_filename,sep="/")
		dir.create(sup_file_pos)
			for(j in 1:10){
			setwd(sup_file_pos)
			repeat{
			out_final1_data<-real.simulate(use.data,missing_rate=n[u],method=1)
			if(sum(apply(out_final1_data$zz,1,function(x){a<-sum(row.names(table(x))%in%c("A","T","C","G"))}))>=(nrow(out_final1_data$zz)*2))break
			}
			file_names<-paste(sup_filename,rp[j],sep="_")
			dir.create(file_names)
			setwd(paste(sup_file_pos,file_names,sep="/"))
			txt<-paste(file_names,"missing_data.txt",sep="_")
			wwtxt<-paste(file_names,"missing_data_ww.txt",sep="_")
			tttxt<-paste(file_names,"missing_data_tt.txt",sep="_")
			write.table(out_final1_data$zz, file = txt, sep = ",", row.names = FALSE,quote=F)
			write.table(out_final1_data$ww, file = wwtxt, sep = ",", row.names = FALSE,quote=F)
			write.table(out_final1_data$tt, file = tttxt, sep = ",", row.names = FALSE,quote=F)
			result[1,count]<-i
			result[2,count]<-n[u]
			result[3,count]<-j
			result[4,count]<-out_final1_data$missing_rate
			result[5,count]<-out_final1_data$missing_number
			result[6,count]<-paste(sup_file_pos,file_names,sep="/")
			result[7,count]<-paste(sup_file_pos,paste(file_names,txt,sep="/"),sep="/")
			count=count+1
			print(count)
			}
		}
	}

	Sys.time()-time
	setwd(root)
	
	write.table(result, file = "simlation_result.txt", sep = ",", row.names = FALSE,quote=F)
	getwd()

	dim(result)
	
	
	#my.recreate <- dget(file_names)
		