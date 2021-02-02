	rm(list=ls())
	data<-read.delim(file.choose(),header=T)
	or.data<-read.delim(file.choose(),header=T)

	time=Sys.time()
	root<-path.expand("~/imputation/pipline/simulation_data/seasiarun3t2/")

	mr<-c("01","05","10","50","80")
	n<-c(0.01,0.05,0.1,0.5,0.8)
	rp<-c("01","02","03","04","05","06","07","08","09",c(10:30))


	count=1
	result<-matrix(NA,7,360)
	for(i in 1:12){
	chr_flag<-i
	chr<-which(data[,3]==chr_flag)
	or_chr<-which(or.data[,3]==chr_flag)
	use.data<-as.matrix(data[chr,])
	orl.use.data<-as.matrix(or.data[or_chr,12:ncol(or.data)])
	chr_names<-paste("chr",rp[i],sep="_")
	chr_pos<-paste(root,chr_names,sep="")
	dir.create(chr_pos)
			for(j in 1:30){
			repeat{
			out_final1_data<-real.simulate(use.data,orl.use.data,method=2)
			if(sum(apply(out_final1_data$zz,1,function(x){a<-sum(row.names(table(x))%in%c("A","T","C","G"))}))>=(nrow(out_final1_data$zz)*2))break
			}
			file_names<-paste(chr_names,rp[j],sep="_")
			setwd(chr_pos)
			dir.create(file_names)
			setwd(paste(chr_pos,file_names,sep="/"))
			txt<-paste(file_names,"missing_data.txt",sep="_")
			wwtxt<-paste(file_names,"missing_data_ww.txt",sep="_")
			tttxt<-paste(file_names,"missing_data_tt.txt",sep="_")
			write.table(out_final1_data$zz, file = txt, sep = ",", row.names = FALSE,quote=F)
			write.table(out_final1_data$ww, file = wwtxt, sep = ",", row.names = FALSE,quote=F)
			write.table(out_final1_data$tt, file = tttxt, sep = ",", row.names = FALSE,quote=F)
			result[1,count]<-i
			result[2,count]<-j
			result[3,count]<-sum(orl.use.data=="N")/length(orl.use.data)
			result[4,count]<-out_final1_data$missing_rate
			result[5,count]<-out_final1_data$missing_number
			result[6,count]<-paste(chr_pos,file_names,sep="/")
			result[7,count]<-paste(chr_pos,paste(file_names,txt,sep="/"),sep="/")
			count=count+1
			print(count)
			}
		
	}
	
	
	Sys.time()-time
	setwd(root)
	
	write.table(result, file = "simlation_result.txt", sep = ",", row.names = FALSE,quote=F)
	
	lo<-t(result)
	fix(lo)
	
	dim(result)
	
	
	#my.recreate <- dget(file_names)
		