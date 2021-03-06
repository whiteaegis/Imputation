	
	# GAPIT - Genomic Association and Prediction Integrated Tool
	# Designed by Zhiwu Zhang
	# Written by Zhiwu Zhang, Alex Lipka and Feng Tian
	# Last update: June 29, 2012
	#Step 0: Set directory to load GAPIT source files (this step is omitted for using package)
	#######################################################################################
	

	rm(list=ls())

	times<-Sys.time()
	library('MASS')
	library(multtest)
	library(gplots)
	library(compiler) #for cmpfun
	rp<-c("01","02","03","04","05","06","07","08","09",c(10:30))
	mr<-c("01","05","10","50","80")
	dd<-c("knn","wknn","ldknn","wldknn","smc","svm","pnn")
	#Import library (each time to start R)
	library(multtest)
	library("gplots")
	source("C:\\Users\\user1\\Documents\\code\\emma.txt")

	#Import GAPIT
	source("C:\\Users\\user1\\Documents\\code\\gapit_functions.txt")
	
	#oraginal data import
	#data<- read.delim("C:\\Users\\user1\\Documents\\imputation\\real data\\seasiarun3t2.hmp.txt", head = F)#choose seasiarun3t2.hmp.txt
	#u<-data[,12:ncol(data)]
	#a<-apply(u,1,function(x){sum(x=="N")})
	#use.data<-data[which(a==0),]

	
	
	root<-path.expand("~\\imputation\\pipline\\simulation_data\\RILsGBSHapmap.f75\\")
	setwd(root)
	result<-array(NA,100)
	count=1
	for(gg in 1:2){#method_f
	for(ff in 1:5){#missing_f
	for(rere in 1:10){
	setwd(root)
	#index.file<- read.delim(file.choose(),sep=",",head = T)#result_for_score_test.txt
	index.file<- read.delim("result_for_score_test.txt",sep=",",head = T)
	########Change for data
	f_chr<-rep(1:12,rep(150,12))
	f_missing<-rep(rep(1:5,rep(30,5)),12)
	f_rep<-rep(1:30,60)
	########
	
	repeat_f=rere
	#repeat_f	ex
	#c(1:30)

	missing_f=ff 	

	#missing_f	ex
	#c(1:5)
	#c(0.01,0.05,0.1,0.5,0.8)

	method_f=gg 
	#method_f	ex
	#c(2:7)
	#c("knn","wknn","ldknn","wldknn","smc","svm","pnn")
	file_name<-paste(dd[gg],paste(paste(mr[ff],paste("simulation",rp[rere],sep="_"),sep="_"),"gapit",sep="_"),sep="_")
	result.file<-paste(root,file_name,sep="")
	dir.create(result.file)
	setwd(result.file)


	myG<- read.delim("C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RIL_answer_for_gabit_05\\ril.new.data.txt",sep=" ",head = F)
	index_path<-index.file[(f_rep==repeat_f)&(f_missing==missing_f),c(23:29)]

	input_data<-read.delim(as.character(index_path[1,method_f]),sep=",",head = T)	
	for(i in 2:nrow(index_path)){input_data<-rbind(input_data,read.delim(as.character(index_path[i,method_f]),sep=",",head = T))}

	trt.data<-cbind(myG[-1,1:11],input_data)	
	colnames(trt.data)<-colnames(myG)
	trt.data<-rbind(myG[1,],trt.data)	

	#############################################################################################
	#Import GAPIT

	#Tutorial 1: Basic Scenario
	#----------------------------------------------------------------------------------------
	#Step 1: Set data directory and import files
	
	myY<-read.table("C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RIL_answer_for_gabit_05\\mydata_Ril_05.phen", head = TRUE)##### simulation phenotype

	#Step 2: Run GAPIT
	myGAPIT <- GAPIT(
	Y=myY,
	G=trt.data,
	PCA.total=0,
	)

	result[count]<-result.file
	count<-count+1
	setwd(root)
	write.table(result, file = "manhattan_file_result1.txt", sep = ",", row.names = FALSE,quote=F)
	}
	}
	}
	setwd(root)
	write.table(result, file = "manhattan_file_result1.txt", sep = ",", row.names = FALSE,quote=F)

	
	Sys.time()-times
	##### simulation phenotype Explanation
	#使用genotype是從seasiarun3t2.hmp.txt 中抽出的 no missing snp data
	#先讀入TASSEL 轉成PLINK的正常讀檔
	#在將軟換過的檔案讀入PLINK轉成二元讀檔
	#./plink --ped ril.new.data.plk.ped --map ril.new.data.plk.map --out mydata_Ril --make-bed
	#在將二元讀檔輸入至GATK設定生成phenotype
	#./gcta64  --bfile mydata_Ril  --simu-qt  --simu-causal-loci causal.snplist_Ril  --simu-hsq 0.5 --simu-rep 3 --out mydata_Ril
	#本試驗的phenotype讀檔就由此產生
	##################################################################################################################