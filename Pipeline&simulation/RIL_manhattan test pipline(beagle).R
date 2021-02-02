	
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

	
	root<-path.expand("C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RILsGBSHapmap.f75\\beagle\\RILsGBSHapmap.f75(compard)")
	setwd(root)

	########################################################################
	#生產Beagle result index
	#
	dir.result<-matrix(NA,1800,1)
	

	s.file.info<- read.delim("simlation_result.txt",sep=",",head = T)
	for(i in 1:ncol(s.file.info)){
	s.file_names<-as.character(s.file.info[6,i])
    	p.s.file_names<-strsplit(s.file_names,"/")[[1]][6:8]
	file_names<-root
 	for(l in 1:length(p.s.file_names)){file_names<-paste(file_names,p.s.file_names[l],sep="/")}
	dir.result[i,]<-f.sult.file<-paste(paste(file_names,"result",sep="."),"Beagle_finish.hmp.txt",sep="/")
	}
	f_chr<-rep(1:12,rep(150,12))
	f_missing<-rep(rep(1:5,rep(30,5)),12)
	f_rep<-rep(1:30,60)
	dir.result<-cbind(dir.result,f_chr,f_missing,f_rep)
	write.table(dir.result, file = "beagle_dir_result.txt", sep = ",", row.names = FALSE,quote=F)

	########################################################################


	result<-array(NA,50)
	count=1
	#for(gg in 1:7){#method_f
	for(ff in 1:1){#missing_f
	for(rere in 1:10){
	setwd(root)
	#index.file<- read.delim(file.choose(),sep=",",head = T)#result_for_score_test.txt
	#index.file<- read.delim("result_for_score_test.txt",sep=",",head = T)
	#index.file<- read.delim("beagle_dir_result.txt",sep=",",head = T)
	index.file<- dir.result
	
	
	########Change for data
	#f_chr<-rep(1:12,rep(150,12))
	#f_missing<-rep(rep(1:5,rep(30,5)),12)
	#f_rep<-rep(1:30,60)
	########
	
	repeat_f=rere
	#repeat_f	ex
	#c(1:30)

	missing_f=ff 	

	#missing_f	ex
	#c(1:5)
	#c(0.01,0.05,0.1,0.5,0.8)

	#method_f=gg 
	#method_f	ex
	#c(2:7)
	#c("knn","wknn","ldknn","wldknn","smc","svm","pnn")
	file_name<-paste("Beagle",paste(paste(mr[ff],paste("simulation",rp[rere],sep="_"),sep="_"),"gapit",sep="_"),sep="_")
	result.file<-paste(root,file_name,sep="/")
	dir.create(result.file)
	setwd(result.file)


	myG<- read.delim("C:\\Users\\user1\\Documents\\imputation\\pipline\\simulation_data\\RIL_answer_for_gabit_05\\ril.new.data.txt",sep=" ",head = F)
	index_path<-index.file[(f_rep==repeat_f)&(f_missing==missing_f),1]
	input_data<-read.delim(as.character(index_path[1]),head = T)
		
	for(i in 2:length(index_path)){input_data<-rbind(input_data,read.delim(as.character(index_path[i]),head = T))}
	colnames(input_data)<-colnames(myG)
	trt.data<-rbind(myG[1,],input_data)
	

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
	write.table(result, file = "manhattan_file_result.txt", sep = ",", row.names = FALSE,quote=F)
	}
	}
	#}
	setwd(root)
	write.table(result, file = "manhattan_file_result.txt", sep = ",", row.names = FALSE,quote=F)

	
	Sys.time()-times
	##### simulation phenotype Explanation
	#使用genotype是從seasiarun3t2.hmp.txt 中抽出的 no missing snp data
	#先讀入TASSEL 轉成PLINK的正常讀檔
	#在將軟換過的檔案讀入PLINK轉成二元讀檔
	#./plink --ped try_snp_plink.plk.ped --map try_snp_plink.plk.map --out mydata --make-bed
	#在將二元讀檔輸入至GATK設定生成phenotype
	#./gcta64  --bfile mydata  --simu-qt  --simu-causal-loci causal.snplist  --simu-hsq 0.5 --simu-rep 3 --out mydata
	#本試驗的phenotype讀檔就由此產生
	##################################################################################################################