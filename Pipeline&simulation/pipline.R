rm(list=ls())

real.simulate<-function(x,number=100000){
  zz = as.matrix(x[,12:ncol(x)])
  row<-sample(1:nrow(zz), size=number,replace=T)
  col<-sample(1:ncol(zz), size=number,replace=T)
  tt<-cbind(row,col)
  tt<-unique(tt)
  ww<-zz[tt]
  zz[tt]<-c("N")
  print("missing number in simulation data:");print(sum(zz=="N"))
  print("missing rate on simulation data:");print(sum(zz=="N")/length(zz))
  result<- new.env()
  result$zz<-zz
  result$ww<-ww
  result$tt<-tt
  result$sida<-sum(zz=="N")
  result$rsida<-sum(zz=="N")/length(zz)
  result<-as.list(result)
  return(result)
}


knnwindowimpute<-function(y,w=20,k=5,match.score=1, miss.score=0.5,mismatch.score=-5){
  score.f <- function(target,train,match.score=1, miss.score=0.5, mismatch.score=-5){
    m=length(target)
    n.allmiss = sum((train=="N") & (target=="N"))
    n.match = sum(target==train) - n.allmiss
    n.miss = sum(target=="N")+sum(train=="N")- n.allmiss
    n.mismatch = m - n.match -n.miss
    score = (match.score*n.match + miss.score*n.miss + mismatch.score*n.mismatch)/m
    return(score)}
  time=Sys.time()
  final=c()
  n.step=floor(nrow(y)/w)
  for(L in 1:n.step){
    if (L <= (n.step-1)){
      x=y[(w*(L-1)+1):(w*L),]
    } else{
      x=y[(w*(L-1)+1):nrow(y),]
    } 
    for(i in 1:nrow(x)){
      N.id = which(x[i,]=="N")
      x.new = x[i, -N.id]
      if (length(N.id) > 0){
        for (j in 1:length(N.id)){
          x.target=x[-i, N.id[j]]
          x.train=x[-i, -N.id]
          s=apply(x.train,2,function(x){score.f(x.target,x,match.score, miss.score,mismatch.score)})
          ss=sort(s,decreasing=TRUE,index.return=TRUE)
          x.tab=table(as.character(x.new[ss$ix[1:k]]))
          genotype=row.names(x.tab)
          x[i, N.id[j]]=genotype[which.max(x.tab)]
        }
      }
    }
    final = rbind(final,x)
  }
  print(Sys.time()-time)
  return(final)
}

ldwknnwindowimpute<-function(y,w=20,k=5,match.score=1, miss.score=0.5,mismatch.score=-5,cd="FALUSE"){
  
  LD<-function(y.1){
    y.1[y.1%in%c("K","M","R","S","W","Y","N")]<-c(-1)
    y.1<-apply(y.1,1,function(x){tr(x)})
    A<-LDmat(y.1,typ="haplotype", plotmat = FALSE)
    A[upper.tri(A)]<-NA
    kk<-as.matrix(as.dist(as.matrix(A), upper = TRUE))
    return(kk)
  }
  score.ld.w.1 <- function(target,train,w.target,match.score=1, miss.score=0.5, mismatch.score=-5){
    id.allmiss = which((train=="N") & (target=="N"))
    id.match = which((target==train)&(train!="N"))
    id.miss = which((train=="N") | (target=="N"))
    score <- sum(w.target[id.match]*match.score) + sum(w.target[id.miss]*miss.score) + sum(w.target[-c(id.match,id.miss)]*mismatch.score)
    return(score)
  }
  tr<-function(x){
    t<-x[x!=-1]
    genotype=row.names(table(t))
    if(length(unique(table(t)))==1){
      x[x!=genotype[2] & x!=genotype[1]& x!="-1"]<-"-1"
      x[x==genotype[1]]<-"1"
      x[x==genotype[2]]<-"0"}
    else{
      x[x!=genotype[which.min(table(t))]& x!=genotype[which.max(table(t))]& x!="-1"]<-"-1"
      x[x==genotype[which.max(table(t))]]<-"1"
      x[x==genotype[which.min(table(t))]]<-"0"
    }
    return(x)
  }
  ff<-function(x){
    b<-which(x==0)
    d<-length(x)
    x<-c(c(1:d-b))
    return(x)
  }
  library(popgen)
  time=Sys.time()
  final=c()
  n.step=floor(nrow(y)/w)
  for(L in 1:n.step){
    if (L <= (n.step-1)){
      x=y[(w*(L-1)+1):(w*L),]
    } else{
      x=y[(w*(L-1)+1):nrow(y),]} 
    ld=LD(x)
    if(sum(ld%in%NaN)!=0)ld[ld%in%NaN]<-0.0001
    for(i in 1:nrow(x)){
      N.id = which(x[i,]=="N")
      w.target=(ld[-i,i]/sum(ld[-i,i]))
      if (length(N.id) > 0){
        for (j in 1:length(N.id)){
          x.new = x[i,-N.id[j]]
          x.target=x[-i, N.id[j]]
          x.train=x[-i,-N.id[j]]
          s=apply(x.train,2,function(x){score.ld.w.1(x.target,x,w.target,match.score, miss.score,mismatch.score)})        
          ss=sort(s,decreasing=TRUE,index.return=TRUE)
          x.tab=table(as.character(x.new[ss$ix[1:k]]))
          genotype=row.names(x.tab)
          if(cd=="TRUE"){
            t<-as.matrix(x.tab)
            sw<-as.matrix(combn(as.matrix(x.tab),2)[,combn(as.matrix(x.tab),2)[1,]==max(as.matrix(x.tab))])        
            if(sum(sw[2,]<max(t))==dim(sw)[2]){
              x[i, N.id[j]]=genotype[which.max(x.tab)]
            }
            else{x[i, N.id[j]]=c("N")}
          }
          else {x[i, N.id[j]]=genotype[which.max(x.tab)]}
        }}}
    final = rbind(final,x)
  }
  print(Sys.time()-time)
  return(final)
}

data=read.delim(file.choose(),header=T)
data<-data[1:240,]
big_time<-Sys.time()

accuracy<-c(5000,10000,20000)
nk<-c(3,5,7,11,13)
nw<-c(10,20,50,100,200)

final_result<-matrix("NA",375,5)
final_time<-matrix("NA",375,3)
final_row_names<-matrix("NA",375,1)
counter=1
for(g in 1:3)#for missing rate
{
  for(k in 1:5)#for knn&ldknn k
  {
    for(w in 1:5)#for knn&ldknn w
    {
      for(r in 1:5)#for repeat
      {
      setwd("~/pipline")
      sup_filename<-paste(paste(accuracy[g],paste(paste(nk[k],"k",sep=""),paste(nw[w],"w",sep=""),sep = "_"),sep = "_"),r,sep = "_")
      system(paste("mkdir",sup_filename))
      setwd(paste("~/pipline/",sup_filename,sep = ""))
      system("ln ~/Documents/Beagle4/b4.r1128.jar b4.jar")
      sim<-real.simulate(data,number=accuracy[g])
      f_time1<-Sys.time()
      final1<-knnwindowimpute(sim$zz,w=nw[w],k=nk[k])
      time1<-Sys.time()-f_time1
      f_time2<-Sys.time()
      final2<-ldwknnwindowimpute(sim$zz,w=nw[w],k=nk[k])
      time2<-Sys.time()-f_time2
      data_title<-data[,1:12]
      out_data<-cbind(data_title,sim$zz)
      write.table(out_data, file = "missing_data.txt", sep = " ", row.names = FALSE,quote=F)
      system("~/Documents/tassel4.0_standalone/run_pipeline.pl -Xmx5g -fork1 -h missing_data.txt -export -exportType VCF -runfork1")
      system("cut -f 1-9,11-365 missing_data.vcf>missing_data_c.vcf")
      f_time3<-Sys.time()
      system("java -Xmx2g -jar b4.jar gt=missing_data_c.vcf out=Beagle_finish")
      time3<-Sys.time()-f_time3
      system("~/Documents/tassel4.0_standalone/run_pipeline.pl -Xmx5g -fork1 -vcf Beagle_finish.vcf.gz -export -exportType Hapmap -runfork1")
      Beagle_result=read.delim("Beagle_finish.hmp.txt",header=T)
      final3<-as.matrix(Beagle_result[,12:ncol(Beagle_result)])
      final_result[counter,1]<-sum(final1[sim$tt]==sim$ww)/sum(sim$zz=="N")
      final_result[counter,2]<-sum(final2[sim$tt]==sim$ww)/sum(sim$zz=="N")
      final_result[counter,3]<-sum(final3[sim$tt]==sim$ww)/sum(sim$zz=="N")
      final_result[counter,4]<-sim$sida
      final_result[counter,5]<-sim$rsida
      final_time[counter,1]<-time1
      final_time[counter,2]<-time2
      final_time[counter,3]<-time3
      final_row_names[counter]<-sup_filename
      out_final1_data<-cbind(data_title,final1)
      write.table(out_final1_data, file = paste("knn",paste(sup_filename,".txt",sep = ""),sep = "_"), sep = " ", row.names = FALSE,quote=F)
      out_final2_data<-cbind(data_title,final2)
      write.table(out_final2_data, file = paste("ldknn",paste(sup_filename,".txt",sep = ""),sep = "_"), sep = " ", row.names = FALSE,quote=F)
      sink("log.txt")
      print(final_result[counter,])
      print(time1)
      print(time2)
      print(time3)
      sink()
      counter=counter+1      

      }}}}

Sys.time()-big_time

setwd("~/pipline")
names(final_result)<-c("knn","ldknn","Beagle4","missing rate","missing number")
row.names(final_result)<-final_row_names

write.table(final_result = "final_result.txt", sep = ",",quote=F)
write.table(final_time, file = "final_time.txt", sep = ",",quote=F)








