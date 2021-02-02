
	rm(list=ls())
	library(scrime)
	y<-real.simulate(data.trt,number=1000)
	time<-Sys.time()
	score<-tr(y$zz)
	final<-score	
	na.position<-y$tt
	na.position<-na.position[order(na.position[,2]), ]
	score[score%in%c(4)]<-c("")
	score<-apply(score,2,as.numeric)
	pior.final<-cbind(na.position,matrix(data = NA, nrow =nrow(na.position), ncol=ncol(score)))
	row.names(pior.final)<-c(1:nrow(na.position))
	
	for(i in 1:ncol(score)){
	tret<-score[,-i]
	result<-knncatimpute(tret, nn = 5, dist = "smc")
	if(sum(na.position[,2]==i)==0){p.sub.na.position<-na.position}
	else{p.sub.na.position<-na.position[-which(na.position[,2]==i),]}
	###sub.na.position[刪掉 i sample 的 na position]
	sub.na.position<-rbind(cbind(p.sub.na.position[which(p.sub.na.position[,2]>i),][,1],p.sub.na.position[which(p.sub.na.position[,2]>i),][,2]-1),p.sub.na.position[which(p.sub.na.position[,2]<i),])
	sub.na.position<-sub.na.position[order(sub.na.position[,2]), ]
	sample<-result[sub.na.position]

	###以sample長度(sub.na.position長度)跑j迴圈
	
	for(j in 1:length(sample)){
	if(sum(p.sub.na.position[j,][2]==pior.final[,2])==1){flag<-names(which(p.sub.na.position[j,][2]==pior.final[,2]))}
	else{flag<-names(which(pior.final[which(p.sub.na.position[j,][2]==pior.final[,2]),1]==p.sub.na.position[j,][1]))}#算出na.psition目標row的位置
	pior.final[flag,(i+2)]<-sample[j]
	}}
	t.pior.final<-pior.final[,3:355]
	for(i in 1:nrow(na.position))final[na.position[i,1],na.position[i,2]]<-names(which.max(table(t.pior.final[i,])))
	for(i in 1:nrow(result)){
	nu.t<-score[i,]
	t<-y$zz[i,]
	if(length(row.names(table(t[nu.t==1])))==1)final[i,][final[i,]==1]<-row.names(table(t[nu.t==1]))
	if(length(row.names(table(t[nu.t==2])))==1)final[i,][final[i,]==2]<-row.names(table(t[nu.t==2]))
	if(length(row.names(table(t[nu.t==3])))==1)final[i,][final[i,]==3]<-row.names(table(t[nu.t==3]))
	}
	
	Sys.time()-time

	

	Sys.time()-time
	result2<-knncatimpute(score, nn = 5, dist = "smc")
	for(i in 1:nrow(result)){
	nu.t<-score[i,]
	t<-y$zz[i,]
	if(length(row.names(table(t[nu.t==1])))==1)result2[i,][result2[i,]==1]<-row.names(table(t[nu.t==1]))
	if(length(row.names(table(t[nu.t==2])))==1)result2[i,][result2[i,]==2]<-row.names(table(t[nu.t==2]))
	if(length(row.names(table(t[nu.t==3])))==1)result2[i,][result2[i,]==3]<-row.names(table(t[nu.t==3]))
	}
	sum(final[y$tt]==y$ww)/sum(y$zz=="N")
	sum(result2[y$tt]==y$ww)/sum(y$zz=="N")
	