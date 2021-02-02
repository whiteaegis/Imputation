	library(lattice) 
	result<-final_result
	
	#資料整理
	chr<-factor(rep(rep(rep(1:12,rep(1500,12))),2))
	d<-as.factor(rep(rep(1:6,rep(250,6)),24))
	a<-as.numeric(result[,2])
	b<-as.numeric(result[,3])
	match<-as.factor(result[,8])
	mismatch<-as.factor(result[,9])
	chr.u<-rep(rep(1:12,rep(1500,12)))
	d.u<-as.factor(rep(rep(1:6,rep(250,6)),12))
	y<-c(a,b)
	x<-factor(rep(1:2,c(18000,18000)))

######
	
	result[result[,10]==9,][,1:3]
	
######

	ct<-matrix(NA,12,1);for(i in 1:12)ct[i]<-max(which(result[,10]==i))
	position<-result[ct,6]

	ff<-c()
	for(i in 1:12){
	cta=as.matrix(read.delim(position[i],header=T,sep=","))
	ff<-cbind(ff,as.matrix(dim(cta)))
	}

	#找出最大值
	jj<-(result[order(a, decreasing = TRUE),])[1:100,]
	dim(jj)
	jj[,1:3]
	
	
	plot(result[,2],type="h")
	
	#用Shapiro-Wilk對每種組合檢定
	anly<-c()
	for(i in 1:5){
	for(j in 1:5){
	for(k in 1:12){
	for(d in 1:6){
	long<-a[chr.u==k&d.u==d&match==i&mismatch==j]
	#f1<-ks.test(long, "pnorm", mean = mean(long), sd = sqrt(var(long)))	
	f2<-shapiro.test(long)
	if(f2$p.value<0.05)anly<-rbind(anly,c(k,d,i,j))
	}}}}
	
	table(anly[,1])
	table(anly[,3],anly[,4],anly[,2],anly[,1])#觀察結果
	
	ab<-(a+b)/2
	
######heatmap 製作

	#for(k in 1:6){
	final<-matrix(NA,5,5)
	for(i in 1:5){
	for(j in 1:5){
	final[i,j]<-mean(b[d.u==3&match==i&mismatch==j])
	}}
	colnames(final)<-c("-1","-2","-3","-4","-5")
	rownames(final)<-c("1","2","3","4","5")
	#heatmap(final,xlab="mismatch point",ylab="match point",main="Missing rate 01%",Rowv=NA,Colv=NA)
	levelplot(final,xlab="match point",ylab="mismatch point",main="Missing rate 10%",col.regions=heat.colors) 
	

	colnames(use)<-c("y","x","chr","d")
	axis(1, at = seq(1, 5, by = 1))
	axis(2, at = seq(-1, -5, by = 1))
	?axis
	fix(result)
	
	boxplot(y~x*chr, notch=TRUE,
	col=(c("gold","darkgreen")),
	main="Tooth Growth", xlab="Suppliment and Dose")	

	length(y)
	fit <- aov(y~chr+x+d)
	
	summary(fit)
	