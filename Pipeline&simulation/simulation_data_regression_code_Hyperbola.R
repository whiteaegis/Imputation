	data[,3]
	use.data<-data.trt[data[,3]==1,]
	gg<-matrix(NA,200,2)

	for(i in 1:200){
	ana<-c(1:200)/100
	l<-length(use.data)
	out_final1_data<-real.simulate(use.data,number=(ceiling((ana[i])*l)))
	gg[i,1]<-out_final1_data$missing_rate
	gg[i,2]<-(ceiling((ana[i])*l))
	}
	gg<-data.frame(gg)
	names(gg)<-c("y","x")
	plot(gg$x,gg$y,xlab="Number of missing point",ylab="Missing rate")

	

######fit regression

	fit<-nls(y~x/(a*x+b),data=gg,list(a=1.,b=0.))
	summary(fit)
	names(fit)
	fit$data
	an<-summary(fit)$coefficients
	
	
######drew curve
	
	grp<-function(x,an){
	y<-x/(an[1,1]*x+an[2,1])
	}

	curve(grp(x,an),0,max(gg$x),add = TRUE, lwd=2, col = "red")
	
######test result

	op2<-function(x,an){
	y<-ceiling(x-an[2,1]/an[1,1])
	return(y)
	}
	
	out_final1_data<-real.simulate(use.data,number=op2(0.01,an))
	

	hh<-matrix(NA,8,2)
	for(i in 1:8){
	ana<-oo
	out_final1_data<-real.simulate(use.data,number=(ana[i]))
	hh[i,2]<-out_final1_data$missing_rate
	hh[i,1]<-ana[i]
	} 
	
	