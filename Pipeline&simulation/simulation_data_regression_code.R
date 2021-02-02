	

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
	plot(gg$x,gg$y)

######
	fit<-nls(y^2~(a*x+b),data=gg,list(a=1.,b=0.))
	summary(fit)
	names(fit)
	fit$data
	an<-summary(fit)$coefficients
	
	
######
	
	op2<-function(x,an){
	y<-ceiling(((x^2)-an[2,1])/an[1,1])
	return(y)
	}
	
	grp<-function(x,an){
	y<-(an[1,1]*x+an[2,1])^0.5
	}

	curve(grp(x,an),0,max(gg$x),add = TRUE, lwd=2, col = "red")
		
	out_final1_data<-real.simulate(use.data,number=op2(0.01,an))
	

	hh<-matrix(NA,8,2)
	for(i in 1:8){
	ana<-oo
	out_final1_data<-real.simulate(use.data,number=(ana[i]))
	hh[i,2]<-out_final1_data$missing_rate
	hh[i,1]<-ana[i]
	} 
	
	