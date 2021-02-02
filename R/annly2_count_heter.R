
	K<-c("GT")
	M<-c("AC")
	S<-c("CG")
	W<-c("AT")
	R<-c("AG")
	Y<-c("CT")

	count<-0

	for(i in 1:302){
	y.1<-u[i,]	
	t<-y.1[y.1!="N"]
	genotype=row.names(table(t))
	if(c("T")%in%genotype){
		if(c("G")%in%genotype){if(c("K")%in%genotype){count=count+1}}
		if(c("A")%in%genotype){if(c("W")%in%genotype){count=count+1}}
		if(c("C")%in%genotype){if(c("Y")%in%genotype){count=count+1}}	
		}	
	if(c("A")%in%genotype){
		if(c("G")%in%genotype){if(c("R")%in%genotype){count=count+1}}
		if(c("C")%in%genotype){if(c("M")%in%genotype){count=count+1}}	
		}	
	if(c("C")%in%genotype){
		if(c("G")%in%genotype){if(c("S")%in%genotype){count=count+1}}	
		}	
	}

dim(u)
	length(table(fff))

	u<-c()
	count<-0
	for(i in 1:(dim(y)[1])){
	if(length(table(y[i,]))>3){u<-rbind(u,y[i,]);count=count+1;}
	}
	
	count
	
	count1<-0
	for(i in 1:(dim(u)[1])){
	y.1<-u[i,]	
	t<-y.1[y.1!="N"]
	genotype=row.names(table(t))
	if(c("T")%in%genotype){
		if(c("G")%in%genotype){if(c("K")%in%genotype){count1=count1+1}}
		if(c("A")%in%genotype){if(c("W")%in%genotype){count1=count1+1}}
		if(c("C")%in%genotype){if(c("Y")%in%genotype){count1=count1+1}}	
		}	
	if(c("A")%in%genotype){
		if(c("G")%in%genotype){if(c("R")%in%genotype){count1=count1+1}}
		if(c("C")%in%genotype){if(c("M")%in%genotype){count1=count1+1}}	
		}	
	if(c("C")%in%genotype){
		if(c("G")%in%genotype){if(c("S")%in%genotype){count1=count1+1}}	
		}	
	}

	count1
	
	
		