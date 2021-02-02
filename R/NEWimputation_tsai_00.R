L=1
dim(x)
table(x)
####
    ld=LD(x)
    #ld=od(w)
####
    i=4
    N.id = which(x[i,]=="N")
     
####     
     w.target=(ld[-i,i]/sum(ld[-i,i]))
#### 
     
j=2
	x.new = x[i,-N.id[j]]
	x.target=x[-i, N.id[j]]
	x.train=x[-i,-N.id[j]]
	s=apply(x.train,2,function(x){score.ld.w.1(x.target,x,w.target)})        
	ss=sort(s,decreasing=TRUE,index.return=TRUE)
#####
	dist.weight=rep(0,k)
      n.ss=ss$x[1:k]
      if (n.ss[k]==n.ss[1]){
         dist.weight=rep(1,k)
      }else{ 
	 for(z in 1:k){dist.weight[z]=(n.ss[z]-n.ss[k])/(z*(n.ss[1]-n.ss[k]))}
      }
	
      x.tab=table(as.character(x.new[ss$ix[1:k]]))
      genotype=row.names(x.tab)
      knn.weight=c()
      for (ii in 1:length(genotype)){knn.weight[ii]=sum(dist.weight[x.new[ss$ix[1:k]]==genotype[ii]])}
      new.s=sort(knn.weight,decreasing=TRUE, index.return=TRUE)

      x[i, N.id[j]]=genotype[new.s$ix[1]]

	
#####
	x.tab=table(as.character(x.new[sss$ix[1:k]]))
	genotype=row.names(x.tab)
	t<-as.matrix(x.tab)
	sw<-as.matrix(combn(as.matrix(x.tab),2)[,combn(as.matrix(x.tab),2)[1,]==max(as.matrix(x.tab))])        
	if(sum(sw[2,]<max(t))==dim(sw)[2])
	{
	x[i, N.id[j]]=genotype[which.max(x.tab)]
	}
	else{x[i, N.id[j]]=c("N")}
