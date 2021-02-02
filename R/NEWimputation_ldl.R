rm(list=ls())
xx=read.delim(file.choose(),header=T)
#for (s in 1:3653){
#xx=read.delim(file.choose(),header=T,nrows=3000,skip=s*3000)

### function for one score ###
ff = function(x,n.score=-3.5,match.score=1,mismatch.score=-5){
   n = nrow(x)
   N.id = which(apply(x,1,function(x){any(x=="N")}))
   if (length(N.id) > 0) length.match = sum(x[-N.id,1]==x[-N.id,2])
   else length.match = sum(x[,1]==x[,2])
   return(match.score*length.match+mismatch.score*(nrow(x)-length(N.id)-length.match)+n.score*length(N.id))
}

### function for score matrix and plot ###
my.snp.heatmap = function(x,plot=FALSE){
   all.x.comb = combn(ncol(x),2)
   dist.mat = matrix(NA,ncol(x),ncol(x))
   dist.mat[lower.tri(dist.mat)] = apply(all.x.comb,2,function(x,dd){ff(dd[,x])},dd=x)
   if (plot==TRUE) {
      aa = hclust(as.dist(-dist.mat-min(-dist.mat,na.rm=TRUE)))
      ii = matrix(NA,nrow(x),ncol(x))
      icolors = rgb(t(col2rgb(c("red","green","yellow","blue"))),max=255)
      ii[x=="A"] = icolors[1]
      ii[x=="C"] = icolors[2]
      ii[x=="G"] = icolors[3]
      ii[x=="T"] = icolors[4]
      ii[!x %in% c("A","C","G","T")] = grey(0.5)
      ii = ii[,aa$order]
      xl = rep(1:ncol(x),nrow(x))
      xr = xl+1
      yb = rep(1:nrow(x),each=ncol(x))
      yt = yb+1
      layout(1:2,heights=c(1,2))
      par(mai=c(0,0.5,0.5,0.1))
      plot(aa,hang=-1,labels=FALSE,xlab="",sub="")
      par(mai=c(0.5,0.5,0.1,0.1))
      plot(c(1,ncol(x)+1),c(1,nrow(x)+1),type="n",xlab="",ylab="")
      rect(xl,yb,xr,yt,col=c(t(ii)),border="white")
   }
   return(as.matrix(as.dist(dist.mat,TRUE,TRUE)))
}

### function for imputation-KNN (k=3)###
Mode3 <- function(x,s) {
  ss=sort(s,decreasing=TRUE)[3]
  xx = x[which(s >= ss)]
  ux = unique(x[!x=="N"])
  if(length(ux) > 0) return(ux[which.max(tabulate(match(xx, ux)))])
  else return("N")
}

###################################
# computation from here
###################################
y = as.matrix(xx[,12:ncol(xx)])
final=c()

Sys.time()
for(L in 1:floor(nrow(y)/20)){
   x=y[(20*L-19):(20*L),]
   mm = my.snp.heatmap(x)
   diag(mm) = min(mm)
   z=x
   for(i in 1:nrow(x)){
      to.do.i = which(y[i,]=="N")
      for (j in to.do.i) {
         z[i,j] = Mode3(x[i,],mm[j,])
      }
   }
   final = rbind(final,z)
}

Sys.time()
my.snp.heatmap(final,TRUE)

sum(final=="N")/length(final)
sum(y=="N")/length(y)

