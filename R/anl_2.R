setwd("C:/Users/user1/Documents/imputation/anl2.test1")
final<-wwknn_final

sum(final[data.p]==kk[data.p])/sum(zz=="N")

result1<-array(0,ncol(kk),1)
for(i in 1:ncol(kk)){
result1[i]<-sum(final[,i]==kk[,i])/length(zz[,i])
}

result2<-array(0,ncol(kk),1)

for(i in 1:ncol(kk)){
result2[i]<-sum(final[(data.p[data.p[,2]==i,])]==kk[(data.p[data.p[,2]==i,])])/sum(zz[(data.p[data.p[,2]==i,])]=="N")
}

result3<-array(0,ncol(kk),1)
for(i in 1:ncol(kk)){
result3[i]<-sum(!kk[(data.p[data.p[,2]==i,])]%in%c("A","T","C","G"))
}


#plot(result1, type = "h")
#plot(result2, type = "h")
plot(result3, type = "h")

which(result2<0.95)

sum(result2<0.95)

final[(data.p[data.p[,2]==2,])]

sum(!kk[(data.p[data.p[,2]==i,])]%in%c("A","T","C","G"))


#plot(n.count, type = "h")

cor(n.count.nol,result1)
cor(n.count.nol,result2)

cor(result1,result3)
cor(result2,result3)

n.count.nol<-n.count-mean(n.count)/max(n.count)

max(n.count)



write.table(data.p,file="imupte position.txt",sep="\t")
write.table(result2,file="wwknn_accuracy_by sample.txt",sep="\t")