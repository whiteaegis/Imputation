setwd("C:/Users/user1/Documents/imputation/wknn&wwknn")
####���ͼƾ�
xx=read.delim(file.choose(),header=T)###Ū������~�p�������
y = as.matrix(xx[,12:ncol(xx)])

data=read.delim(file.choose(),header=T)####Ū���եμƾ�
dim(data)
zz = as.matrix(data[,12:ncol(data)])
kk = as.matrix(data[,12:ncol(data)])
dim(zz)
gg<-function(y){
f<-sum(y=="N")
return(f)
}

n.count.col<-apply(y,2,function(x){gg(x)})
n.count<-round(as.matrix(n.count.col)*(length(zz)/length(y)))
data.p<-c()
for(i in 1:ncol(zz))
{
n.count.i<-n.count[i,]
cc<-sample(1:(dim(zz)[1]), size=n.count.i)
zz[cc,i]<-"N"
pp<-cbind(cc,rep(i,length(cc)))
data.p<-rbind(data.p,pp)
}
y=zz

#############################################
sum(zz=="N")/length(zz)
sum(final[data.p]==kk[data.p])/sum(zz=="N")
sum(final=="N")/length(final)
#############################################
#final:impute���G
#zz:impute�e���
#kk:���T����
#############################################
###��X�n�o�Iop1:�Ĥ@���ƾڻP�ѵ��ۦP���Iop2:�ĤG���ƾڻP�ѵ��ۦP���I
op1<-data.p[final[data.p]==kk[data.p],]#wknn
op1.1<-data.p[!final[data.p]==kk[data.p],]
op2<-data.p[final_2[data.p]==kk[data.p],]#wwknn
op2.1<-data.p[!final_2[data.p]==kk[data.p],]

dim(op1)
dim(op2)

#####�]�X�ۦP�I���Ӽ�
count=0
count_r=0
for(i in 1:449){
count<-sum((op2[op2[,2]==i,])[,1]%in%(op1[op1[,2]==i,])[,1])
count_r=count_r+count
}
count_r


#####��Xop2�����T�W����l
da<-c()
da_full_op2<-c()
for(i in 1:449){
da<-(op2[op2[,2]==i,])[which(!(op2[op2[,2]==i,])[,1]%in%(op1[op1[,2]==i,])[,1]),]
da_full_op2<-rbind(da_full_op2,da)
}

#####��Xop1�����T�W����l
da<-c()
da_full_op1<-c()
for(i in 1:449){
da<-(op1[op1[,2]==i,])[which(!(op1[op1[,2]==i,])[,1]%in%(op2[op2[,2]==i,])[,1]),]
da_full_op1<-rbind(da_full_op1,da)
}
da_full_op1<-as.matrix(da_full_op1)
da_full_op2<-as.matrix(da_full_op2)

#####��Xop1&op2���T�@��(�Ѥj�̶}�Y)
da<-c()
da_full<-c()
for(i in 1:449){
da<-(op1[op1[,2]==i,])[which((op1[op1[,2]==i,])[,1]%in%(op2[op2[,2]==i,])[,1]),]
da_full<-rbind(da_full,da)
}

#####
table(kk[data.p])
table(kk[da_full])
table(kk[da_full_op1])
table(kk[da_full_op2])

dim(data.p)
dim(da_full)
dim(da_full_op1)
dim(da_full_op2)

####################���~���R��

#####�]�X�ۦP�I���Ӽ�
count=0
count_r=0
counter<-unique(op2.1[,2])


dim(op2.1)
dim(op1.1)

for(i in counter){
op2.1_1<-as.matrix(op2.1[op2.1[,2]==i,])
op1.1_1<-as.matrix(op1.1[op1.1[,2]==i,])
count<-sum(op2.1_1[,1]%in%op1.1_1[,1])
count_r=count_r+count
}
count_r


#####��Xop2�����~�W����l
da<-c()
da_full_op2<-c()
for(i in counter){
op2.1_1<-as.matrix(op2.1[op2.1[,2]==i,])
op1.1_1<-as.matrix(op1.1[op1.1[,2]==i,])

da<-op2.1_1[which(!op2.1_1[,1]%in%op1.1_1[,1]),]
da_full_op2<-rbind(da_full_op2,da)
}

#####��Xop1�����~�W����l
da<-c()
da_full_op1<-c()
for(i in counter){
op2.1_1<-as.matrix(op2.1[op2.1[,2]==i,])
op1.1_1<-as.matrix(op1.1[op1.1[,2]==i,])
da<-op1.1_1[which(!op1.1_1[,1]%in%op2.1_1[,1]),]
da_full_op1<-rbind(da_full_op1,da)
}
da_full_op1<-as.matrix(da_full_op1)
da_full_op2<-as.matrix(da_full_op2)

#####��Xop1&op2���~�@��(�Ѥj�̶}�Y)

da<-c()
da_full<-c()
for(i in counter){
op2.1_1<-as.matrix(op2.1[op2.1[,2]==i,])
op1.1_1<-as.matrix(op1.1[op1.1[,2]==i,])
da<-op1.1_1[op1.1_1[,1]%in%op2.1_1[,1],]
da_full<-rbind(da_full,da)
}

#####��ܵ��G
table(kk[data.p])
table(kk[da_full])
table(kk[da_full_op1])
table(kk[da_full_op2])

dim(data.p)
dim(da_full)
dim(da_full_op1)
dim(da_full_op2)
count_r

#####���X�Ӭ�

li<-matrix(0,100,3)
for(i in 1:100){
jj<-da_full_op2[i,]
li[i,1]<-kk[jj[1],jj[2]]
li[i,2]<-final[jj[1],jj[2]]
li[i,3]<-final_2[jj[1],jj[2]]
}

colnames(li)<-c("answer","wknn","wwknn")
a<-zz[jj[1],]
fix(a)
table(a)

li.befor<-c()
for(i in 1:100){
jj<-da_full_op2[i,]
a<-zz[jj[1],]
aa<-as.matrix(table(a))
aaa<-paste(row.names(aa),aa,sep=":")
print(cat(aaa))
li.befor<-rbind(li.befor,aaa)
}

li<-cbind(li,li.befor)




