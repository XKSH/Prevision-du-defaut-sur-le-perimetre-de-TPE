#SMOTE sur la base d'entraînement
library(DMwR)
old.train=PEdat[traincand[,2],]
old.train$top_dft_auto_pro_corp=factor(old.train$top_dft_auto_pro_corp)
set.seed(15)
new.train=SMOTE(top_dft_auto_pro_corp~.,old.train,perc.over = 300,perc.under=200,k=6)
new.train$top_dft_auto_pro_corp= as.numeric(levels(new.train$top_dft_auto_pro_corp))[new.train$top_dft_auto_pro_corp]
nr=5;nc=5
matd=do.call(cbind,new.train)
w=c(rep(3/(8*245),245),1/2,rep(1/(8*59),ncol(disj)))
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1],w)
m=kohonenqualiclass.weight(prof,new.train,dim(new.train)[1],w)
#clustering
clustrain=cbind(new.train,new.train[,"top_dft_auto_pro_corp"])
clustrain=cbind(clustrain,m)[order(m),]
###
cc=table(clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)])
cc
nmi(table(clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)]))
purity(table(clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)]))

stest=PEdat[-traincand[,2],]
mtest=kohonenqualiclass(prof[,c(1:245,247:305)],stest[,c(1:245,247:305)],dim(stest[,c(1:245,247:305)])[1])
clustest=cbind(stest,stest[,"top_dft_auto_pro_corp"])
clustest=cbind(clustest,mtest)[order(mtest),]
table(clustest[,(ncol(clustest)-1)],clustest[,ncol(clustest)])

idc1=as.numeric(colnames(cc)[apply(cc,2,function(x){t=x[1]<x[2];return(t)})])
class.pre=rep(0,length(mtest))
class.pre[which(mtest%in%idc1)]=1
class.actual=stest[,246]  
table(class.pre,class.actual)