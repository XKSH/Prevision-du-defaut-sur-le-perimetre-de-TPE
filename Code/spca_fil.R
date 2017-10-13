#supervised pca
library(superpc)
acp_train=PEdat[traincand[,2],-246]
acp_test=PEdat[-traincand[,2],-246]
acp_dft=PEdat[traincand[,2],246]
matd.complet=do.call(cbind,acp_train)

for(i in 1:ncol(matd.complet))
{
  matd.complet[is.na(matd.complet[,i]),i]=median(matd.complet[,i],na.rm=TRUE)
}
data<-list(x=t(matd.complet),y=acp_dft, featurenames=names(acp_train))
train.obj<- superpc.train(data, type="regression")
#IMv=train.obj$feature.scores[order(train.obj$feature.scores), ,drop = FALSE]
IMv=train.obj$feature.scores
IMv1=IMv[1:245,]
IMv1=IMv[IMv1>1,,drop=FALSE]
IMv2=IMv[-(1:245),]
IMv2=IMv[IMv2>1,,drop=FALSE]
#IMv2=c()
train_spca=cbind(PEdat[traincand[,2],c(rownames(IMv1),rownames(IMv2))],PEdat[traincand[,2],245])
nr=5;nc=5
matd=do.call(cbind,train_spca)

w=c(rep(1/(3*length(IMv1)),length(IMv1)),rep(1/(3*length(IMv2)),length(IMv2)),1/3)
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1],w)
m=kohonenqualiclass.weight(prof,train_spca,dim(train_spca)[1],w)
#clustering
clustrain=cbind(train_spca,PEquan[traincand[,2],ncol(PEquan)])
clustrain=cbind(clustrain,m)[order(m),]
#graphe
grillecarte(nr,nc,2,clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)])
par(xpd=TRUE)
nb=2
ncol=seq(0,240,length.out=nb)
legend("topright", inset=c(-0.15,0.2), title="Groupe", c("Non-défaut","Défaut"), pch=15,col=hcl(ncol,120,85),cex=0.55)
###
cc=table(clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)])
cc
nmi(table(clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)]))
purity(table(clustrain[,(ncol(clustrain)-1)],clustrain[,ncol(clustrain)]))