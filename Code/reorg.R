library(sampling)
library(unbalanced)
rownames(PEdat)=c(1:dim(PEdat)[1])
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
taille=0.5*as.data.frame(table(PEdat$top_dft_auto_pro_corp))[,2]
set.seed(1765)
#idtrain=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")[,2]
traincand=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")
set.seed(1578)
#undersampling
#La proportion 0.4 a l'air cool pour 5 fois 5 grid
id1=sample(1:length(traincand[traincand[,4]==1,4]), 1.1*length(traincand[traincand[,4]==2,4]))
idtrain=c(traincand[id1,2],traincand[which(traincand[,4]==2),2])
btrain=PEdat[idtrain,]
btrain1=PEdat[setdiff(traincand[,2],idtrain),]
source("Kohonennet_weight.R")
source("dist.R")
nr=5;nc=6
matd=do.call(cbind,btrain)
w=c(rep(3/(8*245),245),1/2,rep(1/(8*59),ncol(disj)))
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1],w)
bmu.w=kohonenqualiclass.weight(prof,btrain,dim(btrain)[1],w)
superclu=table(PEdat[idtrain,iddf],bmu.w)
clus.c1=as.numeric(colnames(table(PEdat[idtrain,iddf],bmu.w))[superclu[2,]>superclu[1,]])
#m1=kohonenqualiclass.weight(prof,btrain1,dim(btrain1)[1],w)
trest=setdiff(traincand[,2],idtrain)
bmu.trest=kohonenqualiclass(prof[,-iddf],PEdat[trest,-iddf],dim(PEdat[trest,-iddf])[1])
bmu.com=cbind(c(bmu.w,bmu.trest),c(idtrain,trest))
bmu.com=bmu.com[order(match(bmu.com[,2],traincand[,2])),]
bmu=bmu.com[,1]
#clustrain=cbind(PEdat[traincand[,2],],bmu)[order(bmu),]
#cc=table(clustrain[,iddf],clustrain[,ncol(clustrain)])
#Les classfieurs de logit regression
nqual=unlist(apply(PEqual,2,function(x){x=as.factor(x);t=nlevels(x);t}))
idqual=cumsum(nqual)
disj=disj[,-idqual]

breg=cbind(PEquan[traincand[,2],3:(ncol(PEquan))],disj[traincand[,2],])
breg=cbind(breg,bmu)[order(bmu),]
breg=apply(breg,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
ubtk=ubTomek(breg[,-iddf], breg[,iddf], verbose = TRUE)
breg=breg[-ubtk$id.rm,]
for(i in 1:2){
  ubtk=ubTomek(breg[,-iddf], breg[,iddf], verbose = TRUE)
  breg=breg[-ubtk$id.rm,]
}
breg1=as.data.frame(breg[which(breg[,ncol(breg)]%in%c(1:30)),-ncol(breg)])
if(nrow(breg1)>2& nlevels(as.factor(breg1[,iddf]))==2){
  ubtk1=ubTomek(breg1[,-iddf], breg1[,iddf], verbose = TRUE)
  breg1=breg1[-ubtk1$id.rm,] 
}

wlog=c(rep(1,length(which(breg1$top_dft_auto_pro_corp==0))),rep(30,length(which(breg1$top_dft_auto_pro_corp==1))))
model=glm(top_dft_auto_pro_corp~ ., family = binomial(link = "logit"), data=breg1,control = list(maxit = 60))
#t=predict(model, breg1, type="response") 
#t[t<0.02]=0
#t[t>0.02]=1
#table(t,btest[,iddf])
bmu.test=kohonenqualiclass(prof[,-iddf],PEdat[-traincand[,2],-iddf],dim(PEdat[-traincand[,2],-iddf])[1])
btest=cbind(PEquan[-traincand[,2],3:(ncol(PEquan))],disj[-traincand[,2],])
btest=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
btest=cbind(btest,bmu.test)[order(bmu.test),]
btest1=as.data.frame(btest[which(btest[,ncol(btest)]%in%c(1:30)),])


t=predict(model, btest, type="response") 
table(t,btest[,iddf])
plot(roc( btest[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

kmeans(prof,4,nstart=25)
