#les hyperparamètres sont estimés en utilisant "calibrage.R"
voisin<-function(vec,nb,nbnn)
{
  dista=as.matrix(dist(vec,method = "euclidean"))
  dMat=as.matrix(dista)
  idx=matrix(rep(0,nbnn*nb),ncol=nb)
  for(i in 1:nb){
    idx[,i]=head(order(dMat[-i,i]),nbnn)+1
  }
  return(idx)
}
library(FactoClass)
library(glmnet)
library(pROC)
this.dir = dirname("parent.frame(2)$ofile")
setwd(this.dir)
source("gini plot.R")
PEquan.org=read.table(file = "avb.txt",header = TRUE,sep="\t",na.strings=c("","NA"))
#le traitment de données
nvquan=c("CATJUR_REF","COSEG_REF","TOP_FILLE_CONSO","top_actif","TOP_LBO_REF_AC","COD_ACT_REF", "COD_ALLUR_CAV_CLI","PERIMETRE",
         "TOP_INCID_SIREN", "COT_BDF_ENTREP","TOP_CLNT_ACTIF_CLI_DITG_2_C","TOP_INCD_12M_SIREN_C")
disj=c()
for(i in 1:length(nvquan)){
  t=as.matrix(acm.disjonctif(as.matrix(PEquan.org[,which(colnames(PEquan.org)%in%nvquan[i])],ncol=1)))
  t[which(rowSums(t)==0),]=NA
  disj=cbind(disj,t)
}
PEquan.org=cbind(PEquan.org[,which(!(colnames(PEquan.org)%in%nvquan))],disj)
PEdat.org=cbind(PEquan.org[,3:(ncol(PEquan.org))] )#,disj1[which(PEquan.org[,which(colnames(PEquan.org)%in%c("TOP_FILLE_CONSO.0"))]==c(1)),]
PEdat.org= PEdat.org[,colSums(is.na(PEdat.org))<nrow(PEdat.org)]
PEdat.org=PEdat.org[,c(which(!colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")), which(colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")))]
ndisj=ncol(disj)+1
library(sampling)
taille=0.7*as.data.frame(table(PEdat.org$top_dft_auto_pro_corp))[,2]
set.seed(17)
traincand=strata(PEdat.org, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")

btrain=PEdat.org[traincand[,2],]
btest=PEdat.org[-traincand[,2],]
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))

source("Kohonenprep.R")
source("Kohonennet.R")
source("plotcarte1.R")
source("evaluation.R")
library(e1071)
library(glmnet)
library(pROC)
####regroupement des clients défaillants
id_neg=c(traincand[which(traincand[,4]==1),2])
PEdat=PEdat.org
PEdat[,1:(ncol(PEdat)-ndisj)]=apply(PEdat[,1:(ncol(PEdat)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
id=which(rownames(btrain)%in%id_neg)
matd=do.call(cbind,btrain[id,-iddf])
nr=9;nc=9
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo(17,nr,nc,0.041,0.02,5,0.8,matd,dim(matd)[1])
bmu_neg=kohonenqualiclass(prof,btrain[id,-iddf],dim(PEdat[id_neg,])[1])
nbmu=nlevels(as.factor(bmu_neg))
#####
###remplacement en chaque noeud
btrain.complet=btrain
#matrice de median de base d'apprentissage
mm0=matrix(rep(NA,ncol(btrain.complet)*nbmu),ncol=nbmu)
mm1=matrix(rep(NA,ncol(btrain.complet)*1),ncol=1)
for(i in 1:nbmu){
  idx0=match(c(id_neg[which(bmu_neg==unique(bmu_neg)[i])]),rownames(btrain.complet))
  mm0[,i]=apply(btrain.complet[idx0,],2,function(x){t=median(x,na.rm = TRUE); return(t)})
}
idx1=match(c(traincand[which(traincand[,4]==2),2]),rownames(btrain.complet))
mm1[,1]=apply(btrain.complet[idx1,],2,function(x){t=median(x,na.rm = TRUE); return(t)})
profclu=prof[sort(as.numeric(unique(bmu_neg))),]
vind=voisin(profclu,nbmu,nbmu-1)

for(i in 1:nbmu){
  mq=as.numeric(which(is.na(mm0[,i])==TRUE))
  if(length(mq)>0){
    for(j in 1:length(mq)){
      rmq=which(is.na(mm0[mq[j],])==FALSE)
      idx=vind[which(vind[,i] %in% rmq)[1],i]
      mm0[mq[j],i]=mm0[mq[j],idx]
    }
  }
}
# any(is.na(mm1[,1]))
idx=match(c(traincand[which(traincand[,4]==2),2]),rownames(btrain.complet))
btrain.complet[idx,]=t(apply(btrain.complet[idx,],1,function(x){x[is.na(x)]=mm1[is.na(x),1];return(x)}))
for(i in 1:nbmu){
  idx=match(c(id_neg[which(bmu_neg==unique(bmu_neg)[i])]),rownames(btrain.complet))
  btrain.complet[idx,]=t(apply(btrain.complet[idx,],1,function(x){x[is.na(x)]=mm0[is.na(x),i];return(x)}))
}
#remplacement de base de test
####
btest.complet=btest
bmu_test=kohonenqualiclass(prof,btest[id,-iddf],dim(btest)[1])
nbmu_test=nlevels(as.factor(bmu_test))

mm=matrix(rep(NA,ncol(btest)*nbmu_test),ncol=nbmu_test)
for(i in 1:nbmu_test){
  idx=c(which(bmu_test==unique(bmu_test)[i]))
  mm[,i]=apply(btrain.complet[idx,],2,function(x){t=median(x,na.rm = TRUE); return(t)})
}
for(i in 1:nbmu_test){
  mq=as.numeric(which(is.na(mm[,i])==TRUE))
  if(length(mq)>0){
    for(j in 1:length(mq)){
      rmq=which(is.na(mm[mq[j],])==FALSE)
      idx=vind[which(vind[,i] %in% rmq)[1],i]
      mm[mq[j],i]=mm[mq[j],idx]
    }
  }
}
for(i in 1:nbmu_test){
  idx=c(which(bmu_test==unique(bmu_test)[i]))
  btest.complet[idx,]=t(apply(btest.complet[idx,],1,function(x){x[is.na(x)]=mm[is.na(x),i];return(x)}))
}

#encoder
library(e1071)
svmfit=list()
svmtrain=list()
for(i in 1:nbmu){
  svmtrain[[i]]=btrain.complet[match(c(id_neg[which(bmu_neg==unique(bmu_neg)[i])],traincand[which(traincand[,4]==2),2]),rownames(btrain.complet)),]#rownames(btrain.complet)
  #modelname=paste("svmfit",i)
  #assign(modelname ,svm(top_dft_auto_pro_corp ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE))
  svmfit[[i]]=svm(as.factor(top_dft_auto_pro_corp) ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE)
}
dcode.test=matrix(rep(0,dim(btest.complet)[1]*nbmu),ncol=nbmu)
dcode.train=matrix(rep(0,dim(btrain.complet)[1]*nbmu),ncol=nbmu)
for(i in 1:nbmu){
  pred=predict(svmfit[[i]], btest.complet[,-iddf],decision.values = TRUE)
  dcode.test[,i]=attr(pred, "decision.values")
  pred1=predict(svmfit[[i]], btrain.complet[,-iddf],decision.values = TRUE)
  dcode.train[,i]=attr(pred1, "decision.values")
}

#
library(glmnet)
library(pROC)
x=dcode.train
g=as.vector(btrain.complet[,iddf])
fit=glmnet(x,g,family="binomial",alpha=0,nlambda=200,thresh = 1e-07,standardize = FALSE,maxit=150000,type.logistic="modified.Newton")
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
BIC<-log(n)*k - tLL
plot(fit,xvar="lambda",label=TRUE)
train.pre=predict(fit, x,s= fit$lambda[which.min(AICc)], type="response")
train.pre=as.vector(train.pre)
plot(roc( g,train.pre),col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
newx=dcode.test
newx=data.matrix(newx,nrow(newx))
test.pre=predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response")
test.pre=as.vector(test.pre)
plot(roc(btest.complet[,iddf],test.pre),col="red",lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
roc.train=roc(as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(train.pre))
roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(test.pre))
legend("bottomright", inset=.05, title="Méthode hybride",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test,AUC=",round(roc.test$auc,3))),
       lty=c(1,1), col =c("green","red"), horiz=FALSE)
