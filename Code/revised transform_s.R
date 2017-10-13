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
##grille de paramètres
# r0=c(0.04,0.05);rt=c(0.01,0.02);n0=c(2.5,3.5);nt=c(0.6,0.7)
# ax=c(6);ay=c(5,6)
# para=expand.grid(r0,rt,n0,nt,ax,ay)
id_neg=c(traincand[which(traincand[,4]==1),2])
PEdat=PEdat.org
PEdat[,1:(ncol(PEdat)-ndisj)]=apply(PEdat[,1:(ncol(PEdat)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
#PEdat=PEdat[,which(colnames(PEdat)%in%c(importance_variables,"top_dft_auto_pro_corp"))]
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
id=which(rownames(btrain)%in%id_neg)
matd=do.call(cbind,btrain[id,-iddf])
# res.train=rep(0,nrow(para))
# res.test=rep(0,nrow(para))
##
gridsearch=function(nit){
  nr=para[nit,5];nc=para[nit,6]
  set.seed(15);mat=matd[sample(nrow(matd)),]
  prof=kohonenqualigo(17,nr,nc,para[nit,1],para[nit,2],para[nit,3],para[nit,4],mat,dim(mat)[1])
  bmu_neg=kohonenqualiclass(prof,btrain[id,-iddf],dim(PEdat[id_neg,])[1])
  nbmu=nlevels(as.factor(bmu_neg))
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
  # btest.complet=apply(PEdat[-traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
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
  
  
  # apply(btrain.complet,2,function(x){t=any(is.na(x));return(t)})
  # apply(mm0,2,function(x){t=any(is.na(x));return(t)})
  
  #encoder
  library(e1071)
  svmfit=list()
  svmtrain=list()
  for(i in 1:nbmu){
    svmtrain[[i]]=btrain.complet[match(c(id_neg[which(bmu_neg==unique(bmu_neg)[i])],traincand[which(traincand[,4]==2),2]),rownames(btrain.complet)),]#rownames(btrain.complet)
    #modelname=paste("svmfit",i)
    #assign(modelname ,svm(top_dft_auto_pro_corp ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE))
    svmfit[[i]]=e1071:::svm(as.factor(top_dft_auto_pro_corp) ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE)
  }
  dcode.test=matrix(rep(0,dim(btest.complet)[1]*nbmu),ncol=nbmu)
  dcode.train=matrix(rep(0,dim(btrain.complet)[1]*nbmu),ncol=nbmu)
  for(i in 1:nbmu){
    pred=predict(svmfit[[i]], btest.complet[,-iddf],decision.values = TRUE)
    dcode.test[,i]=attr(pred, "decision.values")
    pred1=predict(svmfit[[i]], btrain.complet[,-iddf],decision.values = TRUE)
    dcode.train[,i]=attr(pred1, "decision.values")
  }
  
  #glmnet
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
  train.pre=predict(fit, x,s= fit$lambda[which.min(AICc)], type="response")
  train.pre=as.vector(train.pre)
  newx=dcode.test
  newx=data.matrix(newx,nrow(newx))
  test.pre=predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response")
  test.pre=as.vector(test.pre)
  roc.train=roc(as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(train.pre))
  roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(test.pre))
  t=c(as.numeric(roc.train$auc),as.numeric(roc.test$auc))
  return(t)
}

r0=c(0.02,0.03,0.04,0.05);rt=c(0.005,0.01,0.015);n0=c(3,3.5);nt=c(0.5,0.6,0.65)
ax=c(6,7);ay=c(6,7)
para=expand.grid(r0,rt,n0,nt,ax,ay)
library(doSNOW)
qend=c(1:floor(nrow(para)/40)*40,nrow(para))
qstart=0:floor(nrow(para)/40)*40+1
for(j in 5:6){
  threads =4
  cl =makeCluster(threads)
  registerDoSNOW(cl)
  result = foreach(i=qstart[j]:qend[j]) %dopar% gridsearch(i)
  stopCluster(cl)
  result=do.call(rbind,result)
  tpara=para[qstart[j]:qend[j],]
  write.table(cbind(tpara,result), file =paste(paste("tuning",j),".txt",sep=""), sep = " ")
}

r0=c(0.04,0.041);rt=c(0.02,0.01);n0=c(5,3.5);nt=c(0.8,0.6,0.65)
ax=c(7,8);ay=c(7,8)
para=expand.grid(r0,rt,n0,nt,ax,ay)
threads =4
cl =makeCluster(threads)
registerDoSNOW(cl)
result = foreach(i=1:nrow(para)) %dopar% gridsearch(i)
stopCluster(cl)
result=do.call(rbind,result)
tpara=para
write.table(cbind(tpara,result), file =paste("tuning_24",".txt", sep = " "))


# [[1]]
# [1] 0.9999996 0.7144218
# 
# [[2]]
# [1] 0.9840918 0.6592566
# 
# [[3]]
# [1] 0.9986120 0.6374759
# 
# [[4]]
# [1] 0.9984021 0.6405407
plot(roc( g,train.pre),col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
plot(roc(btest.complet[,iddf],test.pre),col="red",lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
legend("bottomright", inset=.05, title="Méthode hybride",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test,AUC=",round(roc.test$auc,3))),
       lty=c(1,1), col =c("green","red"), horiz=FALSE)

#0.04 0.01  2.5  0.7    6    5     0.04,0.01,3.5,0.65 6 7
id_neg=c(traincand[which(traincand[,4]==1),2])
PEdat=PEdat.org
PEdat[,1:(ncol(PEdat)-ndisj)]=apply(PEdat[,1:(ncol(PEdat)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
#PEdat=PEdat[,which(colnames(PEdat)%in%c(importance_variables,"top_dft_auto_pro_corp"))]
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
id=which(rownames(btrain)%in%id_neg)
matd=do.call(cbind,btrain[id,-iddf])
nr=6;nc=5
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo(17,nr,nc,0.04,0.01,2.5,0.6,matd,dim(matd)[1])
bmu_neg=kohonenqualiclass(prof,btrain[id,-iddf],dim(PEdat[id_neg,])[1])
nbmu=nlevels(as.factor(bmu_neg))
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
# btest.complet=apply(PEdat[-traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
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


# apply(btrain.complet,2,function(x){t=any(is.na(x));return(t)})
# apply(mm0,2,function(x){t=any(is.na(x));return(t)})

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
#t=predict(fit, x,s= 1.420652e-03, type="response")
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


# 1  0.04 0.01  2.5  0.6    6    5
# 2  0.05 0.01  2.5  0.6    6    5
# 3  0.04 0.02  2.5  0.6    6    5
# 4  0.05 0.02  2.5  0.6    6    5
# 5  0.04 0.01  3.5  0.6    6    5
# 6  0.05 0.01  3.5  0.6    6    5
# 7  0.04 0.02  3.5  0.6    6    5
# 8  0.05 0.02  3.5  0.6    6    5
# 9  0.04 0.01  2.5  0.7    6    5
# 10 0.05 0.01  2.5  0.7    6    5
# 11 0.04 0.02  2.5  0.7    6    5
# 12 0.05 0.02  2.5  0.7    6    5
# 13 0.04 0.01  3.5  0.7    6    5
# 14 0.05 0.01  3.5  0.7    6    5
# 15 0.04 0.02  3.5  0.7    6    5
# 16 0.05 0.02  3.5  0.7    6    5
# 17 0.04 0.01  2.5  0.6    6    6
# 18 0.05 0.01  2.5  0.6    6    6
# 19 0.04 0.02  2.5  0.6    6    6
# 20 0.05 0.02  2.5  0.6    6    6
# 21 0.04 0.01  3.5  0.6    6    6
# 22 0.05 0.01  3.5  0.6    6    6
# 23 0.04 0.02  3.5  0.6    6    6
# 24 0.05 0.02  3.5  0.6    6    6
# 25 0.04 0.01  2.5  0.7    6    6
# 26 0.05 0.01  2.5  0.7    6    6
# 27 0.04 0.02  2.5  0.7    6    6
# 28 0.05 0.02  2.5  0.7    6    6
# 29 0.04 0.01  3.5  0.7    6    6
# 30 0.05 0.01  3.5  0.7    6    6
# 31 0.04 0.02  3.5  0.7    6    6
# 32 0.05 0.02  3.5  0.7    6    6
# 
# 
# [[1]]
# [1] 0.9999996 0.7144218
#
# [[2]]
# [1] 0.9999654 0.7032714
#
# [[3]]
# [1] 0.9999929 0.7216910
# 
# [[4]]
# [1] 0.9999925 0.7249488
# 
# [[5]]
# [1] 0.9008443 0.7499737
# 
# [[6]]
# [1] 0.9839092 0.6868860
# 
# [[7]]
# [1] 0.9999941 0.7235939
# 
# [[8]]
# [1] 0.9999825 0.7229127
# 
# [[9]]
# [1] 0.9840918 0.6592566
# 
# [[10]]
# [1] 0.8500941 0.7773918
# 
# [[11]]
# [1] 0.9999986 0.7255326
# 
# [[12]]
# [1] 0.9999888 0.7274491
# 
# [[13]]
# [1] 0.9303628 0.7243129
# 
# [[14]]
# [1] 0.9999873 0.7145077
# 
# [[15]]
# [1] 0.9999955 0.7287450
# 
# [[16]]
# [1] 0.9999968 0.7197118
# 
# [[17]]
# [1] 0.9986120 0.6374759
# 
# [[18]]
# [1] 0.9978042 0.6289657
# 
# [[19]]
# [1] 0.9999914 0.7107944
# 
# [[20]]
# [1] 0.99999 0.69565
# 
# [[21]]
# [1] 0.8648463 0.7899182
# 
# [[22]]
# [1] 0.9039980 0.7710943
# 
# [[23]]
# [1] 0.9999941 0.7242289
# 
# [[24]]
# [1] 0.9998039 0.7223510
# 
# [[25]]
# [1] 0.9984021 0.6405407
# 
# [[26]]
# [1] 0.9975909 0.7050045
# 
# [[27]]
# [1] 0.9999954 0.7217798
# 
# [[28]]
# [1] 0.9998638 0.7214941
# 
# [[29]]
# [1] 0.9999354 0.7209885
# 
# [[30]]
# [1] 0.8674784 0.7762338
# 
# [[31]]
# [1] 0.9999971 0.7205610
# 
# [[32]]
# [1] 0.9999932 0.7198199