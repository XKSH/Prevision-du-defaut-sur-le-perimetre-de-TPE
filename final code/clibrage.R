#calibrage de paramètre par quadrillage
#carte non supervisée
#si on a besoin de RI indice, attacher "mclust" package
#indice de performance: purity et normalized mutual information  
matd=do.call(cbind,btrain[,-iddf])
gridsearch.som=function(nit){
  source("Kohonenprep.R")
  source("Kohonennet.R")
  source("plotcarte1.R")
  source("evaluation.R")
  nr=para[nit,5];nc=para[nit,6]
  set.seed(15);mat=matd[sample(nrow(matd)),]
  prof=kohonenqualigo(17,nr,nc,para[nit,1],para[nit,2],para[nit,3],para[nit,4],mat,dim(mat)[1])
  mtrain=kohonenqualiclass(prof[,-iddf],btrain[,-iddf],dim(btrain[,-iddf])[1])
  mtest=kohonenqualiclass(prof[,-iddf],btest[,-iddf],dim(btest[,-iddf])[1])
  superclu=table(btrain[,iddf],mtrain)
  #cluster evaluation
  t=c(as.numeric(nmi(superclu)),as.numeric(purity(superclu)),as.numeric(nmi(table(btest[,iddf],mtest))),
      as.numeric(purity(table(btest[,iddf],mtest))))
  return(t)
}
#exemple
n0=c(0.04,0.05);nt=c(0.01,0.015);r0=c(3);rt=c(0.6)
ax=c(6,7);ay=c(6,7)
para=expand.grid(n0,nt,r0,rt,ax,ay)
library(doSNOW)
qend=c(1:floor(nrow(para)/8)*8,nrow(para))
qstart=0:floor(nrow(para)/8)*8+1
for(j in 1:2){
  threads =4
  cl =makeCluster(threads)
  registerDoSNOW(cl)
  result = foreach(i=qstart[j]:qend[j]) %dopar% gridsearch.som(i)
  stopCluster(cl)
  result=do.call(rbind,result)
  tpara=para[qstart[j]:qend[j],]
  write.table(cbind(tpara,result), file =paste(paste("somtuning",j),".txt",sep=""), sep = " ")
}

#continu de "traitement de donnée.R"
#carte supervisée
matd=do.call(cbind,btrain[,-iddf])
gridsearch.ssom=function(nit){
  source("Kohonenprep.R")
  source("Kohonennet.R")
  source("plotcarte1.R")
  source("evaluation.R")
  source("Kohonennet_weight.R")
  source("dist.R")
  nr=para[nit,5];nc=para[nit,6]
  w=c(rep((para[nit,7]*(dim(btrain)[2]-ndisj)),(dim(btrain)[2]-ndisj)),rep((para[nit,8]*(ndisj-1)),(ndisj-1)),para[nit,9])
  set.seed(15);mat=matd[sample(nrow(matd)),]
  prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,mat,dim(mat)[1],w)
  mtrain=kohonenqualiclass(prof[,-iddf],btrain[,-iddf],dim(btrain[,-iddf])[1])
  mtest=kohonenqualiclass(prof[,-iddf],btest[,-iddf],dim(btest[,-iddf])[1])
  superclu=table(btrain[,iddf],mtrain)
  #cluster evaluation
  t=c(as.numeric(nmi(superclu)),as.numeric(purity(superclu)),as.numeric(nmi(table(btest[,iddf],mtest))),
      as.numeric(purity(table(btest[,iddf],mtest))))
  return(t)
}

#exemple
repmat = function(X,m,n){
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)} 

n0=c(0.04,0.05);nt=c(0.01,0.015);r0=c(3);rt=c(0.6)
ax=c(6,7);ay=c(6,7)
para=expand.grid(n0,nt,r0,rt,ax,ay)
w=matrix(c(0.1,0.2,0.1,0.1,0.8,0.7),nrow=2,ncol=3)
para=cbind(repmat(as.matrix(para),nrow(w),1),w[rep(seq_len(nrow(w)), each=nrow(para)),])
library(doSNOW)
qend=c(1:floor(nrow(para)/8)*8,nrow(para))
qstart=0:floor(nrow(para)/8)*8+1
for(j in 1:2){
  threads =4
  cl =makeCluster(threads)
  registerDoSNOW(cl)
  result = foreach(i=qstart[j]:qend[j]) %dopar% gridsearch.ssom(i)
  stopCluster(cl)
  result=do.call(rbind,result)
  tpara=para[qstart[j]:qend[j],]
  write.table(cbind(tpara,result), file =paste(paste("ssomtuning",j),".txt",sep=""), sep = " ")
}
#apprentissage hybride
####
#continu de "traitement de donnée.R"
id_neg=c(traincand[which(traincand[,4]==1),2])
PEdat=PEdat.org
PEdat[,1:(ncol(PEdat)-ndisj)]=apply(PEdat[,1:(ncol(PEdat)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
id=which(rownames(btrain)%in%id_neg)
matd=do.call(cbind,btrain[id,-iddf])
####
gridsearch.hy=function(nit){
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
  #btest.complet=apply(PEdat[-traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
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
#on change qstart et qend pour le calibrage de nombre différent de paramètres
qend=c(1:floor(nrow(para)/40)*40,nrow(para))
qstart=0:floor(nrow(para)/40)*40+1
for(j in 1:2){
  threads =4
  cl =makeCluster(threads)
  registerDoSNOW(cl)
  result = foreach(i=qstart[j]:qend[j]) %dopar% gridsearch.hy(i)
  stopCluster(cl)
  result=do.call(rbind,result)
  tpara=para[qstart[j]:qend[j],]
  write.table(cbind(tpara,result), file =paste(paste("tuning",j),".txt",sep=""), sep = " ")
}
