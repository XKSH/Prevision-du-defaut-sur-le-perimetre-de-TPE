library(FactoClass)
library(glmnet)
library(pROC)
library(ff)
#Le total de variables incomplètes
this.dir = dirname("parent.frame(2)$ofile")
setwd(this.dir)
source("gini plot.R")
memory.limit(size = 20000)
PEquan.org=read.table(file = "temps.txt",header=TRUE,  sep="\t",na.strings=c("","NA"))
#le traitment de données
nvquan=c("CATJUR_REF","COSEG_REF","TOP_FILLE_CONSO","top_actif","TOP_LBO_REF_AC","COD_ACT_REF", "COD_ALLUR_CAV_CLI","PERIMETRE",
         "TOP_INCID_SIREN", "COT_BDF_ENTREP","TOP_CLNT_ACTIF_CLI_DITG_2","TOP_INCD_12M_SIREN")
disj=as.matrix(acm.disjonctif(PEquan.org[,which(colnames(PEquan.org)%in%nvquan)]))
PEquan.org=cbind(PEquan.org[,which(!(colnames(PEquan.org)%in%nvquan))],disj)
PEquan.org <- PEquan.org[,colSums(is.na(PEquan.org))<nrow(PEquan.org)]
PEdat.org=PEquan.org[,3:(ncol(PEquan.org))] 
PEdat.org=PEdat.org[,c(which(!colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")), which(colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")))]
ndisj=ncol(disj)+1
nvx=colnames(PEdat.org)
rm(disj)
rm(PEquan.org)
#sampling
library(sampling)
taille=0.7*as.data.frame(table(PEdat.org$top_dft_auto_pro_corp))[,2]
set.seed(17)
traincand=strata(PEdat.org, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")

btrain=PEdat.org[traincand[,2],]
btest=PEdat.org[-traincand[,2],]
btrain=sapply(btrain,as.numeric)
btest=sapply(btest,as.numeric)
# btrain=as.data.frame(btrain)
# sapply(btrain,class)
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
rm(btrain);rm(btest);rm(PEdat.org)
x=btrain.complet[,-iddf]
#x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables))]
x=data.matrix(x,nrow(x))
#g2=c(btrain.complet[,iddf],btest.complet[,iddf])
g2=as.vector(btrain.complet[,iddf])
fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-8,standardize = TRUE)#grandir 'thresh' pour effectuer early stopping
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
#formula:https://community.jmp.com/t5/Discussions/getting-AIC-likelihood-from-model-coefficients/td-p/31056
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc
BIC<-log(n)*k - tLL
BIC
fit$lambda
plot(fit,xvar="lambda",label=TRUE)
train.pre=predict(fit, x,s=   fit$lambda[which.min(AICc)], type="response")
train.pre=as.vector(train.pre)
trainauc=roc( g2,train.pre)
# YI=1*trainauc$sensitivities+trainauc$specificities-1
# cfpoint=trainauc$thresholds[which.max(YI)]
plot(trainauc,col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
newx=btest.complet[,-iddf]
#newx=btest.complet[,which(colnames(btrain.complet)%in%c(importance_variables))]
test.pre=predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response")
test.pre=as.vector(test.pre)
testauc=roc( btest.complet[,iddf],test.pre)
plot(testauc,col="red",lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
roc.train=trainauc
roc.test=testauc
rm(x);rm(newx);rm(btest.complet);rm(btrain.complet)
#hors temps
hors.org=read.table(file = "horstemps.txt",header=TRUE,  sep="\t",na.strings=c("","NA"))
disj1=as.matrix(acm.disjonctif(hors.org[,which(colnames(hors.org)%in%nvquan)]))
hors.org=cbind(hors.org[,which(!(colnames(hors.org)%in%nvquan))],disj1)
hors.org <- hors.org[,colSums(is.na(hors.org))<nrow(hors.org)]
hors.org=hors.org[,3:(ncol(hors.org))] 
hors.org=hors.org[,c(which(!colnames(hors.org)%in%c("top_dft_auto_pro_corp")), which(colnames(hors.org)%in%c("top_dft_auto_pro_corp")))]
ndisj1=ncol(disj1)+1
rm(disj1)
# hors.org=cbind(hors.org[,which(!(colnames(hors.org)%in%nvquan))],disj)
# #disj1=as.matrix(acm.disjonctif(PEqual))
# hors.org=hors.org[,3:(ncol(hors.org))]#,disj1[which(hors.org[,which(colnames(hors.org)%in%c("TOP_FILLE_CONSO.0"))]==c(1)),]
# hors.org=hors.org[,c(which(!colnames(hors.org)%in%c("top_dft_auto_pro_corp")), which(colnames(hors.org)%in%c("top_dft_auto_pro_corp")))]
hors.org=sapply(hors.org,as.numeric)
hors.org[,1:(ncol(hors.org)-ndisj1)]=apply(hors.org[,1:(ncol(hors.org)-ndisj1)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
hors.complet=apply(hors.org,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
rm(hors.org)
g=hors.complet[,which(colnames(hors.complet) %in% c("top_dft_auto_pro_corp"))]
horsx=hors.complet
rm(hors.complet)
vdiff=setdiff(nvx, colnames(horsx))
vsect=intersect(nvx,colnames(horsx))
if(length(vdiff)>0){
  xsup=matrix(0,nrow=nrow(horsx),ncol=length(vdiff))
  colnames(xsup)=vdiff
}
horsx=cbind(horsx[,vsect],xsup)
iddf=which(colnames(horsx) %in% c("top_dft_auto_pro_corp"))
horsx=horsx[,-iddf]
hors.pre=predict(fit, horsx,s=fit$lambda[which.min(AICc)],type="response")
hors.pre=as.vector(hors.pre)
roc.hors=roc(g,as.numeric(hors.pre))
plot(roc.hors,col="purple",lwd=3,print.auc=FALSE,add=TRUE)
legend("bottomright", inset=.05, title="Régression logistique pénalisée ",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test, AUC=",round(roc.test$auc,3)),paste("Horstemps, AUC=",round(roc.hors$auc,3))),
       lty=c(1,1,1), col =c("green","red","purple"), horiz=FALSE)