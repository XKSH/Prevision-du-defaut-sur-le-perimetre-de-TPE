library(FactoClass)
library(glmnet)
library(pROC)
this.dir = dirname("parent.frame(2)$ofile")
setwd(this.dir)
source("gini plot.R")
PEquan.org=read.table(file = "avb.txt",header = TRUE,sep="\t",na.strings=c("","NA"))
#PEqual=read.table(file = "data_avb_quali.txt",header = TRUE,sep="\t")

#le traitment de données
nvquan=c("CATJUR_REF","COSEG_REF","TOP_FILLE_CONSO","top_actif","TOP_LBO_REF_AC","COD_ACT_REF", "COD_ALLUR_CAV_CLI","PERIMETRE",
         "TOP_INCID_SIREN", "COT_BDF_ENTREP","TOP_CLNT_ACTIF_CLI_DITG_2_C","TOP_INCD_12M_SIREN_C")
#setdiff(colnames(PEquan.org),intersect(colnames(PEquan.org),colnames(PEquan)))
disj=as.matrix(acm.disjonctif(PEquan.org[,which(colnames(PEquan.org)%in%nvquan)]))
PEquan.org=cbind(PEquan.org[,which(!(colnames(PEquan.org)%in%nvquan))],disj)
#disj1=as.matrix(acm.disjonctif(PEqual))
PEdat.org=cbind(PEquan.org[,3:(ncol(PEquan.org))] )#,disj1[which(PEquan.org[,which(colnames(PEquan.org)%in%c("TOP_FILLE_CONSO.0"))]==c(1)),]
PEdat.org= PEdat.org[,colSums(is.na(PEdat.org))<nrow(PEdat.org)]
PEdat.org=PEdat.org[,c(which(!colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")), which(colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")))]
ndisj=ncol(disj)+1#+ncol(disj1)

#PEdat.org=PEdat.org[-c(which(PEquan.org[which(PEquan.org[,"top_dft_auto_pro_corp"]==0),"SIREN"]%in%casdef[,"SIREN"])),]
library(sampling)
taille=0.7*as.data.frame(table(PEdat.org$top_dft_auto_pro_corp))[,2]
set.seed(17)
traincand=strata(PEdat.org, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")

btrain=PEdat.org[traincand[,2],]
btest=PEdat.org[-traincand[,2],]
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))

btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
#lasso logit
#x=rbind(btrain.complet[,-iddf],btest.complet[,-iddf])
x=btrain.complet[,-iddf]
#x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables))]
x=data.matrix(x,nrow(x))
#g2=c(btrain.complet[,iddf],btest.complet[,iddf])
g2=as.vector(btrain.complet[,iddf])
fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-07,standardize = TRUE)
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
YI=1*trainauc$sensitivities+trainauc$specificities-1
cfpoint=trainauc$thresholds[which.max(YI)]
plot(roc( g2,train.pre),col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
newx=btest.complet[,-iddf]
#newx=btest.complet[,which(colnames(btrain.complet)%in%c(importance_variables))]
test.pre=predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response")
test.pre=as.vector(test.pre)
plot(roc( btest.complet[,iddf],test.pre),col="red",lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
roc.train=roc(as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(train.pre))
roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(test.pre))
legend("bottomright", inset=.05, title="Régression logistique pénalisée ",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test,AUC=",round(roc.test$auc,3))),
       lty=c(1,1), col =c("green","red"), horiz=FALSE)
#Gini plot
op <- par(mfrow = c(2, 1))       

# independent of device size
res.pre=cbind(train.pre,btrain.complet[,iddf])
Gini.plot(res.pre,"Base Apprentissage")
res.pre=cbind(test.pre,btest.complet[,iddf])
Gini.plot(res.pre,"Base Test")
par(op)

t[t<cfpoint]=0
t[t>cfpoint]=1
table(t,btest.complet[,iddf])
#xgboost
#evalerror <- function(preds, dtrain) {
# labels <- getinfo(dtrain, "label")
# err <- as.numeric(sum(labels[which(labels==1)] != (preds > 0.031)[which(labels==1)]))/length(labels[which(labels==1)])
#  return(list(metric = "error", value = err))
#}
require(xgboost)
#1 btrain.complet est une liste
xtrain=do.call(cbind,btrain.complet[,-iddf])
xtest=do.call(cbind,btest.complet[,-iddf])
dtrain <- xgb.DMatrix(xtrain, label = btrain.complet[,iddf])
dtest <- xgb.DMatrix(xtest, label = btest.complet[,iddf])
watchlist <- list(eval = dtest, train = dtrain)
#2
dtrain <- xgb.DMatrix(xtrain[,which(colnames(xtrain)%in%c(importance_variables))], label = btrain.complet[,iddf])
dtest <- xgb.DMatrix(xtest[,which(colnames(xtest)%in%c(importance_variables))], label = btest.complet[,iddf])
#2 XGB raffiné
param <- list(max_depth = 4, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nround=160, watchlist)
#1 feature selection max_depth = 6, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, nround=350
param <- list(max_depth = 6, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nround=350, watchlist)

#bst= xgboost(data =btrain.complet[,-iddf] , label = btrain.complet[,iddf], max.depth = 4, eta = 0.1,gamma=0.1, nthread = 2, nround = 150, objective = "binary:logistic")
pred <- predict(bst, dtrain )
plot(roc(as.vector(btrain.complet[,iddf]),pred),col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
pred1 <- predict(bst, dtest)
plot(roc(as.vector(btest.complet[,iddf]),pred1),col="red",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
roc.train=roc(as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(pred))
roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(pred1))
legend("bottomright", inset=.05, title="Gradient Boosting ",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test,AUC=",round(roc.test$auc,3))),
       lty=c(1,1), col =c("green","red"), horiz=FALSE)
#Gini plot
op <- par(mfrow = c(2, 1))       

# independent of device size
res.pre=cbind(pred,btrain.complet[,iddf])
Gini.plot(res.pre,"Base Apprentissage")
res.pre=cbind(pred1,btest.complet[,iddf])
Gini.plot(res.pre,"Base Test")
par(op)


#random forest
library(randomForest)
# rtrain=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables))]
# rtrain=cbind(rtrain,btrain.complet[,iddf]);colnames(rtrain)[ncol(rtrain)]="top_dft_auto_pro_corp"
# rtest=btest.complet[,which(colnames(btest.complet)%in%c(importance_variables))]
# rtest=cbind(rtest,btest.complet[,iddf]);colnames(rtest)[ncol(rtest)]="top_dft_auto_pro_corp"
# #rtrain=data.frame(rtrain,check.names=TRUE);rtest=data.frame(rtest,check.names=TRUE);

colnames(btrain.complet)=make.names(colnames(btrain.complet))
colnames(btest.complet)=make.names(colnames(btest.complet))
btrain.complet=data.frame(btrain.complet)
btest.complet=data.frame(btest.complet)
set.seed(415)
ffit <- randomForest(as.factor(top_dft_auto_pro_corp) ~ .,
                     data=btrain.complet,#rtrain
                     importance=TRUE,nodesize=10,
                     ntree=800,classwt=c(0.5,0.5),maxnodes=25)#,maxnodes=20

prediction1<- predict(ffit,btrain.complet,type="prob")[,2]
plot(roc(as.vector(btrain.complet$top_dft_auto_pro_corp),prediction1),col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
prediction<- predict(ffit,btest.complet,type="prob")[,2]
plot(roc(as.vector(btest.complet$top_dft_auto_pro_corp),prediction),col="red",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
roc.train=roc( as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(prediction1))
roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(prediction))
legend("bottomright", inset=.05, title="Forêt aléatoire",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test,AUC=",round(roc.test$auc,3))),
       lty=c(1,1), col =c("green","red"), horiz=FALSE)
#Gini plot
op <- par(mfrow = c(2, 1))       

# independent of device size
res.pre=cbind(prediction1,btrain.complet[,iddf])
Gini.plot(res.pre,"Base Apprentissage")
res.pre=cbind(prediction,btest.complet[,iddf])
Gini.plot(res.pre,"Base Test")
par(op)


#Recursive Partitioning Trees
library(rpart)
#btrain$top_dft_auto_pro_corp=factor(btrain$top_dft_auto_pro_corp)
tree_fit = rpart(as.factor(top_dft_auto_pro_corp) ~ ., data = btrain,parms = list(prior = c(.7,.3), split = "information"))
#loss=matrix(c(0,3,1,0),byrow=TRUE, nrow=2),
#tree_fitp <- prune(tree_fit, cp = 0.05)
table(predict(tree_fit,btrain,type = "class"),btrain[,"top_dft_auto_pro_corp"])
table(predict(tree_fit,btest,type = "class"),btest[,"top_dft_auto_pro_corp"])
library(pROC)
roc.train=roc( as.numeric(btrain[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btrain,type = "prob")[,2]))
roc.test=roc(as.numeric(btest[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btest,type = "prob")[,2]))
plot(roc(as.numeric(btest[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btest,type = "prob")[,2])),col="red",lwd=3,main="Courbe ROC",print.auc=FALSE)
plot(roc( as.numeric(btrain[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btrain,type = "prob")[,2])),add=TRUE,col="green",lwd=3,main="Courbe ROC",print.auc=FALSE)
legend("bottomright", inset=.05, title="Arbre de classification",c(paste("Apprentissage, AUC=",round(roc.train$auc,3)),paste("Test,AUC=",round(roc.test$auc,3))),
       lty=c(1,1), col =c("green","red"), horiz=FALSE)
#Gini plot
op <- par(mfrow = c(2, 1))       

# independent of device size
res.pre=cbind(as.numeric(predict(tree_fit,btrain,type = "prob")[,2]),btrain[,iddf])
Gini.plot(res.pre,"Base Apprentissage")
res.pre=cbind(as.numeric(predict(tree_fit,btest,type = "prob")[,2]),btest[,iddf])
Gini.plot(res.pre,"Base Test")
par(op)


#MLP
btraintarget=decodeClassLabels(btrain.complet[,iddf])
btestarget=decodeClassLabels(btest.complet[,iddf])
set.seed(188)
model <- mlp(btrain.complet[,-iddf],btraintarget , size=c(6,2), learnFuncParams=c(0.1), 
             maxit=50, inputsTest=btest.complet[,-iddf], targetsTest=btestarget)
pred=predict(model,btrain.complet[,-iddf])
pred1=predict(model,btest.complet[,-iddf])
plot(roc(as.vector(btrain.complet[,iddf]),pred[,2]),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
plot(roc(as.vector(btest.complet[,iddf]),pred1[,2]),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
