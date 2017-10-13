# Continu de "base original_s.R" ligne32
#normalisation de PEdat.org en utilisant le total
source("Kohonenprep.R")
source("Kohonennet.R")
source("plotcarte1.R")
source("evaluation.R")
id_neg=c(traincand[which(traincand[,4]==1),2])
PEdat=PEdat.org
PEdat[,1:(ncol(PEdat)-ndisj)]=apply(PEdat[,1:(ncol(PEdat)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
#PEdat=PEdat[,which(colnames(PEdat)%in%c(importance_variables,"top_dft_auto_pro_corp"))]
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
id=which(rownames(btrain)%in%id_neg)
matd=do.call(cbind,btrain[id,-iddf])
nr=9;nc=9
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo(17,nr,nc,0.04,0.01,2.99,0.65,matd,dim(matd)[1])
bmu_neg=kohonenqualiclass(prof,btrain[id,-iddf],dim(PEdat[id_neg,])[1])
nbmu=nlevels(as.factor(bmu_neg))
##remplacement simple
btrain.complet=apply(PEdat[traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
btest.complet=apply(PEdat[-traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})

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
AICc
BIC<-log(n)*k - tLL
BIC
fit$lambda
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
#Gini plot
op <- par(mfrow = c(2, 1))       

# independent of device size
res.pre=cbind(train.pre,btrain.complet[,iddf])
Gini.plot(res.pre,"Base Apprentissage")
res.pre=cbind(test.pre,btest.complet[,iddf])
Gini.plot(res.pre,"Base Test")
par(op)


#
require(xgboost)
#1
dtrain <- xgb.DMatrix(dcode.train, label = btrain.complet[,iddf])
dtest <- xgb.DMatrix(dcode.test, label = btest.complet[,iddf])
watchlist <- list(eval = dtest, train = dtrain)

param <- list(max_depth =6, eta = 0.1, silent = 1,gamma=0.05, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nround=100, watchlist)
require(caret)
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 5, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = c(50,100),
                        max_depth = c(4,6,8),
                        eta = c(0.05,0.08,0.1),
                        gamma=c(0.05,0.08),
                        colsample_bytree=c(1),
                        min_child_weight=c(1),
                        subsample=c(1)
)
colnames(dcode.train)=c(sapply(1:(nr*nc),function(x){name=paste("v",x,sep="");return(name)}))
lev=as.factor(btrain.complet[,iddf])
levels(lev)=c("c0","c1")
set.seed(1234)
xgb_tune <-train(x= as.matrix(dcode.train), 
                 y = as.factor(lev),
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Kappa",
                 nthread =3
)

