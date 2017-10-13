## Continu de "base original_s.R" ligne 35
#imporatant variables subset for prediction
nbmax=172
importance_variables=list()
for(i in 1:7){
  importance_variables[[i]]=rownames(varIMP[order(varIMP[,i],decreasing = TRUE),])
  importance_variables[[i]]= importance_variables[[i]][1:min(nbmax,length(which(varIMP[,i]!=0)))]
}
colp=palette(rainbow(8))
###glmnet imp
gfit=list()
glambda=rep(0,8)
gauc=rep(0,8)
x=btrain.complet[,-iddf]
x=data.matrix(x,nrow(x))
g2=as.vector(btrain.complet[,iddf])
fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-07,standardize = TRUE)
gfit[[1]]=fit
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
glambda[1]=fit$lambda[which.min(AICc)]
train.pre=predict(fit, x,s= fit$lambda[which.min(AICc)], type="response")
train.pre=as.vector(train.pre)
plot(roc( g2,train.pre),col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
gauc[1]=roc( g2,train.pre)$auc[1]
for(i in 1:7){
    x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables[[i]]))]
    x=data.matrix(x,nrow(x))
    g2=as.vector(btrain.complet[,iddf])
    fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-07,standardize = TRUE)
    gfit[[i+1]]=fit
    tLL <- fit$nulldev - deviance(fit)
    k <- fit$df
    n <- fit$nobs
    AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
    glambda[i+1]=fit$lambda[which.min(AICc)]
    train.pre=predict(fit, x,s=   fit$lambda[which.min(AICc)], type="response")
    train.pre=as.vector(train.pre)
    plot(roc( g2,train.pre),col=colp[i+1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
    gauc[i+1]=roc(g2,train.pre)$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(gauc[i],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(gauc[i+1],4)))
}
legend(0.4, 0.45,title="Régression logistique pénalisée",ct, col = colp,
       lty = rep(1,8),cex=0.6)

gauc1=rep(0,8)
newx=btest.complet[,-iddf]
g2=as.vector(btest.complet[,iddf])
test.pre=predict(gfit[[1]], newx,s=glambda[1],type="response")
test.pre=as.vector(test.pre)
plot(roc( g2,test.pre),col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
gauc1[1]=roc( g2,test.pre)$auc[1]
for(i in 1:7){
  newx=btest.complet[,which(colnames(btest.complet)%in%c(importance_variables[[i]]))]
  g2=as.vector(btest.complet[,iddf])
  test.pre=predict(gfit[[i+1]], newx,s=glambda[i+1],type="response")
  test.pre=as.vector(test.pre)
  plot(roc( g2,test.pre),col=colp[i+1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  gauc1[i+1]=roc( g2,test.pre)$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(gauc1[i],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(gauc1[i+1],4)))
}
legend(0.4, 0.45,title="Régression logistique pénalisée",ct, col = colp,
       lty = rep(1,8),cex=0.6)

###decision tree imp
library(rpart)
treefit=list()
treeauc=rep(0,8)
btrain$top_dft_auto_pro_corp=factor(btrain$top_dft_auto_pro_corp)
tree_fit = rpart(top_dft_auto_pro_corp ~ ., data = btrain,parms = list(prior = c(.7,.3), split = "information"))
treefit[[1]]=tree_fit
roc.train=roc( as.numeric(btrain[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btrain,type = "prob")[,2]))
treeauc[1]=roc.train$auc[1]
plot(roc( as.numeric(btrain[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btrain,type = "prob")[,2])),col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE)

for( i in 1:7){
  x=btrain[,which(colnames(btrain)%in%c(importance_variables[[i]]))]
  x=cbind(x,btrain[,iddf])
  colnames(x)[ncol(x)]="top_dft_auto_pro_corp"
  tree_fit = rpart(top_dft_auto_pro_corp ~ ., data = x,parms = list(prior = c(.7,.3), split = "information"))
  treefit[[i+1]]=tree_fit
  roc.train=roc( as.numeric(btrain[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,btrain,type = "prob")[,2]))
  treeauc[i+1]=roc.train$auc[1]
  plot(roc.train,col=colp[1+i],lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
  }
ct=paste(paste("Modèle complet",",AUC="),round(treeauc[1],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(treeauc[i+1],4)))
}
legend(0.4, 0.45,title="Arbre de décision",ct, col = colp,
       lty = rep(1,8),cex=0.6)

treeauc1=rep(0,8)
newx=btest
roc.test=roc(as.numeric(btest[,"top_dft_auto_pro_corp"]),as.numeric(predict(treefit[[1]],newx,type = "prob")[,2]))
treeauc1[1]=roc.test$auc[1]
plot(roc.test,col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE)
for(i in 1:7){
  newx=btest[,which(colnames(btest)%in%c(importance_variables[[i]]))]
  newx=cbind(newx,btest[,iddf])
  colnames(newx)[ncol(newx)]="top_dft_auto_pro_corp"
  roc.test=roc(as.numeric(btest[,"top_dft_auto_pro_corp"]),as.numeric(predict(treefit[[i+1]],newx,type = "prob")[,2]))
  plot(roc.test,col=colp[1+i],lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
  treeauc1[i+1]=roc.test$auc[1]
  }
ct=paste(paste("Modèle complet",",AUC="),round(treeauc1[1],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(treeauc1[i+1],4)))
}
legend(0.4, 0.45,title="Arbre de décision",ct, col = colp,
       lty = rep(1,8),cex=0.6)

##rf Imp
library(randomForest)
rffit=list()
rfauc=rep(0,8)
x=btrain.complet
colnames(x)=make.names(colnames(x))
x=data.frame(x)
set.seed(415)
ffit <- randomForest(as.factor(top_dft_auto_pro_corp) ~ .,
                     data=x,
                     importance=TRUE,nodesize=10,
                     ntree=800,classwt=c(0.5,0.5),maxnodes=25)#,maxnodes=20
rffit[[1]]=ffit
roc.train=roc(btrain.complet[,iddf],as.numeric(predict(ffit,x,type="prob")[,2])) 
plot(roc.train,col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
rfauc[1]=roc.train$auc[1]
for( i in 1:7){
  x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables[[i]]))]
  x=cbind(x,btrain.complet[,iddf])
  colnames(x)[ncol(x)]="top_dft_auto_pro_corp"
  x=data.frame(x)
  set.seed(415)
  ffit <- randomForest(as.factor(top_dft_auto_pro_corp) ~ .,
                       data=x,
                       importance=TRUE,nodesize=10,
                       ntree=800,classwt=c(0.5,0.5),maxnodes=25)#,maxnodes=20
  rffit[[i+1]]=ffit
  roc.train=roc(btrain.complet[,iddf],as.numeric(predict(ffit,x,type="prob")[,2])) 
  rfauc[i+1]=roc.train$auc[1]
  plot(roc.train,col=colp[1+i],lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
}
ct=paste(paste("Modèle complet",",AUC="),round(rfauc[1],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(rfauc[i+1],4)))
}
legend(0.4, 0.45,title="Forêts aléatoires",ct, col = colp,
       lty = rep(1,8),cex=0.6)

rfauc1=rep(0,8)
newx=btest.complet
colnames(newx)=make.names(colnames(newx))
newx=data.frame(newx)
roc.test=roc(btest.complet[,iddf],as.numeric(predict(rffit[[1]],newx,type="prob")[,2])) 
plot(roc.test,col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
rfauc1[1]=roc.test$auc[1]
for(i in 1:7){
  newx=btest.complet[,which(colnames(btest.complet)%in%c(importance_variables[[i]]))]
  newx=cbind(newx,btest.complet[,iddf])
  colnames(newx)[ncol(newx)]="top_dft_auto_pro_corp"
  colnames(newx)=make.names(colnames(newx))
  roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(predict(rffit[[i+1]],newx,type = "prob")[,2]))
  plot(roc.test,col=colp[1+i],lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
  rfauc1[i+1]=roc.test$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(rfauc1[1],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(rfauc1[i+1],4)))
}
legend(0.4, 0.45,title="Forêts aléatoires",ct, col = colp,
       lty = rep(1,8),cex=0.6)

#xgb-imp
require(xgboost)
xgbfit=list()
xgbauc=rep(0,8)
xtrain=btrain.complet[,-iddf]
dtest <- xgb.DMatrix(xtest, label = btest.complet[,iddf])
dtrain <- xgb.DMatrix(xtrain, label = btrain.complet[,iddf])
#watchlist <- list(eval = dtest, train = dtrain)
param <- list(max_depth = 6, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nround=350)#, watchlist
pred <- predict(bst, dtrain)
roc.train=roc(as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(pred))
plot(roc.train,col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
xgbfit[[1]]=bst
xgbauc[1]=roc.train$auc[1]

for( i in 1:7){
  x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables[[i]]))]
  dtrain <- xgb.DMatrix(x, label = btrain.complet[,iddf])
  param <- list(max_depth = 4, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, 
                objective = "binary:logistic", eval_metric = "auc")
  bst <- xgb.train(param, dtrain, nround=150)
  pred <- predict(bst, dtrain)
  roc.train=roc(as.numeric(btrain.complet[,"top_dft_auto_pro_corp"]),as.numeric(pred))
  plot(roc.train,col=colp[i+1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  xgbfit[[i+1]]=bst
  xgbauc[i+1]=roc.train$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(xgbauc[1],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(xgbauc[i+1],4)))
}
legend(0.4, 0.45,title="Gradient Boosting",ct, col = colp,
       lty = rep(1,8),cex=0.6)

xgbauc1=rep(0,8)
xtest=btest.complet[,-iddf]
dtest <- xgb.DMatrix(xtest, label = btest.complet[,iddf])
pred1 <- predict(xgbfit[[1]], dtest)
roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(pred1))
plot(roc.test,col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
xgbauc1[1]=roc.test$auc[1]

for(i in 1:7){
  xtest=btest.complet[,which(colnames(btest.complet)%in%c(importance_variables[[i]]))]
  dtest <- xgb.DMatrix(xtest, label = btest.complet[,iddf])
  pred1 <- predict(xgbfit[[i+1]], dtest)
  roc.test=roc(as.numeric(btest.complet[,"top_dft_auto_pro_corp"]),as.numeric(pred1))
  plot(roc.test,col=colp[1+i],lwd=3,main="Courbe ROC",print.auc=FALSE,add=TRUE)
  xgbauc1[i+1]=roc.test$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(xgbauc1[1],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(xgbauc1[i+1],4)))
}
legend(0.4, 0.45,title="Gradient Boosting",ct, col = colp,
       lty = rep(1,8),cex=0.6)

#hybrid method
#svm logit(ridge) boosting
library(glmnet)
library(pROC)
library(e1071)
x=btrain
id_neg=c(traincand[which(traincand[,4]==1),2])
id=which(rownames(x)%in%id_neg)
matd=do.call(cbind,x[,-iddf])
nr=9;nc=9
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo(17,nr,nc,0.04,0.01,2.99,0.65,matd,dim(matd)[1])
bmu_neg=kohonenqualiclass(prof,x[id,-iddf],dim(x[id,])[1])
nbmu=nlevels(as.factor(bmu_neg))
Snbmu=rep(0,8);Snbmu[1]=nbmu
Sfit=list()  
svmfit=list()
svmtrain=list()
for(i in 1:nbmu){
  svmtrain[[i]]=btrain.complet[match(c(id_neg[which(bmu_neg==bmu_neg[i])],traincand[which(traincand[,4]==2),2]),rownames(btrain.complet)),]#rownames(btrain.complet)
  #modelname=paste("svmfit",i)
  #assign(modelname ,svm(top_dft_auto_pro_corp ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE))
  svmfit[[i]]=svm(as.factor(top_dft_auto_pro_corp) ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE)
}
Sfit[[1]]=svmfit

dcode.train=matrix(rep(0,dim(btrain.complet)[1]*nbmu),ncol=nbmu)
for(i in 1:nbmu){
  pred1=predict(svmfit[[i]], btrain.complet[,-iddf],decision.values = TRUE)
  dcode.train[,i]=attr(pred1, "decision.values")
}

Sgfit=list()
Sglambda=rep(0,8)
Sgauc=rep(0,8)
x=dcode.train
g=as.vector(btrain.complet[,iddf])
fit=glmnet(x,g,family="binomial",alpha=0,nlambda=200,thresh = 1e-07,standardize = FALSE,maxit=150000,type.logistic="modified.Newton")
Sgfit[[1]]=fit
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
Sglambda[1]=fit$lambda[which.min(AICc)]
train.pre=predict(fit, x,s= fit$lambda[which.min(AICc)], type="response")
train.pre=as.vector(train.pre)
plot(roc( g,train.pre),col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
Sgauc[1]=roc( g,train.pre)$auc[1]


for(i in 1:7){
  x=btrain[,which(colnames(btrain)%in%c(importance_variables[[i]]))]
  id_neg=c(traincand[which(traincand[,4]==1),2])
  id=which(rownames(x)%in%id_neg)
  matd=do.call(cbind,x)
  nr=9;nc=9
  set.seed(15);matd=matd[sample(nrow(matd)),]
  prof=kohonenqualigo(17,nr,nc,0.04,0.01,2.99,0.65,matd,dim(matd)[1])
  bmu_neg=kohonenqualiclass(prof,x[id,],dim(x[id,])[1])
  nbmu=nlevels(as.factor(bmu_neg))
  Snbmu[i+1]=nbmu
  svmfit=list()
  svmtrain=list()
  for(j in 1:nbmu){
    svmtrain[[j]]=btrain.complet[match(c(id_neg[which(bmu_neg==bmu_neg[j])],traincand[which(traincand[,4]==2),2]),rownames(btrain.complet)),c(which(colnames(btrain)%in%c(importance_variables[[i]])),iddf)]
    #modelname=paste("svmfit",j)
    #assign(modelname ,svm(top_dft_auto_pro_corp ~ .,cost=1,scale = FALSE,data = svmtrain[[i]],probability = FALSE))
    svmfit[[j]]=svm(as.factor(top_dft_auto_pro_corp) ~ .,cost=1,scale = FALSE,data = svmtrain[[j]],probability = FALSE)
  }
  Sfit[[i+1]]=svmfit
  dcode.train=matrix(rep(0,dim(x)[1]*nbmu),ncol=nbmu)
  for(k in 1:nbmu){
    pred1=predict(svmfit[[k]], btrain.complet[,which(colnames(btrain)%in%c(importance_variables[[i]]))],decision.values = TRUE)
    dcode.train[,k]=attr(pred1, "decision.values")
  }
  x=dcode.train
  x=data.matrix(x,nrow(x))
  g2=as.vector(btrain.complet[,iddf])
  fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-07,standardize = TRUE)
  Sgfit[[i+1]]=fit
  tLL <- fit$nulldev - deviance(fit)
  k <- fit$df
  n <- fit$nobs
  AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
  Sglambda[i+1]=fit$lambda[which.min(AICc)]
  train.pre=predict(fit, x,s=   fit$lambda[which.min(AICc)], type="response")
  train.pre=as.vector(train.pre)
  plot(roc( g2,train.pre),col=colp[i+1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  Sgauc[i+1]=roc(g2,train.pre)$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(Sgauc[i],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(Sgauc[i+1],4)))
}
legend(0.4, 0.45,title="Méthode hybride",ct, col = colp,
       lty = rep(1,8),cex=0.6)



Sgauc1=rep(0,8)
dcode.test=matrix(rep(0,dim(btest.complet)[1]*Snbmu[1]),ncol=Snbmu[1])
for(i in 1:Snbmu[1]){
  fit=Sfit[[1]][[i]]
  pred=predict(fit, btest.complet[,-iddf],decision.values = TRUE)
  dcode.test[,i]=attr(pred, "decision.values")
}
newx=dcode.test
newx=data.matrix(newx,nrow(newx))
test.pre=predict(Sgfit[[1]], newx,s=Sglambda[1],type="response")
test.pre=as.vector(test.pre)
plot(roc(btest.complet[,iddf],test.pre),col=colp[1],lwd=3,main="Courbe ROC",print.auc=FALSE)
Sgauc1[1]=roc( btest.complet[,iddf],test.pre)$auc[1]

for(i in 1:7){
  x=btest.complet[,which(colnames(btest)%in%c(importance_variables[[i]]))]
  dcode.test=matrix(rep(0,dim(btest.complet)[1]*Snbmu[i+1]),ncol=Snbmu[i+1])
  for(j in 1:Snbmu[i+1]){
    pred=predict(Sfit[[i+1]][[j]], x,decision.values = TRUE)
    dcode.test[,j]=attr(pred, "decision.values")
  }
  newx=dcode.test
  newx=data.matrix(newx,nrow(newx))
  test.pre=predict(Sgfit[[i+1]], newx,s=Sglambda[i+1],type="response")
  test.pre=as.vector(test.pre)
  plot(roc( btest.complet[,iddf],test.pre),col=colp[i+1],lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  Sgauc1[i+1]=roc(btest.complet[,iddf],test.pre)$auc[1]
}
ct=paste(paste("Modèle complet",",AUC="),round(Sgauc1[i],4))
for(i in 1:7){
  ct=c(ct,paste(paste(colnames(varIMP)[i],",AUC="),round(Sgauc1[i+1],4)))
}
legend(0.4, 0.45,title="Méthode hybride",ct, col = colp,
       lty = rep(1,8),cex=0.6)
