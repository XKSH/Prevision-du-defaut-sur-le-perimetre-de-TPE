#les 4 algorithmes d'apprentissage supervisé
## Continu de "traitement de donnée.R" 
#remplacement de donnée
btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
###
#lasso logit regression
x=btrain.complet[,-iddf]
x=data.matrix(x,nrow(x))
g2=as.vector(btrain.complet[,iddf])
fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-07,standardize = TRUE)
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
#critre d'information
#formula:https://community.jmp.com/t5/Discussions/getting-AIC-likelihood-from-model-coefficients/td-p/31056
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
BIC<-log(n)*k - tLL
fit$lambda
plot(fit,xvar="lambda",label=TRUE)
train.pre=predict(fit, x,s=   fit$lambda[which.min(AICc)], type="response")
train.pre=as.vector(train.pre)
trainauc=roc( g2,train.pre)
plot(roc( g2,train.pre),col="green",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
newx=btest.complet[,-iddf]
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
#########
#xgboost
require(xgboost)
#1  btrain.complet ici est "list"
xtrain=do.call(cbind,btrain.complet[,-iddf])
xtest=do.call(cbind,btest.complet[,-iddf])
dtrain <- xgb.DMatrix(xtrain, label = btrain.complet[,iddf])
dtest <- xgb.DMatrix(xtest, label = btest.complet[,iddf])
watchlist <- list(eval = dtest, train = dtrain)
#1 feature selection max_depth = 6, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, nround=350
param <- list(max_depth = 6, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nround=350, watchlist)

#2 xgboosting utilisant les variables sélectionnées
#importance_variables est obtenu par R fichier "feature selection.R"
dtrain <- xgb.DMatrix(xtrain[,which(colnames(xtrain)%in%c(importance_variables))], label = btrain.complet[,iddf])
dtest <- xgb.DMatrix(xtest[,which(colnames(xtest)%in%c(importance_variables))], label = btest.complet[,iddf])
#2 xgboosting raffiné
param <- list(max_depth = 4, eta = 0.05, silent = 1,gamma=0.08, nthread = 2, 
              objective = "binary:logistic", eval_metric = "auc")
bst <- xgb.train(param, dtrain, nround=160, watchlist)
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
tree_fit = rpart(as.factor(top_dft_auto_pro_corp) ~ ., data = btrain,parms = list(prior = c(.7,.3), split = "information"))
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
