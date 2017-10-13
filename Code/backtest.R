#échantillonnage de proba d'inclusion 0.7
library(sampling)
taille=0.7*as.data.frame(table(PEdat$top_dft_auto_pro_corp))[,2]
set.seed(10)
#idtrain=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")[,2]
traincand=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")
set.seed(12)
#undersampling
#La proportion 0.4 a l'air cool pour 5 fois 5 grid
id1=sample(1:length(traincand[traincand[,4]==1,4]), 1.1*length(traincand[traincand[,4]==2,4]))
idtrain=c(traincand[id1,2],traincand[which(traincand[,4]==2),2])
btrain=PEdat[idtrain,]
btest=PEdat[-traincand[,2],]
source("Kohonennet_weight.R")
source("dist.R")
nr=5;nc=5
matd=do.call(cbind,btrain)
#
prof=kohonenqualigo(17,nr,nc,0.4,0.1,3,3,matd[sample(nrow(matd)),],dim(matd)[1])
m=kohonenqualiclass(prof,btrain,dim(btrain)[1])

#som supervisé
#w=c(rep(1/(3*196),length(var_cand)),2/3,rep(1/(3*196),ncol(disj)))
w=c(rep(3/(8*245),245),1/2,rep(1/(8*59),ncol(disj)))
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1],w)
m=kohonenqualiclass.weight(prof,btrain,dim(btrain)[1],w)
#clustering
clustrain=cbind(btrain,PEquan[idtrain,ncol(PEquan)])
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
#backtest
#mtest=kohonenqualiclass.weight(prof,btest[which(btest$top_dft_auto_pro_corp==1),],dim(btest[which(btest$top_dft_auto_pro_corp==1),])[1],w)
#mtest=kohonenqualiclass(prof[,c(1:245,247:305)],btest[which(btest$top_dft_auto_pro_corp==1),c(1:245,247:305)],dim(btest[which(btest$top_dft_auto_pro_corp==1),c(1:245,247:305)])[1])
mtest=kohonenqualiclass(prof[,c(1:245,247:305)],btest[,c(1:245,247:305)],dim(btest[,c(1:245,247:305)])[1])
table(mtest)
clustest=cbind(btest,PEquan[-traincand[,2],ncol(PEquan)])
clustest=cbind(clustest,mtest)[order(mtest),]
table(clustest[,(ncol(clustest)-1)],clustest[,ncol(clustest)])
#graphe
grillecarte(nr,nc,2,clustest[,(ncol(clustest)-1)],clustest[,ncol(clustest)])
par(xpd=TRUE)
nb=2
ncol=seq(0,240,length.out=nb)
legend("topright", inset=c(-0.15,0.2), title="Groupe", c("Non-défaut","Défaut"), pch=15,col=hcl(ncol,120,85),cex=0.55)


#classification
idc1=as.numeric(colnames(cc)[apply(cc,2,function(x){t=x[1]<x[2];return(t)})])
class.pre=rep(0,length(mtest))
class.pre[which(mtest%in%idc1)]=1
class.actual=btest[,246]  
table(class.pre,class.actual)
library("pROC")
plot(roc(class.actual, class.pre),col="purple",print.auc=TRUE,main="Courbe ROC")
plot(roc(class.actual, class.actual),add=TRUE)
##1.8 no sample in kohonenqualigo 1.1 sample in kohonengo
class.pre=rep(0,length(btrain))
class.pre[which(m%in%idc1)]=1
class.actual=btrain[,246]

par(xpd=TRUE)
legend("topright", inset=c(0,0.1), title="Algo", c("Supsom","Tree","RF","Kohonenpkg"), pch=15,col=c("purple","red","green","blue"),cex=0.55)
#scatter plot of pair variables
my_cols <- c("#00AFBB", "#FC4E07") 
pairs(PEdat[,1:5], pch = 19,  cex = 0.5,
      col = my_cols[PEdat$top_dft_auto_pro_corp],
      lower.panel=NULL)


#Recursive Partitioning Trees
library(rpart)
PEdat.tree=cbind(PEquan[,3:(ncol(PEquan))],PEqual)
tree_train=PEdat.tree[traincand[,2],]
#tree_train=PEdat.tree[idtrain,]
tree_train=data.frame(tree_train)
tree_test=PEdat.tree[-traincand[,2],]
#tree_test=PEdat.tree[-idtrain,]
tree_test=data.frame(tree_test)
tree_train$top_dft_auto_pro_corp=factor(tree_train$top_dft_auto_pro_corp)
tree_fit = rpart(top_dft_auto_pro_corp ~ ., data = tree_train,parms = list(prior = c(.7,.3), split = "information"))
#loss=matrix(c(0,3,1,0),byrow=TRUE, nrow=2),
#tree_fitp <- prune(tree_fit, cp = 0.05)
table(predict(tree_fit,tree_train,type = "class"),tree_train[,"top_dft_auto_pro_corp"])
table(predict(tree_fit,tree_test,type = "class"),tree_test[,"top_dft_auto_pro_corp"])
library(pROC)
plot(roc(as.numeric(tree_test[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,tree_test,type = "prob")[,2])),col="red",lwd=3,main="Courbe ROC",print.auc=TRUE)
plot(roc( as.numeric(tree_train[,"top_dft_auto_pro_corp"]),as.numeric(predict(tree_fit,tree_train,type = "prob")[,2])),add=TRUE,col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
#prop:1:1

#random forest
library("randomForest")
#NA imputation by mode
PEdat.complet=do.call(cbind,PEdat.tree)
PEdat.complet[,-246]=as.numeric(PEdat.complet[,-246])
for(i in 1:ncol(PEdat.complet))
{
  PEdat.complet[is.na(PEdat.complet[,i]),i]=median(PEdat.complet[,i],na.rm=TRUE)
}
PEdat.complet[247:ncol(PEdat.complet)]=as.factor(PEdat.complet[247:ncol(PEdat.complet)])
#forest_train=PEdat.complet[traincand[,2],]
forest_train=PEdat.complet[idtrain,]
forest_train=data.frame(forest_train)
#forest_test=PEdat.complet[-traincand[,2],]
forest_test=PEdat.complet[-idtrain,]
forest_test=data.frame(forest_test)
set.seed(415)
fit <- randomForest(as.factor(forest_train$top_dft_auto_pro_corp) ~ .,
                    data=forest_train, 
                    importance=TRUE, 
                    ntree=2000,classwt=c(0.7,0.3))
varImpPlot(fit)
IV=importance(fit)
IVn=rownames(IV[order(IV[,4],decreasing = TRUE),])[1:40]
prediction <- predict(fit,forest_test,type="prob")
table(prediction,forest_test[,"top_dft_auto_pro_corp"])
plot(roc( as.numeric(forest_test[,"top_dft_auto_pro_corp"]),as.numeric(prediction[,1])),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
#prop:1:1
PEre=cbind(PEdat.complet[,IVn],PEdat.complet[,"top_dft_auto_pro_corp"])
colnames(PEre)[ncol(PEre)]="top_dft_auto_pro_corp"
#forest_train=PEre[traincand[,2],]
forest_train=PEdat.complet[idtrain,]
forest_train=data.frame(forest_train)

#forest_test=PEre[-traincand[,2],]
forest_test=PEdat.complet[-idtrain,]
forest_test=data.frame(forest_test)
fit1 <- randomForest(as.factor(forest_train$top_dft_auto_pro_corp) ~ .,
                    data=forest_train, 
                    importance=TRUE, 
                    ntree=1000,classwt=c(0.7,0.3))
prediction1 <- predict(fit1,forest_test)
table(prediction1,forest_test[,"top_dft_auto_pro_corp"])
plot(roc( as.numeric(forest_test[,"top_dft_auto_pro_corp"]),as.numeric(prediction1)),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

#tree bagging
library(caret)
set.seed(1234)
ctrl <- trainControl(method = "cv", number = 5)
tbmodel <- train(top_dft_auto_pro_corp ~ ., data = forest_train, method = "tan",
                 trControl = ctrl)

predictors <- names(forest_train)[names(forest_train) != 'top_dft_auto_pro_corp']
pred <- predict(tbmodel$finalModel, forest_test[,predictors])
auc <- roc(as.numeric(forest_test[,"top_dft_auto_pro_corp"]), pred)
plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
pred[(pred>0.53)]=1
pred[(pred<=0.53)]=0
plot(roc( as.numeric(forest_test[,"top_dft_auto_pro_corp"]),pred),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)


#svm
library(e1071)
obj <- tune.svm(top_dft_auto_pro_corp~., data = forest_train, 
               gamma = 2^(-1:1), cost = 2^(-2:4),
             kernel = "sigmoid",scale=FALSE) 
model <- svm(top_dft_auto_pro_corp ~ ., kernel="radial",cost=1,scale = FALSE,data = forest_train,probability = TRUE)
pred <- predict(model, forest_train[,-246],probability = TRUE)
plot(roc( as.numeric(forest_train[,"top_dft_auto_pro_corp"]),pred),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
pred <- predict(model, forest_test[,-246],probability = TRUE)
plot(roc( as.numeric(forest_test[,"top_dft_auto_pro_corp"]),pred),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)