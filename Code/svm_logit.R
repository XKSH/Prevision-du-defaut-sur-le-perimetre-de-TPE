#svm logit(ridge) boosting
library(sampling)
rownames(PEdat)=c(1:dim(PEdat)[1])
taille=0.7*as.data.frame(table(PEdat$top_dft_auto_pro_corp))[,2]
set.seed(17)
#idtrain=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")[,2]
traincand=strata(PEdat, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")
id_neg=c(traincand[which(traincand[,4]==1),2])
matd=do.call(cbind,PEdat[id_neg,])
nr=5;nc=6
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo(17,nr,nc,0.04,0.01,2.99,0.65,matd,dim(matd)[1])
bmu_neg=kohonenqualiclass(prof,PEdat[id_neg,],dim(PEdat[id_neg,])[1])
nbmu=nlevels(as.factor(bmu_neg))
btrain.complet=apply(PEdat[traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
btest.complet=apply(PEdat[-traincand[,2],],2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
iddf=which(colnames(PEdat) %in% c("top_dft_auto_pro_corp"))
#undersampling
# library(unbalanced)
# ubtk=ubTomek(btrain.complet[,-iddf], btrain.complet[,iddf], verbose = TRUE)
# btrain.complet1=btrain.complet[-ubtk$id.rm,]
# for(i in 1:10){
#   ubtk=ubTomek(btrain.complet1[,-iddf], btrain.complet1[,iddf], verbose = TRUE)
#   btrain.complet1=btrain.complet1[-ubtk$id.rm,]
# }
library(e1071)
svmfit=list()
svmtrain=list()
for(i in 1:nbmu){
  svmtrain[[i]]=btrain.complet[match(c(id_neg[which(bmu_neg==i)],traincand[which(traincand[,4]==2),2]),rownames(btrain.complet)),]#rownames(btrain.complet)
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
#visuslisation variable importance
library(caret)

# densityplots for each attribute by class value
featurePlot(x=dcode.train[,c(1: nr*nc)], y=as.factor(btrain.complet[,iddf]),plot="box", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=6))

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
t=predict(fit, x,s= fit$lambda[which.min(AICc)], type="response")
#t=predict(fit, x,s= 1.420652e-03, type="response")
t=as.vector(t)
plot(roc( g,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
newx=dcode.test
newx=data.matrix(newx,nrow(newx))
t=predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response")
t=as.vector(t)
plot(roc(btest.complet[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

#svm +svm 
dcode.train1=cbind(dcode.train,btrain.complet[,iddf])
dcode.test1=cbind(dcode.test,btest.complet[,iddf])
svmfit1=svm(dcode.train1[,1:nr*nc],as.factor(dcode.train1[,(nr*nc+1)]),cost=1,scale = FALSE,data = dcode.train1,probability = TRUE,kernel="linear")
t=predict(svmfit1,dcode.train1[,1:nr*nc],probability = TRUE)
plot(roc(btrain.complet[,iddf],attr(t, "probabilities")[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
t=predict(svmfit1,dcode.test1[,1:nr*nc],probability = TRUE)
plot(roc(btest.complet[,iddf],attr(t, "probabilities")[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))


#svmfit1=svm(x3,as.factor(y3),cost=1,scale = FALSE,data = dcode.train1,probability = TRUE,kernel ="linear")
t=predict(svmfit1,x3,probability = TRUE)
plot(roc(btrain.complet[,iddf],attr(t, "probabilities")[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
t=predict(svmfit1,newx3,probability = TRUE)
plot(roc(btest.complet[,iddf],attr(t, "probabilities")[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))

#tree
library(rpart)
dcode.train2=as.data.frame(dcode.train1)
dcode.test2=as.data.frame(dcode.test1)
tree_fit = rpart(as.factor(V31) ~ ., data = dcode.train2,parms = list(prior = c(.5,.5), split = "information"))
tree_fitp <- prune(tree_fit, cp = 0.05)
table(predict(tree_fit,dcode.train2,type = "class"),dcode.train2[,dim(dcode.train2)[2]])
plot(roc(as.vector(dcode.train2[,dim(dcode.train2)[2]]),predict(tree_fit,dcode.train2,type = "prob")[,2]),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
table(predict(tree_fit,dcode.test2,type = "class"),dcode.test2[,dim(dcode.test2)[2]])
plot(roc(as.vector(dcode.test2[,dim(dcode.test2)[2]]),predict(tree_fit,dcode.test2,type = "prob")[,2]),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
t=predict(tree_fit,dcode.test2,type = "prob")[,2]
#randomforest
ubtk=ubTomek(dcode.train2[,-dim(dcode.train2)[2]], dcode.train2[,dim(dcode.train2)[2]], verbose = TRUE)
dcode.train3=dcode.train2[-ubtk$id.rm,]
ubse=ubTomek(dcode.train2[,-dim(dcode.train2)[2]], dcode.train2[,dim(dcode.train2)[2]], verbose = TRUE)
dcode.train3=dcode.train2[-ubtk$id.rm,]
for(i in 1:10){
  ubtk=ubTomek(dcode.train3[,-dim(dcode.train3)[2]], dcode.train3[,dim(dcode.train3)[2]], verbose = TRUE)
  dcode.train3=dcode.train3[-ubtk$id.rm,]
}
ubse=ubSMOTE(dcode.train3[,-dim(dcode.train3)[2]],as.factor(dcode.train3[,dim(dcode.train3)[2]]), perc.over = 200, k = 5, perc.under = 800, verbose = TRUE)
summary(ubse$Y)
dcode.train4=as.data.frame(cbind(ubse$X,ubse$Y))
colnames(dcode.train4)[dim(dcode.train4)[2]]=paste("V",nr*nc+1,sep="")
library(randomForest)
set.seed(415)
ffit <- randomForest(as.factor(V31) ~ .,
                    data=dcode.train2,
                    importance=TRUE,
                    ntree=800,classwt=c(0.7,0.3),maxnodes=20)
prediction1<- predict(ffit,dcode.train2,type="prob")[,2]
plot(roc(as.vector(dcode.train2[,dim(dcode.train2)[2]]),prediction1),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
prediction<- predict(ffit,dcode.test2,type="prob")[,2]
plot(roc(as.vector(dcode.test2[,dim(dcode.train2)[2]]),prediction),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))

#logitboost
library(ada)
default <- rpart.control()
gdis <- ada(V31~., data = dcode.train3, iter = 50, loss = "l", type = "discrete",
 control = default)
log2=predict(gdis,dcode.train2[,-31],type="prob")[,2]
plot(roc(as.vector(dcode.train2[,31]),log2),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
log2t=predict(gdis,dcode.test2[,-31],type="prob")[,2]
plot(roc(as.vector(dcode.test2[,31]),log2t),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
#xgboost
#tuning of xgboosting
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,number = 2, 
                        #summaryFunction = twoClassSummary,
                        classProbs = TRUE,
                        allowParallel=T)

xgb.grid <- expand.grid(nrounds = c(100,150),
                        max_depth = c(4,6,8,10),
                        eta = c(0.05,0.1,0.15),
                        gamma=c(0.1,0.15),
                        colsample_bytree=c(1),
                        min_child_weight=c(1),
                        subsample=c(1)
)
dcode.train2[,31]=as.factor(dcode.train2[,31])
levels(dcode.train2[,31])=c("c0","c1")
set.seed(1234)
xgb_tune <-train(x= as.matrix(dcode.train2[,-31]), 
                 y = as.factor(dcode.train2[,31]),
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgb.grid,
                 verbose=T,
                 metric="Kappa",
                 nthread =3
)

bst= xgboost(data = as.matrix(dcode.train2[,-31]), label = dcode.train2[,31], max.depth = 4, eta = 0.1,gamma=0.15, nthread = 2, nround = 100, objective = "binary:logistic")
pred <- predict(bst, as.matrix(dcode.train2[,-31]))
plot(roc(as.vector(dcode.train2[,31]),pred),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
pred1 <- predict(bst, as.matrix(dcode.test2[,-31]))
plot(roc(as.vector(dcode.test2[,31]),pred1),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))


set.seed(1234)

bst= xgboost(data = as.matrix(btrain.complet[,-iddf]), label = btrain.complet[,iddf], max.depth = 20, eta = 0.1, nthread = 2, nround = 90, objective = "binary:logistic")
pred <- predict(bst, as.matrix(btrain.complet[,-iddf]))
plot(roc(as.vector(btrain.complet[,iddf]),pred),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
pred1 <- predict(bst, as.matrix(btest.complet[,-iddf]))
plot(roc(as.vector(btest.complet[,iddf]),pred1),col="blue",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))

#step3
x3=cbind(predict(fit, x,s=fit$lambda[which.min(AICc)],type="response"),predict(tree_fit,dcode.train2,type = "prob")[2],predict(ffit,dcode.train2,type="prob")[,2],log2)
y3=as.vector(btrain.complet[,iddf])
fit3=glmnet(x3,y3,family="binomial",alpha=0,nlambda=200,thresh = 1e-07,standardize = FALSE,maxit=150000,type.logistic="modified.Newton")
tLL <- fit3$nulldev - deviance(fit3)
k <- fit3$df
n <- fit3$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc
t3=predict(fit3, x3,s= fit3$lambda[which.min(AICc)], type="response")
plot(roc(y3,t3),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
newx3=cbind(predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response"),predict(tree_fit,dcode.test2,type = "prob")[,2],predict(ffit,dcode.test2,type="prob")[,2],log2t)
t3=predict(fit3, newx3,s=fit3$lambda[which.min(AICc)],type="response")
t3=as.vector(t3)
plot(roc(btest.complet[,iddf],t3),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

#sampling

x3=cbind(as.numeric(predict(fit, x,s=fit$lambda[which.min(AICc)],type="class")),as.numeric(predict(tree_fit,dcode.train2,type = "class"))-1,as.numeric(predict(ffit,dcode.train2,type="class"))-1,as.numeric(predict(gdis,dcode.train2[,-31],type="vector"))-1)
y3=as.vector(btrain.complet[,iddf])
x3[x3==0]=-1
lab=as.vector(btrain.complet[,iddf])
lab[lab==0]=-1
dcode.trainex=dcode.train2
x4=x3[which(lab==1),]
l4=lab[which(lab==1)]
for(i in 1:1){
  ubtk=ubTomek(dcode.trainex[,-dim(dcode.train2)[2]], dcode.trainex[,dim(dcode.train2)[2]], verbose = TRUE)
  dcode.trainex=dcode.trainex[-ubtk$id.rm,]
  x4=rbind(x4,x3[ubtk$id.rm,])
  l4=c(l4,lab[ubtk$id.rm])
  x3=x3[-ubtk$id.rm,]
  lab=lab[-ubtk$id.rm]
}

W=rep(1/dim(x4)[1],dim(x4)[1])
#W=rep(1/438,438)
nit=30
alpha=rep(0,nit)
wc=rep(0,nit)
for(j in 1:nit){
  err = colSums(-0.5*W*(x4*l4-1)*1)
  i=which.min(err)[1]
  wc[j]=i
  alpha[j]=0.5*log( (1-err[i])/err[i] )
  W= W*exp(-alpha[j]*x4*l4)
  W = W/sum(W);
}
#t=ccode.train%*%alpha
alphac=rep(0,length(unique(wc)))
for(i in 1:length(unique(wc)))
{
  alphac[i]=sum(alpha[which(wc==wc[i])])
}
t=(ccode.train[,unique(wc)])%*%t(alphac)
plot(roc(lab,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
lab1=as.vector(btest.complet[,iddf])
lab1[lab1==0]=-1
t=(ccode.test[,unique(wc)])%*%t(alphac)
t=as.vector(t)
plot(roc(lab1,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
table(t,lab1)
for(q in 1:30){
  print(table(ccode.train[,q],lab))
  #plot(roc(lab1,ccode.test[,q]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
}




#Gini plot
res.pre=cbind(as.numeric(rownames(PEdat[-traincand[,2],])),rep(0,dim(PEdat[-traincand[,2],])[1]),PEdat[-traincand[,2],iddf])
res.pre[match(as.numeric(rownames(btest.complet)),res.pre[,1]),2]=t
plot(roc( PEdat[-traincand[,2],iddf],res.pre[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
#Indice de Gini 
res.pre=res.pre[order(res.pre[,2],decreasing=TRUE),]
pp=seq(0, 1, length.out = 101)
nd=rep(0,length(pp))
for(i in 2:length(nd))
{
  if(floor(pp[i]*length(res.pre[,1]))>0){
  #nd[i]=length(which(res.pre1[1:(floor(pp[i]*dim(btest1)[1])),3]==1))
  nd[i]=length(which(res.pre[1:(floor(pp[i]*dim(btest.complet)[1])),3]==1))
  }
  else{
    nd[i]=0
  }
}
pd=nd/length(which(res.pre[,3]==1))
aire=sum(pd[-1]+pd[-length(pd)])*1/(((length(pp)-1))*2)
plot(pp,pd,type="line",col="purple",xlab="% de la population sélectionnée",ylab="% des défauts sélectionnés",yaxt="n",main="Courbe de discrimination")
axis(2, at=pretty(pd), lab=paste0(pretty(pd) * 100, " %"),  las=TRUE)
pd1=rep(1,length(pp))
pd1[which(pp<length(which(res.pre[,3]==1))/dim(btest.complet)[1])]=dim(btest.complet)[1]/length(which(res.pre[,3]==1))*pp[which(pp<length(which(res.pre[,3]==1))/dim(btest.complet)[1])]
lines(pp,pd1,col="orange")
pd2=rep(1,length(pp))*pp
lines(pp,pd2,col="green")
legend(0.4, 0.3, c("Courbe parfaite", "Courbe modélisation", "Courbe aléatoire"), col = c("orange", "purple", "green"),
       lty = c(1, 1, 1))
Gini=(aire-0.5)/(0.5-length(which(res.pre[,3]==1))/(2*dim(btest.complet)[1]))

t=predict(fit, x,s= fit$lambda[which.min(AICc)], type="response")
#t=predict(fit, x,s= 1.420652e-03, type="response")
t=as.vector(t)
res.pre1=cbind(as.numeric(rownames(PEdat[traincand[,2],])),rep(0,dim(PEdat[traincand[,2],])[1]),PEdat[traincand[,2],iddf])
res.pre1[match(as.numeric(rownames(btrain.complet)),res.pre1[,1]),2]=t
plot(roc( PEdat[traincand[,2],iddf],res.pre1[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
#Indice de Gini 
res.pre1=res.pre1[order(res.pre1[,2],decreasing=TRUE),]
pp=seq(0, 1, length.out = 101)
nd=rep(0,length(pp))
for(i in 2:length(nd))
{
  #nd[i]=length(which(res.pre1[1:(floor(pp[i]*dim(btest1)[1])),3]==1))
  nd[i]=length(which(res.pre1[1:(floor(pp[i]*dim(btrain.complet)[1])),3]==1))
}
pd=nd/length(which(res.pre1[,3]==1))
aire=sum(pd[-1]+pd[-length(pd)])*1/(((length(pp)-1))*2)
plot(pp,pd,type="line",col="purple",xlab="% de la population sélectionnée",ylab="% des défauts sélectionnés",yaxt="n",main="Courbe de discrimination")
axis(2, at=pretty(pd), lab=paste0(pretty(pd) * 100, " %"),  las=TRUE)
pd1=rep(1,length(pp))
pd1[which(pp<length(which(res.pre1[,3]==1))/dim(btrain.complet)[1])]=dim(btrain.complet)[1]/length(which(res.pre1[,3]==1))*pp[which(pp<length(which(res.pre1[,3]==1))/dim(btrain.complet)[1])]
lines(pp,pd1,col="orange")
pd2=rep(1,length(pp))*pp
lines(pp,pd2,col="green")
legend(0.4, 0.3, c("Courbe parfaite", "Courbe modélisation", "Courbe aléatoire"), col = c("orange", "purple", "green"),
lty = c(1, 1, 1))
Gini=(aire-0.5)/(0.5-length(which(res.pre1[,3]==1))/(2*dim(btrain.complet)[1]))


#adaboost svm based on som (too unbalanced guess, a lose)
ccode.train=dcode.train
ccode.train[ccode.train<0]=-1
ccode.train[ccode.train>0]=1
ccode.train=-1*ccode.train
lab=as.vector(btrain.complet[,iddf])
lab[lab==0]=-1
alpha=rep(0,dim(ccode.train)[2])
W=rep(1/dim(ccode.train)[1],dim(ccode.train)[1])
nit=30
alpha=rep(0,nit)
alpha1=rep(0,nit)
alpha2=rep(0,nit)
wc=rep(0,nit)
for(j in 1:nit){
  #zz=1*(lab==1)
  #werr1=colSums(-0.5*W*(ccode.train*lab-1)*zz)
  #werr2=colSums(-0.5*W*(ccode.train*lab-1)*(1-zz))
  #i=which.min(2*werr1+0.8*werr2)[1]
  err = colSums(-0.5*W*(ccode.train*lab-1)*1)
  i=which.min(err)[1]
  wc[j]=i
  alpha[j]=0.5*log( (1-err[i])/err[i] )
  #err1=sum(-0.5*W[which(lab==1)]*(ccode.train[which(lab==1),i]*lab[which(lab==1)]-1)*1)
  #err2=sum(-0.5*W[which(lab==-1)]*(ccode.train[which(lab==-1),i]*lab[which(lab==-1)]-1)*1)
  #alpha1=0.5*log( (1-err1)/err1 )
  #alpha2=0.5*log( (1-err2)/err2 )
  #alpha[j]=0.5*alpha1+0.5*alpha2
  # update the weight
  #W[which(lab==1)] = 0.5*W[which(lab==1)]*exp(-alpha1*ccode.train[which(lab==1),i]*lab[which(lab==1)]*1);
  #W[which(lab==-1)] = 0.2*W[which(lab==-1)]*exp(-alpha2*ccode.train[which(lab==-1),i]*lab[which(lab==-1)]*1);
  W= W*exp(-alpha1*ccode.train*lab)
  W = W/sum(W);
}
ccode.test=dcode.test
ccode.test[ccode.test<0]=-1
ccode.test[ccode.test>0]=1
ccode.train=-1*ccode.train
#t=ccode.train%*%alpha
alphac=rep(0,length(unique(wc)))
for(i in 1:length(unique(wc)))
{
  alphac[i]=sum(alpha[which(wc==wc[i])])
}
t=(ccode.train[,unique(wc)])%*%t(alphac)
t[t>0]=1
t[t<0]=-1
plot(roc(lab,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
lab1=as.vector(btest.complet[,iddf])
lab1[lab1==0]=-1
t=(ccode.test[,unique(wc)])%*%t(alphac)
t=as.vector(t)
plot(roc(lab1,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
table(t,lab1)
for(q in 1:30){
  print(table(ccode.train[,q],lab))
#plot(roc(lab1,ccode.test[,q]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
}



cvfit = cv.glmnet(x, g,family="binomial",type.measure="deviance")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")
t=predict(cvfit, newx, s = "lambda.min",type="response")
t=as.vector(t)
plot(roc( btest.complet[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

#simple logit
x=cbind(t,predict(tree_fit,dcode.train2,type = "prob")[,2], predict(ffit,dcode.train2,type="prob")[,2])#dcode.train[,22]
g=as.vector(btrain.complet[,iddf])
regdata=as.data.frame(cbind(x,g))
model=glm(g~ ., family = binomial(link = "logit"),data=regdata,control = list(maxit = 60))
t=predict(model, regdata, type="response") 
plot(roc( g,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
newx=cbind(predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response"),predict(tree_fit,dcode.test2,type = "prob")[2],predict(ffit,dcode.test2,type="prob")[,2])#dcode.test[,22]
newdata=as.data.frame(cbind(newx,btest.complet[,iddf]))
t=predict(model, newdata, type="response") 
plot(roc(btest.complet[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

#clu=kmeans(prof,4,nstart=25)
apply(dcode.train,2,function(x){r=length(which(x>0));return(r)})
x=as.matrix(dcode.train[,c(1:30)],ncol=12)  
regdata=as.data.frame(cbind(x,g))
model=glm(g~ ., family = binomial(link = "logit"),data=regdata,control = list(maxit = 60))
t=predict(model, regdata, type="response") 
plot(roc( g,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
#newx=as.matrix(cbind(dcode.test[,c(which(clu$cluster==1))],rowSums(dcode.test[,c(which(clu$cluster==2))]),rowSums(dcode.test[,c(which(clu$cluster==3))]),rowSums(dcode.test[,c(which(clu$cluster==4))])),ncol=4)  
newx=as.matrix(dcode.test[,c(1:30)],ncol=12) 
newdata=as.data.frame(cbind(newx,btest.complet[,iddf]))
t=predict(model, newdata, type="response") 
plot(roc(btest.complet[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
