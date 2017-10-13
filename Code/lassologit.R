#Lasso Logistic Model by glmnet
#https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
library(glmnet)
library(pROC)
#disj modifié
#PEqual=lapply(PEqual,as.factor)
#nlev=unlist(sapply(PEqual,nlevels))
#disj=disj[,-cumsum(nlev)]
#PEdat=cbind(PEquan[,3:(ncol(PEquan))],disj)
#c2:alpha0,s0.01 c1:alpha0,s0.008553595
#When change alpha, the default lambda sequence will also change
#s=0 is not necessarily the case lambda=0,instead it's the case of smallest lambda which is near 0 but not 0
#can be checked by using coef.glmnet function
x=breg1[,-iddf]
x=data.matrix(x,nrow(x))
g2=as.vector(breg1[,iddf])
fit=glmnet(x,g2,family="binomial",alpha=1,nlambda=200,thresh = 1e-07,standardize = TRUE)
tLL <- fit$nulldev - deviance(fit)
k <- fit$df
n <- fit$nobs
AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
AICc
BIC<-log(n)*k - tLL
BIC
fit$lambda
plot(fit,xvar="lambda",label=TRUE)
t=predict(fit, x,s=   fit$lambda[which.min(AICc)], type="response")
t=as.vector(t)
plot(roc( g2,t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))
#Youden's index to judge cutoff point point on AUC curve
trainauc=roc( g2,t)
YI=1*trainauc$sensitivities+trainauc$specificities-1
cfpoint=trainauc$thresholds[which.max(YI)]
newx=btest1[,-c(iddf,ncol(btest1))]
newx=data.matrix(newx,nrow(newx))
t=predict(fit, newx,s=fit$lambda[which.min(AICc)],type="response")
t=as.vector(t)
plot(roc( btest1[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

res.pre=cbind(as.numeric(rownames(PEdat[-traincand[,2],])),rep(0,dim(PEdat[-traincand[,2],])[1]),PEdat[-traincand[,2],iddf])
res.pre[match(as.numeric(rownames(btest1)),res.pre[,1]),2]=t
plot(roc( PEdat[-traincand[,2],iddf],res.pre[,2]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)
#Indice de Gini 
res.pre=res.pre[order(res.pre[,2],decreasing=TRUE),]
pp=seq(0, 1, length.out = 101)
nd=rep(0,length(pp))
for(i in 2:length(nd))
{
  nd[i]=length(which(res.pre[1:(floor(pp[i]*dim(btest1)[1])),3]==1))
}
pd=nd/length(which(res.pre[,3]==1))
aire=sum(pd[-1]+pd[-length(pd)])*1/(((length(pp)-1))*2)
plot(pp,pd,type="line",col="purple")
Gini=(aire-0.5)/(0.5-length(which(res.pre[,3]==1))/(2*dim(btest)[1]))

res.pre1=cbind(as.numeric(rownames(breg1)),rep(0,dim(breg1)[1]),breg1[,iddf])
res.pre1[,2]=t
res.pre1=res.pre1[order(res.pre1[,2],decreasing=TRUE),]
pp=seq(0, 1, length.out = 101)
nd=rep(0,length(pp))
for(i in 2:length(nd))
{
  nd[i]=length(which(res.pre1[1:(floor(pp[i]*dim(breg1)[1])),3]==1))
}
pd=nd/length(which(res.pre1[,3]==1))
aire=sum(pd[-1]+pd[-length(pd)])*1/(((length(pp)-1))*2)
plot(pp,pd,type="line",col="purple")
Gini=(aire-0.5)/(0.5-length(which(res.pre[,3]==1))/(2*dim(btest)[1]))


t[t<cfpoint]=0
t[t>cfpoint]=1
table(t,btest1[,iddf])


#lasso cross validation
cvfit = cv.glmnet(x, g2,family="binomial",type.measure="deviance")
plot(cvfit)
cvfit$lambda.min
coef(cvfit, s = "lambda.min")

t=predict(cvfit, newx, s = "lambda.min",type="response")
t=as.vector(t)
plot(roc( btest1[,iddf],t),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)

