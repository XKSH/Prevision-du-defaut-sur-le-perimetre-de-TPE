#k fold pas pratique à cause de la désequilibration des classes
# require(caret)
# set.seed(3527)
# flds <- createFolds(PEdat[,246], k =5, list = TRUE, returnTrain = FALSE)

## Continu de "selected test.R"
## Continu de "base original_s.R" ligne 20
#Repeated random sub-sampling validation
library(sampling)
taille=0.7*as.data.frame(table(PEdat.org$top_dft_auto_pro_corp))[,2]
set.seed(17)
traincand=list()
ntrain=10
for(i in 1:ntrain){
  traincand[[i]]=strata(PEdat.org, stratanames="top_dft_auto_pro_corp",size=taille, method="srswor")
}
#cross validation for glmnet model
#choose the model from gfit list and corresponding importance_variables in "selected test.R"
#change gfit and importance_variables
train.pre=list()
roclis=list()
btrain=PEdat.org[traincand[[1]][,2],]
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables[[7]]))]
train.pre[[1]]=predict(gfit[[8]], x,s= glambda[8], type="response")
plot(roc( btrain[,iddf],as.numeric(train.pre[[1]])),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
roclis[[1]]=roc(btrain[,iddf],as.numeric(train.pre[[1]]))
for(i in 2:ntrain){
  btrain=PEdat.org[traincand[[i]][,2],]
  btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
  iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
  btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
  x=btrain.complet[,which(colnames(btrain.complet)%in%c(importance_variables[[7]]))]
  train.pre[[i]]=predict(gfit[[8]], x,s= glambda[8], type="response")
  plot(roc( btrain[,iddf],as.numeric(train.pre[[i]])),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  roclis[[i]]=roc(btrain[,iddf],as.numeric(train.pre[[i]]))
}
t=roclis[[1]]$specificities
axe.x=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$specificities)})
axe.x=rowMeans(axe.x)
t=roclis[[1]]$sensitivities
axe.y=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$sensitivities)})
axe.y=rowMeans(axe.y)
axe.x=axe.x[which(!duplicated(axe.y))]
axe.y=axe.y[which(!duplicated(axe.y))]
axe.x=axe.x[order(axe.x)]
axe.y=axe.y[order(axe.y,decreasing=TRUE)]
axe.x=axe.x[c(1+10*c(0:floor(length(axe.x)/10)))]
axe.y=axe.y[c(1+10*c(0:floor(length(axe.y)/10)))]
lines(x=axe.x,y=axe.y,col="red",xlim=c(1,0),ylim=c(0,1),cex=0.01)
auc_avg=0
for(i in 1:ntrain){
  auc_avg=auc_avg+roclis[[i]]$auc[1]
}
auc_avg=auc_avg/ntrain
ct=c("Courbe simulée",paste(paste("Courbe moyenne",",AUC="),round(auc_avg,4)))
legend(0.4, 0.2,title="Régression logistique pénalisée",ct, col = c("lightgrey","red"),
       lty = rep(1,2),cex=0.6)


test.pre=list()
roclis=list()
btest=PEdat.org[-traincand[[1]][,2],]
btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
x=btest.complet[,which(colnames(btest.complet)%in%c(importance_variables[[7]]))]
test.pre[[1]]=predict(gfit[[8]], x,s= glambda[8], type="response")
plot(roc( btest[,iddf],as.numeric(test.pre[[1]])),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
roclis[[1]]=roc(btest[,iddf],as.numeric(test.pre[[1]]))
for(i in 2:ntrain){
  btest=PEdat.org[-traincand[[i]][,2],]
  btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
  iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
  btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
  x=btest.complet[,which(colnames(btest.complet)%in%c(importance_variables[[7]]))]
  x[,which(apply(x,2, function(x)all(is.na(x)))==TRUE)]=0
  test.pre[[i]]=predict(gfit[[8]], x,s= glambda[8], type="response")
  plot(roc( btest[,iddf],as.numeric(test.pre[[i]])),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  roclis[[i]]=roc(btest[,iddf],as.numeric(test.pre[[i]]))
}
t=roclis[[1]]$specificities
axe.x=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$specificities)})
axe.x=rowMeans(axe.x)
t=roclis[[1]]$sensitivities
axe.y=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$sensitivities)})
axe.y=rowMeans(axe.y)
axe.x=axe.x[which(!duplicated(axe.y))]
axe.y=axe.y[which(!duplicated(axe.y))]
axe.x=axe.x[order(axe.x)]
axe.y=axe.y[order(axe.y,decreasing=TRUE)]
#glisser la courbe
axe.x=axe.x[c(1+10*c(0:floor(length(axe.x)/10)))]
axe.y=axe.y[c(1+10*c(0:floor(length(axe.y)/10)))]
lines(x=axe.x,y=axe.y,col="red",xlim=c(1,0),ylim=c(0,1),cex=0.01)
auc_avg=0
for(i in 1:ntrain){
  auc_avg=auc_avg+roclis[[i]]$auc[1]
}
auc_avg=auc_avg/ntrain
ct=c("Courbe simulée",paste(paste("Courbe moyenne",",AUC="),round(auc_avg,4)))
legend(0.4, 0.2,title="Régression logistique pénalisée",ct, col = c("lightgrey","red"),
       lty = rep(1,2),cex=0.6)



#méthode hybride
train.pre=list()
roclis=list()
btrain=PEdat.org[traincand[[1]][,2],]
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
x=btrain.complet[,which(colnames(btrain)%in%c(importance_variables[[7]]))]
dcode.train=matrix(rep(0,dim(btrain.complet)[1]*Snbmu[8]),ncol=Snbmu[8])
for(i in 1:Snbmu[8]){
  pred1=predict(Sfit[[8]][[i]], x,decision.values = TRUE)
  dcode.train[,i]=attr(pred1, "decision.values")
}
x=dcode.train
train.pre[[1]]=predict(Sgfit[[8]], x,s= Sglambda[8], type="response")
train.pre[[1]]=as.vector(train.pre[[1]])
plot(roc( btrain[,iddf],train.pre[[1]]),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
roclis[[1]]=roc(btrain[,iddf],as.numeric(train.pre[[1]]))
for(i in 2:ntrain){
  btrain=PEdat.org[traincand[[i]][,2],]
  btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
  btrain.complet=apply(btrain,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
  x=btrain.complet[,which(colnames(btrain)%in%c(importance_variables[[7]]))]
  dcode.train=matrix(rep(0,dim(btrain.complet)[1]*Snbmu[8]),ncol=Snbmu[8])
  for(j in 1:Snbmu[8]){
    pred1=predict(Sfit[[8]][[j]], x,decision.values = TRUE)
    dcode.train[,j]=attr(pred1, "decision.values")
  }
  x=dcode.train
  train.pre[[i]]=predict(Sgfit[[8]], x,s= Sglambda[8], type="response")
  train.pre[[i]]=as.vector(train.pre[[i]])
  plot(roc( g,train.pre[[i]]),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  roclis[[i]]=roc(btrain[,iddf],as.numeric(train.pre[[i]]))
}
t=roclis[[1]]$specificities[1:13215]
axe.x=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$specificities[1:13215])})
axe.x=rowMeans(axe.x)
t=roclis[[1]]$sensitivities[1:13215]
axe.y=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$sensitivities[1:13215])})
axe.y=rowMeans(axe.y)
axe.x=axe.x[which(!duplicated(axe.y))]
axe.y=axe.y[which(!duplicated(axe.y))]
axe.x=axe.x[order(axe.x)]
axe.y=axe.y[order(axe.y,decreasing=TRUE)]
#glisser la courbe
axe.x=axe.x[c(1+10*c(0:floor(length(axe.x)/10)))]
axe.y=axe.y[c(1+10*c(0:floor(length(axe.y)/10)))]
lines(x=axe.x,y=axe.y,col="red",xlim=c(1,0),ylim=c(0,1),cex=0.01)
auc_avg=0
for(i in 1:ntrain){
  auc_avg=auc_avg+roclis[[i]]$auc[1]
}
auc_avg=auc_avg/ntrain
ct=c("Courbe simulée",paste(paste("Courbe moyenne",",AUC="),round(auc_avg,4)))
legend(0.4, 0.2,title="Méthode hybride",ct, col = c("lightgrey","red"),
       lty = rep(1,2),cex=0.6)


test.pre=list()
roclis=list()
btest=PEdat.org[-traincand[[1]][,2],]
btest[,1:(ncol(btest)-ndisj)]=apply(btest[,1:(ncol(btest)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
x=btest.complet[,which(colnames(btest)%in%c(importance_variables[[7]]))]
dcode.test=matrix(rep(0,dim(btest.complet)[1]*Snbmu[8]),ncol=Snbmu[8])
for(i in 1:Snbmu[8]){
  pred1=predict(Sfit[[8]][[i]], x,decision.values = TRUE)
  dcode.test[,i]=attr(pred1, "decision.values")
}
x=dcode.test
test.pre[[1]]=predict(Sgfit[[8]], x,s= Sglambda[8], type="response")
test.pre[[1]]=as.vector(test.pre[[1]])
plot(roc( btest[,iddf],test.pre[[1]]),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1))
roclis[[1]]=roc(btest[,iddf],as.numeric(test.pre[[1]]))

for(i in 1:ntrain){
  btest=PEdat.org[-traincand[[i]][,2],]
  btest[,1:(ncol(btest)-ndisj)]=apply(btest[,1:(ncol(btest)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
  btest.complet=apply(btest,2,function(x){x[is.na(x)]=median(x,na.rm = TRUE); return(x)})
  x=btest.complet[,which(colnames(btest)%in%c(importance_variables[[7]]))]
  x[,which(apply(x,2, function(x)all(is.na(x)))==TRUE)]=0
  dcode.test=matrix(rep(0,dim(btest.complet)[1]*Snbmu[8]),ncol=Snbmu[8])
  for(j in 1:Snbmu[8]){
    pred1=predict(Sfit[[8]][[j]], x,decision.values = TRUE)
    dcode.test[,j]=attr(pred1, "decision.values")
  }
  x=dcode.test
  test.pre[[i]]=predict(Sgfit[[8]], x,s= Sglambda[8], type="response")
  test.pre[[i]]=as.vector(test.pre[[i]])
  plot(roc( btest[,iddf],test.pre[[i]]),col="lightgrey",lwd=3,main="Courbe ROC",print.auc=FALSE,xlim=c(1,0),ylim=c(0,1),add=TRUE)
  roclis[[i]]=roc(btest[,iddf],as.numeric(test.pre[[i]]))
}
t=roclis[[1]]$specificities[1:5666]
axe.x=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$specificities[1:5666])})
axe.x=rowMeans(axe.x)
t=roclis[[1]]$sensitivities[1:5666]
axe.y=sapply(2:10,function(x){t=cbind(t,roclis[[x]]$sensitivities[1:5666])})
axe.y=rowMeans(axe.y)
axe.x=axe.x[which(!duplicated(axe.y))]
axe.y=axe.y[which(!duplicated(axe.y))]
axe.x=axe.x[order(axe.x)]
axe.y=axe.y[order(axe.y,decreasing=TRUE)]
#glisser la courbe
axe.x=axe.x[c(1+10*c(0:floor(length(axe.x)/10)))]
axe.y=axe.y[c(1+10*c(0:floor(length(axe.y)/10)))]
lines(x=axe.x,y=axe.y,col="red",xlim=c(1,0),ylim=c(0,1),cex=0.01)
auc_avg=0
for(i in 1:ntrain){
  auc_avg=auc_avg+roclis[[i]]$auc[1]
}
auc_avg=auc_avg/ntrain
ct=c("Courbe simulée",paste(paste("Courbe moyenne",",AUC="),round(auc_avg,4)))
legend(0.4, 0.2,title="Méthode hybride",ct, col = c("lightgrey","red"),
       lty = rep(1,2),cex=0.6)
