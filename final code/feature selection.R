require(caret)
# ####embedded model####
# #L1-based feature selection using lasso logistic ## Continu de "algo-supervisé.R"
glmImp = varImp(fit, scale = FALSE,lambda =   fit$lambda[which.min(AICc)])
glmImp=100*glmImp/sum(glmImp)

# Tree based model-random forest ##  Continu de "algo-supervisé.R"
require(randomForest)
rfIMP=importance(ffit)[,4]
rfIMP=100*rfIMP/sum(rfIMP)

# xgboost
model = xgb.dump(bst, with.stats = T)
names = dimnames(data.matrix(btrain.complet[,-iddf]))[[2]]
importance_matrix = xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:10,])
xgbImp=rep(0,dim(xtrain)[2])
xgbImp[which(colnames(xtrain)%in%importance_matrix$Feature)]=importance_matrix$Gain
xgbImp=100*xgbImp/sum(xgbImp)
#importance_variables=importance_matrix$Feature

#####stability selection###
require("stabs")
require("mboost")
rho = function(y, f, w = 1) {
  p = pmax(pmin(1 - 1e-5, f), 1e-5)
  -y * log(p) - (1 - y) * log(1 - p)
}
ngradient = function(y, f, w = 1) y - f
offset = function(y, w) weighted.mean(y, w)
L2fm = Family(ngradient = ngradient,
              loss = rho, offset = offset)
ctrl = boost_control(mstop = 200)
stab_x=btrain.complet[,-iddf]
stab_y=btrain.complet[,iddf]
stab_glm = glmboost(stab_x, stab_y, family = L2fm, center = TRUE,
                    control = ctrl)
stab = stabsel(stab_glm,q=10, PFER = 1, sampling.type = "MB")
stabImp=rowSums(stab$phat)
stabImp=100*stabImp/sum(stabImp)

#####filter model###
####
#Removing features with low variance
var.x=btrain[,-iddf]
var.x=apply(var.x, 2, function(X){ X[!is.na(X)]= (X[!is.na(X)]- min(X[!is.na(X)]))/diff(range(X[!is.na(X)])) ;return(X)})
varImp=apply(var.x,2,function(x){var(x,na.rm = TRUE)})
varImp=100*varImp/sum(varImp)

#Univariate feature selection(statistical test etc; here using distance correlation)
#chi square
require(ggplot2)
chi.x=apply(btrain.complet[,1:(ncol(btrain.complet)-ndisj)],2,function(x){cut_interval(x, 5)})
chi.xs=apply(btrain.complet[,(ncol(btrain.complet)-ndisj+1):(ncol(btrain.complet)-1)],2,factor)
chi.x=cbind(chi.x,chi.xs)
chiImp=apply(chi.x,2,function(x){t=chisq.test(x,as.factor(btrain.complet[,iddf]));return(t$p.value)})
chiImp=100*(1/chiImp)/sum(1/chiImp)


#average varaible importance
varIMP=cbind(glmImp,rfIMP,xgbImp,stabImp,varImp,chiImp)
moyImp=rowMeans(varIMP);colnames(varIMP)[1]="glmImp"
varIMP=cbind(varIMP,moyImp)
varIMP=varIMP[order(moyImp,decreasing = TRUE),]
write.table(varIMP,file = "impvariable.txt")
require(xlsx)
write.xlsx(varIMP[1:20,], "impvariable.xlsx") 

#
varIMP=read.table(file = "impvariable.txt",header=TRUE)
#importance_variables
nbmax=172
importance_variables=list()
for(i in 1:7){
  importance_variables[[i]]=rownames(varIMP[order(varIMP[,i],decreasing = TRUE),])
  importance_variables[[i]]= importance_variables[[i]][1:min(nbmax,length(which(varIMP[,i]!=0)))]
}

require(xtable)
print(xtable(varIMP[1:20,]))
