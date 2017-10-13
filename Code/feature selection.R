require(caret)
# ####embedded model####
# #L1-based feature selection using lasso logistic ## Continu de "base original_s.R" ligne36
glmImp = varImp(fit, scale = FALSE,lambda =   fit$lambda[which.min(AICc)])
glmImp=100*glmImp/sum(glmImp)
# Tree based model-random forest ## Continu de "base original_s.R" ligne36
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
#stabImp[order(stabImp,decreasing = TRUE)]
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

#####wrapper model###
##
# #Recursive feature elimination (trop coûteux)
# require(doParallel)
# cl <- makeCluster(detectCores(), type='PSOCK')
# registerDoParallel(cl)
# require(caret)
# set.seed(10)
# subsets=c(5,10)
# ctrl <- rfeControl(functions = lrFuncs,
#                    method = "repeatedcv",
#                    repeats = 5,
#                    verbose = FALSE)
# rfe.x=btrain.complet[,-iddf];
# rfe.y=as.factor(btrain.complet[,iddf])
# levels(rfe.y)=c("sain","defaut")
# rfe.lrProfile <- rfe(rfe.x, rfe.y,
#                  sizes = subsets,
#                  rfeControl = ctrl)
# rfeImp=rfe.lrProfile$metric

#average varaible importance
varIMP=cbind(glmImp,rfIMP,xgbImp,stabImp,varImp,chiImp)
moyImp=rowMeans(varIMP);colnames(varIMP)[1]="glmImp"
varIMP=cbind(varIMP,moyImp)
varIMP=varIMP[order(moyImp,decreasing = TRUE),]
write.table(varIMP,file = "impvariable.txt")
require(xlsx)
write.xlsx(varIMP[1:20,], "impvariable.xlsx") 


varIMP=read.table(file = "impvariable.txt",header=TRUE)
importance_variables

require(xtable)
print(xtable(varIMP[1:20,]))
