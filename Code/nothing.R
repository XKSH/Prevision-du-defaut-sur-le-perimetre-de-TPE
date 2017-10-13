N1 <- length(which(breg1[,246]==0))
N2 <- nrow(breg1)-N1

# the points of sets A and B

#train=do.call(cbind,train)
breg1=breg1[order(breg1[,246]),]
breg1=breg1[,-246]

dim <- ncol(breg1)
P <-breg1

# the matrix A defining the lhs of the conditions
A <- cbind(P * c(rep(-1,N1),rep(1,N2)), c(rep(1,N1),rep(-1,N2)))

# the objective function - no optimization necessary
obj <- rep(0, dim+1)

# the vector b defining the rhs of the conditions
b <- rep(-1, N1+N2)

# by default GLPK assums positive boundaries for the
# variables. but we need the full set of real numbers.
bounds <- list(
  lower = list(ind = 1:(dim+1), val = rep(-Inf, dim+1)),
  upper = list(ind = 1:(dim+1), val = rep(Inf, dim+1))
)

# solving the linear program
s <- Rglpk_solve_LP(obj, A, rep("<=", N1+N2), b, bounds=bounds)
# status 0 means that a solution was found
if(s$status == 0) {
  cat("Linearly separable.")
} else {
  cat("Not linearly separable.")
}
tv=c(which(s$solution>0))


library(rpart)
PEdat.tree=cbind(PEquan[,3:(ncol(PEquan))],PEqual)
tree.train=PEdat.tree[traincand[,2],]
tree.train=cbind(tree.train,bmu)[order(bmu),]
tree.train1=as.data.frame(tree.train[which(tree.train[,ncol(tree.train)]%in%c(1)),-ncol(tree.train)])
tree.train1$top_dft_auto_pro_corp=factor(PEdat.tree1$top_dft_auto_pro_corp)
tree_fit = rpart(top_dft_auto_pro_corp ~ ., data = tree.train1,parms = list(prior = c(.9,.1), split = "information"))
#loss=matrix(c(0,3,1,0),byrow=TRUE, nrow=2),
#tree_fitp <- prune(tree_fit, cp = 0.05)
tree.test=PEdat.tree[-traincand[,2],]
tree.test=cbind(tree.test,bmu.test)[order(bmu.test),]
tree.test1=as.data.frame(tree.test[which(tree.test[,ncol(tree.test)]%in%c(1)),-ncol(tree.test)])

table(predict(tree_fit,tree.test1,type = "class"),tree.test1[,"top_dft_auto_pro_corp"])


PEdat.complet=do.call(cbind,PEdat.tree)
PEdat.complet[,-246]=as.numeric(PEdat.complet[,-246])
for(i in 1:ncol(PEdat.complet))
{
  PEdat.complet[is.na(PEdat.complet[,i]),i]=median(PEdat.complet[,i],na.rm=TRUE)
}
PEdat.complet[247:ncol(PEdat.complet)]=as.factor(PEdat.complet[247:ncol(PEdat.complet)])
#forest_train=PEdat.complet[traincand[,2],]
forest_train=data.frame(PEdat.complet[traincand[,2],])
forest_train=cbind(forest_train,bmu)[order(bmu),]
forest_train1=as.data.frame(forest_train[which(forest_train[,ncol(forest_train)]%in%c(1)),-ncol(forest_train)])
#forest_test=PEdat.complet[-traincand[,2],]
forest_test=data.frame(PEdat.complet[-traincand[,2],])
forest_test=cbind(forest_test,bmu.test)[order(bmu.test),]
forest_test1=as.data.frame(forest_test[which(forest_test[,ncol(forest_test)]%in%c(1)),-ncol(forest_test)])
set.seed(415)
fit <- randomForest(as.factor(forest_train1$top_dft_auto_pro_corp) ~ .,
                    data=forest_train1, 
                    importance=TRUE, 
                    ntree=2000,classwt=c(0.7,0.3))
prediction <- predict(fit,forest_test1)
table(prediction,forest_test1[,"top_dft_auto_pro_corp"])