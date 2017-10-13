#semisupervised learning
library(SSL)
x<-rbind(btrain[,-iddf],btest[,-iddf])
#Suppose we know the first twenty observations of each class and we want to propagate
#these labels to unlabeled data.
# 1 setosa, 2 versicolor, 3 virginica
y<-btrain[,iddf]
known.label <-c(1:nrow(btrain))
f1<-sslLabelProp(x,y,known.label,graph.type="enn",epsilon = 0.5)
f2<-sslLabelProp(x,y,known.label,graph.type="knn",k =5)
f3<-sslLabelProp(x,y,known.label,graph.type="tanh",alpha1=-2,alpha2=1)
f4<-sslLabelProp(x,y,known.label,graph.type="exp",alpha = 1)

l=sslGmmEM(as.matrix(btrain[,-iddf]),c(btrain[,-iddf]), as.matrix(btest[,-iddf]), seed = 1234, improvement = 1e-04, p = 0.3)
yu<-sslSelfTrain(btrain[,-iddf],btrain[,iddf],btest[,-iddf],nrounds = 100,n=3000)

#self training
#Interdit pour une classe à cause de fonction de perte: softprob
selftrain=function(xl, yl, xu, n = 10, nrounds=100, max_dep = 4, lr = 0.3, pgamma=0.08,nthr=2)
{
  yu <- NULL
  seq <- NULL
  yu.prob=NULL
  num.class <- length(unique(yl))
  all.obs <- dim(xu)[1]
  remain <- 1:all.obs
  while ((is.null(seq)) || (length(seq) < all.obs)) {
    num <- min(dim(xu)[1], n)
    dtrain <- xgb.DMatrix(data = as.matrix(xl), label = yl)
    h <- xgb.train(data = dtrain, nrounds = nrounds, num_class = num.class, 
                   objective = "multi:softprob", max_depth = max_dep, eta = lr, silent = 1,gamma=pgamma,nthread=nthr)
    if (is.null(seq)) {
      pred <- matrix(predict(h, as.matrix(xu)), ncol = num.class, 
                     byrow = T)
    }else {
      pred <- matrix(predict(h, as.matrix(xu[remain, ])), 
                     ncol = num.class, byrow = T)
    }
    label <- sapply(1:dim(pred)[1], function(x) {
      which.max(pred[x, ])-1
    })
    label.prob <- sapply(1:dim(pred)[1], function(x) {
      max(pred[x, ])
    })
    new <- sort(label.prob, decreasing = T, index.return = T)$ix[1:num]
    xl <- rbind(xl, xu[remain[new], ])
    yl <- c(yl, label[new])
    yu <- c(yu, label[new])
    yu.prob=c(yu.prob,label.prob[new])
    seq <- c(seq, remain[new])
    remain <- setdiff(remain, seq)
  }
  seq <- sort(seq, index.return = T)$ix
  yu <- yu[seq]
  yu.prob=yu.prob[seq]
  re=list(yu,yu.prob)
  return(re)
}
re<-selftrain(as.matrix(btrain.complet[,-iddf]),as.vector(btrain.complet[,iddf]), as.matrix(btest.complet[,-iddf]),n = 50, nrounds=100, max_dep = 4, lr = 0.3, pgamma=0.08, nthr = 4)
plot(roc( btest.complet[,iddf],re[[2]]),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE)