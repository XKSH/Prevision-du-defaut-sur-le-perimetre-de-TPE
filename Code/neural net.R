set.seed(1234)
nn2 <- nnet(as.factor(V31) ~ ., data = dcode.train1, size = 20, rang = 0.1,
            decay = 5e-4, maxit = 300)
table(predict(nn2,dcode.train1 , type = "class"),dcode.train1[,31])

library(neuralnet)
set.seed(1234)
dcode.train1=as.data.frame(dcode.train1)
dcode.test1=as.data.frame(dcode.test1)
nn2 <- neuralnet(f,data = dcode.train1, hidden = c(30,15,8,2), lifesign = "minimal", 
                       linear.output = FALSE, threshold = 0.1,rep=10)
nn2.results=compute(nn2, dcode.test1[,-31])
table(nn2.results$net.result,dcode.train1[,31])
n <- colnames(dcode.train1)
f <- as.formula(paste("V31 ~", paste(n[!n %in% "V31"], collapse = " + ")))
plot(roc(dcode.test1[,31],nn2.results$net.result),col="green",lwd=3,main="Courbe ROC",print.auc=TRUE,xlim=c(1,0),ylim=c(0,1))

#auc:0.642