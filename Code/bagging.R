#test version (3/8,0.5,1/8) (2/8,0.5,2/8) (1/8,0.5,3/8)
comp=cbind(mtest,class.actual,class.pre)
comp=cbind(comp,class.pre)
comp=comp[order(comp[,2]),]
f=apply(comp[,3:5],1,function(x){names(sort(table(x),decreasing=TRUE)[1])})
f=as.numeric(f)
comp=cbind(comp,f)
table(f,comp[,2])
table(comp[,3],comp[,2])
table(comp[,4],comp[,2])
table(comp[,5],comp[,2])
table(comp[,6],comp[,2])
plot(roc(comp[,2], comp[,6]),col="purple",print.auc=TRUE,main="Courbe ROC")
plot(roc(comp[,2], comp[,4]),col="purple",print.auc=TRUE,main="Courbe ROC")

#
