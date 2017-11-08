source("Kohonenprep.R")
source("Kohonennet.R")
source("plotcarte1.R")
source("evaluation.R")
## Continu de "traitement de donnée.R" 
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
#undersampling;sous-échantillonnage
#La proportion 0.4 a l'air cool pour 5 fois 5 grid
set.seed(1578)
id1=sample(1:length(traincand[traincand[,4]==1,4]), 1.1*length(traincand[traincand[,4]==2,4]))
idu=c(traincand[id1,2],traincand[which(traincand[,4]==2),2])
idtrain=which(rownames(btrain)%in%idu)
butrain=btrain[idtrain,]
butrain1=btrain[-idtrain,]

#som supervisé (dossier original"backtest.R")
source("Kohonennet_weight.R")
source("dist.R")
nr=5;nc=5
matd=do.call(cbind,butrain)
w=c(rep(1.55/(8*(dim(butrain)[2]-ndisj)),(dim(butrain)[2]-ndisj)),rep(1.9/(8*(ndisj-1)),(ndisj-1)),1/2)
set.seed(15);matd=matd[sample(nrow(matd)),]
prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1],w)
bmu.w=kohonenqualiclass.weight(prof,butrain,dim(butrain)[1],w)
superclu=table(butrain[,iddf],bmu.w)
#identifier les clusters  dominés par classe défaillante
clus.c1=as.numeric(colnames(superclu)[superclu[2,]>superclu[1,]])
mtrain=kohonenqualiclass(prof[,-iddf],btrain[,-iddf],dim(btrain[,-iddf])[1])
mtest=kohonenqualiclass(prof[,-iddf],btest[,-iddf],dim(btest[,-iddf])[1])
#cluster evaluation
library(mclust)
nmi(superclu)
purity(superclu)
adjustedRandIndex(c(butrain[,iddf]),c(bmu.w))
nmi(table(btrain[,iddf],mtrain))
purity(table(btrain[,iddf],mtrain))
adjustedRandIndex(c(btrain[,iddf]),c(mtrain))
nmi(table(btest[,iddf],mtest))
purity(table(btest[,iddf],mtest))
adjustedRandIndex(btest[,iddf],mtest)
class.pre=rep(0,length(mtest))
class.pre[which(mtest%in%clus.c1)]=1
class.actual=btest[,iddf]  
table(class.pre,class.actual)
#confusion matrix
tre= as.vector(table(class.pre,class.actual))
confusionmatrix=function(y,met){
  TClass =factor(c(0, 0, 1, 1))
  PClass =factor(c(0, 1, 0, 1))
  Y =y
  df <- data.frame(TClass, PClass, Y)
  m=met
  library(ggplot2)
  ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
    geom_tile(aes(fill = Y), colour = "white") +labs(x="Classe réelle",y="Classe estimée")+
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
    scale_fill_gradient(low = "grey", high = "red") +
    theme_bw() + theme(legend.position = "none")+ggtitle(m)+theme(plot.title = element_text(hjust = 0.5))
}
confusionmatrix(tre,"SSOM")
