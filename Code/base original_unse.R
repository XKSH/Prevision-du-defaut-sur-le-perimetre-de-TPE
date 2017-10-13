# Continu de "base original_s.R" ligne 26

source("Kohonenprep.R")
source("Kohonennet.R")
source("plotcarte1.R")
source("evaluation.R")
#petite technique;sinon le meilleur sera 3841 1642 130 59 sur 1.55/8 1.9/8 1/2
PEqual=read.table(file = "data_avb_quali.txt",header = TRUE,sep="\t")
disj1=as.matrix(acm.disjonctif(PEqual))
PEdat.org=cbind(PEquan.org[,3:(ncol(PEquan.org))],disj1)#[which(PEquan.org[,which(colnames(PEquan.org)%in%c("TOP_FILLE_CONSO.0"))]==c(1)),]
PEdat.org= PEdat.org[,colSums(is.na(PEdat.org))<nrow(PEdat.org)]
PEdat.org=PEdat.org[,c(which(!colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")), which(colnames(PEdat.org)%in%c("top_dft_auto_pro_corp")))]
ndisj=ncol(disj)+1+ncol(disj1)
btrain=PEdat.org[traincand[,2],]
btest=PEdat.org[-traincand[,2],]
btrain[,1:(ncol(btrain)-ndisj)]=apply(btrain[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
btest[,1:(ncol(btrain)-ndisj)]=apply(btest[,1:(ncol(btrain)-ndisj)],2,function(x){x[!is.na(x)]=scale(x[!is.na(x)]); return(x)})
#### pas de petite technique;Continu de "base original_s.R" ligne 35
iddf=which(colnames(PEdat.org) %in% c("top_dft_auto_pro_corp"))
#undersampling
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


#som non supervisé
matd=do.call(cbind,btrain[,-iddf])
nr=5;nc=5
prof=kohonenqualigo(17,nr,nc,0.4,0.1,3,3,matd[sample(nrow(matd)),],dim(matd)[1])
mtrain=kohonenqualiclass(prof[,-iddf],btrain[,-iddf],dim(btrain[,-iddf])[1])
mtest=kohonenqualiclass(prof[,-iddf],btest[,-iddf],dim(btest[,-iddf])[1])
superclu=table(btrain[,iddf],mtrain)
clus.c1=as.numeric(colnames(superclu)[superclu[2,]>superclu[1,]])
#cluster evaluation
nmi(superclu)
purity(superclu)
adjustedRandIndex(c(btrain[,iddf]),c(mtrain))
nmi(table(btest[,iddf],mtest))
purity(table(btest[,iddf],mtest))
adjustedRandIndex(c(btest[,iddf]),c(mtest))

#plot 2D
grillecarte1(nr,nc,2,btrain[,iddf],mtrain)
par(xpd=TRUE)
nb=2
ncol=seq(0,240,length.out=nb)
legend("topright", inset=c(-0.15,0.2), title="Groupe", c("Non-défaut","Défaut"), pch=15,col=hcl(ncol,120,85),cex=0.55)

#plot3d
plan=expand.grid(1:nr,1:nc)
x.bin=c(1:5);y.bin=c(1:5)
xmid <- 0.5*(2*x.bin-1)
ymid <- 0.5*(2*y.bin-1)
counts=rep(0,nr*nc);class=rep(0,nr*nc)
counts[as.numeric(names(table(mtest)))]=as.vector(table(mtest))
#counts[as.numeric(names(table(mtrain)))]=as.vector(table(mtrain))
counts=matrix(counts,nrow=5,ncol=5)
counts=t(counts)
class[clus.c1]=1

hist3D(x = xmid, y = ymid, z =counts, 
       zlim = c(0, max(counts)+200), zlab = "Nombre d' observation", bty= "f", 
       xlab = "Latitude",ylab = "Longitude",colkey = list(length = 0.2, width = 0.4, shift = 0.15,
       cex.axis = 0.8, cex.clab = 0.85),clab = c("","Nombre"),
       phi = 25, theta = -55, 
       shade = 0.2, col =NULL, border = "black",
       d = 1, ticktype = "detailed")

scatter3D(x=plan[,2], y=plan[,1],
          z = rep(max(counts)+200,length(plan[,1])),
          colvar = class, col = gg.col(100),
          add = TRUE,pch = 18 , colkey = list(length = 0.2, width = 0.4, shift = -0.15, 
                                              cex.axis = 0.8, cex.clab = 0.85,
                                              at = c(0,1), labels=c("Sain","Défaut")), 
          clab = c("","Classes"))
title(main="Répartition des observations")
#taux de défaut
bclu=table(btrain[,iddf],mtrain)
#bclu=table(btest[,iddf],mtest)
taux=rep(0,nr*nc)
taux[as.numeric(colnames(bclu))]=bclu[2,]/colSums(bclu)
taux=matrix(taux,nrow=nr)
taux=t(taux)
hist3D(x = xmid, y = ymid, z =taux, 
       zlim = c(0, max(taux)), zlab = "Taux de défaut", bty= "f", 
       xlab = "Latitude",ylab = "Longitude",colkey = list(length = 0.2, width = 0.4, shift = 0.15,
                                                          cex.axis = 0.8, cex.clab = 0.85),clab = c("","Taux"),
       phi = 25, theta = -55, 
       shade = 0.2, col =NULL, border = "black",
       d = 1, ticktype = "detailed")

scatter3D(x=plan[,2], y=plan[,1],
          z = rep(max(taux),length(plan[,1])),
          colvar = class, col = gg.col(100),
          add = TRUE,pch = 18 , colkey = list(length = 0.2, width = 0.4, shift = -0.15, 
                                              cex.axis = 0.8, cex.clab = 0.85,
                                              at = c(0,1), labels=c("Sain","Défaut")), 
          clab = c("","Classes"))
title(main="Répartition du défaut")