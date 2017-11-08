#som non supervisé
### Continu de "traitement de donnée.R" 
source("Kohonenprep.R")
source("Kohonennet.R")
source("plotcarte1.R")
source("evaluation.R")
matd=do.call(cbind,btrain[,-iddf])
nr=8;nc=8
set.seed(15)
prof=kohonenqualigo(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1])
mtrain=kohonenqualiclass(prof[,-iddf],btrain[,-iddf],dim(btrain[,-iddf])[1])
mtest=kohonenqualiclass(prof[,-iddf],btest[,-iddf],dim(btest[,-iddf])[1])
superclu=table(btrain[,iddf],mtrain)
clus.c1=as.numeric(colnames(superclu)[superclu[2,]>superclu[1,]])
#cluster evaluation
require(mclust)
nmi(superclu)
purity(superclu)
adjustedRandIndex(c(btrain[,iddf]),c(mtrain))
nmi(table(btest[,iddf],mtest))
purity(table(btest[,iddf],mtest))
adjustedRandIndex(c(btest[,iddf]),c(mtest))

#plot 2D
grillecarte(nr,nc,2,btrain[,iddf],mtrain)
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
