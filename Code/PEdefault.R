library(FactoClass)
this.dir = dirname("parent.frame(2)$ofile")
setwd(this.dir)
PEquan=read.table(file = "data_avb.txt",header = TRUE,sep="\t")
PEquan=PEquan[,c(1:6,8:ncol(PEquan),7)]
PEqual=read.table(file = "data_avb_quali.txt",header = TRUE,sep="\t")
disj=as.matrix(acm.disjonctif(PEqual))
#couche input
PEdat=cbind(PEquan[,3:(ncol(PEquan))],disj)#[which(PEquan[,"TOP_FILLE_CONSO0"]==c(1)),]
PEdat<- PEdat[,colSums(is.na(PEdat))<nrow(PEdat)]
#PEquan=cbind(PEquan[,3:(ncol(PEquan))],disj)[which(PEquan[,"TOP_FILLE_CONSO0"]==c(1)),]
#PEquan<- PEquan[,colSums(is.na(PEquan))<nrow(PEquan)]

#PEdat=cbind(PEquan[,(var_cand+2)],disj)
#couche output

source("Kohonenprep.R")
source("Kohonennet.R")
source("plotcarte1.R")
source("evaluation.R")
matd=do.call(cbind,PEdat)
nr=5;nc=5
prof=kohonenqualigo(17,nr,nc,0.4,0.1,3,3,matd[sample(nrow(matd)),],dim(matd)[1])
m=kohonenqualiclass(prof,PEdat,dim(PEdat)[1])
iPEdat=cbind(PEdat,PEquan[,ncol(PEquan)])
iPEdat=cbind(iPEdat,m)[order(m),]
grillecarte(nr,nc,2,iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)])
cc=table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)])
cc
nmi(table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)]))
purity(table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)]))

#imblanced class, coding,net choosing
#oversampling
nind=sample(which(PEquan[,ncol(PEquan)]==1),15000,replace=TRUE)
matd=rbind(PEdat,PEdat[nind,])
matd=do.call(cbind,PEdat)
prof=kohonenqualigo(17,nr,nc,0.4,0.1,3,3,matd[sample(nrow(matd)),],dim(matd)[1])
m=kohonenqualiclass(prof,PEdat,dim(PEdat)[1])
iPEdat=cbind(PEdat,PEquan[,ncol(PEquan)])
iPEdat=cbind(iPEdat,m[1:nrow(PEdat)])[order(m[1:nrow(PEdat)]),]

##Réseaux doubles
matd1=matd[which(PEquan[,ncol(PEquan)]==0),]
prof1=kohonenqualigo(17,nr,nc,0.4,0.1,3,3,matd1[sample(nrow(matd1)),],dim(matd1)[1])
m1=kohonenqualiclass(prof1,matd1,dim(matd1)[1])
matd2=matd[which(PEquan[,ncol(PEquan)]==1),]
matd2=apply(matd2, 2, rep, 10)
prof2=kohonenqualigo(17,nr,nc,0.4,0.1,3,3,matd2[sample(nrow(matd2)),],dim(matd2)[1])
m2=kohonenqualiclass(prof2,matd2[1:627,],627)
prof=prof1
prof[as.numeric(levels(factor(m2)))]=prof2[as.numeric(levels(factor(m2)))]
pcl= c(rep(0,100))
pcl[as.numeric(levels(factor(m2)))]=1
ptest=kohonenqualigo1(nr,nc,0.4,0.1,3,3,matd[sample(nrow(matd)),],dim(matd)[1],prof)
m=kohonenqualiclass(ptest,PEdat,dim(PEdat)[1])

#weighted
PEdat=cbind(PEquan[,c((var_cand+2),ncol(PEquan))],disj)
matd=do.call(cbind,PEdat)
#w=c(rep(1/(3*196),length(var_cand)),2/3,rep(1/(3*196),ncol(disj)))
w=c(rep(1/(3*304),245),2/3,rep(1/(3*304),ncol(disj)))
prof=kohonenqualigo.weight(17,nr,nc,0.04,0.01,2.99,0.65,matd[sample(nrow(matd)),],dim(matd)[1],w)
m=kohonenqualiclass.weight(prof,PEdat,dim(PEdat)[1],w)
iPEdat=cbind(PEdat,PEquan[,ncol(PEquan)])
iPEdat=cbind(iPEdat,m)[order(m),]
grillecarte(nr,nc,2,iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)])
cc=table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)])
cc
nmi(table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)]))
purity(table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)]))
