library(class)
#cl= factor(PEquan[,ncol(PEquan)])
cl= factor(c(rep(1,627)))
proto=list(x=prof,cl=factor(pcl))
matd[is.na(matd)] <- 0
matd1=matd[which(PEquan[,ncol(PEquan)]==1),]
cd2 = lvq1(matd1, cl, proto,niter = 100 * nrow(matd1), alpha = 0.1)
m=kohonenqualiclass(cd2$x,PEdat,dim(PEdat)[1])
iPEdat=cbind(PEdat,PEquan[,ncol(PEquan)])
iPEdat=cbind(iPEdat,m)[order(m),]
cc=table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)])
nmi(table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)]))
purity(table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)]))

which(cc[2,]>cc[1,])
cp=rep(1,100)
#Une boucle de LVQ1

for(i in 1:3){
  cp[as.numeric(c(colnames(cc)))]=rownames(cc)[apply(cc,2,which.max)]
  proto=list(x=cd2$x,cl=factor(cp))
  cd2 = lvq1(matd1, cl, proto,niter = 10 * nrow(matd), alpha = 0.03)
  m=kohonenqualiclass(cd2$x,PEdat,dim(PEdat)[1])
  iPEdat[,ncol(iPEdat)]=m
  cc=table(iPEdat[,(ncol(iPEdat)-1)],iPEdat[,ncol(iPEdat)])
}


#carte de la distribution de nb de observations
nb=rep(0,nr*nc)
nb[as.numeric(c(colnames(cc)))]=colSums(cc)
mnb=cbind(expand.grid(1:nr,1:nc),nb)
library(ggplot2)
ggplot(data = mnb, aes(Var1, Var2, fill = nb))+ggtitle("Nombre d'observations")+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red",mid="yellow",  
                       midpoint = 500,limit=c(0,900), space = "Lab",
                       name="Valeur") +
  theme_minimal()+ 
  theme(
    plot.title = element_text(size = rel(1.5),hjust = 0.5,margin=margin(t = 10, unit = "pt")),
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
        )
