grillecarte=function(nr,nc,nb,obj1,obj2){
# Plot le grid 5x5
adr=expand.grid(1:nr,1:nc)
par(bg = "white",mai=c(0.2,0.2,0.6,0.8))
plot(c(0, nr), c(0, nc), xlab = "", ylab = "",axes=FALSE,xaxs="i",yaxs="i",main="Carte Kohonen")
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col ="white")
# Construire la palette de trois type
ncol=seq(0,240,length.out=nb);
# Faire varier les couleurs en utilisant hcl
vcol=ncol%*%(prop.table(table(obj1,obj2), 2))
vnames=c(as.numeric(colnames(vcol)))
rvcol=hcl(vcol,120,85)
# Affecter les couleurs aux noeuds
for(i in 1:(nr*nc)){
  if(i %in% vnames)
  rect(adr[i,1]-1,adr[i,2]-1,adr[i,1],adr[i,2],col=hcl(vcol[which(vnames==i)],120,85),lwd=0)
}
grid(nx = nr,ny=nc)
}
#grillecarte(4,4,3,irclust[,5],irclust[,6])
# Ajouter le legend dans la marge
#par(xpd=TRUE)
#legend("topright", inset=c(-0.16,0.2), title="Iris Type", c("setosa","versicolo","virginica"), pch=15,col=hcl(ncol,120,85),cex=0.55)

