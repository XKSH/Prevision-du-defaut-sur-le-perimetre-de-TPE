#nquan=c(as.numeric(which(sapply(heart.data, is.numeric))))
#nqual=c(as.numeric(which(sapply(heart.data, is.factor))))
dist.mix=function(x,y,nquan,nqual,weight){
  if (length(weight)!=length(x)||length(weight)!=length(y)) 
  stop("invalid weight")
  nna=c(intersect(which(!is.na(x)),which(!is.na(y))))
  n1=c(intersect(nna,nquan))
  n2=c(intersect(nna,nqual))
  x1=c(x[n1]*sqrt(weight[n1]))
  y1=c(y[n1]*sqrt(weight[n1]))
  # La distance euclidienne
  dist1=dist(rbind(t(x1),t(y1)))
  # La distance Hamming
  vlogic=c(as.numeric(x[n2]==y[n2]))
  dist2=t(vlogic)%*%weight[n2]
  dist=c(dist1)+c(dist2)
  dist
}

dist.weight=function(x,y,weight){
  newx=x*sqrt(weight)
  newy=y*sqrt(weight)
  distw=dist(rbind(t(newx),t(newy)))
  distw
}
# x,y sont des lignes d'un dataframe
distna.weight=function(x,y,weight){
  nna=intersect(which(!is.na(x)),which(!is.na(y)))
  newx=x[nna]*sqrt(weight[nna])
  newy=y[nna]*sqrt(weight[nna])
  distw=dist(rbind(t(newx),t(newy)))
  distw
}
